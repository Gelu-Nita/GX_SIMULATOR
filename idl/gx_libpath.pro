function gx_libpath_select_so,candidates
  ; Pick the platform-appropriate shared library from a file_search list.
  if n_elements(candidates) eq 0 then return,''
  if size(candidates,/type) ne 7 then return,''
  good=where(candidates ne '',ngood)
  if ngood eq 0 then return,''
  candidates=candidates[good]
  if n_elements(candidates) eq 1 then return,candidates[0]
  arm_idx=where(strmatch(candidates,'*arm*',/fold_case) eq 1,arm_count)
  x86_idx=where(strmatch(candidates,'*x86*',/fold_case) eq 1,x86_count)
  linux_idx=where((strmatch(candidates,'*x86*',/fold_case) eq 0) and $
                  (strmatch(candidates,'*arm*',/fold_case) eq 0),linux_count)
  if !version.os eq 'darwin' then begin
    if (!version.arch eq 'x86_64') and (x86_count gt 0) then return,candidates[x86_idx[0]]
    if (!version.arch ne 'x86_64') and (arm_count gt 0) then return,candidates[arm_idx[0]]
    ; Fall back to a generic .so name if no arch-tagged binary exists.
    if linux_count gt 0 then return,candidates[linux_idx[0]]
    return,candidates[0]
  endif
  if linux_count gt 0 then return,candidates[linux_idx[0]]
  return,candidates[0]
end

function gx_libpath_select_makefile,root_path
  ; Prefer source/makefile over repo-root / docs / test Makefiles.
  makefiles=file_search(root_path,'makefile',/fold)
  good=where(makefiles ne '',nmake)
  if nmake eq 0 then return,''
  makefiles=makefiles[good]
  if n_elements(makefiles) eq 1 then return,makefiles[0]
  sep=path_sep()
  source_pat='*'+sep+'source'+sep+'makefile'
  src_idx=where(strmatch(makefiles,source_pat,/fold_case) eq 1,nsrc)
  if nsrc ge 1 then return,makefiles[src_idx[0]]
  ; Next prefer any makefile whose parent directory is named source.
  parent=file_basename(file_dirname(makefiles))
  src_idx=where(strmatch(parent,'source',/fold_case) eq 1,nsrc)
  if nsrc ge 1 then return,makefiles[src_idx[0]]
  ; Prefer lowercase "makefile" (library builds) over "Makefile" (often tests/docs).
  low_idx=where(file_basename(makefiles) eq 'makefile',nlow)
  if nlow ge 1 then return,makefiles[low_idx[0]]
  return,makefiles[0]
end

function gx_libpath,root,update=update,unix=unix
  ;Returns the precompiled WinOS name*.dll
  ;returns the path to name*.so library on Unix if found under ~/gx_binaries,
  ;or builds it when missing or /update is requested.
  if n_elements(root) eq 0 then return,!null
  root_path=(file_search(getenv('gxpath'),root))[0]
  if ~file_test(root_path) then begin
    message,'fatal error: the root path provided does not exist! no valid library path to be returned!',/info
    return,!null
  endif
  lib_path=''
  if !version.os_family eq 'Windows' and ~keyword_set(unix) then begin
    lib_path=file_search(root_path,(!version.arch eq 'x86_64')?'*64*.dll':'*32*.dll')
    lib_path=(lib_path ne '')?lib_path:file_search(root_path,'*.dll')
  endif else begin
    tmpdir=(keyword_set(unix) ? curdir() : getenv('HOME'))
    binary_path=filepath('gx_binaries',root=tmpdir)
    log=filepath(root+'.log',root=binary_path)
    if ~file_test(binary_path) then file_mkdir,binary_path
    source_lib=gx_libpath_select_so(file_search(root_path,'*.so',/fold))
    libname=''
    if file_test(source_lib) then begin
      libname=file_basename(source_lib)
      lib_path=filepath(libname,root=binary_path)
      if file_test(lib_path) and ~keyword_set(update) then return,lib_path
      file_copy,source_lib,binary_path,/overwrite,/force
    endif
    makefile=gx_libpath_select_makefile(root_path)
    if ~file_test(makefile) then begin
      if libname ne '' then lib_path=filepath(libname,root=binary_path)
      if file_test(lib_path) then begin
        message,'warning: no makefile found! The distribution library was copyed instead to '+binary_path,/info
        return,lib_path
      endif else begin
        message,'fatal error: no distributed library or makefile found, no valid library path to be returned!',/info
        return,!null
      endelse
    endif
    make_root=filepath(file_basename(root_path),root=tmpdir)
    if file_test(make_root) then spawn,('rm -r '+make_root)
    message,'copying "'+root_path +'" to "'+make_root+'"',/info
    file_copy,root_path,tmpdir,/overwrite,/force,/recursive
    ; Relocate all distributed .so files out of the build tree so a failed
    ; make cannot be mistaken for a successful rebuild.
    dist_libs=file_search(make_root,'*.so',/fold)
    dist_good=where(dist_libs ne '',ndist)
    fallback_libname=libname
    if ndist gt 0 then begin
      dist_libs=dist_libs[dist_good]
      preferred=gx_libpath_select_so(dist_libs)
      if file_test(preferred) then fallback_libname=file_basename(preferred)
      for i=0L,n_elements(dist_libs)-1 do begin
        if file_test(dist_libs[i]) then $
          file_move,dist_libs[i],binary_path,/overwrite
      endfor
      if fallback_libname eq '' and file_test(preferred) then $
        fallback_libname=file_basename(preferred)
    endif
    makefile=gx_libpath_select_makefile(make_root)
    if ~file_test(makefile) then begin
      message,'fatal error: makefile disappeared after copy to '+make_root,/info
      if fallback_libname ne '' then begin
        lib_path=filepath(fallback_libname,root=binary_path)
        return,(file_test(lib_path) ? lib_path : !null)
      endif
      return,!null
    endif
    message,'using makefile: '+makefile,/info
    cd,file_dirname(makefile),current=cdr
    ; Capture stdout and stderr for diagnosis.
    spawn,'make > "'+log+'" 2>&1'
    cd,cdr
    built_lib=gx_libpath_select_so(file_search(make_root,'*.so',/fold))
    if file_test(built_lib) then begin
      libname=file_basename(built_lib)
      file_copy,built_lib,binary_path,/overwrite
      lib_path=filepath(libname,root=binary_path)
      message,libname+' succesfully built and copied to '+binary_path,/info
    endif else begin
      if fallback_libname ne '' then begin
        message,fallback_libname+' could not be built on this system! The distribution library was copied instead to '+binary_path,/info
        lib_path=filepath(fallback_libname,root=binary_path)
      endif else begin
        message,'fatal error: distributed library or makefile missing, no valid library path to be returned!',/info
        lib_path=''
      endelse
    endelse
    if file_test(make_root) then spawn,('rm -r '+make_root)
  endelse
  return,(file_test(lib_path[0]) ? lib_path : !null)
end
