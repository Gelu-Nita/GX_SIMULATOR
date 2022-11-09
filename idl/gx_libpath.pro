function gx_libpath,root,update=update,unix=unix
  ;Returns the precompiled WinOS name*.dll 
  ;returns the path to name*.so library on Linux if found in the var/tmp/gx_binaries, 
  ;or build it, if not found or /update is requested 
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
    tmpdir=keyword_set(unix)?curdir():getenv('HOME')
    binary_path=filepath('gx_binaries',root=tmpdir)
    log=filepath(root+'.log',root=binary_path)
    if ~file_test(filepath('gx_binaries',root=tmpdir)) then file_mkdir,binary_path
    source_lib=(file_search(root_path,'*.so',/fold))
    if n_elements(source_lib) ne 1 then begin
      linux_lib=(arm_lib=(x86_lib=''))
      arm_idx=where((strmatch(source_lib,'*arm*') eq 1),arm_count)
      if arm_count then arm_lib=source_lib[arm_idx]
      x86_idx=where((strmatch(source_lib,'*x86*') eq 1),x86_count)
      if x86_count then x86_lib=source_lib[x86_idx]
      linux_idx=where((strmatch(source_lib,'*x86*') eq 0) and (strmatch(source_lib,'*arm*') eq 0),linux_count)
      if linux_count then linux_lib=source_lib[linux_idx]
      source_lib=linux_lib
      if !version.os eq 'darwin' then begin
        source_lib=(!version.arch eq "x86_64")?x86_lib:arm_lib
      endif else source_lib=linux_lib
    endif else source_lib=source_lib[0]
    if file_test(source_lib) then begin
      libname=file_basename(source_lib)
      lib_path=filepath(libname,root=binary_path)
      if file_test(lib_path) and ~keyword_set(update) then return,lib_path 
      file_copy,source_lib,binary_path,/overwrite,/force
    endif else libname=''
    makefile=(file_search(root_path,'makefile',/fold))[0]
    if ~file_test(makefile) then begin
      if libname ne '' then lib_path=(file_search(binary_path,libname,/fold))[0]
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
    source_lib=(file_search(make_root,'*.so',/fold))[0]
    if file_test(source_lib) then begin
      file_move,source_lib,binary_path,/overwrite
      libname=file_basename(source_lib)
    endif else libname=''
    makefile=(file_search(make_root,'makefile',/fold))[0]
    cd,file_dirname(makefile),current=cdr
    spawn,'make >> '+log
    cd,cdr
    source_existed=file_test(source_lib)
    source_lib=(file_search(make_root,'*.so',/fold))[0]
    if file_test(source_lib) then begin
      file_copy,source_lib,binary_path,/overwrite
      libname=file_basename(source_lib)
      if ~source_existed then begin
        binpath=filepath('binaries'+path_sep(),root=root_path)
        if ~file_test(binpath) then file_mkdir,binpath
        file_copy,source_lib,binpath,/overwrite,/force
      endif
      lib_path=(file_search(binary_path,libname,/fold))[0]
      message,libname+ ' succesfully built and copied to '+binary_path,/info
    endif else begin
      if libname ne '' then begin
        message,libname +' could not be built on this system! The distribution library was copied instead to '+binary_path,/info
        lib_path=(file_search(binary_path,libname,/fold))[0]
      endif else message, 'fatal error: distributed library or makefile missing, no valid library path to be returned!',/info
    endelse
    if file_test(make_root) then spawn,('rm -r '+make_root)
  endelse
  return,file_test(lib_path[0])?lib_path:!null
end
