function gx_libpath,name,update=update,unix=unix
  ;Returns the precompiled WinOS name*.dll 
  ;returns the path to name*.so library on Linux if found in the var/tmp/gx_binaries, 
  ;or build it, if not found or /update is requested 
  ;use /unix to force the unix branch on windows system, for testing purposes 
  if n_elements(name) eq 0 then return,!null
  if !version.os_family eq 'Windows' and ~keyword_set(unix) then begin
    lib_path32=gx_findfile(name+'*32.dll')
    lb_path32=lib_path32 ne !null?lib_path32:gx_findfile(name+'*.dll')
    lib_path64=gx_findfile(name+'*64.dll')
    lib_path64=lib_path64 ne !null?lib_path64:gx_findfile(name+'*.dll')
    lib_path=(!version.arch eq 'x86_64')?lib_path64:lib_path32
  endif else begin
     tmpdir=getenv('IDL_TMPDIR')
     binary_path=filepath('gx_binaries',root=tmpdir)
     if not file_test(binary_path) then file_mkdir, binary_path
     source_lib_path=gx_findfile(name+'*.so')
     lib_path=filepath(file_basename(source_lib_path),root=binary_path)
     if keyword_set(update) and file_test(lib_path) then spawn,'rm '+lib_path
     if ~file_test(lib_path) then begin
     root_path=file_dirname(file_dirname(source_lib_path))
     source_make_path=gx_findfile('makefile',folder=file_basename(root_path),/fold_case)
     file_copy,root_path,getenv('IDL_TMPDIR'),/overwrite,/force,/recursive
     make_path=file_dirname(filepath(str_replace(source_make_path,file_dirname(root_path)+path_sep(),''),root=tmpdir))
     tmp_lib_path=filepath(str_replace( source_lib_path,file_dirname(root_path)+path_sep(),''),root=tmpdir)
     spawn,'rm -r '+tmp_lib_path
     cd, make_path, current=cdr
     print,curdir()
     spawn, 'make'
     cd,cdr
     if file_test(tmp_lib_path) then file_copy,tmp_lib_path,lib_path,/overwrite,/force
     spawn,'rm -r '+filepath(file_basename(root_path),root=tmpdir)
     lib_path=file_test(lib_path)?lib_path:!null
     end
  endelse  
  return,lib_path
end