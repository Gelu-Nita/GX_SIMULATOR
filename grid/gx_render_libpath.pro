function gx_render_libpath,update=update
  ;Returns the precompiled WinOS RenderIrregular.dll or build the RenderIrregular.so library on Linux, if not found
  gx_path=getenv('gxpath')
  tmpdir=getenv('IDL_TMPDIR')
  source_path=filepath('grid',root=gx_path)
  make_path=filepath('grid',root=tmpdir)
  binary_path=filepath('gx_binaries',root=tmpdir)
  if !version.os_family eq 'Windows' then begin
    lib_path=source_path+'RenderIrregular'+((!version.arch eq 'x86_64') ? '64' : '32')+'.dll'
  endif else begin
    lib_path=filepath('RenderIrregular.so',root=binary_path)
    if keyword_set(update) and file_test(lib_path) then spawn,'rm '+lib_path
    if ~file_test(lib_path) then begin
      spawn, 'cp -R '+source_path+' '+tmpdir
      cd,make_path,current=cdr
      spawn,'make'
      spawn,'cp -R RenderIrregular.so '+binary_path
      cd,cdr
      spawn,'rm -r '+make_path
    end  
    lib_path=file_test(lib_path)?lib_path:!null
  endelse  
  return,lib_path
end