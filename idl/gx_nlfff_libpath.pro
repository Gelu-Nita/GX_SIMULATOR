function gx_nlfff_libpath,update=update
  ;Returns the precompiled WinOS dll or build the NLFFF library on Linux, if not found
    if !version.os_family ne 'Windows' then begin
      binary_path=filepath('gx_binaries',root=getenv('IDL_TMPDIR'))
      lib_path=filepath('WWNLFFFReconstruction.so',root=binary_path)
      if keyword_set(update) and file_test(lib_path) then spawn,'rm '+lib_path
      if ~file_test(lib_path) then begin
        if not file_test(binary_path) then file_mkdir, binary_path
        make_path=filepath(filepath('Linux',root=filepath('source',root='nlfff')),root=getenv('IDL_TMPDIR'))
        src_root=filepath('nlfff',root=getenv('gxpath'))
        spawn, 'cp -R '+src_root+' '+getenv('IDL_TMPDIR')
        cd, make_path, current=cdr
        spawn, 'make'
        cd, '../../idl'
        spawn,'cp -R WWNLFFFReconstruction.so '+binary_path
        cd, cdr
        spawn,'rm -r '+filepath('nlfff',root=getenv('IDL_TMPDIR'))
      endif
      lib_path=file_test(lib_path)?lib_path:!null
    endif else lib_path=gx_findfile('WWNLFFFReconstruction.dll')
 return,lib_path
end 