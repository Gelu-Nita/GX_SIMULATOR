function gx_nlfff_libpath
  ;Build NLFFF library on Linux if not found
    if !version.os_family ne 'Windows' then begin
      make_path=getenv('HOME')+'/nlfff/source/Linux
      lib_path=make_path+'/WWNLFFFReconstruction.so'
      if ~file_test(lib_path) then begin
        which,'gx_simulator',outfile=outfile,/quiet
        src_root=file_dirname(file_dirname(outfile,/mark))
        spawn, 'cp -R '+src_root+'/nlfff '+getenv('HOME')+'/'
        cd, make_path, current=cdr
        spawn, 'make'
        cd, cdr
      endif
      lib_path=file_test(lib_path)?lib_path:!null
    endif else lib_path=gx_findfile('WWNLFFFReconstruction.dll')
 return,lib_path
end 