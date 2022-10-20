function gx_findfile,file,folder=folder,_extra=_extra
 default,file,''
 ;default,folder,'bitmaps'
 default,folder,''
 which,'gx_simulator',outfile=outfile,/quiet
 gxpath=file_dirname(file_dirname(outfile,/mark),/mark)
 fullpath=(gxpath+folder+((file ne '')?PATH_SEP():'')+file)[0]
 if ~file_exist(fullpath) or ((file_info(fullpath)).DIRECTORY eq !true) then begin
  ;if not found above, try to find it in any of the subfolders
 fullpath=(file_search(gxpath+folder,file,_extra=_extra))[0]
 if ~file_exist(fullpath) or ((file_info(fullpath)).DIRECTORY eq !true)then return,!null else return,fullpath
 endif else return,fullpath
end