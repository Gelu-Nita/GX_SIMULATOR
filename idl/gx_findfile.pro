function gx_findfile,file,folder=folder
 default,file,''
 default,folder,'bitmaps'
 which,'gx_simulator',outfile=outfile,/quiet
 gxpath=file_dirname(file_dirname(outfile,/mark),/mark)
 fullpath=(gxpath+folder+PATH_SEP()+file)[0]
 if file_exist(fullpath) then return,fullpath
  ;if not found above, try to find it in any of the subfoldrs
 fullpath=(file_search(gxpath+folder,file))[0]
 if file_exist(fullpath) then return,fullpath else return,!null
end