function gx_findfile,file,folder=folder
 default,file,''
 default,folder,'bitmaps'
 which,'gx_simulator',outfile=outfile,/quiet
 gxpath=file_dirname(file_dirname(outfile,/mark),/mark)
 fullpath=(gxpath+folder+PATH_SEP()+file)[0]
 return,fullpath
end