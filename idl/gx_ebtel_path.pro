function gx_ebtel_path,path,ss=ss,default=default
  if keyword_set(default) then goto,default_path
  if file_exist(path) then begin
    setenv,(keyword_set(ss)?'ebtel_ss':'ebtel')+'='+path
    message,(keyword_set(ss)?'ebtel_ss':'ebtel')+' environment  path set to '+path,/cont
    return,path
  endif
  path=keyword_set(ss)?GETENV('ebtel_ss'):GETENV('ebtel')
  if file_exist(path) then return, path
  default_path:
  path=gx_findfile(filename,folder='userslib'+path_sep()+'aia'+path_sep()+'ebtel')+(keyword_set(ss)?'ebtel_ss.sav':'ebtel.sav')
  setenv,(keyword_set(ss)?'ebtel_ss':'ebtel')+'='+path
  message,(keyword_set(ss)?'ebtel_ss':'ebtel')+' environment  path set to '+path,/cont
  return,path
end