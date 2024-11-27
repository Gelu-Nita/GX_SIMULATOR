function gx_euv_response_valid_path,path, quiet=quiet
  CATCH, Error_status
  ;This statement begins the error handler:
  IF Error_status NE 0 THEN BEGIN
    goto,not_valid
    CATCH, /CANCEL
  ENDIF
  path=file_exist(path)?(file_basename(path)eq path?filepath(path,root=curdir()):path):gx_findfile(file_basename(path))
  if ~file_exist(path) then goto,not_valid
  restore,path
  ; to allow alternative gxresponse variable name found in the restored  file
  if size(gxresponse,/tname) eq 'STRUCT' then response=temporary(gxresponse)
  if size(response,/tname) ne 'STRUCT' then goto,not_valid
  if ~tag_exist(response,'date') then goto,not_valid
  if ~tag_exist(response,'logte') then goto,not_valid
  if ~tag_exist(response,'all') then goto,not_valid
  return,1
  not_valid:
  if ~keyword_set(quiet) then message,'This is not a valid EUV response path!',/info
  has_ddm=0
  return,0
end