function gx_ebtel_valid_path,path
  CATCH, Error_status
  ;This statement begins the error handler:
  IF Error_status NE 0 THEN BEGIN
    goto,not_valid
    CATCH, /CANCEL
  ENDIF
  restore,path
  if ~isa(DEM_COR_RUN) or $
     ~isa(DEM_TR_RUN) or $
     ~isa(LOGTDEM) or $
     ~isa(LRUN) or $
     ~isa(QRUN) then goto, not_valid
  return,1
  not_valid:
  message,'This is not a valid EBTEL file!',/cont
  return,0
end