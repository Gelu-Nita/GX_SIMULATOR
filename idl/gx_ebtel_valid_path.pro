function gx_ebtel_valid_path,path,has_ddm=has_ddm,quiet=quiet
  CATCH, Error_status
  ;This statement begins the error handler:
  IF Error_status NE 0 THEN BEGIN
    goto,not_valid
    CATCH, /CANCEL
  ENDIF
  restore,gx_findfile(path)
  if ~isa(DEM_COR_RUN) or $
     ~isa(DEM_TR_RUN) or $
     ~isa(LOGTDEM) or $
     ~isa(LRUN) or $
     ~isa(QRUN) then goto, not_valid
  has_ddm=(n_elements(ddm_cor_run) eq n_elements(dem_cor_run)) and (n_elements(ddm_cor_run) eq n_elements(dem_cor_run))   
  return,1
  not_valid:
  if ~keyword_set(quiet) then message,'This is not a valid EBTEL file!',/info
  has_ddm=0
  return,0
end