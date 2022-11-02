function gx_ebtel_path,path,default=default,has_ddm=has_ddm,quiet=quiet
  if keyword_set(default) then goto,default_path
  if n_elements(path) eq 0 then goto, exit_point
  if gx_ebtel_valid_path(path,has_ddm=has_ddm,quiet=quiet) then goto,set_env
  path=gx_findfile(file_basename(path))
  if gx_ebtel_valid_path(path,has_ddm=has_ddm,quiet=quiet) then goto,set_env
  path=GETENV('ebtel')
  if gx_ebtel_valid_path(path,has_ddm=has_ddm,quiet=quiet) then goto,set_env
  default_path:
  path=gx_findfile('ebtel.sav')
  if ~gx_ebtel_valid_path(path,has_ddm=has_ddm,quiet=quiet) then begin
    message,'No valid ebtel path provided, no action taken! '+path,/info
  endif
  set_env:
  setenv,'ebtel='+path
  exit_point:
  path=GETENV('ebtel')
  valid=gx_ebtel_valid_path(path,has_ddm=has_ddm,quiet=quiet)
  if ~keyword_set(quiet) then message,'ebtel environment  path is set to '+path,/info
  return,path
end