pro gx_message,msg,wConsole,overwrite=overwrite,prompt=prompt,_extra=_extra
  default,prompt,'% '
  if n_elements(wConsole) ne 0 then console= long(wConsole)
  if widget_valid(console) then begin
    if ~keyword_set(overwrite) then begin
      widget_control,console,get_value=txt
      txt=[txt,prompt+msg]
    endif else txt=prompt+msg
    widget_control,console,set_value=txt
  endif else message,msg,level=-1,/cont,_extra=_extra
end