pro gx_heat2volume_event,event
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    widget_control,event.top,/destroy
    return
  End
  case strupcase(widget_info(event.id,/uname)) of
    else:
  endcase
end

pro gx_heat2volume,volume,font=font
  default,volume,obj_new()
  if ~obj_isa(volume,'gxvolume') then begin
    message,'No valid gxVolume object provided as input, application aborted!',/cont
    return
  endif
  if ~volume->HasBL() then begin
    message,'Volume missing B-L pairs provided as input, application aborted!',/cont
    return
  endif
  gx_setfonts,font=font
  device, get_screen_size=scr
  if !version.os_family eq 'Windows' then begin
    if scr[0] lt 3200 then !defaults.font='lucida console*12' else !defaults.font='lucida console*24'
  endif else begin
    if scr[0] lt 3200 then !defaults.font='' else !defaults.font=''
  endelse
  tlb=widget_base(title='gx_heat2volume',/column,$
    mbar=mbar, /tlb_size_events, $
    /tlb_kill)
  wheat2volume=cw_heat2volume(tlb,volume)
  widget_control,wheat2volume,get_uvalue=h2v
  widget_control,tlb,/realize
  h2v->Display
  XMANAGER, 'gx_heat2volume',tlb ,/no_block
end