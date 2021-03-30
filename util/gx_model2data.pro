pro gx_model2data_event,event
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    widget_control,event.top,/destroy
    return
  End
  case strupcase(widget_info(event.id,/uname)) of
    else:
  endcase
end

pro gx_model2data,font=font
  gx_setfonts,font=font
  device, get_screen_size=scr
  if !version.os_family eq 'Windows' then begin
    if scr[0] lt 3200 then !defaults.font='lucida console*12' else !defaults.font='lucida console*24'
  endif else begin
    if scr[0] lt 3200 then !defaults.font='' else !defaults.font=''
  endelse
  tlb=widget_base(title='gx_model2data',/column,$
    mbar=mbar, /tlb_size_events, $
    /tlb_kill)
  wgx2data=cw_gx2data(tlb)
  widget_control,tlb,/realize
  XMANAGER, 'gx_model2data',tlb ,/no_block
end