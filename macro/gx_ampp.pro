pro gx_ampp_event,event
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    widget_control,event.top,/destroy
    return
  End
  case strupcase(widget_info(event.id,/uname)) of
    else:
  endcase
end

pro gx_ampp,wgxampp,font=font,_extra=_extra
  gx_setfonts,font=font
  device, get_screen_size=scr
  if !version.os_family eq 'Windows' then begin
    if scr[0] lt 3200 then !defaults.font='lucida console*12' else !defaults.font='lucida console*24'
  endif else begin
    if scr[0] lt 3200 then !defaults.font='' else !defaults.font=''
  endelse
  tlb=widget_base(title='GX Automatic Production Pipeline Interface',/column,$
    mbar=mbar, /tlb_size_events, $
    /tlb_kill,uname='gx_ampp')
  wgxampp=cw_gxampp(tlb,_extra=_extra)
  widget_control,tlb,/realize
  XMANAGER, 'gx_ampp',tlb ,/no_block
end