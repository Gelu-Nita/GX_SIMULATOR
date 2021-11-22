pro gx_chmp_event,event
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    widget_control,event.top,get_uvalue=wgxchmp
    widget_control,wgxchmp,get_uvalue=obj
    answ=obj->SaveSolution(/question)
    if strupcase(answ) eq 'CANCEL' then return
    widget_control,event.top,/destroy
    return
  End
  case strupcase(widget_info(event.id,/uname)) of
    else:
  endcase
end

pro gxchmpTimerCallback,id,tlb
  if widget_valid(tlb) eq 0 then begin
    void=Timer.Cancel(id)
    return
  endif
  widget_control,tlb,get_uvalue=wgxchmp
  widget_control,wgxchmp,get_uvalue=obj
  obj->UpdateBridgeStatus
 end

pro gx_chmp,wgxchmp,fresh=fresh,font=font,_extra=_extra
  gx_setfonts,font=font
  device, get_screen_size=scr
  if !version.os_family eq 'Windows' then begin
    if scr[0] lt 3200 then !defaults.font='lucida console*12' else !defaults.font='lucida console*24'
  endif else begin
    if scr[0] lt 3200 then !defaults.font='' else !defaults.font=''
  endelse
  tlb=widget_base(title='EBTEL Coronal Heating Model Parameter Automatic Search Interface',/column,$
    mbar=mbar, /tlb_size_events, $
    /tlb_kill_request_events,uname='gx_chmp')
  if n_elements(_extra) eq 0 then begin
    if file_exist('gxchmp.ini') then begin
     if ~keyword_set(fresh) then restore,'gxchmp.ini'
      wgxchmp=cw_gxchmp(tlb,GXMpath=GXMpath,RefDataPath=RefDataPath,modDir=modDir,psDir=psDir,$
                       RefDataStruct=RefDataStruct,alist=alist,blist=blist,qlist=qlist,$
                       levels=levels,solution=solution, renderer=renderer,$
                       fov=fov,res=res,nBridges=nBridges,EBTELpath=EBTELpath)
    endif
  endif
  if n_elements(wgxchmp) eq 0 then wgxchmp=cw_gxchmp(tlb,_extra=_extra)
  widget_control,tlb,set_uvalue=wgxchmp
  widget_control,tlb,/realize
  XMANAGER, 'gx_chmp',tlb ,/no_block
  id = Timer.Set( 1.0, 'gxchmpTimerCallback', tlb,/REPEAT) 
end