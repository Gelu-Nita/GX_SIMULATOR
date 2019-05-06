function objStatusBar::INIT,wParent,uname=uname,label=label,xsize=xsize,ysize=ysize,_extra=_extra
  compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return, 0
  end
  void=self->IDLexWidget::Init(wParent,_extra=_extra)
  widget_control,self.wIDBase,set_uname=uname
  self.wBase = widget_base( $
    self.wIDBase, $
    /Row, $
    event_func='IDLexWidget__HandleEvent', $
    uvalue=self, $
    notify_realize='IDLexWidget__OnRealize', $
    KILL_NOTIFY='objStatusBarKill',$
    _extra=_extra)
  statusbarbase=widget_base(self.wBase,/row,/align_center,/toolbar)
  self.wAbort=widget_button(self.wBase,value=gx_bitmap(gx_findfile('abort.bmp')),tooltip='Abort current action',/bitmap,uname='ABORT')
  self.wdraw = WIDGET_DRAW(statusbarbase,xsize=xsize-ysize*1.1,ysize=ysize,uname='DRAW')
  return,1
end

pro objStatusBar::Start,action=action
  compile_opt hidden
  default,action,'Undefined action'
  self.action=action+':'
  self.SetValue,0d
end

pro objStatusBar::SetValue,value
  compile_opt hidden
  default,value,0
  ; Make sure value is only between 0 and 1
  self.value=value<1>0
  per = self.value
  WIDGET_CONTROL, self.wDraw, GET_VALUE=win
  ; Set the draw window as the current window, erase it and draw
  ; the bar using normalized coordinates
  wset,win
  erase

  ; Handle some annoying differences between UNIX and PC behavior.
  ; For UNIX, set font to default hardware font.  I have found by
  ; trial and error that XOR only works for vector fonts on Win NT,
  ; so cannot use hardware default font on PC.  Also, setting color=0
  ; in XOR mode gives white on UNIX, but black on PC, so the color
  ; must be set differently on the two -- ARRGH.

  old_font = !p.font
  !p.font = 0
  white=!d.table_size-1
  charsize=2
  if n_elements(color) eq 0 then color=white
  if (strlowcase(os_family()) eq 'shit') then begin
    ; Set graphics mode to XOR and draw bar (XOR will make it contrast with bgnd)
    device,set_gr=20

    polyfill,[0,per,per,0], [0,0,1,1], /norm,color=0

    ; Write out the percent complete in the appropriate font.  I have found
    ; that vector fonts look lousy in XOR mode unless the font is decently
    ; large, so set charsize to 2.  In UNIX, this is ignored because we are
    ; using the default hardware font.
    
    xyouts, 0.5,0.2,/norm,charsize=charsize,$
      self.action+strcompress(fix(per*100))+'%',align=0.5,color=0
    device,set_gr=3

  endif else begin

    polyfill,[0,per,per,0], [0,0,1,1], /norm, color=white

    ; Write out the percent complete in the appropriate font.  I have found
    ; that vector fonts look lousy in XOR mode unless the font is decently
    ; large, so set charsize to 2.  In UNIX, this is ignored because we are
    ; using the default hardware font.

    if per lt 0.5 then color = white else color = 0
    xyouts, 0.5,0.2,/norm,charsize=charsize,$
      self.action+strcompress(fix(per*100))+'%',align=0.5,color=color
  endelse

  !p.font=old_font
end

function objStatusBar::GetValue,value
  return,self.value
end

pro objStatusBar::SetProperty,value=value,_extra=extra
  if n_elements(value) ne 0 then self->SetValue,value
  self->IDLexWidget::SetProperty,_extra=extra
end

pro objStatusBar::GetProperty,value=value,_ref_extra=extra
  value=self->GetValue()
  self->IDLexWidget::GetProperty,_extra=extra
end

function objStatusBar::Rewrite, event
  compile_opt hidden
  return, event;{objStatusBarEVENT,id: self.wIDBase, top: event.top, handler:0L}
end

function objStatusBar::HandleEvent, event
  compile_opt hidden
  case event.id of
    self.wAbort: begin
                  widget_control,self.wAbort,get_uvalue=oScanBox
                  if isa(oScanbox,'gxScanbox') then oScanBox->OnAbortScan
                 end
    else:
  endcase
  return, self->Rewrite(event)
end

pro objStatusBar::Erase
  widget_control,self.wDraw,get_value=win
  white=!d.table_size-1
  wset,win
  erase,white
end

function objStatusBar::AbortButton
  quit=widget_event(self.wAbort,/nowait)
  if (quit.id eq self.wAbort) then ret = 'Cancel' else ret=''
  if ret ne '' then message,'Action aborted!',/cont
  return,ret
end

pro objStatusBarKill,wBase
  widget_control,wBase,get_uvalue=obj
  obj_destroy,obj
  message,'StatusBar Destroyed',/cont
end

pro objStatusBar__define
  struct_hide,{objStatusBar,inherits idlexwidget,wBase:0l,action:'',value:0d,wAbort:0L,wDraw:0L}
end

function cw_objStatusBar,Base,_extra=_extra
  obj=obj_new('objStatusBar',Base,_extra=_extra)
  obj->GetProperty,widget_id=widget_id
  return,widget_id
end
