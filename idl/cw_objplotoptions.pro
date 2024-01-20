function objPlotOptions::INIT,wParent,title=title,uname=uname,map=map,$
         label=label,frame=frame,xlog=xlog,ylog=ylog,zlog=zlog,charsize=charsize, $
         _extra=_extra
 compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
      catch, /cancel
      MESSAGE, /INFO, !ERROR_STATE.MSG
      return, 0
  end
 default,uname,'PlotOptions'
 default,title,uname
 void=self->IDLexWidget::Init(wParent,frame=frame)
 widget_control,self.wIDBase,set_uname=uname,map=map
 self.wBase = widget_base( $
    self.wIDBase, $
    /Row, $
    event_func='IDLexWidget__HandleEvent', $
    uvalue=self, $
    notify_realize='IDLexWidget__OnRealize', $
    KILL_NOTIFY='objPlotOptionsKill',$
    _extra=_extra)
    
    OptionBase = WIDGET_BASE(self.wBase,/Column,frame=1)
    wTitle=widget_label(OptionBase,value=title,_extra=_extra)
    ExtraBase=widget_base(OptionBase,/row)
    self.wRange=widget_combobox(ExtraBase,value=['Auto','Manual'],_extra=_extra)
    self.wScale=widget_combobox(ExtraBase,value=['Log-Log','XLog','YLog','Lin-Lin'],_extra=_extra)
    if keyword_set(xlog) then widget_control,self.wScale,set_combobox_select=1
    if keyword_set(ylog) then widget_control,self.wScale,set_combobox_select=2
    if keyword_set(xlog) and keyword_set(ylog)then widget_control,self.wScale,set_combobox_select=0
    if ~keyword_set(xlog) and ~keyword_set(ylog)then widget_control,self.wScale,set_combobox_select=3
    self.wRangeBase=widget_base(OptionBase,/column,sensitive=0)
    self.wXmin=cw_objField(self.wRangeBase,value=0.0,label='Xmin',/flat,format='(g10.3)')
    self.wXmax=cw_objField(self.wRangeBase,value=0.0,label='Xmax',/flat,format='(g10.3)')
    self.wYmin=cw_objField(self.wRangeBase,value=0.0,label='Ymin',/flat,format='(g10.3)')
    self.wYmax=cw_objField(self.wRangeBase,value=0.0,label='Ymax',/flat,format='(g10.3)')  
    if keyword_set(charsize) then $
    self.wCharSize=cw_objField(OptionBase,value=float(charsize),label='Charsize  ',/flat,format='(g4.2)')  
    if n_elements(zlog) ne 0 then begin
      self.wZlog=cw_bgroup(OptionBase,/non,' Image Log Scale')
      widget_control,self.wZlog,set_value=keyword_set(zlog)
    endif
 return,1
end

pro objPlotOptions::SetProperty,auto=auto,manual=manual,scale=scale,xrange=xrange,yrange=yrange
 if keyword_set(auto) eq 1 then begin
  widget_control, self.wRange, SET_COMBOBOX_SELECT=0
  widget_control,self.wRangeBase,sensitive=0 
 endif
 if keyword_set(manual) eq 1 then begin
  widget_control, self.wRange, SET_COMBOBOX_SELECT=1
  widget_control,self.wRangeBase,sensitive=1 
 endif
 if size(scale,/tname) eq 'STRING' then begin
  case strlowcase(scale) of
    'log-log':select=0
    'xlog':select=1
    'ylog':select=2
    else: select=3
  endcase
  widget_control,self.wScale,SET_COMBOBOX_SELECT=select
 endif
 
 if n_elements(xrange) eq 2 then begin
  widget_control,self.wXmin,Set_Value=xrange[0]
  widget_control,widget_info(self.wXmin,/child),get_uvalue=obj
  obj->SetProperty,increment=(xrange[1]-xrange[0])/10
  widget_control,self.wXmax,Set_Value=xrange[1]
  widget_control,widget_info(self.wXmax,/child),get_uvalue=obj
  obj->SetProperty,increment=(xrange[1]-xrange[0])/10
 end
 if n_elements(yrange) eq 2 then begin
  widget_control,self.wYmin,Set_Value=yrange[0]
  widget_control,widget_info(self.wYmin,/child),get_uvalue=obj
  obj->SetProperty,increment=(yrange[1]-yrange[0])/10
  widget_control,self.wYmax,Set_Value=yrange[1]
  widget_control,widget_info(self.wYmax,/child),get_uvalue=obj
  obj->SetProperty,increment=(yrange[1]-yrange[0])/10
 end
end

pro objPlotOptions::GetProperty,value=value,range=range,scale=scale,xrange=xrange,yrange=yrange,xlog=xlog,ylog=ylog,zlog=zlog,charsize=charsize,_ref_extra=extra
 value=self
 range=widget_info(self.wRange,/combobox_gettext)
 scale=widget_info(self.wScale,/combobox_gettext)
 case scale of
 'Log-Log':begin
            xlog=1
            ylog=1
           end 
 'XLog': begin
          xlog=1
          ylog=0
         end  
 'YLog': begin
          xlog=0
          ylog=1
         end       
  else: begin
          xlog=0
          ylog=0
         end
 end 
 widget_control,self.wXmin,Get_Value=Xmin
 widget_control,self.wXmax,Get_Value=Xmax
 if range eq 'Manual' then xrange=[xmin,xmax] else if n_elements(xrange) gt 0 then dummy=temporary(xrange)
 widget_control,self.wYmin,Get_Value=Ymin
 widget_control,self.wYmax,Get_Value=Ymax
 if range eq 'Manual' then yrange=[ymin,ymax] else if n_elements(yrange) gt 0 then dummy=temporary(yrange)
 if widget_valid(self.wCharsize) then widget_control,self.wCharsize,get_value=charsize
 if widget_valid(self.wZlog) then widget_control,self.wZlog,get_value=zlog
 if n_elements(zlog) eq 1 then zlog=zlog[0]
 self->IDLexWidget::GetProperty,_extra=extra
end


function objPlotOptions::Rewrite, event
compile_opt hidden
return, {objPlotOptionsEVENT,id: self.wIDBase, top: event.top, handler:0L}
end

function objPlotOptions::HandleEvent, event
compile_opt hidden
  case event.id of
   self.wRange: widget_control,self.wRangeBase,sensitive=event.index              
  else:
  endcase
  return, self->Rewrite(event)
end

pro objPlotOptionsKill,wBase
 widget_control,wBase,get_uvalue=obj
 obj_destroy,obj
end

pro objPlotOptions__define
struct_hide,{objPlotOptions,inherits idlexwidget,wBase:0l,wRange:0L,wRangeBase:0L,wScale:0L,wXmin:0L,wXmax:0L,wYmin:0L,wYmax:0L,wZlog:0L,wCharSize:0l}
end

function cw_objPlotOptions,Base,_extra=_extra
 obj=obj_new('objPlotOptions',Base,_extra=_extra)
 obj->GetProperty,widget_id=widget_id
 return,widget_id
end