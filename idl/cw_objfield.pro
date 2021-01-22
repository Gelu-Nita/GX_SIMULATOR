function objField::INIT,wParent,uname=uname,label=label,frame=frame,$
                             xtextsize=xtextsize,tfont=tfont,increment=increment, right_label=right_label,$
                             format=format,units=units,value=value,xlabelsize=xlabelsize,lfont=lfont,flat=flat,map=map,min=min,max=max,indicator=indicator,$
                             type=type,scr_labelsize=scr_labelsize,_extra=_extra
 compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
      catch, /cancel
      MESSAGE, /INFO, !ERROR_STATE.MSG
      return, 0
  end
 flat=n_elements(flat)?flat:1
 self.type=n_elements(type) gt 0?size(type,/type):size(1d,/type)
 self.min=n_elements(min)?min:-!values.d_infinity
 self.max=n_elements(max)?max:!values.d_infinity
 value=keyword_set(value)?value>self.min<self.max:0.0d
 self.increment=keyword_set(increment)?fix(increment,type=self.type):fix(0.1,type=self.type)
 self.units=keyword_set(units)?units:''
 self.format=keyword_set(format)?format:'(g0)'
 self.label=n_elements(label)?label:''
 self.xtextsize=keyword_set(xtextsize)?xtextsize>strlen(string(value,format=self.format)+self.units):strlen(string(value,format=self.format)+self.units)
 self.xlabelsize=n_elements(xlabelsize)?(xlabelsize>strlen(self.label))*12:strlen(self.label)*12
 default,tfont,!defaults.font
 default,lfont,!defaults.font

 void=self->IDLexWidget::Init(wParent,frame=frame)
 widget_control,self.wIDBase,set_uname=uname,map=map
 self.wBase = widget_base( $
    self.wIDBase, $
    /Row, $
    event_func='IDLexWidget__HandleEvent', $
    uvalue=self, $
    notify_realize='IDLexWidget__OnRealize', $
    KILL_NOTIFY='objFieldKill',$
    _extra=_extra)
    
    
   if ~keyword_set(right_label) then  wLabelBase = WIDGET_BASE(self.wBase, $
        /ALIGN_Left, $
        /Row, $
        XPAD=0, YPAD=0, SPACE=0)
   
   wTextBase = WIDGET_BASE(self.wBase, $
    /ALIGN_Left, $
    /COLUMN, $
    XPAD=0, YPAD=0, SPACE=0)
    
    wButBase = WIDGET_BASE(self.wBase, $
            /ALIGN_CENTER, $
            /COLUMN, $
            /TOOLBAR, $
            XPAD=0, YPAD=0, SPACE=0)
    

    if keyword_set(right_label) then wLabelBase = WIDGET_BASE(self.wBase, $
        /ALIGN_Left, $
        /Row, $
        XPAD=0, YPAD=0, SPACE=0)


    if n_elements(xlabelsize) gt 0 then xlabelsize=self.xlabelsize
    wLabel=widget_label(wLabelBase,value=self.label,font=lfont,uname='_label',/dynamic_resize,xsize=xlabelsize,scr_xsize=scr_labelsize)


    self.wText=widget_text(wTextBase,/editable,font=tfont,value=string(value,format=self.format)+self.units,uname='_text',xsize=self.xtextsize)


    ; Motif needs an extra 1 pixel padding around bitmaps.
    isMotif = !version.os_family ne 'Windows'
    isv8=(float(!version.release) ge 8.0)
if ~keyword_set(indicator) then begin
    if isv8 then begin
              bitmap = FILEPATH('spinup.bmp', SUBDIR=['resource','bitmaps'])
              self.wButUp = WIDGET_BUTTON(wButBase, $
                  /BITMAP, VALUE=bitmap,FLAT=flat, $
                  UNAME='_up', $
                  XSIZE=16+isMotif, YSIZE=10+isMotif)
          
              bitmap = FILEPATH('spindown.bmp', SUBDIR=['resource','bitmaps'])
              self.wButDown = WIDGET_BUTTON(wButBase, $
                  /BITMAP, VALUE=bitmap, FLAT=flat, $
                  UNAME='_down', $
                  XSIZE=16+isMotif, YSIZE=10+isMotif) 
    endif else begin
            bitmap = FILEPATH('spinup.bmp', SUBDIR=['resource','bitmaps'])
            self.wButUp = WIDGET_BUTTON(wButBase, $
                /BITMAP, VALUE=bitmap, $
                UNAME='_up', $
                XSIZE=16+isMotif, YSIZE=10+isMotif)
        
            bitmap = FILEPATH('spindown.bmp', SUBDIR=['resource','bitmaps'])
            self.wButDown = WIDGET_BUTTON(wButBase, $
                /BITMAP, VALUE=bitmap, $
                UNAME='_down', $
                XSIZE=16+isMotif, YSIZE=10+isMotif)    
    end      
 end          
 return,1
end

function objField::GetValue
compile_opt hidden
widget_control,self.wText, Get_Value=text
  if self.units ne '' then text=STRMID(text,0,strpos(text,self.units))
  return,(fix(text,type=self.type))[0]
end

pro objField::SetValue,value
compile_opt hidden
 widget_control,self.wText,Set_Value=string(value>self.min<self.max,format=self.format)+self.units
end

pro objField::SetProperty,value=value,increment=increment,units=units,min=min,max=max,format=format
 if n_elements(min) ne 0 then self.min=min
 if n_elements(max) ne 0 then self.max=max
 if n_elements(min) ne 0 or n_elements(max) ne 0 then begin
  value=self->GetValue()
  self->SetValue,value
 end
 if n_elements(value) ne 0 then self->SetValue,value
 if keyword_set(increment) then self.increment=increment
 if n_elements(units) ne 0 then self.units=units
 if n_elements(format) ne 0 then self.format=format
end

pro objField::GetProperty,value=value,increment=increment,units=units,format=format,label=label,$
xlabelsize=xlabelsize,xtextsize=xtextsize,min=min,max=max,_ref_extra=extra
 min=self.min 
 max=self.max
 value=self->GetValue()
 increment=self.increment
 units=self.units
 format=self.format
 label=self.label
 xtextsize=self.xtextsize
 if self.xlabelsize ne 0 then xlabelsize=self.xlabelsize
 self->IDLexWidget::GetProperty,_extra=extra
end

function objField::Rewrite, event
compile_opt hidden
return, {objFieldEVENT,id: self.wIDBase, top: event.top, handler:0L}
end

function objField::HandleEvent, event
compile_opt hidden
  case event.id of
   self.wText: self->SetValue,self->GetValue()
   self.wButUp: self->SetValue,self->GetValue()+self.increment
   self.wButDown: self->SetValue,self->GetValue()-self.increment
  else:
  endcase
  return, self->Rewrite(event)
end


pro objFieldKill,wBase
 widget_control,wBase,get_uvalue=obj
 obj_destroy,obj
end

pro objField__define
struct_hide,{objField,inherits idlexwidget,wBase:0l,wText:0l,wButUp:0l,wButDown:0l,increment:0.0d,$
format:'',units:'',label:'',xtextsize:0,xlabelsize:0,min:0d,max:0d,type:0}
end

function cw_objField,Base,_extra=_extra
 obj=obj_new('objField',Base,_extra=_extra)
 obj->GetProperty,widget_id=widget_id
 return,widget_id
end
