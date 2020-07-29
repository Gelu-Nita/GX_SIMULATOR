function objArray::INIT,wParent,value=value,units=units,uname=uname,label=label,names=names,frame=frame,sensitive=sensitive,display=display,$
                             column=column,vertical=vertical,row=row,static=static,xlabelsizes=xlabelsizes,_extra=_extra
 compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
      catch, /cancel
      MESSAGE, /INFO, !ERROR_STATE.MSG
      return, 0
  end
 self.flat=n_elements(flat)?flat:1
 self.label=n_elements(label)?label:''
 value=keyword_set(value)?value:[0.0d]
 ilabels=strarr(n_elements(value))
 if self.label ne '' then begin
  for i=0,n_elements(ilabels)-1 do ilabels[i]=strcompress(self.label+string(i,format="('[',i2,']')"),/rem)
 end 
 if n_elements(names) eq n_elements(value) then ilabels=names
 case n_elements(units) of
  1: iunits=replicate(units,  n_elements(value))
  n_elements(value): iunits=units
  else: iunits=replicate('',  n_elements(value))
 endcase
 
 case n_elements(xlabelsizes) of
   1: ixlabelsizes=replicate(xlabelsizes,  n_elements(value))
   n_elements(value): ixlabelsizes=xlabelsizes
   else: ixlabelsizes=strlen(ilabels)
 endcase
 
 case n_elements(sensitive) of
   1: isensitive=replicate(sensitive,  n_elements(value))
   n_elements(value): isensitive=sensitive
   else: isensitive=replicate(1,  n_elements(value))
 endcase
 
 case n_elements(display) of
   1: idisplay=replicate(idisplay,  n_elements(value))
   n_elements(value): idisplay=display
   else: idisplay=replicate(1,  n_elements(value))
 endcase
  
 void=self->IDLexWidget::Init(wParent,frame=frame)
 widget_control,self.wIDBase,set_uname=uname,map=map
 self.wBase = widget_base( $
    self.wIDBase, $
    row=row,column=column, $
    event_func='IDLexWidget__HandleEvent', $
    uvalue=self, $
    notify_realize='IDLexWidget__OnRealize', $
    KILL_NOTIFY='objArrayKill');,$
 ;   _extra=_extra)

    wbase=widget_base(self.wbase,/row)

  if ~keyword_set(static) then begin   
    wButBase = WIDGET_BASE(wBase, $
        /ALIGN_CENTER, $
        /Row, $
        /TOOLBAR, $
        XPAD=0, YPAD=0, SPACE=0,/frame)
    ; Motif needs an extra 1 pixel padding around bitmaps.
    isMotif = !version.os_family ne 'Windows'
    isv8=(float(!version.release) ge 8.0)
    if isv8 then begin
          bitmap = FILEPATH('shift_left.bmp', SUBDIR=['resource','bitmaps'])
          self.wButLeft = WIDGET_BUTTON(wButBase, $
              /BITMAP, VALUE=bitmap,FLAT=self.flat, $
              UNAME='_up', $
              XSIZE=16+isMotif, YSIZE=10+isMotif)
      
          bitmap = FILEPATH('shift_right.bmp', SUBDIR=['resource','bitmaps'])
          self.wButRight = WIDGET_BUTTON(wButBase, $
              /BITMAP, VALUE=bitmap, FLAT=self.flat, $
              UNAME='_down', $
              XSIZE=16+isMotif, YSIZE=10+isMotif)
      
    endif else begin
            bitmap = FILEPATH('shift_left.bmp', SUBDIR=['resource','bitmaps'])
            self.wButLeft = WIDGET_BUTTON(wButBase, $
                /BITMAP, VALUE=bitmap, $
                UNAME='_up', $
                XSIZE=16+isMotif, YSIZE=10+isMotif)
        
            bitmap = FILEPATH('shift_right.bmp', SUBDIR=['resource','bitmaps'])
            self.wButRight = WIDGET_BUTTON(wButBase, $
                /BITMAP, VALUE=bitmap, $
                UNAME='_down', $
                XSIZE=16+isMotif, YSIZE=10+isMotif)
    endelse
    end
    self.wItemBase = WIDGET_BASE(wBase, $
                /ALIGN_Left, $
                column=keyword_set(vertical)?1:0, $
                row=keyword_set(vertical)?0:1, $
                XPAD=0, YPAD=0, SPACE=0,/frame)             
    for i=0,n_elements(value)-1 do begin
     item=cw_objField(self.wItemBase,value=value[i],units=iunits[i],label=ilabels[i],xlibelsize=ixlabelsizes[i],$
                      flat=self.flat,sensitive=isensitive[i],map=idisplay[i],_extra=_extra)
    end
    

 return,1
end


function objArray::GetValue
compile_opt hidden
items=widget_info(self.wItemBase,/all_children)
for i=0,n_elements(items)-1 do begin
 widget_control,items[i],get_value=v
 value=i eq 0?v:[value,v]
end
return,value
end

pro objArray::SetValue,value
compile_opt hidden
items=widget_info(self.wItemBase,/all_children)
for i=0,(n_elements(value)<n_elements(items))-1 do begin
 widget_control,items[i],set_value=value[i]
end
end

pro objArray::SetProperty,value=value,_extra=_extra
 if n_elements(value) ne 0 then self->SetValue,value
 self->IDLexWidget::SetProperty,_extra=extra
end

pro objArray::GetProperty,value=value,_ref_extra=extra
 value=self->GetValue()
 self->IDLexWidget::GetProperty,_extra=extra
end
;
function objArray::Rewrite, event
compile_opt hidden
return, {objFieldEVENT,id: self.wIDBase, top: event.top, handler:0L}
end

function objArray::HandleEvent, event
compile_opt hidden
  case event.id of
   self.wButLeft:  begin
                    items=widget_info(self.wItemBase,/all_children)
                    if n_elements(items) gt 1 then widget_control,items[n_elements(items)-1],/destroy
                   end
   self.wButRight: begin
                    items=widget_info(self.wItemBase,/all_children)
                    if self.label ne '' then label=strcompress(self.label+string(n_elements(items),format="('[',i2,']')"),/rem)
                    widget_control,widget_info(items[0],/child),get_uvalue=obj
                    text_geo=widget_info(widget_info(items[0],find_by_uname='_text'),/geometry)
                    tfont=widget_info(widget_info(items[0],find_by_uname='_text'),/fontname)
                    label_geo=widget_info(widget_info(items[0],find_by_uname='_label'),/geometry)
                    lfont=widget_info(widget_info(items[0],find_by_uname='_label'),/fontname)
                    obj->GetProperty,format=format,xtextsize=xtextsize,xlabelsize=xlabelsize
                    new=cw_objField(self.wItemBase,label=label,flat=self.flat,format=format,$
                    lfont=lfont,tfont=tfont,xtextsize=xtextsize,xlabelsize=xlabelsize)
                   end
  else:
  endcase
  return, self->Rewrite(event)
end


pro objArrayKill,wBase
 widget_control,wBase,get_uvalue=obj
 obj_destroy,obj
end

pro objArray__define
struct_hide,{objArray,inherits idlexwidget,wBase:0l,wItemBase:0l,wButLeft:0l,wButRight:0l,label:'',flat:0}
end

function cw_objArray,Base,_extra=_extra
 obj=obj_new('objArray',Base,_extra=_extra)
 obj->GetProperty,widget_id=widget_id
 return,widget_id
end