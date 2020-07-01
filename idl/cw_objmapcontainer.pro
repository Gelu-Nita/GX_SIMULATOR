function gxMapContainer::INIT,wBase,uname=uname,frame=frame,plotman_obj=plotman_obj,_extra=_extra
 compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
      catch, /cancel
      MESSAGE, /INFO, !ERROR_STATE.MSG
      return, 0
  end
 void=self->IDL_Container::Init()
 void=self->gxWidget::Init(wBase,self,frame=frame,KILL_NOTIFY='objMapContainerKill',_extra=_extra)
 self.plotman=obj_valid(plotman_obj)?plotman_obj:obj_new('plotman')
 return,1
end

function gxMapContainer::GetPlotmanObj
 return,self.plotman
end
 
pro gxMapContainer::ADD,obj,name,k
  default,k,0
  self->IDL_Container::Add,obj
  wMenu=widget_info(self.wbase,find_by_uname='GXMAPCONTAINER:MENU')
  widget_control,wMenu,sensitive=1
  newItem=widget_button(wMenu,/menu,value=name,uvalue=obj)
  wRemove=widget_button(newItem,value='REMOVE THIS GROUP',uname='GXMAPCONTAINER:REMOVE',uvalue={omap:obj,button:newItem})
  wColorTable=widget_button(newItem,value='SET COLOR FOR THIS GROUP',uname='GXMAPCONTAINER:SETCOLOR',uvalue={omap:obj,plotman:self.plotman},/sep)
  wSave=widget_button(newItem,value='SAVE GROUP TO FILE',uname='GXMAPCONTAINER:SAVE',uvalue={omap:obj,button:newItem},/sep)
  count=obj->get(/count)
  maxcount=10
  idx=0
  if count gt maxcount then begin
   rootitem=newitem
   subcount=1
   newitem=widget_button(rootitem,/menu,value=strcompress(name+string(subcount,format="('--Group ',i4)")),uvalue=obj,/sep)
  end 
  for i=0,count-1 do begin
   map=obj->getmap(i)
   idx+=1
   if count gt maxcount and idx eq maxcount then begin
   idx=0
   subcount+=1
   newitem=widget_button(rootitem,/menu,value=strcompress(name+string(subcount,format="('--Group ',i4)")),uvalue=obj)
   end
   newTag=widget_button(newItem,value=map.id,uvalue={omap:obj,k:i,plotman:self.plotman},uname='GXMAPCONTAINER:SELECT',sep=((i eq 0) and (idx eq 1) and (count le maxcount))) 
  end
  obj->plotman,k,plotman_obj=self.plotman,/use_colors,nodup=0,desc=obj->get(k,/id)
end

pro gxMapContainer::REMOVE,obj,name
  self->IDL_Container::REMOVE,obj
  if self->count() eq 0 then widget_control,widget_info(self.wbase,find_by_uname='GXMAPCONTAINER:MENU'),sensitive=0
end

pro objMapContainerKill,wBase
 widget_control,wBase,get_uvalue=obj
 obj_destroy,obj
end

pro gxMapContainer__define
 struct_hide,{gxMapContainer, inherits IDL_Container, inherits gxWidget,plotman:obj_new()}
end

function cw_objMapContainer,Base,_extra=_extra
 obj=obj_new('gxMapContainer',Base,_extra=_extra)
 obj->GetProperty,widget_id=widget_id
 return,widget_id
end

pro cw_objMapContainer_test_event,event
 IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
  widget_control,event.top,/destroy
  return
 End
 case strupcase(widget_info(event.id,/uname)) of
   'MAP':begin
    file=dialog_pickfile(Display_name='Please select a file containing an IDL map structure',filter=['*.sav'],path=gx_findfile(folder='demo'),/must_exist) 
    if file eq '' then return
    break_file, file, dsk_log, dir, filename, ext
    restore,file
    omap=obj_new('map')
    for k=0,n_elements(map)-1 do begin
      omap->setmap,k,map[k]
    end
    widget_control,widget_info(widget_info(event.top,find_by_uname='GXMAPCONTAINER:MENU'),/parent),get_uvalue=oMapContainer
    oMapContainer->Add,omap,filename
    end
   else:
 endcase
end

pro cw_objMapContainer_test
 gx_defparms
 device, get_screen_size=scr
 if !version.os_family eq 'Windows' then begin
   if scr[0] lt 3200 then !defaults.font='lucida console*12' else !defaults.font='lucida console*24'
 endif else begin
   if scr[0] lt 3200 then !defaults.font='' else !defaults.font=''
 endelse
 tlb=widget_base(title='cw_objMapContainer_test',/column,$
                  mbar=mbar, /tlb_size_events, $
                  /tlb_kill)
 tbar= widget_base(tlb, /row,/toolbar)   
 w_file=WIDGET_BUTTON(tbar, VALUE='File', /MENU)
 wNewFits=widget_button(w_file,value='Import FITS map from file',uvalue=wMapContainer,uname='FITS')
 wNewMap=widget_button(w_file,value='Import MAP structure from file',uvalue=wMapContainer,uname='MAP')

 wPlotmanBase=widget_base(tlb,/column,uvalue={mbar:widget_base(tbar,/row),w_file:w_file})
 plotman_obj = obj_new ('plotman', mainbase=wPlotmanBase, /multi_panel, $
   wxsize=wxsize, wysize=wysize, $
   colortable=colortable, $
   widgets=widgets, error=error) 
   geom = widget_info (widgets.w_maindrawbase, /geom)
   scale=0.65
   w_splashdraw = widget_draw ( widgets.w_maindrawbase, xsize=geom.scr_xsize, ysize=geom.scr_ysize)
   state = { $
     w_file: w_file, $
     w_splashdraw: w_splashdraw, $
     widgets: widgets, $
     plotman_obj: plotman_obj}
  
   widget_control,tlb,set_uvalue=state   
   
 wMapContainer=cw_objMapContainer(tbar,name='MAP_CONTAINER',plotman_obj=plotman_obj)
 widget_control, wNewFits,set_uvalue= wMapContainer   
 widget_control, wNewMap,set_uvalue= wMapContainer               
 widget_control,tlb,/realize
 XMANAGER, 'cw_objMapContainer_test',tlb ,/no_block
end