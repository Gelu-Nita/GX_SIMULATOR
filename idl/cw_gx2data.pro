function gx2data::INIT,wBase,uname=uname,frame=frame,plotman_obj=plotman_obj,_extra=_extra
 compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
      catch, /cancel
      MESSAGE, /INFO, !ERROR_STATE.MSG
      return, 0
  end
  if obj_valid(plotman_obj) then self.plotman=plotman_obj
  void=self->gxWidget::Init(wBase,self,frame=frame,KILL_NOTIFY='objGx2DataKill',_extra=_extra)
  return,1
end

pro gx2data__define
 struct_hide,{gx2data, inherits gxwidget, plotman:obj_new(),$
  beam:obj_new(),gxmap:obj_new(),refmap:obj_new(),sdevmap:obj_new(),metrics:obj_new()}
end

pro objGx2DataKill,wBase
  widget_control,wBase,get_uvalue=obj
  obj_destroy,obj
end

pro gx2data::CreatePanel,xsize=xsize,ysize=ysize
  device, get_screen_size=scr
  if not exist(xsize) then xsize = fix (scr[0] * .4)
  if not exist(ysize) then ysize = xsize * 1.1
  if ~obj_valid(self.plotman) then begin
    wPanelBase=widget_base(self.wBase,/row)
    wLeftBase=widget_base(wPanelBase,/column)
    wRightBase=widget_base(wPanelBase,/column)
    tbar= widget_base(wLeftBase, /row,/toolbar)
    w_file=WIDGET_BUTTON(tbar, VALUE='File', /MENU)
    wNewFits=widget_button(w_file,value='Import FITS map from file',uname='GXMAPCONTAINER:IMPORTFITS',event_func='IDLexWidget__HandleEvent')
    wNewMap=widget_button(w_file,value='Import MAP structure from file',uname='GXMAPCONTAINER:IMPORTMAP',event_func='IDLexWidget__HandleEvent')
    
    wPlotmanBase=widget_base(wLeftBase,/column,uvalue={mbar:widget_base(tbar,/row),w_file:w_file})
    self.plotman = obj_new ('plotman', mainbase=wPlotmanBase, /multi_panel, $
      wxsize=xsize, wysize=ysize, $
      colortable=colortable, $
      widgets=widgets, error=error)
    geom = widget_info (widgets.w_maindrawbase, /geom)

    w_splashdraw = widget_draw ( widgets.w_maindrawbase, xsize=geom.scr_xsize, ysize=geom.scr_ysize)
    state = { $
      w_file: w_file, $
      w_splashdraw: w_splashdraw, $
      widgets: widgets, $
      plotman_obj: self.plotman}
      
    parent=widget_info(self.wBase,/parent)
    if parent ne 0 then begin
      repeat begin
        tlb=parent
        parent=widget_info(tlb,/parent)
      endrep until parent eq 0l
    end

    widget_control,tlb,set_uvalue=state  
    wMapContainer=cw_objMapContainer(tbar,name='MAP_CONTAINER',plotman_obj=self.plotman)
    widget_control,wMapContainer,get_uvalue=objMapContainer
    widget_control,wNewFits,set_uvalue=objMapContainer
    widget_control,wNewMap,set_uvalue=objMapContainer
    wControlTab=WIDGET_TAB(wRightBase,/Align_top,UNAME='ControlTab',UVALUE='ControlTab',LOCATION=2)
    wGX2DataControl=Widget_Base(wControlTab,Title='Data To Model Image Comparison',UNAME='gx2data')
  endif else begin
   wGX2DataControl=self.wBase
  endelse
   ;font=!defaults.font
   wControlPanel=widget_base(wGX2DataControl,/column)
   wControlBase=widget_base(wControlPanel,/column,xsize=xsize,ysize=ysize)
   toolbar= widget_base(wControlBase, /row,/toolbar)
   geom = widget_info (wControlPanel, /geom)
   wConvolve=WIDGET_BUTTON(toolbar, VALUE='Convolve Selected', uname='CONVOLVE',font=font)
   wCompute=WIDGET_BUTTON(toolbar, VALUE='Compute Metrics', uname='COMPUTE',font=font)
   wSelectBase=widget_base(wControlBase,/row,/frame)
   wButtonBase=widget_base(wSelectBase,/column,/frame)
   button=widget_button(wButtonBase,VALUE='SELECT MODEL MAP',uname='MODEL:SELECT',font=font,/align_left,/frame)
   geom_button = widget_info(button, /geom)
   scr_xsize=geom_button.scr_xsize
   scr_ysize=geom_button.scr_ysize
   button=widget_button(wButtonBase,VALUE='SELECT REF MAP',uname='REF:SELECT',font=font,/align_left,xsize=scr_xsize,/frame)
   button=widget_button(wButtonBase,VALUE='SELECT SDEV MAP',uname='SDEV:SELECT',font=font,/align_left,xsize=scr_xsize,/frame)
   button=widget_button(wButtonBase,VALUE='SELECT BEAM MAP',uname='BEAM:SELECT',font=font,/align_left,xsize=scr_xsize,/frame)
   geom_button = widget_info (wButtonBase, /geom)
   scr_xsize=geom.scr_xsize-geom_button.scr_xsize

   wBase=widget_base(wSelectBase,/column,/frame)
   wlabel=widget_label(wbase,value='no model map selected',font=font,scr_xsize=scr_xsize,scr_ysize=scr_ysize,uname='MODEL:ID',/dynamic,/frame)
   wlabel=widget_label(wbase,value='no reference map selected',font=font,scr_xsize=scr_xsize,scr_ysize=scr_ysize,uname='REF:ID',/dynamic,/frame)
   wlabel=widget_label(wbase,value='no sdev map selected',font=font,scr_xsize=scr_xsize,scr_ysize=scr_ysize,uname='SDEV:ID',/dynamic,/frame)
   wlabel=widget_label(wbase,value='no beam map selected',font=font,scr_xsize=scr_xsize,scr_ysize=scr_ysize,uname='BEAM:ID',/dynamic,/frame)
   wMask=cw_objfield(wControlBase,value='12.00',unit='%',font=font,/frame,label='ROI MASK COUNTOUR LEVEL:',inc=1,xtextsize=6,uname='MASK:LEVEL')
end

function gx2data::select_map
  self.plotman->select
  panel_struct=self.plotman->get(/current_panel_struct)
  if size(panel_struct,/tname) eq 'STRUCT' then return, (*panel_struct.saved_data.data) else return, obj_new()
end
function gx2data::HandleEvent, event
  compile_opt hidden
  catch, error_status
  if error_status ne 0 then begin
    catch, /cancel
    void = dialog_message( $
      dialog_parent=event.top, $
      title='Error', $
      /error, $
      !error_state.msg + ' ' + !error_state.sys_msg $
      )
    return, self->Rewrite(event)
  endif
  case strupcase(widget_info(event.id,/uname)) of
    'MODEL:SELECT': begin
                      obj_destroy,self.gxmap
                      self.gxmap=obj_clone(self.select_map())
                      if valid_map(self.gxmap) then widget_control,widget_info(event.handler,find_by_uname='MODEL:ID'),set_value=self.gxmap->get(/id)
                    end
    'REF:SELECT': begin
                      obj_destroy,self.refmap
                      self.refmap=obj_clone(self.select_map())
                      if valid_map(self.refmap) then widget_control,widget_info(event.handler,find_by_uname='REF:ID'),set_value=self.refmap->get(/id)
                    end  
    'SDEV:SELECT': begin
                      obj_destroy,self.sdevmap
                      self.sdevmap=obj_clone(self.select_map())
                      if valid_map(self.sdevmap) then widget_control,widget_info(event.handler,find_by_uname='SDEV:ID'),set_value=self.sdevmap->get(/id)
                    end
    'BEAM:SELECT': begin
                      obj_destroy,self.beam
                      self.beam=obj_clone(self.select_map())
                      if valid_map(self.beam) then widget_control,widget_info(event.handler,find_by_uname='BEAM:ID'),set_value=self.beam->get(/id)
                    end   
    'CONVOLVE': begin
                      if valid_map(self.beam) then begin
                        map=self.select_map()
                        if valid_map(map) then begin
                          cmap=map->get(/map)
                          cmap.data=convol(cmap.data, self.beam->get(/data), /edge_zero)
                          cmap.id='Convolved '+cmap.id
                          omap=obj_new('map')
                          omap->setmap,0,cmap
                          omap->plotman,0,plotman_obj=self.plotman,nodup=0
                          obj_destroy,omap
                        endif
                      end
                    end   
     'COMPUTE': begin
                      if valid_map(self.refmap) and valid_map(self.gxmap) then begin
                       if valid_map(self.sdevmap) then sdev=self.sdevmap->get(/map)
                       widget_control,widget_info(event.handler,find_by_uname='MASK:LEVEL'),get_value=mask
                       self.metrics=gx_metrics_map(self.gxmap->get(/map),self.refmap->get(/map),sdev,mask=mask)
                       widget_control,widget_info(widget_info(event.top,find_by_uname='GXMAPCONTAINER:MENU'),/parent),get_uvalue=oMapContainer
                       oMapContainer->add,self.metrics,'GX METRICS '+atime(systime(/s))
                      end
                    end                                                        
    else:
  endcase
return,self->Rewrite(event)
end  


function cw_gx2data,Base,_extra=_extra
 obj=obj_new('gx2data',Base,_extra=_extra)
 obj->GetProperty,widget_id=widget_id
 return,widget_id
end
