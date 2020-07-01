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
 struct_hide,{gx2data, inherits gxwidget, plotman:obj_new()}
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
   wControlPanel=widget_base(wGX2DataControl,/column)
   wControlBase=widget_base(wControlPanel,/column,xsize=xsize,ysize=ysize)
   toolbar= widget_base(wControlBase, /row,/toolbar)
   dummy=WIDGET_BUTTON(toolbar, VALUE='Menu Placeholder', /MENU,font=!defaults.font)
   
end

function cw_gx2data,Base,_extra=_extra
 obj=obj_new('gx2data',Base,_extra=_extra)
 obj->GetProperty,widget_id=widget_id
 return,widget_id
end
