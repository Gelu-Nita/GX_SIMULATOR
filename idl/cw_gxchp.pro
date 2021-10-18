function gxchp::INIT,wBase,uname=uname,_extra=_extra
  compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return, 0
  end
  self.WinOS=!version.os_family eq 'Windows'
  void=self->gxWidget::Init(wBase,self,KILL_NOTIFY='objgxchpKill',_extra=_extra)
  self->CheckOS
  return,1
end

pro gxchp__define
  struct_hide,{gxchp, inherits gxwidget,WorkDir:'',psDir:'',modDir:'',ref:obj_new(),WinOS:0l}
end

pro objgxchpKill,wBase
  widget_control,wBase,get_uvalue=obj
  obj_destroy,obj
end

pro gxchp::CheckOS
  if ~keyword_set(self.WinOS) then begin
   ;just in case platform specific action is needed
  end
end

pro gxchp::CreatePanel,xsize=xsize,ysize=ysize
  device, get_screen_size=scr
  if not exist(xsize) then xsize = fix (scr[0] * .35)
  if not exist(ysize) then ysize = xsize *1.1

  wgxamppControl=self.wBase
  wControlPanel=widget_base(wgxamppControl,/column,xsize=scr[0],ysize=scr[1],$
    x_scroll_size=xsize,y_scroll_size=ysize,/scroll)
  geom = widget_info (wControlPanel, /geom)
  scr_xsize=0.98*geom.scr_xsize
  scr_ysize=0.98*geom.scr_ysize
  wControlBase=widget_base(wControlPanel,/column,uname='control_base')

  wWorkDirBase=widget_base(wControlBase,/row,scr_xsize=scr_xsize,/frame)
  wWorkBase=widget_base(wWorkDirBase,/row)
  wlabel=widget_label(wWorkBase,value='Working Directory    ')
  label=widget_info(wlabel,/geometry)
  label_scr_xsize=label.scr_xsize
  wSelectWorkDir= widget_button(wWorkBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Select root working directory',uname='workdir_select')
  geom = widget_info (wWorkBase, /geom)
  wWorkDirPath=widget_text(wWorkDirBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='WorkDir',/editable,$
    value=curdir())

end


function cw_gxchp,Base,_extra=_extra
  obj=obj_new('gxchp',Base,_extra=_extra)
  obj->GetProperty,widget_id=widget_id
  return,widget_id
end