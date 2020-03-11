pro gx_explorer_event,event
 widget_control,widget_info(event.top,Find_By_Uname='STATEBASE'),get_uvalue=state
 case event.id of
  state.wVolumeSelect:begin
                       state.oVolume->GetVertexAttributeData,'parms',parms
                       print,total(parms[*,*,*,event.index],/d)
                       state.oVolume->SetProperty,data0=bytscl(parms[*,*,*,event.index])
                       state.ObjviewWid->Draw
                      end
  state.wFOV:begin
        state.ObjviewWid->GetProperty,widget_id=widget_id
        wDraw=widget_info(widget_id,find_by_uname='xobjview:draw')
        widget_control, wDraw, get_value=oWindow
        state.VolView->Reset, /full, oWindow 
        state.ObjviewWid->Draw
       end  
  state.wUpload:begin
                  file=file_exist(file)?file:dialog_pickfile(Title='Please choose a GX Simulator log file to upload',filter='*.gxl')
                   if file ne '' then begin
                     if ~file_exist(file) then return
                     widget_control,state.self,/destroy
                     GX_Explorer,file
                   end
                end 
  state.wWrite:answ=dialog_message('SORRY, THIS FEATURE IS NOT IMPLEMENTED YET!')                  
 else:
 end
end 


pro gx_explorer,file
defsysv,'!DEFAULTS',EXISTS=exists
if not exists then gx_defparms
xy= GET_SCREEN_SIZE(RESOLUTION=resolution)
if xy[0] lt 3200 then fontsize=12 else fontsize=24
!defaults.font=strcompress('lucida console*'+string(fontsize),/rem)
Widget_Control, DEFAULT_FONT=!defaults.font
  if ~file_exist(file) then begin
    file=dialog_pickfile(Title='Please choose a GX Simulator log file to upload',filter='*.gxl')
    if ~file_exist(file) then return
  end
  rec=MULTI_RESTORE(lun,file=file, header=header,/new,/verb)
  stat=fstat(lun)
  ny=(stat.size-stat.cur_ptr)/n_tags(rec,/data)
  parmdim=size(rec.parms,/dim)
  parmdim=[parmdim[0],ny,parmdim[1:*]]
  datadim=size(rec.data,/dim)
  datadim[1]=ny
  row=lonarr(ny)
  parms=make_array(parmdim,/float)
  data=make_array(datadim,/float)
  for i=0,ny-1 do begin
    rec=MULTI_RESTORE(lun,file=file)
    row=rec.row
    if row lt ny then begin
      parms[*,row,*,*,*]=rec.parms
      data[*,row,*,*,*,*]=rec.data
    end
  end
  close,lun
 mainbase=widget_base(title='GX Explorer',mbar=mbar,/column,UNAME='MAINBASE');,/TLB_KILL_REQUEST_EVENTS)
 statebase=widget_base(mainbase,/row,uname='STATEBASE')
 imgBase=widget_base(statebase)
 viewBase=widget_base(statebase) 
 ViewTab=WIDGET_TAB(ViewBase,/Align_top,UNAME='ControlTab',UVALUE='ControlTab',LOCATION=2)
 wVolumeView=Widget_Base(ViewTab,Title='VOLUME VIEW',/column)
 wProfiles=Widget_Base(ViewTab,Title='PROFILE PLOTS')
 wPlotBase=widget_base(wProfiles,/column)  
 oWid=obj_new('gxImgViewWid',ImgBase,wToolbarbase=wToolbarbase,wPlotBase=wPlotBase,$
               info=header.info,renderer=tag_exist(header,'renderer')?header.renderer:'',$
               nx=header.nx,ny=header.ny,xrange=header.xrange,yrange=header.yrange,$
               fovmap=tag_exist(header,'fovmap')?header.fovmap:obj_new(),/upload)
 oWid->GetProperty,wUpload=wUpload
 if ~obj_valid(oWid) then return
 oWid->MoveData,data
 wToolbarBase=widget_base(wVolumeView,/row,/toolbar,/frame)
 wFOV=widget_button(widget_base(wToolbarBase,/toolbar),value=filepath('image.bmp', subdirectory=['resource', 'bitmaps'] ), $
              /bitmap,tooltip='Reset to FOV',uname='FOV',EVENT_PRO='GX_Explorer_EVENT')
 VolView = obj_new('IDLexVolview')
 sz=size(parms)
 oVolume=IDLgrVolume(bytscl(parms[*,*,*,sz[4]-1]))
 oVolume->SetVertexAttributeData,'parms',temporary(parms)
 VolView->Add,oVolume
 ObjViewWid = obj_new('IDLexVolviewWid', $
        wVolumeView, $
        VolView, $
        renderer=renderer, $
        draw_xsize=xsize, $
        draw_ysize=ysize, $
        scale=scale, $
        ;menu_parent=widget_button(mbar,value='Volume View'), $
        toolbar_parent=wToolbarBase, $
        /use_instancing, $
        /debug $
        )
    if not obj_valid(ObjViewWid) then $
        message, 'Failed to create IDLexVolviewWid object.'
        
 wVolumeSelect=widget_combobox( wToolbarBase,value=header.info.parms.name)
 widget_control,wVolumeSelect,set_combobox_select=11    
 wWrite=widget_button(wToolbarBase,$
             value=filepath('copy.bmp', subdirectory=['resource', 'bitmaps']), $
             /bitmap,tooltip='Write a paper for me')
 state={self:mainbase,VolView:VolView,ObjViewWid :ObjViewWid ,ImgViewWid:oWid,oVolume:oVolume,wFOV:wFOV,wVolumeSelect:wVolumeSelect,wUpload:wUpload,wWrite:wWrite}
 widget_control,statebase,set_uvalue=state
 widget_control,mainbase,/realize

 state.ObjviewWid->GetProperty,widget_id=widget_id
        wDraw=widget_info(widget_id,find_by_uname='xobjview:draw')
        widget_control, wDraw, get_value=oWindow
        state.VolView->Reset, /full, oWindow 
 XMANAGER, 'GX_Explorer', mainbase ,/no_block
end