;+
; NAME:
;   gx_simulator
;
; PURPOSE:
;   This is the main gx_simulator routine
;
; CATEGORY:
;   Modeling
;
; CALLING SEQUENCE:
;   gx_simulator, nthreads
;
; INPUTS:
;  
;
; OUTPUTS:
;   nthreads: number of parallel threads to be used, default 4.
;   
;
; COMMON BLOCKS:
; 
;
; RESTRICTIONS:
; ONLY WINDOWS OS SUPPORTED AT INITIAL RELEASE DATE
;
; EXAMPLE:
;   gx_simulator,4
;   
; HELP:
;  //gx_simulator/help/GX_Simulator.chm
;
; MODIFICATION HISTORY:
;   Written by:  Gelu M. Nita, September 2010- January 2013. 
;   01/ /13: Initial SSW Release
;-

function my_colorbar,direction, index, value,data=data
 default,data,[0,255]
 min=min(data,max=max,/nan)
 return,string(min+value*(max-min)/256,format='(g0)')
end

pro gx_simulator_event,event
 widget_control,widget_info(event.top,Find_By_Uname='STATEBASE'),get_uvalue=state
 IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
  answer=dialog_message(/question,'Do you want to quit gx_simulator?')
  if strupcase(answer) eq 'YES' then begin
  obj_destroy,state.sun
  obj_destroy,state.view
  obj_destroy,state.MapView
  return
  end
 END

 CASE TAG_NAMES(event, /STRUCTURE_NAME) OF
  
  'GXDRAW':begin
            state.oObjviewWid->Draw
           end  
  'GXMODELSELECTEVENT':begin
                        time=''
                        model=event.model
                        if obj_isa(model,'GXMODEL') then begin
                         if model->isROI() then begin
                           time=model->GetTime()
                           all=state.Sun->get(isa='GXMODEL',/all,count=count)
                           if count gt 0 then begin
                             for i=0, n_elements(all)-1 do begin
                               if all[i] ne model then begin
                                  all[i]->SetProperty,IsROI=0
                               endif
                             endfor
                           endif
                          endif
                         endif
                         state.Sun->SetTime,time
                         state.scanbox->SetRefModel,model
                         state.scanbox->CreateArrayInputControls
                         if obj_valid(model) then begin
                           sroi=model->getroi(/scanbox)
                           sdata=gx_transform(sroi->GetScanboxData(),model->GetSTM(),/inv)
                           xrange=minmax(sdata[0,*])
                           yrange=minmax(sdata[1,*])
                           zrange=minmax(sdata[2,*])
                           dim=sroi->GetDim()
                           nx=dim[0]
                           ny=dim[1]
                           nz=dim[2]   
                           compute=0
                         endif else begin
                           xrange=[-1.5,1.5]
                           yrange=[-1.5,1.5]
                           zrange=[-1.5,1.5]
                           nx=64
                           ny=64
                           nz=64
                           compute=1
                         endelse
                         state.scanbox->SetDim,dim
                         state.scanbox->NewGrid,xrange=xrange, yrange=yrange,zrange=zrange,nx=nx,ny=ny,compute=compute
                         state.oObjviewWid->Draw
                       end
  'GXWIDGETEVENT':begin
                   if obj_isa(event.subject,'gxModel') then ((event.subject)->GetVolume())->Update
                   state.oObjviewWid->Draw
                  end
  'GXSCANBOXEVENT':begin
                   if event.auto then begin
                    state.scanbox->ComputeFOV
                    state.scanbox->DrawSlicer
                   endif
                   state.oObjviewWid->Draw
                  end                
  'GXREMOVEMODELEVENT': begin
                           if dialog_message('Dou you want to destroy this model?',/question) eq 'Yes' then begin
                             widget_control,widget_info(event.id,/child),get_uvalue=gxWidget
                             obj_destroy, gxWidget
                             state.ModelCount-=1
                             state.scanbox->SetRefModel, model
                             state.scanbox->UpdateAllParms
                             if ~obj_isa(model,'GXMODEL') then state.Sun->SetTime,''
                             state.oObjviewWid->Draw
                           end
                         end
  'GXZOOMINMODELEVENT': begin
                          widget_control,event.id,get_uvalue=oModel
                            state.oObjviewWid->SetModelView,event.select
                          if event.select then begin
                            widget_control,state.wFOV,sensitive=0
                            widget_control,state.wGrid,sensitive=0
                            (oModel->GetByName('Volume'))->SetProperty,zbuffer=1
                            state.oObjviewWid->Remove,state.sun
                            state.sun->Remove,oModel
                            oModel->ResetPosition
                            state.oObjviewWid->Add,oModel
                            state.oObjviewWid->FieldLineButton,/active
                            state.oObjviewWid->Zoom2ViewButton,active=0
                            state.oObjviewWid->Reset,/full
                            state.oObjviewWid->Zoom,1
                          endif else begin
                            widget_control,state.wFOV,sensitive=1
                            widget_control,state.wGrid,sensitive=1
                           (oModel->GetByName('Volume'))->SetProperty,zbuffer=1
                            state.oObjviewWid->Remove,oModel
                            oModel->ResetPosition
                            widget_control,event.id,set_uvalue=oModel
                            state.sun->Add,oModel
                            state.oObjviewWid->Add,state.sun
                            state.oObjviewWid->FieldLineButton,active=1
                            state.oObjviewWid->Zoom2ViewButton,/active
                            state.oObjviewWid->Zoom,1
                          endelse
                          state.oObjviewWid->Draw
                         end
   'BOX2GX': begin
                  if tag_exist(event,'box') then begin
                   widget_control,widget_info(event.top,find_by_uname='ViewTab'),set_tab_current=0
                   widget_control,widget_info(event.top,find_by_uname='ControlTab'),set_tab_current=0
                   model=gx_ImportModel(*event.box)
                   goto,UploadModel
                  endif
                end
    'MODEL2GX': begin
      if tag_exist(event,'model') then begin
        widget_control,widget_info(event.top,find_by_uname='ViewTab'),set_tab_current=0
        widget_control,widget_info(event.top,find_by_uname='ControlTab'),set_tab_current=0
        model=event.model
        goto,UploadModel
      endif
    end             
   'PLOTMANDRAWEVENT':begin
                        event=state.MapView->HandleEvent(event)
                      end
   'PLOTMANDELETEEVENT':begin
                        event=state.MapView->HandleEvent(event)
                      end  
   'GX2PLOTMAN':begin
                  widget_control,widget_info(event.top,find_by_uname='ViewTab'),set_tab_current=2
                  widget_control,get_tlb(event.id),get_uvalue=uvalue
                  widget_control,uvalue.widgets.w_message,set_value='CREATING AND TRANSFERING MAPS FROM SIMULATOR TO PLOTMAN.......'
                  widget_control,/hourglass
                  event=state.MapView->HandleEvent(event)
                end       
   'GXSPEC2PLOTMAN':begin
                  widget_control,widget_info(event.top,find_by_uname='ViewTab'),set_tab_current=2
                  event=state.MapView->HandleEvent(event)
                end    
   'INPUTMODEL':begin
               if tag_exist(event,'model') then begin
                   widget_control,widget_info(event.top,find_by_uname='ViewTab'),set_tab_current=0
                   widget_control,widget_info(event.top,find_by_uname='ControlTab'),set_tab_current=0
                   model=event.model
                   goto,UploadModel
                  endif
             end
                                                        
 ELSE:
 ENDCASE

 case event.id of
 state.wAbout:answ=dialog_message('GX Simulator v4.0 (February 2018)'+string(10b)+$
                                  'Gelu M. Nita (gnita@njit.edu)'+string(10b)+$
                                  'Center for Solar-Terrestrial Research'+string(10b)+$
                                  'New Jersey Institute of Technology'+string(10b)+$
                                  'Newark, NJ, 07102, U.S.A.',/info,title='About GX_Simulator')
 state.wHelp: begin
               spawn,gx_findfile('GX_Simulator.chm',folder='doc'),unit=unit
               wait,0.5
               free_lun,unit 
              end                             
 state.wFOV:begin
              state.scanbox->ComputeFOV,/auto
              state.scanbox->DrawSlicer
              state.oObjviewWid->Draw
            end 
 state.wModelFOV:begin            
                   MOI=state.scanbox->GetMOI()
                     if obj_isa(MOI,'gxModel') then begin
                     state.scanbox->SetDim,(moi->getroi(/scanbox))->GetDim()
                     state.scanbox->ComputeFOV,/upload
                     state.scanbox->DrawSlicer
                     state.scanbox->Slice
                     state.oObjviewWid->Draw
                   end     
                  end
 state.wImportFOV:begin
             file=dialog_pickfile(filter=['*.sav','*.map'],$
                  DEFAULT_EXTENSION='sav',$
                  /read,/must_exist,$
                  title='Please select a file containig an map onject or map structure to import its FOV')
              if file ne '' then begin    
                  osav=obj_new('idl_savefile',file)
                  names=osav->names()
                  valid=0
                  for i=0,n_elements(names)-1 do begin
                    osav->restore,names[i];,/RELAXED_STRUCTURE_ASSIGNMENT
                    e=execute('result=size('+names[i]+',/tname)')
                    if (result eq 'STRUCT') or (result eq 'OBJREF') then begin
                      e=execute('m=temporary('+names[i]+')')
                      if valid_map(m) then map=temporary(m)
                    endif
                  endfor
                  if ~(size(map,/tname) eq 'STRUCT' or size(map,/tname) eq 'OBJREF') then begin
                    answ=dialog_message('Unexpected file content!',/error)
                  endif else begin
                  if size(map,/tname) eq 'STRUCT' then begin
                    fovmap=obj_new('map')
                    for k=0,n_elements(map)-1 do begin
                      fovmap->setmap,k,map[k]
                    endfor
                  endif else fovmap=map
                  state.scanbox->ComputeFOV,fovmap=fovmap,/compute
                  state.scanbox->DrawSlicer
                  state.oObjviewWid->Draw
                  endelse
               end   
            end           
            
 state.wGrid:begin
              state.scanbox->ComputeFOV,/compute
              state.scanbox->DrawSlicer
              state.oObjviewWid->Draw
            end            
 state.wUploadModelIcon: goto, ReadModel
 state.wImportModelIcon: goto, ImportModel
 state.wUploadModel:begin
                     ReadModel:              
                     model=gx_read(file)
                     if file eq '' then goto,nevermind
                     UploadModel:
                     if obj_isa(model,'gxmodel') then begin
                       widget_control,widget_info(event.top,find_by_uname='ControlTab'),set_tab_current=0
                       state.ModelCount+=1
                       model->SetIsRoi,state.ModelCount eq 1
                       name=model->GetName()
                       if name eq '' then begin
                        if n_elements(file) gt 0 then begin
                          break_file,file, disk_log, dir, name, ext, fversion, node, /last_dot
                        endif else name=string(state.ModelCount,format="('Model',i2)")
                        model->SetName,name
                       endif 
                       
                       wParent=WIDGET_BASE(state.wModelsTab,/Align_Center,UNAME=name,Title=name)
                       model->GetProperty,parent=parent,refmaps=refmaps,XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV
                       if obj_valid(parent) then begin
                         parent->remove,model
                         obj_destroy,parent
                       end    
                                            
                       state.sun->Add,model
                       state.oObjviewWid->Draw  
                       ;widget_control,event.top,redraw=0 
                       t0=systime(/s)               
                       void=obj_new('gxwidget',wParent,model)
                       message,string(systime(/s)-t0,format="('Model uploaded in' , f0.2 ,' seconds')"),/info
                       ;widget_control,event.top,redraw=1  
                       if ~obj_isa(void,'gxwidget') then begin
                        answ=dialog_message('GX model initialization failed. Operation aborted!')
                        if widget_valid(wParent) then widget_control,wParent,/destroy
                        obj_destroy,model
                        goto,nevermind
                       endif
                       void->GetProperty,widget_id=wIdBase
                       widget_control,state.wModelsTab,SET_TAB_CURRENT=widget_info(state.wModelsTab,/N_CHILDREN)-1
                       wBaseSelect=widget_info(wIdBase,find_by_uname='GXMODEL:BaseMapSelect')
          
                       omaps=(model->refmaps())   
                       if obj_valid(*omaps) then begin
                       nitems=(*omaps)->get(/count)
                         for i=3,nitems-1 do begin
                             widget_control,wBaseSelect,COMBOBOX_ADDITEM=(*omaps)->get(i,/id)eq''?'Noname':(*omaps)->get(i,/id)
                         end  
                       end
                       roi=model->GetRoi(/scanbox)
                       roi->DisplayMap,2
                       (Model->GetByName('Volume'))->SetProperty,zbuffer=1
                       all=model->Get(/all,isa='gxFluxtube',count=count)
                       for i=0,count-1 do begin
                        all[i]->Update_N_th,/no_volume_update
                       end
                        model->SetProperty,wParent=wParent
                        widget_control,state.scanbox->GetSliceSelector(),COMBOBOX_INDEX=index,get_value=value
                       (Model->GetVolume())->Update,state.scanbox->GetSliceSelector(/item),/update,/force
                       if model->isROI() then begin
                           time=model->GetTime()
                           all=state.Sun->get(isa='GXMODEL',/all)
                           for i=0, n_elements(all)-1 do begin
                             if all[i] ne model then begin
                                all[i]->SetProperty,IsROI=0
                             endif
                           endfor
                         state.Sun->SetTime,model->GetTime()
                         state.scanbox->SetRefModel,model
                         state.scanbox->CreateArrayInputControls
                       endif  
                     endif else answ=dialog_message('Invalid GX model!')
                     state.scanbox->DrawSlicer
                     state.oObjviewWid->Draw
                     nevermind:
                  end
 state.wImportModel:begin
                  ImportModel:
                  file=dialog_pickfile(filter='*.sav',$
                  DEFAULT_EXTENSION='sav',$
                  /read,/must_exist,$
                  title='Please select a file containig a GX datacube structure')
                  if file ne '' then begin
                   model=gx_ImportModel(file)
                   if obj_isa(model,'gxmodel') then goto, UploadModel else obj_destroy,model
                  end
                end
                
 state.wScan:state.scanbox->OnStartScan,event
 state.wDebug:state.scanbox->OnStartScan,event,/debug              
 else: void=state.scanbox.handleevent(event)
 endcase
 widget_control,widget_info(event.top,Find_By_Uname='STATEBASE'),set_uvalue=state
end


pro gx_simulator,nthreads_or_model,main_base=main_base,expert=expert,_extra=_extra
if (XREGISTERED('gx_simulator') ne 0) then begin
  answ=dialog_message('Only one instance of GX Simulator may run in the same IDL session!')
  return
endif
if isa(nthreads_or_model,/number) then nthreads=nthreads_or_model
if isa(nthreads_or_model,'gxmodel') then model=nthreads_or_model
setenv, 'WCS_RSUN=6.96d8'
if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'
gx_setfonts,_extra=_extra
device, get_screen_size=scr
if scr[0] lt 3200 then nb=16 else nb=32
if not exist(xsize) then xsize = fix (scr[0] * .4)
if not exist(ysize) then ysize = xsize

subdirectory=['resource', 'bitmaps']
state={oObjviewWid:obj_new(),sun:obj_new(),$
wModelsTab:0l,wUploadModelIcon:0l,wUploadModel:0l,wImportModelIcon:0l,$
wImportModel:0l,wFOV:0l,wModelFOV:0l,wImportFOV:0l,wGrid:0l,wHelp:0l,wAbout:0l,ModelCount:0l,view:obj_new(),$
Scanbox:obj_new(),wScan:0l,wPause:0l,wDebug:0l,wAbort:0l,MapView:obj_new(),expert:keyword_set(expert)}

state.view=OBJ_NEW('IDlexObjview')
state.sun=OBJ_NEW('gxSUN',grid=10)
state.view->Add,state.sun


main_base= WIDGET_BASE(Title =keyword_set(expert)?'GX SIMULATOR (Expert Version)':'GX SIMULATOR',/column,UNAME='gx_simulator',/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=0,$
  x_scroll_size=0.95*scr[0],y_scroll_size=scr[1]*0.90,/scroll)

state_base=widget_base(main_base, /column,UNAME='STATEBASE')
upper_base=WIDGET_BASE(state_base,/row)
control_Base=WIDGET_BASE(state_base,/row,UNAME='Control_Base')
Left_Base=WIDGET_BASE(Control_Base,/COLUMN,UNAME='Left_Base')
Right_Base=WIDGET_BASE(Control_Base,/COLUMN,UNAME='Right_Base')

status_base=WIDGET_BASE(upper_Base,/row,UNAME='STATUSBASE')
ViewTab=WIDGET_TAB(Left_Base,/Align_top,UNAME='ViewTab',UVALUE='ViewTab',LOCATION=2)

scan_base=WIDGET_BASE(upper_Base,/row,UNAME='SCANBASE')
wControlTab=WIDGET_TAB(Right_Base,/Align_top,UNAME='ControlTab',UVALUE='ControlTab',LOCATION=2)

wModels=Widget_Base(wControlTab,Title='MODELS')   
state.wModelsTab=WIDGET_TAB(wModels,/Align_Center,UNAME='ModelsTab',LOCATION=0)

objview_base=WIDGET_BASE(ViewTab,/Align_Center,UNAME='VOLUME VIEW',Title='VOLUME VIEW',/column)

wToolbarBase=widget_base(objview_base, /row,/frame,UNAME='TOOLBAR',/toolbar)
view_menu = WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('switch_down.bmp', subdirectory=subdirectory)),$
                           /bitmap, /MENU,tooltip='Pulldown menu')

state.wFOV=widget_button(widget_base(wToolbarBase,/toolbar,/row),$
              value=gx_bitmap(filepath('view.bmp', subdirectory=subdirectory)), $
              /bitmap,tooltip='Compute inscribing FOV',sensitive=1)
state.wModelFOV=widget_button(widget_base(wToolbarBase,/toolbar,/row),$
              value=gx_bitmap(filepath('copy.bmp', subdirectory=subdirectory)), $
              /bitmap,tooltip='Copy Model FOV',sensitive=1)   
state.wImportFOV=widget_button(widget_base(wToolbarBase,/toolbar,/row),$
              value=gx_bitmap(filepath('importf.bmp', subdirectory=subdirectory)), $
              /bitmap,tooltip='Import Map FOV',sensitive=1)                         
state.wGrid=widget_button(widget_base(wToolbarBase,/toolbar,/row),$
              value=gx_bitmap(filepath('scatter.bmp', subdirectory=subdirectory)), $
              /bitmap,tooltip='Compute rendering grid',sensitive=1)              

wInfo=widget_base(objview_base,/column)

state.oObjviewWid = obj_new('gxObjviewWid', $
    objview_base, $
    state.view, $
    menu_parent=view_menu, $
    toolbar_parent=wToolbarBase,$
    scale=scale, $
    draw_xsize=xsize, $
    draw_ysize=ysize, $
    background=background, $
    stationary=oStationary, $
    /double_view, $
    renderer=renderer, $
    use_instancing=use_instancing, $
    /include_refresh_button, $
    /include_full_reset_button, $
    debug=debug )
  
    if not obj_valid(state.oObjviewWid) then begin
     message, !error_state.msg + ' ' + !error_state.sys_msg, /noname
    end   
    
  view_geometry=widget_info(objview_base,/geometry) 
  wToolbarMenuBase= widget_base(status_base, /row,/toolbar,/frame)
  state.wHelp=widget_button( wToolbarMenuBase, $
    value=gx_bitmap(filepath('help.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='GX Simulator Help')
  state.wAbout=widget_button( wToolbarMenuBase, $
    value=gx_bitmap(filepath('button.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='About GX Simulator')  
  state.wImportModelIcon= widget_button( wToolbarMenuBase, $
    value=gx_bitmap(filepath('importf.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Import Model Data')
  state.wUploadModelIcon= widget_button( wToolbarMenuBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Upload Saved Model')
  geometry=widget_info(wToolbarMenuBase,/geometry)
  statusbarbase=widget_base(status_base,xsize=xsize-3*nb,ysize=geometry.scr_ysize,uname='STATUSBARBASE',/frame)
  wStatusBar=cw_objStatusBar(statusbarbase,uname='StatusBar',xsize=xsize-3*nb,ysize=geometry.scr_ysize)  
  state.wAbort=widget_info(wStatusBar,find_by_uname='ABORT')
  ;create image view
  wImgBase=WIDGET_BASE(ViewTab,/Align_Center,UNAME='IMAGE VIEW',Title='IMAGE VIEW',/column)
  
  
  wScanBase = widget_base(scan_base, /row, /frame,/TOOLBAR,map=0)
  space=widget_label(wScanBase,value=' Synthetic map computation: ')
  wRadioButtons=widget_base(wScanBase,/exclusive,/row,/toolbar)
  state.wScan=widget_button(wRadioButtons,value=gx_bitmap(gx_findfile('play.bmp')),tooltip='Compute Radiation Maps (Parallel threads)',/bitmap,uname='SCAN_START')
  if state.expert then state.wDebug=widget_button(wRadioButtons,value=gx_bitmap(gx_findfile('debug.bmp')),tooltip='Debug Map Computation (IDL thread)',/bitmap,uname='SCAN_DEBUG')

  ;create SCAN object
  wScanner=Widget_Base(wControlTab,Title='SCANNER')
  wBase=Widget_base(wControlTab,/column,Title='LOS PROFILES')
  wScanbox=Widget_base(wBase,/column,/frame)
  state.Scanbox=obj_new('gxScanbox',wScanbox,wInfo=wInfo,wToolbar=wToolbarBase,wScanner=wScanner,nthreads=nthreads,wImgBase=wImgBase,uname='Scanbox')
  widget_control,state.wAbort,set_uvalue= state.Scanbox
  state.Sun->add,state.Scanbox
 
  ;create map view
  wMapControl=Widget_Base(wControlTab,Title='MAGNETIC FIELD EXTRAPOLATION PROJECT',UNAME='MAPCONTROL')
  wMapBase=WIDGET_BASE(ViewTab,/Align_Center,UNAME='MAP VIEW',Title='MAP VIEW',/column)
  ;create gxMapVieWid object
  state.MapView=obj_new('gxMapViewWid',wMapControl,mbar,wMapBase=wMapBase,frame=frame,wxsize=xsize,wysize=ysize)
  ; create gx2data object
  wGX2DataControl=Widget_Base(wControlTab,Title='DATA T0 MODEL IMAGE COMPARISON',UNAME='gx2data')
  wgx2data=cw_gx2data(wGX2DataControl,plotman=state.MapView->GetPlotmanObj())          
  widget_control,state_base,set_uvalue=state
  widget_control,widget_info(state_base,find_by_uname='ControlTab'),set_tab_current=4
  WIDGET_CONTROL, /REALIZE, main_base
  XMANAGER, 'gx_simulator', main_base ,/no_block
  widget_control,widget_info(wStatusBar,/child),get_uvalue=oStatusBar
  oStatusBar->Erase
  state.scanbox->ComputeFOV,/auto
  state.scanbox->DrawSlicer
  if isa(model,'gxmodel') then widget_control,main_base,send_event={inputmodel,id:0l,top:0l,handler:0l,model:model}
end

