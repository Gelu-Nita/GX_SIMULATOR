function gxScanbox::Init,wParent,wInfo=wInfo,wToolbar=wToolbar,wScanner=wScanner,wImgBase=wImgBase,$
xrange=xrange,yrange=yrange,zrange=zrange,Nx=Nx,Ny=Ny,Nz=Nz,nthreads=nthreads,_extra=_extra
 compile_opt hidden
 if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'
 device, get_screen_size=scr
 xscale=scr[0]/1920.
 font=!defaults.font
 if widget_valid(wInfo) then self.wInfo=wInfo
 if widget_valid(wToolbar) then self.wToolbar=wToolbar
 if obj_valid(ImgViewWid) then self.ImgViewWid=ImgViewWid
 default,xrange,[-1.5,1.5]
 default,yrange,[-1.5,1.5]
 default,zrange,[-1.5,1.5]
 default,Nx,64
 default,Ny,64
 default,Nz,64
 result=self->IDLgrModel::Init(_extra=_extra,name='scanboxobject')
 if result ne 1 then message,'initialization of idlgrmodel part of scanbox object failed'
 self.xrange=xrange
 self.yrange=yrange
 self.zrange=zrange
 self.Nx=Nx
 self.Ny=Ny
 self.Nz=Nz
 self.dx=delta(self.xrange)/self.nx
 self.dy=delta(self.yrange)/self.ny
 self.dz=delta(self.zrange)/self.nz
 
 p=dblarr(3,8)
 for i=0,7 do p[*,i] = [self.xrange[(i AND 1)], self.yrange[((i/2) AND 1)], self.zrange[((i/4) AND 1)]]
 index=[0,1,3,1,5,7,5,4,6,4,0,2,3,7,6,2]
 roi=p[*,index] 
 self.roi=obj_new('IDLgrRoi',roi,name='ScanBox',color=[0,0,255],linestyle=1)
 self->Add,self.roi
 data=p[*,[0,4,5,1]]
 self.slicer=obj_new('IDLgrPolygon',data,style=1,name='Slicer',color=[0,0,255],linesty=2)
 self.profiler=obj_new('IDLgrPolygon',data,style=1,name='Profiler',color=[0,255,0],linesty=0)
 self->Add,self.slicer
 self->Add,self.profiler
 cdir=curdir()
 self.renderer=self->DefaultRenderer()
 info=self->RendererInfo()
 cd,cdir
 self.info=ptr_new(info)
 wControlTab=widget_info(wScanner,/parent)
 wPlotBase=Widget_base(wControlTab,/column,Title='IMAGE PROFILES',/frame)
 self.ImgViewWid=obj_new('gxImgViewWid', wImgBase,info=self.info,renderer=self.renderer,wPlotBase=wPlotBase,/save)
 parms=(*self.info).parms
 row_labels=strarr(n_elements(parms))
 for i=0,n_elements(parms)-1 do row_labels[i]=string(i+1,format='(i3)')  
 wScannerBase=widget_base(wScanner,/column,event_func='gxScanboxHandleEvent',uvalue=self)
 wExecBase=widget_base(wScannerBase,/column)
 self.wParmBase=widget_base(wScannerBase,/column)
 wParmToolbarBase = widget_base(self.wParmBase, /row, /frame,/TOOLBAR)
 self.wSelectRenderer= widget_button( wParmToolbarBase, $
             value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
             /bitmap,tooltip='Select Rendering Method IDL Wrapper')
 edit=1
   
 self.wParmsTable=widget_table(font=!defaults.font,self.wParmBase,xsize=4,ysize=n_elements((*self.info).parms),$
 y_scroll_size=0,x_scroll_size=0,value=(*self.info).parms,COLUMN_WIDTHS =xscale*[100,100,100,400],$
 edit=edit,format=format,$
 column_labels=['Parameter','Value','Unit','Comments'],/RESIZEABLE_COLUMNS,uname='ParmsTable',$
 row_labels=row_labels,SCR_ySIZE=300*xscale);550*xscale)

 geometry1=widget_info(self.wParmsTable,/geometry)
 geometry2=widget_info(self.wSelectRenderer,/geometry)
 self.wRenderer=widget_text(font=!defaults.font,wParmToolbarBase,value=self.renderer,SCR_XSIZE=geometry1.SCR_XSIZE-3*geometry2.SCR_XSIZE,/wrap)
 if n_elements(nthreads) eq 0 then nthreads=1;!CPU.HW_NCPU
 self.bridges=obj_new('IDL_Container')
 bridge_state=replicate({status:'',task:'',time:'',calls:'',error:''},nthreads)
 for i=0,nthreads-1 do begin
  bridge=obj_new('gxBridge',userdata=self,out=GETENV('IDL_TMPDIR')+GETENV('USER')+strcompress('gxBridge'+string(i)+'.log',/rem));curdir()+path_sep()+strcompress('Bridge'+string(i)+'.log',/rem))
  if obj_valid(bridge) then begin
    bridge->SetVar,'id',i
    code=bridge->Status(error=error)
    case code of 
     0:bridge_state[i].status='Idle'
     1:bridge_state[i].status='Active'
     2:bridge_state[i].status='Completed'
     3:bridge_state[i].status='Error'
     4:bridge_state[i].status='Aborted'
     else:bridge_state[i].status='Unknown'
    endcase
    bridge_state[i].error=error
    self.bridges->Add,bridge
  end  
 end 
main_base=get_tlb(wExecBase) 
scan_base=widget_info(main_base,find_by_uname='SCANBASE')
geometry3=widget_info(scan_base,/geometry)
wEBTELToolbarBase = widget_base(scan_base, /row, /frame,/TOOLBAR,map=1)
self.wSelectEBTEL= widget_button( wEBTELToolbarBase, $
  value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
  /bitmap,tooltip='Select EBTEL Table',uname='EBTEL')
self.wEBTELTable=widget_text(font=!defaults.font,wEBTELToolbarBase,value=gx_ebtel_path(),SCR_XSIZE=geometry1.SCR_XSIZE-geometry3.SCR_XSIZE,/wrap);-3*geometry2.SCR_XSIZE
  
 self->CreateArrayInputControls
 self.wScan=widget_info(main_base,find_by_uname='SCAN_START')
 self.wDebug=widget_info(main_base,find_by_uname='SCAN_DEBUG')
 self.wAbort=widget_info(main_base,find_by_uname='ABORT')
 
 status_base=widget_base(wExecBase,/row)
 ntasks=self.bridges->Count()
 self.wBridges=cw_ObjField(status_base,value=ntasks,increment=1,label='GX Parallel Threads:',uname='wBridges')
 self.wStatusBar=widget_label(status_base,value='',font=font,/dynamic_resize)
 self.wTaskTable=widget_table(wExecBase,value=bridge_state,xsize=5,ysize=ntasks,$
 y_scroll_size=xscale*4,COLUMN_WIDTHS =xscale*[100,100,100,100,300],$
 column_labels=['STATUS','TASK','ON TASK','CALLS','ERROR MESSAGE'],$
 row_labels=[string(1+indgen(ntasks),format='("THREAD",i2)')],$
 /RESIZEABLE_COLUMNS,uname='TaskTable') 
 
 void=self->gxWidget::Init(wParent,self,_extra=_extra)
 return,1
end

pro gxScanBox::CreateArrayInputControls

  if widget_valid(self.wArrayParmBase) then widget_control,self.wArrayParmBase,/destroy
  map=1
  moi=self->GetMoi()
  if obj_valid(moi) then begin
    *self.info=moi->UpdateEUVinfo(*self.info)
  endif
  if tag_exist((*self.info),'nparms') then begin
    if ~widget_valid(self.wArrayParmBase) then self.wArrayParmBase=widget_base(self.wParmBase,/row,map=map)
    wNbase=widget_base(self.wArrayParmBase,/column)
    self.wNparms=cw_objarray(wNbase,value=(*self.info).nparms.value,items=strcompress((*self.info).nparms.name,/rem),names=strcompress((*self.info).nparms.name+'; '+$
    (*self.info).nparms.unit+'; '+(*self.info).nparms.hint),/frame,inc=1,/static,$
    sensitive=(*self.info).nparms.user,/vert,/right,xtextsize=10,font=!defaults.font,type=1l)
    widget_control,self.wNparms,set_value=(*self.info).nparms.value,set_uvalue=(*self.info).nparms.name,set_uname='renderer:nparms'
  endif
  
  if tag_exist((*self.info),'rparms') then begin
    if ~widget_valid(self.wArrayParmBase) then self.wArrayParmBase=widget_base(self.wParmBase,/row,map=map)
    wRbase=widget_base(self.wArrayParmBase,/column)
    self.wRparms=cw_objarray(wRbase,value=(*self.info).rparms.value,items=strcompress((*self.info).rparms.name,/rem),names=strcompress((*self.info).rparms.name+'; '+$
    (*self.info).rparms.unit+'; '+(*self.info).rparms.hint),/frame,inc=1,/static,$
    sensitive=(*self.info).rparms.user,/vert,/right,xtextsize=10,font=!defaults.font,type=0d)
    widget_control,self.wRparms,set_uvalue=(*self.info).rparms.name,set_uname='renderer:rparms'
  endif
  
  if self->AcceptFreqList() then begin
    if ~widget_valid(self.wArrayParmBase) then self.wArrayParmBase=widget_base(self.wParmBase,/row,map=map)
    
    if ~widget_valid(wRbase) then wRbase=widget_base(self.wArrayParmBase,/column)
    
    wFbase=widget_base(wRbase,/column,/frame)
    
    wToolbarBase=widget_base(wFbase,/toolbar,/row)
    
    self.wUploadFreqList=widget_button(wToolbarBase, $
      value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
      /bitmap,tooltip='Upload comma separated frequency list from text file (GHz)')
    
    self.wResetFreqList=widget_button(wToolbarBase, $
      value=gx_bitmap(filepath('reset.bmp', subdirectory=['resource', 'bitmaps'])), $
      /bitmap,tooltip='Reset frequency list',$
      uvalue=[self->GetParmValue('f_min'),self->GetParmValue('df'),self->GetParmValue('N_freq')])   
    
    self.wUndoFreqList=widget_button(wToolbarBase, $
      value=gx_bitmap(filepath('undo.bmp', subdirectory=['resource', 'bitmaps'])), $
      /bitmap,tooltip='Undo frequency list changes') 
       
    self.wDelFreqList=widget_button(wToolbarBase, $
      value=gx_bitmap(filepath('delete.bmp', subdirectory=['resource', 'bitmaps'])), $
      /bitmap,tooltip='Delete current frequency list')
        
    self.wSaveFreqList=widget_button(wToolbarBase, $
      value=gx_bitmap(filepath('save.bmp', subdirectory=['resource', 'bitmaps'])), $
      /bitmap,tooltip='Save current frequency list')  
      
    self.wUseFreqList=cw_bgroup(wToolbarBase, ['Edit','Use List'],/row,/exclusive,set_value=0,/no_release,font=!defaults.font)
    
    self.wEditedFreqList=cw_bgroup(wToolbarBase, ['Edited'],/row,/nonexclusive,set_value=0,/no_release,font=!defaults.font)
    
    widget_control,self.wEditedFreqList,sensitive=0
    g=widget_info(wRbase,/geometry)
    wLabel=widget_label(wFbase,value='Comma Separated Frequency List (GHz)', font=!defaults.font,scr_xsize=g.scr_xsize)
    self.wFreqList=widget_text(wFbase,scr_xsize=g.scr_xsize,ysize=5,/scroll,/editable,/wrap,/KBRD_FOCUS_EVENTS)
  endif
  if widget_valid(self.wArrayParmBase) then widget_control, self.wArrayParmBase,map=1
end

function gxScanBox::DefaultRenderer
 which,'gx_simulator',outfile=outfile
 cdir=file_dirname(file_dirname(outfile))
 renderer=cdir+path_sep()+'userslib'+path_sep()+'xray'+path_sep()+'xray_tt.pro'
 if strupcase(renderer) eq strupcase(find_file(renderer)) then return,renderer
 return,self->SelectRenderer()
end 

function gxScanBox::SelectRenderer
  which,'gx_simulator',outfile=outfile
  cdir=file_dirname(file_dirname(outfile))
  path=cdir+path_sep()+'userslib'
 return,dialog_pickfile(filter='*.pro',TITLE='Please select a renderer IDL routine/wrapper',path=path)
end

function gxScanBox::RendererInfo,info
 which,'gx_simulator',outfile=outfile
 cdir=curdir()
 catch, error_stat
 if error_stat ne 0 then begin
    catch, /cancel
    goto,invalid_renderer
 end
 dirpath=file_dirname(self.renderer,/mark)
 cd,dirpath
 break_file, self.renderer, dsk_log, dir, filename, ext
 compile_test=execute('RESOLVE_ROUTINE, filename , /COMPILE_FULL_FILE ,/either')
 cd,cdir
 par=ROUTINE_INFO(filename,/par)
 if par.num_args lt 2 or par.num_kw_args lt 1 then goto,invalid_renderer
 template=filename+',parms,rowdata'
 for i=2,par.num_args-1 do template+=','+strlowcase(par.args[i])
 for i=0,par.num_kw_args-1 do begin
 if strupcase(par.kw_args[i]) ne 'INFO' then template+=','+strlowcase(par.kw_args[i])+'='+strlowcase(par.kw_args[i])
 end
 if execute(filename+',INFO=INFO') then begin
  if size(info,/tname) ne 'STRUCT' then goto,invalid_renderer
  return,CREATE_STRUCT('execute',template,info)
 end 
 invalid_renderer:
 cd,cdir
 return,0
end


pro gxScanBox::ReplaceParmValue,name,value
 if ptr_valid(self.info) then begin
   idx=where(strupcase(((*self.info).parms).name) eq strupcase(name),count)
    if count eq 1 then begin
     parms=(*self.info).parms
     parms[idx].value=value
     (*self.info).parms=parms
     widget_control,self.wParmsTable,get_value=parms
     parms[idx].value=value
     widget_control,self.wParmsTable,set_value=parms
    endif
    if tag_exist(*self.info,'nparms') and widget_valid(self.wNparms) then begin
      idx=where(strcompress(strupcase(((*self.info).nparms).name),/rem) eq strcompress(strupcase(name),/rem),count)
      if count eq 1 then begin
        nparms=(*self.info).nparms
        nparms[idx].value=value
        (*self.info).nparms=nparms
        widget_control,widget_info(self.wNparms,find_by_uname=name),set_value=value
      endif
    end
    if tag_exist(*self.info,'rparms') and widget_valid(self.wRparms) then begin
      idx=where(strcompress(strupcase(((*self.info).rparms).name),/rem) eq strcompress(strupcase(name),/rem),count)
      if count eq 1 then begin
        rparms=(*self.info).rparms
        rparms[idx].value=value
        (*self.info).rparms=rparms
        widget_control,widget_info(self.wRparms,find_by_uname=name),set_value=value
      endif
    end
 end
end

function gxScanBox::GetParmValue,name,gui=gui
  if ptr_valid(self.info) then begin
    idx=where(strupcase(((*self.info).parms).name) eq strupcase(name),count)
    if count eq 1 then begin
        if keyword_set(gui) then begin
          widget_control,self.wParmsTable,get_value=parms
          parms=parms.value
        endif else parms=((*self.info).parms).value
        return,parms[idx]
    endif
    
    if tag_exist(*self.info,'nparms') and widget_valid(self.wNparms) then begin
      idx=where(strcompress(strupcase(((*self.info).nparms).name),/rem) eq strcompress(strupcase(name),/rem),count)
      if count eq 1 then begin
        if keyword_set(gui) then begin
          widget_control,self.wNparms,get_value=nparms
        endif else nparms=((*self.info).nparms).value
        return,nparms[idx]
      endif
    end
    
    if tag_exist(*self.info,'rparms') and widget_valid(self.wRparms) then begin
      idx=where(strcompress(strupcase(((*self.info).rparms).name),/rem) eq strcompress(strupcase(name),/rem),count)
      if count eq 1 then begin
        if keyword_set(gui) then begin
          widget_control,self.wRparms,get_value=rparms
        endif else rparms=((*self.info).rparms).value
        return,rparms[idx]
      endif
    end
  end
  return,!values.f_nan
end

function gxScanbox::GetInfo
  return,self.info
end

function gxScanbox::GetRenderer
  return,self.Renderer
end

pro gxScanBox::ReplaceRenderer,renderer
current=self.renderer
case size(renderer,/tname) of
  'STRING': begin
              if renderer eq '' then return
              if renderer[0] ne find_file(renderer[0]) then return
              self.renderer=renderer[0] 
              info=self->RendererInfo()  
              if size(info,/tname) eq 'STRUCT' then begin
                nx=self.nx 
                ny=self.ny 
                xrange=self.xrange
                yrange=self.yrange
              end
            end
     'STRUCT': begin
                 if ~tag_exist(renderer,'renderer') or ~tag_exist(renderer,'data') $
                 or ~tag_exist(renderer,'info') or ~tag_exist(renderer,'fovmap') then return
                 if ~obj_valid(renderer.fovmap) then return
                 self.renderer=renderer.renderer
                 info=renderer.info
                 fovmap=renderer.fovmap
                 data=renderer.data
                 if tag_exist(renderer,'EBTEL') then widget_control,self.wEbtelTable,set_value=gx_ebtel_path(renderer.ebtel)
                 gx_fovmap2scanbox,fovmap,xc=xc,yx=yc,xfov=xfov,yfov=yfov,xrange=xrange,yrange=yrange,nx=nx,ny=ny,rsun=rsun,b0=b0,l0=l0
                 self.Rsun=rsun
                 self.nx=nx
                 self.ny=ny
                 self.xrange=xrange
                 self.yrange=yrange
                 self.ImgViewWid->GetProperty,model=model
                 if isa(model,'gxmodel') then begin
                   newgrid=model->SetFOV(xc=xc,yc=yc,xfov=xfov, yfov=yfov,nx=nx,ny=ny)
                 endif
               end       
    else:return       
endcase

if size(info,/tname) eq 'STRUCT' then begin
 widget_control,self.wRenderer,set_value=self.renderer
 ptr_free,self.info
 self.info=ptr_new(info)
 row_labels=strarr(n_elements(info.parms))
 for i=0,n_elements(info.parms)-1 do row_labels[i]=string(i+1,format='(i3)') 
 widget_control,self.wParmsTable,set_value=(*self.info).parms,row_labels=row_labels,table_ysize=n_elements(info.parms)
 self.pData=(self.ImgViewWid)->NewView(self.info,renderer=self.renderer,nx=nx,ny=ny,xrange=xrange,yrange=yrange,data=data,fovmap=fovmap)
 self->CreateArrayInputControls
 self->UpdateAllParms
 self.ImgViewWid->Draw
 self->ComputeFOV,/compute_grid
 self->MakeGrid
 self->Slice
 self->ResetAllBridges
 widget_control,self.wSliceSelect,set_combobox_select=(where(((*self.info).parms).name eq 'n_0'))[0],set_value=[((*self.info).parms).name,'B','curlB','divB+','divB-','helB+','helB-','Q0','Q','VoxelID']
 endif else begin
  self.renderer=current
  answ=dialog_message(/error,'Invalid rendering method selected. Operation aborted!')
 end 
end

function gxScanBox::name2idx,name
 return,where(strcompress(strupcase(((*self.info).parms).name),/rem) eq strcompress(strupcase(name),/rem))
end

function gxScanBox::AcceptFreqList
  if tag_exist(*self.info,'execute') then begin
    idx=where(str2arr((*self.info).execute) eq 'freqlist',count)
  endif else count=0
  return,count
end

function gxScanBox::Rewrite, event,auto=auto
compile_opt hidden

default,auto,0b
case TAG_NAMES(event, /STRUCTURE_NAME) of
  'WIDGET_DRAW':return,event
  else:return, {GXSCANBOXEVENT,id: self.wIDBase, top: event.top, handler:0L,auto:auto}
endcase
end

function gxScanbox::GetRoi
return,self.roi
end

function gxScanbox::GetSlicer
return,self.slicer
end

function gxScanbox::GetLocation
return,[mean(self.xrange),mean(self.yrange)]
end
 
function gxScanbox::GetFOV
return,max([delta(self.xrange),delta(self.yrange)])
end

pro gxScanbox::SetRefModel,model
 if n_elements(model) eq 0 then begin
  self.ImgViewWid->GetProperty,model=model
 endif else self.ImgViewWid->SetProperty,model=model
 if obj_isa(model,'gxmodel') then begin
  model_info='SELECTED MODEL: '+model->GetName()+'['+model->GetTime()+']'
  widget_control,widget_info(widget_info(self.wScan,/parent),/parent),map=1
  hasBL=((model->GetVolume())->getflags()).hasBL
  self->SetLos,/model
 endif else begin model_info='NO GX MODEL SELECTED!'
  widget_control,widget_info(widget_info(self.wScan,/parent),/parent),map=0
 end
 widget_control,self.wModelInfo,set_value=model_info
end

function gxScanbox::GetRefModel
 self.ImgViewWid->GetProperty,model=model
 if obj_isa(model,'gxmodel') then return,model else return,obj_new()
end 

function gxScanbox::GetMOI
  self->GetProperty,parent=oSun
  if ~obj_isa(osun,'gxsun') then return,obj_new()
  all=oSun->Get(/all,count=count,isa='gxmodel')
  for i=0, count-1 do begin
    all[i]->GetProperty,IsROI=IsROI
    if IsROI then begin
      MOI=all[i]
    endif
  endfor
  if obj_valid(moi) eq 0 then MOI=obj_new()
  return,MOI
end

pro gxScanbox::ComputeFOV,compute_grid=compute_grid,upload=upload,auto=auto,fovmap=fovmap
 if self.active then begin
  answ=dialog_message('There is an active scan in progress.'+string(10b)+$
                      'Any unsaved results will be lost!'+string(10b)+$
                      'Do you want to continue anyway?',/question)
  if strupcase(answ) eq 'NO' then return                    
 end
 self->GetProperty,parent=oSun
 if ~obj_isa(osun,'gxsun') then return
 all=oSun->Get(/all,count=count,isa='gxmodel')
 for i=0, count-1 do begin
   all[i]->GetProperty,IsROI=IsROI,XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV
   if IsROI then begin
     MOI=all[i]
     MOI->ResetPosition
     hasrange=get_obj_range(MOI,oSun,odestination,range,ignore=['IDLgrLight','IDLgrImage','IDLgrVolume','gxROI']) 
     inscribing_xrange=reform(range[0,*])
     inscribing_yrange=reform(range[1,*])
     zrange=reform(range[2,*])
     dx=XCOORD_CONV[1]
     dy=YCOORD_CONV[1]
     dz=ZCOORD_CONV[1]
   endif
 endfor 
 if obj_valid(moi) eq 0 then MOI=obj_new()
 
 if keyword_set(upload) and isa(moi,'gxmodel') then fovmap=moi->GetFovMap()
 if valid_map(fovmap) then begin
   gx_fovmap2scanbox,fovmap,xc=xc,yx=yc,xfov=xfov,yfov=yfov,xrange=xrange,yrange=yrange,nx=nx,ny=ny,rsun=rsun,b0=b0,l0=l0
   self.nx=nx
   self.ny=ny
   self.xrange=xrange
   self.yrange=yrange
   widget_control,self.wX,set_value=xc
   widget_control,self.wY,set_value=yc
   widget_control,self.wXrange,set_value=xfov
   widget_control,self.wYrange,set_value=yfov
   self.rsun=rsun
   widget_control,self.wR,set_value=rsun
   widget_control,self.wB0,set_value=b0
   widget_control,self.wL0,set_value=l0
   widget_control,self.wObserver,SET_COMBOBOX_SELECT=1-keyword_set(l0 eq 0)
   self->SetLos,/user
 endif else begin
 if keyword_set(auto) then begin
       if obj_valid(moi) then begin
        if self->SquareFOV() then begin
          fov=max([delta(inscribing_xrange),delta(inscribing_yrange)])
          xrange=mean(inscribing_xrange)+[-1,1]*fov/2
          yrange=mean(inscribing_yrange)+[-1,1]*fov/2
        end
       endif else begin
        xrange=[-1.5,1.5]
        yrange=[-1.5,1.5]
        zrange=[-1.5,1.5]
        self.nz=mean([self.nx,self.ny])
       end 
  endif else begin
    if n_elements(xrange) ne 2 then begin
      widget_control,self.wXrange,get_value=xfov
      widget_control,self.wX,get_value=xc
      xrange=(xc+[-1,1]*xfov/2)/self.r()
    endif
    if n_elements(yrange) ne 2 then begin
      widget_control,self.wYrange,get_value=yfov
      widget_control,self.wY,get_value=yc
      yrange=(yc+[-1,1]*yfov/2)/self.r()
    end
    if n_elements(zrange) ne 2 then begin
      zrange=[-1.5,1.5]
      self.nz=mean([self.nx,self.ny])
    endif
   endelse
  end
 self->Reset
 self->SetRefModel,MOI
 self->NewGrid,xrange=xrange, yrange=yrange,zrange=zrange,compute_grid=compute_grid
 if obj_valid(moi) then moi->DisplayMap
 self->TV_SLICE
end

pro gxScanBox::NewGrid,xrange=xrange,yrange=yrange,zrange=zrange,nx=nx,ny=ny,compute_grid=compute_grid;,upload=upload
    self.Grid2Update=1
    newbox=1
    ;nz=0
    self.ImgViewWid->GetProperty,model=MOI  
    default,xrange,self.xrange
    default,yrange,self.yrange
    default,zrange,self.zrange
    default,nx,self.nx
    default,ny,self.ny
    default,nz,self.nz
    sdata=gx_getboxedges(xrange=xrange,yrange=yrange,zrange=zrange)
    if isa(moi,'gxmodel') then begin
     flags=(moi->GetVolume())->setflags(/newGrid)
     if keyword_set(compute_grid) then begin
       self.Grid2Update=moi->ReplaceScanboxData(gx_transform(sdata,moi->GetSTM()),nx=nx,ny=ny,/compute_grid)
     endif else self.Grid2Update=flags.NewGrid
     sroi=moi->getroi(/scanbox)
     dim=sroi->GetDim()
     nz=dim[2]
    endif
    self.roi->ReplaceData,sdata
    self.xrange=xrange
    self.yrange=yrange
    self.zrange=zrange
    self.nx=nx
    self.ny=ny
    self.nz=nz
    self.dx=delta(self.xrange)/self.nx
    self.dy=delta(self.yrange)/self.ny
    self->UpdateFields
    widget_control,self.wslice,get_value=index
    index=(index<(self.ny-1)>0)
    widget_control,self.wslice,SET_SLIDER_MAX=((self.ny-1)>0)
    widget_control,self.wLOS,SET_SLIDER_MAX=((self.nx-1)>0)
    widget_control,self.wslice,set_value=index
    
    dS=self.dx*self.dy*(gx_rsun()^2)
    dR=self.dz*(gx_rsun()) 
    widget_control,self.wParmsTable,get_value=table
    parms=(*self.info).parms
    idx=self->name2idx('dS')
    if idx ge 0 then begin
      table[idx].value=dS
      parms[idx].value=dS
    endif else begin
      if tag_exist(*self.info,'rparms') and widget_valid(self.wRparms) then begin
        wdS=widget_info(self.wRparms,find_by_uname='dS')
        if widget_valid(wdS) then widget_control,wdS,set_value=dS
        idx=gx_name2idx((*self.info).rparms,'dS')
        if idx ge 0 then begin
          rparms=(*self.info).rparms
          rparms[idx].value=ds
          (*self.info).rparms=rparms
        endif
      endif
    endelse
    idx=self->name2idx('dR')
    if idx ge 0 then begin
      table[idx].value=dR
      parms[idx].value=dR
    end
    (*self.info).parms=parms
    widget_control,self.wParmsTable,set_value=table
    
    self.pData=(self.ImgViewWid)->NewView(self.info,nx=self.nx,ny=self.ny,xrange=self.xrange,yrange=self.yrange)
    self->MakeGrid
    if keyword_set(compute_grid) then begin
      self.ImgViewWid->OnStartScan
      self->Slice,index
    endif
end

pro gxScanBox::UpdateGrid
  MOI=self->GetRefModel()
  if ~obj_isa(MOI,'gxModel') then return
  grid=(Moi->GetGrid())
  if ~ptr_valid(grid) then return
  rendering_grid=*grid
  rg=rendering_grid
  id=MOI->GetVoxelId()
  xgrid=reform(rendering_grid[1,*,*,*])
  ygrid=reform(rendering_grid[2,*,*,*])
  zgrid=reform(rendering_grid[3,*,*,*])
  boundary=where(id[xgrid,ygrid,zgrid] and ishft(1L,30) ne 0,bcount)
  if bcount gt 0 then begin
   xgrid[boundary]=fix(xgrid[boundary])
   ygrid[boundary]=fix(ygrid[boundary])
   zgrid[boundary]=fix(zgrid[boundary])
   rg[1,*,*,*]=xgrid
   rg[2,*,*,*]=ygrid
   rg[3,*,*,*]=zgrid
  endif
end



pro gxScanBox::MakeGrid
 
 if self.nx eq 0 or self.ny eq 0 or self.nz eq 0 then return
 
 ptr_free,self.grid
 
 empty_slice=fltarr(self.nx,self.nz)
  
 self.grid=ptr_new({grid:ptr_new(),B:dblarr(self.nx*self.nz,3),$
   parms:dblarr(self.nx,self.nz,n_elements((*self.info).parms)),slice:empty_slice})    
               
end

pro gxScanBox::CleanGrid,oculted=oculted  
  if ptr_valid(self.grid) then begin
     if  n_elements(oculted) gt 0 then begin
     sz=size((*self.grid).parms)
     for i=2,sz[3]-1 do  begin
      temp=(*self.grid).parms[*,*,i]
      temp[oculted]=0
      (*self.grid).parms[*,*,i]=temp
     end
     endif else for i=1,n_tags(*self.grid)-1 do (*self.grid).(i)[*]=0    
  endif
end

pro gxScanbox::DrawSlicer,column,row
  default,column,0
  default,row,0
  p=dblarr(3,8)
  for i=0,7 do p[*,i] = [self.xrange[(i AND 1)], self.yrange[((i/2) AND 1)], self.zrange[((i/4) AND 1)]]
  data=p[*,[0,4,5,1]]
  data[1,*]=self.yrange[0]+(row+0.5)*self.dy
  self.slicer->SetProperty,data=data
  self->DrawPixel,column,row
end  

PRO gxScanBox::Slice,row
  time0=systime(/s)
  if n_elements(row) eq 0 then begin
    widget_control,self.wSlice,get_value=row
    default,row,0
  endif
  widget_control,self.wSlice,set_value=row
  widget_control,self.wLOS,get_value=column
 
  self->GetProperty,parent=oSun
  if ~obj_isa(osun,'gxsun') then return
  self->CleanGrid   
  self->DrawSlicer,column,row
  
  self.ImgViewWid->GetProperty,model=model
  if ~obj_valid(model) then begin
    goto, unassigned
  endif
 scanner=self.grid
 model->Slice,(*self.info).parms,row,scanner=scanner
 self.grid=scanner
unassigned: 
if ptr_valid(self.grid) then self->TV_Slice    
END

pro gxScanBox::SaveLOS
 file=dialog_pickfile(filter='*.los',$
                   DEFAULT_EXTENSION='los',$
                   /write,/OVERWRITE_PROMPT,$
                   title='Please select a file to log LOS slices for current FOV')
 if file ne '' then begin
  widget_control,/hourglass
  self->CleanGrid
  ;order  matters
  self->UpdateAllParms
  self.ImgViewWid->OnStartScan
  self.ImgViewWid->GetProperty,fovmap=fovmap
  ;order matters
  MULTI_SAVE,/new,log,{row:-1L,parms:(*self.grid).parms,$
    grid:transpose(reform((*(*self.grid).grid)[*,*,0,*]),[1,2,0])},file=file,$
     header={renderer:self.renderer ,info:(*self.info),fovmap:fovmap,nx:self.nx,ny:self.ny,xrange:self.xrange,yrange:self.yrange,ebtel:gx_ebtel_path()}

  
  for row=0l,self.ny-1 do begin
   self->slice,row
   self->TV_SLICE
   MULTI_SAVE,log,{row:row,parms:(*self.grid).parms,$
    grid:transpose(reform((*(*self.grid).grid)[*,*,row,*]),[1,2,0])}
  end
  close,log
 end
end

pro gxScanbox::CheckFreqList
 if widget_valid(self.wUploadFreqList) then begin
  f_min_check= self->GetParmValue('f_min') ne self->GetParmValue('f_min',/gui)
  df_check= self->GetParmValue('df') ne self->GetParmValue('df',/gui)
  Nfreq_check= self->GetParmValue('N_freq') ne self->GetParmValue('N_freq',/gui)
  check=f_min_check or df_check or Nfreq_check
  if check then widget_control,self.wUseFreqList,set_value=0
  widget_control,self.wUseFreqList,get_value=UseFreqList
  if UseFreqList then begin
    widget_control, self.wFreqList,get_value=freqlist
    freqlist=double(str2arr(freqlist))
    info=*self.info
    spectrum=info.spectrum
    x=spectrum.x
    x=rep_tag_value(x,freqlist,'axis')
    spectrum=rep_tag_value(spectrum,x,'x')
    info=rep_tag_value(info,spectrum,'spectrum')
    ptr_free,self.info
    self.info=ptr_new(info)
  endif
 endif
end

function gxScanBox::FormatFreqList,freqlist
  if n_elements(freqlist) gt 1 then freqlist=float(freqlist)
  return,strcompress(str_replace(arr2str(freqlist[sort(freqlist)],/trim_str,/compress),',',', '))
end

pro gxScanBox::SaveFreqList
  file = DIALOG_PICKFILE(FILTER='*.txt',TITLE='Please select a filename to save the current comma-separted frequency list file (GHz)',path=curdir())
  if file eq '' then return
  widget_control,self.wFreqList,get_value=freqlist
  OPENW, lun, file, /GET_LUN
  WRITEU, lun, freqlist
  FREE_LUN, lun
end

pro gxScanBox::UploadFreqList
  file = DIALOG_PICKFILE(FILTER='*.txt',TITLE='Please select an instrument specific comma-separated frequency list file (GHz)',path=gx_findfile(folder='freqlists'))
  if file eq '' then return
  widget_control,self.wUseFreqList,set_value=0
  OPENR, lun, file, /GET_LUN
  ; Read one line at a time, saving the result into array
  line = ''
  k=0
  WHILE NOT EOF(lun) DO BEGIN
    READF, lun, line & $
      freqlist = (k eq 0)?line:[freqlist, line]
    k+=1
  ENDWHILE
  ;Close the file and free the file unit
  FREE_LUN, lun
  widget_control,self.wFreqList,set_value=self->FormatFreqList(freqlist)
  edited=self->CheckIfEditedFreqList()
end

pro gxScanBox::UseFreqList,event
  ;if ~(self->CheckIfEditedFreqList()) then return
  if event.value eq 1 then begin
    widget_control, self.wFreqList,get_value=freqlist
    freqlist=double(str2arr(freqlist))
    good=where(freqlist gt 0,count)
    if count eq 0 then begin
      answ=dialog_message(['There is no valid frequency in the current list!','Please upload a valid comma-separated frequency list or type in one.'],/info)
      widget_control,event.id,set_value=0
    endif else begin
      freqlist=strcompress(freqlist[good],/rem)
      widget_control,self.wFreqList,set_value=self->FormatFreqList(freqlist)
      self->ReplaceParmValue,'f_min',0
      self->ReplaceParmValue,'df',0
      self->ReplaceParmValue,'N_freq',count
      self->UpdateAllParms
    endelse
  endif
  edited=self->CheckIfEditedFreqList()
end

function gxScanBox::CheckIfEditedFreqList
  widget_control,self.wFreqList,get_value=old_freqlist
  new_freqlist=self->FormatFreqList((*self.info).spectrum.x.axis)
  edited=~array_equal(old_freqlist,new_freqlist)
  widget_control,self.wEditedFreqList,set_value=edited
  return,edited
end

pro gxScanbox::UpdateAparms
  self.ImgViewWid->GetProperty,model=MOI
  if isa(MOI) then begin
    if tag_exist(*self.info,'aparms') then begin
     aparms=moi->concatenate_aparms()
     if size(aparms,/tname) eq 'STRUCT' then begin
       info=*self.info
       info=rep_tag_value(info,aparms,'aparms',/rep)
       ptr_free,self.info
       self.info=ptr_new(info)
     end
    endif
  endif
end

pro gxScanbox::UpdateAllParms
 self->CheckFreqList
 self->UpdateAparms
 widget_control,self.wParmsTable,get_value=Parms
 if widget_valid(self.wNparms) then begin
   widget_control,self.wNparms,get_value=nparms
   (*self.info).nparms.value=nparms
 endif
 if widget_valid(self.wRparms) then begin
   widget_control,self.wRparms,get_value=rparms
   (*self.info).rparms.value=rparms
 endif
 (*self.info).parms=Parms
 info=self->RendererInfo(*self.info)
 ptr_free,self.info
 self.info=ptr_new(info)
 if widget_valid(self.wFreqList) then begin
  if self->CheckIfEditedFreqList() then widget_control,self.wUseFreqList,set_value=0
  freqlist=self->FormatFreqList((*self.info).spectrum.x.axis)
  widget_control,self.wFreqList,set_value=freqlist
  widget_control,self.wUndoFreqList,set_uvalue=freqlist
 end
 self.pData=(self.ImgViewWid)->NewView(self.info,nx=self.nx,ny=self.ny,xrange=self.xrange,yrange=self.yrange)
 self->UpdateFields
 self.ImgViewWid->Draw
 self->MakeGrid
 self->Slice
 self->ResetAllBridges
end

function gxScanbox::HandleEvent,event
subdirectory=['resource', 'bitmaps']
case event.id of   
  self.wParmsTable: self->UpdateAllParms 
  self.wNParms: self->UpdateAllParms 
  self.wRParms: self->UpdateAllParms 
  self.wUploadFreqList: self->UploadFreqList  
  self.wUseFreqList: self->UseFreqList,event
  self.wFreqList:edited=self->CheckIfEditedFreqList()
  self.wSaveFreqList:self->SaveFreqList
  self.wResetFreqList:begin
                       widget_control,event.id,get_uvalue=uvalue
                       if n_elements(uvalue) eq 3 then begin
                           widget_control,self.wUseFreqList,set_value=0
                           self->ReplaceParmValue,'f_min',uvalue[0]
                           self->ReplaceParmValue,'df',uvalue[1]
                           self->ReplaceParmValue,'N_freq',uvalue[2]
                           self->UpdateAllParms
                       endif
                      end
  self.wUndoFreqList:begin
                      widget_control,self.wUndoFreqList,get_uvalue=undo
                      if n_elements(undo) ne 0 then widget_control,self.wFreqList,set_value=undo
                      widget_control,self.wEditedFreqList,set_value=1
                      widget_control,self.wUseFreqList,set_value=0
                     end
  self.wDelFreqList: begin
                      widget_control,self.wFreqList,get_value=undo
                      widget_control,self.wEditedFreqList,set_value=1
                      widget_control,self.wFreqList,set_value=''
                      if (n_elements(undo) ne 0) then  if  (undo ne '') then widget_control,self.wUndoFreqList,set_uvalue=undo
                      widget_control,self.wUseFreqList,set_value=0
                     end 
  self.wSquareFOV: begin
                    square=widget_info(self.wSquareFOV,/button_set)
                    if event.select  then begin
                      self.Ny=self.Nx
                      self.Yrange=self.Xrange
                      widget_control,self.wNx,set_value=self.Nx
                      widget_control,self.wNy,set_value=self.Ny,sensitive=0
                      widget_control,self.wYrange,sensitive=0
                      widget_control,self.wXrange,set_value=delta(self.Xrange)*self.R()
                      widget_control,self.wYrange,set_value=delta(self.YRange)*self.R()
                    endif else begin
                      widget_control,self.wNy,sensitive=1
                      widget_control,self.wYrange,sensitive=1
                    endelse
                   end
                   
 self.wXrange: begin
                 square=widget_info(self.wSquareFOV,/button_set)
                 widget_control,self.wXrange,get_value=xrange
                 widget_control,self.wX,get_value=xc
                 widget_control,self.wY,get_value=yc
                 self.Xrange=xc+[-1,1]*xrange/self.R()/2
                 if square then begin
                  yrange=(yc+[-1,1]*xrange/2)
                  self.Yrange=yrange/self.R()
                  widget_control,self.wYrange,set_value=delta(yrange)
                 endif
                 auto=1
               end                  
               
  self.wNx: Begin
    widget_control,event.id,get_value=value
    value=value>8
    self.Nx=value
    widget_control,self.wNx,set_value=self.Nx
    if self->SquareFOV() then begin
      self.Ny=self.Nx
      widget_control,self.wNy,set_value=self.Ny
      auto=1
    endif
  End 
  
  self.wNy: Begin
    widget_control,event.id,get_value=value
    value=value>8
    self.Ny=value
    widget_control,self.wNy,set_value=self.Ny
    auto=1
  End            
  
  self.wNz: Begin
             widget_control,event.id,get_value=value
             self.Nz=value
             auto=1
            END  
  self.wX:auto=1
  self.wY:auto=1          
  self.wHideScanbox: Begin
              ; Set the button state if called manually.
               if (WIDGET_INFO(event.id, /BUTTON_SET) ne event.select) then $
               widget_control, event.id, SET_BUTTON=event.select
               self->SetProperty,Hide=event.select
               tooltip=~event.select?'Hide Scanbox':'Show Scanbox'
               widget_control, event.id,/bitmap,set_value=~event.select?gx_bitmap(filepath('image.bmp', $
               subdirectory=subdirectory)):gx_bitmap(filepath('eye_closed.bmp', subdirectory=subdirectory)),tooltip=tooltip
            END 
  self.wHideVolume: Begin
              ; Set the button state if called manually.
               if (WIDGET_INFO(event.id, /BUTTON_SET) ne event.select) then $
               widget_control, event.id, SET_BUTTON=event.select 
                widget_control,widget_info(event.top,Find_By_Uname='STATEBASE'),get_uvalue=state
                container=state.view->Get(/all,count=count)
                if obj_isa(container,'gxSun') then models=container->Get(/all,ISA='gxComponent',count=count)
                if obj_isa(container,'gxModel') then begin
                  models=self.parent->Get(/all,isa='gxModel',count=count)
                  if count eq 0 then models=container else models=[models,container]
                  count=n_elements(models)
                end  
                for i=0,count-1 do begin
                 (models[i]->GetVolume())->SetProperty,hide=event.select
                end
               tooltip=~event.select?'Hide Volume':'Show Volume'  
               widget_control, event.id,/bitmap,set_value=~event.select?gx_bitmap(filepath('image.bmp', $
               subdirectory=subdirectory)):gx_bitmap(filepath('eye_closed.bmp', subdirectory=subdirectory)),tooltip=tooltip
            END 
 self.wHideSun: Begin
              widget_control,widget_info(event.top,Find_By_Uname='STATEBASE'),get_uvalue=state
              container=state.view->Get(/all,count=count)
              if obj_isa(container,'gxSun') then begin
               select=event.select 
               ((container->GetByName('Solar Grid'))->GetByName('Solar Surface'))->SetProperty,hide=select
              endif else begin
               select=0 
              endelse
              tooltip=~event.select?'Hide Sun':'Show Sun'
              widget_control, event.id,/bitmap,set_value=~event.select?gx_bitmap(filepath('image.bmp', $
              subdirectory=subdirectory)):gx_bitmap(filepath('eye_closed.bmp', subdirectory=subdirectory)),tooltip=tooltip,SET_BUTTON=select
            END             
  self.wSelectRenderer: begin
                         self->ReplaceRenderer,self->SelectRenderer()
                        end 
  self.wSelectEBTEL: begin
                          self->ReplaceEBTELtables
                        end                                                                
  self.wSlice: Begin
                 if ptr_valid(self.grid) then begin
                   self->Slice,event.value
                 end
                End  
  self.wPlotLOSOptions:Begin
                 if ptr_valid(self.grid) then begin
                   widget_control,self.wSlice,get_value=y
                   self->Slice,y
                 end
                End               
  self.wLOS: Begin
                 if ptr_valid(self.grid) then begin
                    x=event.value
                    widget_control,self.wSlice,get_value=y
                    self->DrawPixel,x,y
                    self->TV_SLICE
                 end
                End               
 self.wSliceSelect: Begin  
                     select=event.str  
                     update_volume:
                     self.ImgViewWid->GetProperty,model=model
                     if obj_valid(model) then begin
                        widget_control,self.wSliceSelect,get_uvalue=scale
                        widget_control,self.wSliceSelect,get_value=list
                        if n_elements(scale) ne n_elements(list) then begin
                         scale=replicate({range:[0d,0d],pwr_idx:1d},n_elements(list))
                        endif
                        volume=(model->GetVolume())
                        if n_elements(select) eq 0 then select=volume->Selected()
                        idx=(gx_list2idx(list,select))[0]
                        default,data_range,scale[idx].range
                        if array_equal(minmax(data_range),[0.0,0.0]) or keyword_set(rescale) then dummy=temporary(data_range)
                        default,pwr_idx,scale[idx].pwr_idx
                        if keyword_set(rescale) then dummy=temporary(pwr_idx)
                        volume->Update,select,range=data_range,pwr_idx=pwr_idx,/update;explicitely request volume update
                        widget_control,self.wMinVolume,Set_Value=data_range[0]
                        widget_control,self.wMaxVolume,Set_Value=data_range[1]
                        widget_control,self.wPowerIndexVolume,Set_Value=pwr_idx
                        scale[gx_list2idx(list,volume->Selected())].range=data_range
                        scale[gx_list2idx(list,volume->Selected())].pwr_idx=pwr_idx
                        widget_control,self.wSliceSelect,set_uvalue=scale
                     endif       
                End 
   self.wPowerIndexVolume: begin
                           widget_control,self.wMinVolume,Get_Value=min_data
                           widget_control,self.wMaxVolume,get_value=max_data
                           data_range=[min_data,max_data]
                           widget_control,self.wPowerIndexVolume,Get_Value=pwr_idx
                           goto,update_volume
                         end  
   self.wMinVolume: begin
                           widget_control,self.wMinVolume,Get_Value=min_data
                           widget_control,self.wMaxVolume,get_value=max_data
                           data_range=[min_data,max_data]
                           widget_control,self.wPowerIndexVolume,Get_Value=pwr_idx
                           goto,update_volume
                         end      
   self.wMaxVolume: begin
                           widget_control,self.wMinVolume,Get_Value=min_data
                           widget_control,self.wMaxVolume,get_value=max_data
                           data_range=[min_data,max_data]
                           widget_control,self.wPowerIndexVolume,Get_Value=pwr_idx
                           goto,update_volume
                         end    
   self.wBridges: begin
                   widget_control,/hourglass
                   widget_control,event.id,get_value=nbridges
                   nbridges=nbridges>1
                   widget_control,event.id,set_value=nbridges
                   if (self.bridges->Count() eq nbridges) then begin
                     message,'Requested number of bridges matches the number already existing, nothing to be done!',/info
                     goto, exit_bridges
                   endif
                   
                   widget_control,self.wTaskTable,get_value=bridge_state
                   
                   if (self.bridges->Count() gt nbridges) then begin
                     n=self.bridges->Count()-nbridges
                     message,string(n,format=(n gt 1)?"('Removing ',g0,' bridges')":"('Removing ',g0,' bridge')"),/info
                     for i= nbridges, self.bridges->Count()-1  do begin
                       bridge=self.bridges->Get(position=self.bridges->Count()-1 )
                       self.bridges->Remove,bridge
                       widget_control,self.wTaskTable,get_value=bridge_state
                       obj_destroy,bridge
                     endfor
                     bridge_state=bridge_state[0:nbridges-1]
                     ntasks=n_elements(bridge_state)
                     row_labels=string(1+indgen(ntasks),format='("THREAD",i2)')
                     widget_control,self.wTaskTable,set_value=bridge_state,row_labels=[row_labels]
                     goto,exit_bridges
                   endif
                   if (self.bridges->Count() lt nbridges) then begin
                     n=nbridges-self.bridges->Count()
                     message,string(n,format=(n gt 1)?"('Adding ',g0,' bridges')":"('Adding ',g0,' bridge')"),/info
                     start_index=self.bridges->Count()
                   endif
                   bridge_state=[bridge_state,replicate({status:'Initializing',task:'',time:'',calls:'',error:''},n)]
                   ntasks=n_elements(bridge_state)
                   row_labels=string(1+indgen(ntasks),format='("THREAD",i2)')
                   widget_control,self.wTaskTable,set_value=bridge_state,table_ysize=ntasks,row_labels=[row_labels]
                   for i=start_index,nbridges-1 do begin
                     message,strcompress(string(i+1,format="('Initializing bridge #',i3)")),/info
                     bridge=obj_new('gxBridge',userdata=self,out=GETENV('IDL_TMPDIR')+GETENV('USER')+strcompress('gxBridge'+string(i)+'.log',/rem))
                     if obj_valid(bridge) then begin
                       bridge->SetVar,'id',i
                       code=bridge->Status(error=error)
                       case code of
                         0:bridge_state[i].status='Idle'
                         1:bridge_state[i].status='Active'
                         2:bridge_state[i].status='Completed'
                         3:bridge_state[i].status='Error'
                         4:bridge_state[i].status='Aborted'
                         else:bridge_state[i].status='Unknown'
                       endcase
                       bridge_state[i].error=error
                       self.bridges->Add,bridge
                       widget_control,self.wTaskTable,set_value=bridge_state
                       widget_control,self.wBridges,set_value=self.bridges->Count()
                     end
                   end
                   exit_bridges:
                   do_nothing:
                  end                      
   self.wResetVolumeScale: begin
                             rescale=1
                             goto,update_volume
                           end                                                                                                                                     
   self.wTV_Slice: self->TV_SLICE    
   self.wSaveLOS:self->SaveLOS  
   self.wObserver:self->SetLos  
   self.wL0:self->SetLos,/user
   self.wB0:self->SetLos,/user
   self.wR:self->SetLos,/user                                                                                    
 else:
 endcase
return, self->Rewrite(event,auto=auto)
END

pro gxScanBox::SetLos,user=user,model=model
  self.ImgViewWid->GetProperty,model=moi
  if obj_valid(moi) and keyword_set(model) then begin
    widget_control,self.wObserver,set_combobox_select=moi->SpaceView()
    widget_control,self.wL0,set_value=moi->GetLos(/L0)
    widget_control,self.wB0,set_value=moi->GetLos(/B0)
    widget_control,self.wR, set_value=moi->GetLos(/R)*60
    user=1
  endif
  view=widget_info(self.wObserver,/combobox_gettext)
  widget_control,self.wObserver,get_value=view_list,get_uvalue=pbrl_list
  idx=where(view_list eq view)
  case view of
    'Earth View': begin
                    EarthView=1
                    if obj_valid(moi) then begin
                      moi->SetLos,/EarthView
                      widget_control,self.wL0,set_value=moi->GetLos(/L0)
                      widget_control,self.wB0,set_value=moi->GetLos(/B0)
                      widget_control,self.wR, set_value=moi->GetLos(/R)*60
                    endif else begin
                      widget_control,self.wL0,set_value=pbrl_list[idx].L0
                      widget_control,self.wB0,set_value=pbrl_list[idx].B0
                      widget_control,self.wR, set_value=pbrl_list[idx].R*60
                    endelse
                  end
    else: begin
            if keyword_set(user) then begin
              widget_control,self.wL0,get_value=L0
              widget_control,self.wB0,get_value=B0
              widget_control,self.wR, get_value=R
              pbrl_list[idx].L0=l0
              pbrl_list[idx].B0=B0
              pbrl_list[idx].R=R/60
            endif
            widget_control,self.wL0,set_value=pbrl_list[idx].L0
            widget_control,self.wB0,set_value=pbrl_list[idx].B0
            widget_control,self.wR, set_value=pbrl_list[idx].R*60
            if obj_valid(moi) then moi->SetLos, struct=pbrl_list[idx]
          end              
  endcase
        widget_control,self.wL0,get_value=L0,sensitive=~keyword_set(EarthView)
        widget_control,self.wB0,get_value=B0,sensitive=~keyword_set(EarthView)
        widget_control,self.wR,get_value=R,sensitive=~keyword_set(EarthView)
        pbrl_list[idx].b0=L0
        pbrl_list[idx].b0=B0
        pbrl_list[idx].R=R/60
        widget_control,self.wObserver,set_uvalue=pbrl_list
        if obj_valid(moi) then moi->ResetPosition
        self.parent->Reset
end

function gxScanBox::GetLos,b0=b0,r=r,L0=L0
  self.ImgViewWid->GetProperty,moi=moi
  if obj_valid(moi) then return, moi->GetLos(b0=b0,r=r,L0=L0,pbR=pbR,pb0R=pb0R)
  p=0;to b addressed at a later tim, for now it is not used anywhere
  widget_control,self.wL0,get_value=thisL0
  widget_control,self.wB0,get_value=thisB0
  widget_control,self.wR,get_value=thisR
  if keyword_set(b0) then return,thisb0
  if keyword_set(r) then return,thisR/60
  if keyword_set(l0) then return,thisl0
  L0=thisL0
  return,{p:p,b0:thisb0,r:thisR/60,l0:thisl0}
end

pro gxScanBox::ReplaceEBTELtables,path=path
 if self.active then begin
  answ=dialog_message('There is an active scan in progress.'+string(10b)+$
    'Any unsaved results will be lost!'+string(10b)+$
    'Do you want to continue anyway?',/question)
  if strupcase(answ) eq 'NO' then return
 end
 path=gx_ebtel_valid_path(path)?path:dialog_pickfile(path=file_dirname(gx_findfile('ebtel.sav')),default='.sav')
 if gx_ebtel_valid_path(path) then begin
  widget_control,self.wEbtelTable,set_value=gx_ebtel_path(path)
  bridges=self.bridges->Get(/all,count=count)
  for i=0, count-1 do begin
    self->BridgeResetEBTEL,bridges[i]
  endfor
  self.ImgViewWid->GetProperty,model=MOI 
  if isa(MOI) then begin
    volume=moi->GetVolume()
    flags=(volume->setflags(newNT=volume->NewNT()))
    volume->Update
  endif
 if not keyword_set(ss) and widget_valid(self.wNparms) then begin
  widget_control,self.wNparms,get_uvalue=names,get_value=nparms
  idx=where(strupcase(strcompress(names,/rem)) eq 'N_TEMP',count)
  if count eq 1 then begin
    restore,gx_ebtel_path(path)
    hasDDM=(n_elements(DDM_cor_run) gt 0)?1:0
    nparms[idx]=n_elements(logtdem)
    idx=where(strupcase(strcompress(names,/rem)) eq 'DDM_KEY',count)
    if count eq 1 then begin
      nparms[idx]=1-hasDDM
      wDDM_key=widget_info(self.wNparms,find_by_uname='DDM_key')
      if widget_valid(wDDM_key) then widget_control,wDDM_key,sensitive=hasDDM
    endif
    widget_control,self.wNparms,set_value=nparms
    (*self.info).nparms.value=nparms
  endif
 endif
 endif else answ=dialog_message('This is not a valid EBTEL file, operation aborted!',/info)
end

pro gxScanBox::DrawPixel,x,y
  data=dblarr(3,4)
  data[2,*]=self.zrange[1]
  data[0,[0,1,2,3]]=self.xrange[0]+(x+0.5)*self.dx+[-self.dx,self.dx,self.dx,-self.dx]/2
  data[1,[0,1,2,3]]=self.yrange[0]+(y+0.5)*self.dy+[-self.dy,-self.dy,self.dy,self.dy]/2
  self.profiler->SetProperty,data=data
end

pro gxScanBox::BridgeReset,bridge
 bridge->Reset    
 gxGetBridgeVar,bridge,(*self.info).execute,vars
 for k=0,n_elements(vars)-1 do bridge->Execute,'delvar,'+vars[k]
 break_file, self.renderer, disk_log, dir, IDL_Renderer, ext
 dirpath=file_dirname(self.renderer,/mark)
 bridge->SetVar,'dirpath',dirpath
 bridge->SetVar,'IDL_Renderer',IDL_Renderer
 bridge->Execute,'cd,dirpath'
 bridge->Execute,'RESOLVE_ROUTINE, IDL_Renderer, /COMPILE_FULL_FILE ,/either' 
 bridge->SetVar,'rowdata',make_array([self.nx,(*self.info).pixdim],/float)
 bridge->SetVar,'calls',0
end

pro gxScanBox::ResetAllBridges
    self->OnAbortScan
    bridges=self.bridges->Get(/all,count=count)
    for i=0, count-1 do begin
     self->BridgeReset,bridges[i]
     bridges[i]->SetVar,'id',i
     bridges[i]->SetVar,'row',-1
     t_start=systime(/s)
     bridges[i]->SetVar,'t_start',t_start
     bridges[i]->SetVar,'calls',0
    end
    self.active=0
    self.new_view=1
end

pro gxScanbox::BridgeResetEBTEL,bridge
  ebtel=gx_ebtel_path()
  bridge->Execute,'ebtel=GETENV("ebtel")'
  if bridge->GetVar('ebtel') ne ebtel then begin
    bridge->Execute,'setenv, "ebtel='+ebtel+'"'
    bridge->Execute,'message,"ebtel enviroment variable set to '+ebtel+'",/info'
  endif
end

function gxScanbox::ds
 return,self.dx*self.dy*((gx_rsun())^2)
end

pro gxScanBox::OnStartScan,event,debug=debug
 if event.select then begin
   if self.active eq 0 then begin
     if file_exist(GETENV('IDL_TMPDIR')+GETENV('USER')+'GX_Simulator.log') then begin
       answ=dialog_message(/question,/cancel,'The previous scan has not been saved.'+string(10b)+$
                            'Starting a new scan would result in deleting any unsaved work!'+string(10b)+$
                            'Do you want to save it before starting a new scan?')
       case strupcase(answ) of
       'YES':if self.ImgViewWid->SaveLog() eq 0 then goto,cancel
       'CANCEL':begin
                 cancel:
                 self.pause=1
                 self.active=0
                 widget_control,self.wScan,set_button=0,sensitive=1
                 if widget_valid(self.wDebug) then begin
                   widget_control,self.wDebug,set_button=0,sensitive=1
                 endif
                 return
                end
       else:
       endcase                   
     end
     self.ImgViewWid->GetProperty,model=MOI
     if obj_valid(MOI) then begin
       self.Grid2Update=ptr_valid(moi->GetGrid())?((moi->GetVolume())->getflags()).newGrid:1
       if ~ptr_valid(self.grid) or self.Grid2Update then begin
         answ=dialog_message('You must compute the rendering grid to perform this operation. Do you want to proceed now?',/question)
         case strupcase(answ) of
           'YES': begin
             self->NewGrid,/compute
           end
           else:begin
                 goto,cancel
                end 
         endcase
       end
     end
     self->UpdateAllParms
     widget_control,self.wBridges,sensitive=0
     if ~keyword_set(Debug) then begin
      if widget_valid(self.wDebug) then widget_control,self.wDebug,sensitive=0
     endif else widget_control,self.wScan,sensitive=0
     prog_id=gx_progmeter(/init,label='Synthetic map computation progress')
     self.new_view=0
     self.row=-1 
     self.pause=0
     self.completed=0
     self.active=1
     widget_control,self.wStatusBar,set_value=''
     ; order matters!
     self.ImgViewWid->OnStartScan
     self.ImgViewWid->GetProperty,fovmap=fovmap
     ; order matters!
     self->CleanGrid
     (*self.pData)[*]=0
     self.t_start=systime(/s)
     MULTI_SAVE,/new,log,{row:-1L,parms:(*self.grid).parms,$
     data:make_array([self.nx,1,(*self.info).pixdim],/float),$
     grid:transpose(reform((*(*self.grid).grid)[*,*,0,*]),[1,2,0])},file=GETENV('IDL_TMPDIR')+GETENV('USER')+'GX_Simulator.log', $
     header={renderer:self.renderer ,info:(*self.info),fovmap:fovmap,nx:self.nx,ny:self.ny,xrange:self.xrange,yrange:self.yrange,ebtel:gx_ebtel_path()}
     self.log=log
   endif else begin
   self.active=1
   self.pause=0
   end
   bridges=self.bridges->Get(/all,count=count)
   widget_control,self.wTaskTable,get_value=bridge_state
   to_execute=bytarr(count)
;Scheduling LOOP   
    for i=0, count-1 do begin
     status=bridges[i]->Status()
     if (status eq 0) or (status eq 2) then begin
      self->BridgeReset,bridges[i]
      bridges[i]->SetVar,'id',i
      widget_control,self.wTaskTable,get_value=bridge_state
      bridge_state[i].time=''
      bridge_state[i].error=''
      bridge_state[i].calls=0
      if ~keyword_set(Debug) and (i lt self.ny) then begin
          self.row+=1   
          widget_control,self.wSlice,set_value=self.row
          self->Slice,self.row
          widget_control,widget_info(get_tlb(self.wBase),Find_By_Uname='STATEBASE'),get_uvalue=state
          if n_elements(state) ne 0 then state.oObjviewWid->Draw
          t_start=systime(/s)
          bridges[i]->SetVar,'t_start',t_start
          bridges[i]->SetVar,'t_end',t_start
          bridges[i]->SetVar,'parms',(*self.grid).parms
          if tag_exist(*self.info,'nparms') then bridges[i]->SetVar,'nparms',(*self.info).nparms.value
          if tag_exist(*self.info,'rparms') then bridges[i]->SetVar,'rparms',(*self.info).rparms.value
          if tag_exist(*self.info,'aparms') then begin
            bridges[i]->SetVar,'E_arr',(*self.info).aparms.E_arr
            bridges[i]->SetVar,'mu_arr',(*self.info).aparms.mu_arr
            bridges[i]->SetVar,'f_arr',(*self.info).aparms.f_arr
          endif
          bridges[i]->SetVar,'freqlist',(((*self.info).spectrum).x.axis)
          bridges[i]->SetVar,'calls',1
          bridges[i]->SetVar,'row',self.row   
          bridges[i]->SetVar,'OnDebug',0
          bridge_state[i].task=string(i+1,self.ny,format="(i4,' of ',i4)")
          bridge_state[i].status='Active'
          to_execute[i]=1     
      endif else begin
         bridge_state[i].task='None assigned'
         bridge_state[i].status='Idle'
      endelse
      widget_control,self.wTaskTable,set_value=bridge_state
     endif
    endfor
;ENDScheduling LOOP  
 if ~keyword_set(Debug) then begin      
  for id=0, count-1 do begin
    if to_execute[id] then bridges[id]->Execute,(*self.info).execute,/nowait
  endfor
 endif else begin
  t_start=systime(/s)
  bridges[0]->SetVar,'t_start',t_start
  bridges[0]->SetVar,'t_end',t_start
  bridge_state[0].task='On Debug'
  bridge_state[0].status='Active'
  bridges[0]->SetVar,'row',-1
  bridges[0]->SetVar,'OnDebug',1
  bridges[0]->Execute,'wait,0.1',/nowait
  widget_control,self.wTaskTable,set_value=bridge_state
 endelse
 endif else self.pause=1
end

pro gxScanBox::OnCallback,Status, Error,bridge
  t_end=systime(/s)
  bridge->SetVar,'t_end',t_end
  t_start=bridge->GetVar('t_start')
  row=bridge->GetVar('row')
  OnDebug=bridge->GetVar('OnDebug')
  id=bridge->GetVar('id')
  calls=bridge->GetVar('calls')
  if row ge 0 and row lt self.ny and status eq 2 then begin
   (*self.pData)[*,row,*,*,*]=bridge->GetVar('rowdata')
   self.completed+=1
   self.ImgViewWid->SelectImg
   parms=bridge->GetVar('parms')
   MULTI_SAVE,self.log,{row:long(row),parms:parms,data:(*self.pData)[*,row,*,*,*],$
    grid:transpose(reform((*(*self.grid).grid)[*,*,row,*]),[1,2,0])},$
    file=GETENV('IDL_TMPDIR')+GETENV('USER')+'GX_Simulator.log', header=(*self.info)
   widget_control,self.wTaskTable,get_value=bridge_state
   case status of
     0:bridge_state[id].status='Idle'
     1:bridge_state[id].status='Active'
     2:bridge_state[id].status='Completed'
     3:bridge_state[id].status='Error'
     4:bridge_state[id].status='Aborted'
     else:bridge_state[id].status='Unknown'
   endcase
   bridge_state[id].time=string(t_end-t_start,format="(g0,'s')")
   bridge_state[id].task=string(row+1,self.ny,format="(i4,' of ',i4)")
   bridge_state[id].calls=calls
   bridge_state[id].error=error
   widget_control,self.wTaskTable,set_value=bridge_state
   prog=float(self.completed)/(self.ny)
   if prog gt 0 then prog_status=gx_progmeter(prog_id,prog)
   status_message=strcompress(string(self.completed,self.ny,systime(/s)-self.t_start,format="('Progress: ',i4,' rows out of ',i4,' in process or processed in ',f10.3,' seconds')"))
   widget_control,self.wStatusBar,set_value=status_message
   if (self.completed eq self.ny) and (self.active eq 1) then begin
    self->OnEndScan
    return
   endif
  end 
  if (self.row lt self.ny-1) then begin
        if ~self.pause then begin
          self.row+=1   
          widget_control,self.wSlice,set_value=self.row
          self->Slice,self.row
          widget_control,widget_info(get_tlb(self.wBase),Find_By_Uname='STATEBASE'),get_uvalue=state
          if n_elements(state) ne 0 then state.oObjviewWid->Draw
          bridge->SetVar,'parms',(*self.grid).parms
          if tag_exist(*self.info,'nparms') then bridge->SetVar,'nparms',(*self.info).nparms.value
          if tag_exist(*self.info,'rparms') then bridge->SetVar,'rparms',(*self.info).rparms.value
          if tag_exist(*self.info,'aparms') then begin
            bridge->SetVar,'E_arr',(*self.info).aparms.E_arr
            bridge->SetVar,'mu_arr',(*self.info).aparms.mu_arr
            bridge->SetVar,'f_arr',(*self.info).aparms.f_arr
          endif
          bridge->SetVar,'freqlist',(((*self.info).spectrum).x.axis)
          bridge->SetVar,'calls',calls+1
          bridge->SetVar,'row',self.row
          bridge->SetVar,'t_start',systime(/s)
          wait,0.1
          ev=widget_event(self.wAbort,/nowait)
          if ev.id eq self.wAbort then self->OnAbortScan
          if OnDebug eq 1 then begin
           gxGetBridgeVar,bridge,(*self.info).execute,vars
           run=execute((*self.info).execute)
           for i=0,n_elements(vars)-1 do begin
            lookup=execute('var='+vars[i],1,1)
            if lookup eq 1 then begin
             bridge->SetVar,vars[i],var
            end
           end
           bridge->SetVar,'rowdata',rowdata
           bridge->Execute,'wait,0.1',/nowait
          endif else bridge->Execute,(*self.info).execute,/nowait
        endif    
    endif 
end

pro gxScanBox::OnEndSCan
  if self.log gt 0 then close,self.log
  if self.log gt 0 then free_lun,self.log
  self.active=0
  self.pause=1
  wait,1
  self.ImgViewWid->OnEndScan
  widget_control,self.wScan,set_button=0,sensitive=1
  if widget_valid(self.wDebug) then begin
    widget_control,self.wDebug,set_button=0,sensitive=1
  endif
  widget_control,self.wBridges,sensitive=1
  widget_control,get_tlb(self.wbase),/clear_events
end

pro gxScanBox::OnAbortScan
  self.active=1; to make sure deactivated buttons are properly reactivated, self.active will be set to zero by OnEndScan
  self.new_view=1
  bridges=self.bridges->Get(/all,count=count)
  for i=0, count-1 do begin
   CATCH, Error_status
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'OnAbortScan Error index: ', Error_status
      PRINT, 'OnAbortScan Error message: ', !ERROR_STATE.MSG
      goto,skip
      CATCH, /CANCEL
   ENDIF
   bridges[i]->Abort  
   skip:
   status=bridges[i]->Status() 
   code=bridges[i]->Status(error=error)
     widget_control,self.wTaskTable,get_value=bridge_state
      case code of 
       0:bridge_state[i].status='Idle'
       1:bridge_state[i].status='Active'
       2:bridge_state[i].status='Completed'
       3:bridge_state[i].status='Error'
       4:bridge_state[i].status='Aborted'
       else:bridge_state[i].status='Unknown'
      endcase
    bridge_state[i].error=error
    bridge_state[i].task=string(self.row,format='(i4)')
    widget_control,self.wTaskTable,set_value=bridge_state
   end
   self->OnEndScan
end

pro gxScanBox::TV_SLICE
  if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'
  widget_control,self.wGrid2Update,set_value=''
  if ~ptr_valid(self.grid) then begin
    widget_control,self.wGrid2Update,set_value='WARNING: No grid defined yet! Please compute the grid.'
    return
  endif
  self.ImgViewWid->GetProperty,model=MOI
  if obj_valid(MOI) then begin
    self.Grid2Update=((moi->GetVolume())->getflags()).newGrid
    if self.Grid2Update then $
      widget_control,self.wGrid2Update,set_value='WARNING: Grid needs to be updated! Please compute the grid to display valid data!'
  end
  retry:
  widget_control,self.wTV_Slice,get_value=window,get_uvalue=size
  widget_control,self.wPlotLOS,get_value=loswin
  ;_________________-
  name=widget_info(self.wSliceSelect,/COMBOBOX_GETTEXT)
  slice=(*self.grid).parms[*,*,self->name2idx(name)]
  case strupcase(name) of
    'VOXELID':slice=ishft(ishft(ulong(slice),16),-16)
    else:
  endcase
  widget_control,self.wLOS,get_value=x
  los=reform(slice[x,*])
  if self.nz eq 0 then return
  xaxis=findgen(self.nz)
  sz=size(slice)
  x=(x+0.5)*size[0]/self.nx
  cursor=[x,x]
  widget_control,self.wPlotLOSOptions,get_value=objPlotOptions
  objPlotOptions->GetProperty,range=range,xrange=pxrange,yrange=pyrange,xlog=xlog,ylog=ylog
  wset,window
  erase,255
  slice=congrid(slice,size[0],size[1])
  if keyword_set(ylog) then tvscl,alog10(slice),/nan else tvscl,slice
  plot,cursor,[0,size[1]],/noerase,xmargin=[0,0],ymargin=[0,0],xrange=[0,size[0]],yrange=[0,size[1]],xsty=5,ysty=5,linesty=2,thick=3
  wset,loswin
  good=where(finite(los) ne 0 and finite(xaxis) ne 0,count)
  if count gt 0 then $
  plot,xaxis,los,psym=10,color=0,back=255,xtitle='LOS (voxels)',/xsty,ytitle=name,ylog=max(los)gt min(los)?ylog:0,xlog=xlog,xrange=pxrange,yrange=pyrange
end     

pro gxScanBox::CreatePanel,_extra=_extra
 subdirectory=['resource', 'bitmaps']                                                           
 ;xlabelsize=36
 xtextsize=10
 device, get_screen_size=scr
 if not exist(xsize) then xsize = fix (scr[0] * .3)
 if not exist(ysize) then ysize = xsize 
 frame=1
 format='(f10.2)'
 wRow0=widget_base(self.wbase,/column)
 wToolbar=widget_base(wRow0,/row,/toolbar,/frame)
 self.wSaveLOS=widget_button( wToolbar, $
             value=gx_bitmap(filepath('copy.bmp', subdirectory=['resource', 'bitmaps'])), $
             /bitmap,tooltip='Save line of sight slices')
self.wGrid2Update=widget_label( wToolbar, value='',/Dynamic_Resize)            
              
 wColumn=widget_base(wRow0,/column)
 wTV_SliceBase=widget_base(wColumn,/row)  
 wLOSBase=widget_base(wTV_SliceBase,/column)              
         
 self.wTV_Slice = widget_draw(wLOSBase, $
        xsize=XSIZE, $
        ysize=YSIZE, $
        graphics_level=1, $
        retain=0, $
        /expose_events, $
        /button_events, $
        uvalue=[xSize,ySize], $
        Uname='LOS PROFILES')                         
 self.wSlice= WIDGET_SLIDER(wTV_SliceBase, MINIMUM = 0, $
      MAXIMUM =self.ny-1, VALUE = 0,  UNAME ='Slice',/vertical, font=!defaults.font)   
 
 self.wLOS= WIDGET_SLIDER(wLOSBase, MINIMUM = 0, $
      MAXIMUM =self.nx-1, VALUE = 0,  UNAME ='LOS',font=!defaults.font)      
      

 wPlotLOSBase=widget_base(wColumn,/row)
   
 self.wPlotLOS = widget_draw(wPlotLOSBase, $
        xsize=XSIZE, $
        ysize=0.35*YSIZE, $
        graphics_level=1, $
        retain=0, $
        /expose_events, $
        /button_events, $
        uvalue=[xSize,ySize], $
        Uname='LOS PLOT')        
self.wPlotLOSOptions=cw_objPlotOptions(wPlotLOSBase,uname='LOS Profile Plot Options',/ylog) 
  
 if widget_valid(self.winfo) then wColumn=self.winfo 

 wRow1=widget_base(wColumn,/row)  
 wRow2=widget_base(wColumn,/row,Event_FUNC='gxScanboxHandleEvent',uvalue=self)
 wRow3=widget_base(wColumn,/row,Event_FUNC='gxScanboxHandleEvent',uvalue=self)
 wRow4=widget_base(wColumn,/row,Event_FUNC='gxScanboxHandleEvent',uvalue=self)
 
 wSelect=widget_base(wRow4, /row,/toolbar)
 self.wSliceSelect= widget_combobox(wSelect, value=[((*self.info).parms).name,'B','curlB','divB+','divB-','helB+','helB-','Q0','Q','VoxelID'])
 widget_control,self.wSliceSelect,set_combobox_select=(where(((*self.info).parms).name eq 'n_0'))[0]

 self.wMinVolume=CW_objFIELD(wSelect, UNAME='MinVolume', LABEL=' Min:',$
   XTEXTSIZE=XTEXTSIZE*0.5, XLABELSIZE=XLABELSIZE,$
   INCREMENT=1, $
   UNITS='', $
   VALUE=1.11111111e32,Sensitive=1,frame=frame)
 widget_control, self.wMinVolume,set_value=0  

 self.wMaxVolume=CW_objFIELD(wSelect, UNAME='MaxVolume', LABEL=' Max:',$
   XTEXTSIZE=XTEXTSIZE*0.5, XLABELSIZE=XLABELSIZE,$
   INCREMENT=1, $
   UNITS='', $
   VALUE=1.11111111e32,Sensitive=1,frame=frame)
   
 widget_control, self.wMaxVolume,set_value=0  

 self.wPowerIndexVolume=CW_objFIELD(wSelect, UNAME='PowerIndex', LABEL=' Index',$
   XTEXTSIZE=XTEXTSIZE*0.5, XLABELSIZE=XLABELSIZE,$
   INCREMENT=0.1, $
   UNITS='', $
   VALUE=1.11,Sensitive=1,frame=frame)
 widget_control, self.wPowerIndexVolume, set_value=1 

 wButtonBase=widget_base( wrow4,/row,/nonexclusive,Event_FUNC='gxScanboxHandleEvent',uvalue=self,/toolbar)
 self.wResetVolumeScale=widget_button(wButtonBase, $
   value=gx_bitmap(filepath('reset.bmp', subdirectory=subdirectory)), $
   /bitmap,tooltip='Reset Volume Scale',uname='ResetVolumeScale')
   
 self.wHideVolume=widget_button(wButtonBase, $
   value=~self.hide?gx_bitmap(filepath('image.bmp', subdirectory=subdirectory)):$
   gx_bitmap(filepath('eye_closed.bmp', subdirectory=subdirectory)), $
   /bitmap,tooltip='Hide Volume',uname='HideVolume')
 self.wHideScanbox=widget_button( wButtonBase, $
   value=~self.hide?gx_bitmap(filepath('image.bmp', subdirectory=subdirectory)):$
   gx_bitmap(filepath('eye_closed.bmp', subdirectory=subdirectory)), $
   /bitmap,tooltip='Hide scanbox',uname='HideScanbox')
 self.wHideSun=widget_button( wButtonBase, $
   value=~self.hide?gx_bitmap(filepath('image.bmp', subdirectory=subdirectory)):$
   gx_bitmap(filepath('eye_closed.bmp', subdirectory=subdirectory)), $
   /bitmap,tooltip='Hide Sun',uname='HideSun')
 
 self.wNx=CW_objFIELD(wRow2, UNAME='Nx', LABEL=' Nx',$
   XTEXTSIZE=XTEXTSIZE*0.5, XLABELSIZE=XLABELSIZE,$
   INCREMENT=64, $
   UNITS='', $
   VALUE=self.Nx,Sensitive=1,frame=frame)
 self.wNy=CW_objFIELD(wRow2, UNAME='Ny', LABEL=' Ny',$
   XTEXTSIZE=XTEXTSIZE*0.5, XLABELSIZE=XLABELSIZE,$
   INCREMENT=64, $
   UNITS='', $
   VALUE=self.Ny,Sensitive=1,frame=frame)
 widget_control, self.wNy,Sensitive=0 
 
 self.wNz=CW_objFIELD(wRow2, UNAME='Nz', LABEL=' Nz',$
   XTEXTSIZE=XTEXTSIZE*0.5, XLABELSIZE=XLABELSIZE,$
   INCREMENT=64, $
   UNITS='', $
   VALUE=self.Nz,Sensitive=0,frame=frame,/indicator)

 self.wdx=CW_objFIELD(wRow2, UNAME='dx', LABEL=' dx',$
   XTEXTSIZE=XTEXTSIZE*0.5, XLABELSIZE=XLABELSIZE,$
   INCREMENT=1, $
   UNITS='"', $
   VALUE=self.dx,Sensitive=0,frame=frame, format=format,/indicator)   
   
 self.wdy=CW_objFIELD(wRow2, UNAME='dy', LABEL=' dy',$
   XTEXTSIZE=XTEXTSIZE*0.5, XLABELSIZE=XLABELSIZE,$
   INCREMENT=1, $
   UNITS='"', $
   VALUE=self.dx,Sensitive=0,frame=frame, format=format,/indicator) 
 
 self.wL=CW_objFIELD(wRow2, UNAME='L', LABEL=' Lz',$
   XTEXTSIZE=XTEXTSIZE, XLABELSIZE=XLABELSIZE,$
   INCREMENT=0.2, $
   UNITS='"', $
   VALUE=self.zrange[1]-self.zrange[0],Sensitive=0,frame=frame, format=format,/indicator)   
    
 wSquareFOVbase=widget_base(wRow2,/row,/nonexclusive)
 self.wSquareFOV=widget_button(font=font,wSquareFOVbase ,value='Square FOV',uname='SquareFOV') 
 Widget_Control,self.wSquareFOV,Set_Button=1

 self.wx=CW_objFIELD(wRow3, UNAME='X', LABEL=' Xc',$
        XTEXTSIZE=XTEXTSIZE, XLABELSIZE=XLABELSIZE,$
        INCREMENT=10, $
        UNITS='"', $
        VALUE=mean(self.xrange),Sensitive=1,frame=frame, format=format)     
 self.wy=CW_objFIELD(wRow3, UNAME='Y', LABEL=' Yc',$
        XTEXTSIZE=XTEXTSIZE, XLABELSIZE=XLABELSIZE,$
        INCREMENT=10, $
        UNITS='"', $
        VALUE=mean(self.Yrange),Sensitive=1,frame=frame, format=format)                       
 self.wXrange=CW_objFIELD(wRow3, UNAME='Xrange', LABEL=' Xrange',$
        XTEXTSIZE=XTEXTSIZE, XLABELSIZE=XLABELSIZE,$
        INCREMENT=10, $
        UNITS='"', $
        VALUE=delta(self.Xrange),Sensitive=1,frame=frame, format=format)       
 self.wYrange=CW_objFIELD(wRow3, UNAME='Yrange', LABEL=' Yrange',$
        XTEXTSIZE=XTEXTSIZE, XLABELSIZE=XLABELSIZE,$
        INCREMENT=10, $
        UNITS='"', $
        VALUE=delta(self.Yrange),Sensitive=1,frame=frame, format=format)
 if obj_valid(self.ImgViewWid) then begin
  self.ImgViewWid->GetProperty,model=moi
  if obj_valid(moi) then begin
    Spaceview=moi->SpaceView()
    l0=moi->GeLos(/l0)
    b0=moi->GeLos(/b0)
    r=moi->GeLos(/r)
  endif else begin
    SpaceView=0
    pbr=pb0r()
    l0=0
    b0=pbr[1]
    r=pbr[2]*60
  endelse
 endif
 pbrl_list=replicate({p:0d,b0:0d,r:0d,l0:0d},2)
 pbrl_list[SpaceView].b0=L0
 pbrl_list[SpaceView].b0=B0
 pbrl_list[SpaceView].R=R/60
 self.wObserver= widget_combobox(wRow2, value=['Earth View','Space View'],uvalue=pbrl_list) 
 widget_control,self.wObserver,SET_COMBOBOX_SELECT=SpaceView
 
 self.wL0=CW_objFIELD(wRow3, UNAME='L0', LABEL=' L0',$
        XTEXTSIZE=3, XLABELSIZE=XLABELSIZE,$
        INCREMENT=10, $
        UNITS=STRING(176b), $
        VALUE=l0,Sensitive=1,frame=frame, format=format)  
 widget_control,self.wL0,sensitive=keyword_set(SpaceView)        
 self.wB0=CW_objFIELD(wRow3, UNAME='B0', LABEL=' B0',$
        XTEXTSIZE=3, XLABELSIZE=XLABELSIZE,$
        INCREMENT=10, $
        UNITS=STRING(176b), $
        VALUE=b0,Sensitive=1,frame=frame, format=format) 
 widget_control,self.wB0,sensitive=keyword_set(SpaceView)        
 self.wR=CW_objFIELD(wRow3, UNAME='RSUN', LABEL='RSUN',$
        XTEXTSIZE=4, XLABELSIZE=XLABELSIZE,$
        INCREMENT=10, $
        UNITS='"', $
        VALUE=R,Sensitive=1,frame=frame, format=format)   
 widget_control,self.wR,sensitive=keyword_set(SpaceView)                     
 wAutobase=widget_base(wRow3,/row,/nonexclusive)

 self->UpdateFields

 geometry=widget_info(widget_info(self.wInfo,/parent),/geometry)
 
 
 self.wModelInfo=widget_text(wRow1,value='NO GX MODEL SELECTED YET!',/align_center,scr_xsize=geometry.xsize)      
                                                   
end

function gxScanbox::R
  self.ImgViewWid->GetProperty,model=moi
  if obj_isa(moi,'gxmodel') then begin
    self.Rsun=moi->Rsun()
  endif else begin
    if self.Rsun eq 0 then begin
      pbr=pb0r()
      self.Rsun=pbr[2]*60
    end
  endelse
  return,self.Rsun
end


pro gxScanbox::SetDim,dim
 if n_elements(dim) ne 3 then return
 self.nx=dim[0]
 self.ny=dim[1]
 self.nz=dim[2]
 self->UpdateFields
end  



pro gxScanbox::UpdateFields
  R=self->R()

  widget_control,self.wNx,set_value=self.nx
  widget_control,self.wNy,set_value=self.ny
  widget_control,self.wNz,set_value=self.nz
  
  widget_control,self.wdx,set_value=R*self.dx
  widget_control,self.wdy,set_value=R*self.dy
  
  widget_control,self.wx,set_value=R*mean(self.xrange)
  widget_control,self.wy,set_value=R*mean(self.yrange)
  widget_control,self.wXrange,set_value=R*delta(self.xrange)
  widget_control,self.wYrange,set_value=R*delta(self.yrange)
  widget_control,self.wl,set_value=R*(self.zrange[1]-self.zrange[0])
  
  dS=self.dx*self.dy*(gx_rsun()^2)
  dR=self.dz*(gx_rsun())
  widget_control,self.wParmsTable,get_value=table
  parms=(*self.info).parms
  idx=self->name2idx('dS')
  if idx ge 0 then begin
    table[idx].value=dS
    parms[idx].value=dS
  endif
  if tag_exist(*self.info,'rparms') and widget_valid(self.wRparms) then begin
    wdS=widget_info(self.wRparms,find_by_uname='dS')
    if widget_valid(wdS) then widget_control,wdS,set_value=dS
    idx=gx_name2idx((*self.info).rparms,'dS')
    if idx ge 0 then begin
      rparms=(*self.info).rparms
      rparms[idx].value=ds
      (*self.info).rparms=rparms
    endif
  endif
  idx=self->name2idx('dR')
  if idx ge 0 then begin
    table[idx].value=dR
    parms[idx].value=dR
  end
  if tag_exist(*self.info,'nparms') and widget_valid(self.wNparms) then begin
    wNpix=widget_info(self.wNparms,find_by_uname='N_pix')
    if widget_valid(wNpix) then widget_control,wNpix,set_value=self.Nx
    idx=gx_name2idx((*self.info).nparms,'N_pix')
    if idx ge 0 then begin
      nparms=(*self.info).nparms
      nparms[idx].value=self.Nx
      (*self.info).nparms=nparms
    endif
    wNvox=widget_info(self.wNparms,find_by_uname='N_vox')
    if widget_valid(wNvox) then widget_control,wNvox,set_value=self.Nz
    idx=gx_name2idx((*self.info).nparms,'N_vox')
    if idx ge 0 then begin
      nparms=(*self.info).nparms
      nparms[idx].value=self.Nz
      (*self.info).nparms=nparms
    endif
    if tag_exist(*self.info,'aparms') then begin
      N_E=n_elements((*self.info).aparms.E_arr)
      wN_E=widget_info(self.wNparms,find_by_uname='N_E')
      if widget_valid(wN_E) then widget_control,wN_E,set_value=(N_E eq 1)?0:N_E
      N_mu=n_elements((*self.info).aparms.mu_arr)
      wN_mu=widget_info(self.wNparms,find_by_uname='N_mu')
      if widget_valid(wN_mu) then widget_control,wN_mu,set_value=(N_mu eq 1)?0:N_mu
    endif
  endif
  (*self.info).parms=parms
  widget_control,self.wParmsTable,set_value=table
end


function gxScanbox::SquareFOV
 return,widget_info(self.wSquareFOV,/button_set)
end

Function gxScanboxHandleEvent,event
 widget_control,event.handler,get_uvalue=ScanBox
 return,ScanBox->HandleEvent(event)
end

function gxScanbox::GetSliceSelector,item=item
 return,keyword_set(item)?widget_info(self.wSliceSelect,/COMBOBOX_GETTEXT):self.wSliceSelect
end

pro gxScanBox::Cleanup
  compile_opt hidden
  self->OnAbortScan
  ptr_free,self.grid
  ptr_free,self.pData
  ptr_free,self.info
  obj_destroy,self.bridges
  self->gxWidget::Cleanup
end
 
pro gxScanbox__define
self={gxScanbox,inherits IDLgrModel, inherits gxWidget,row:0L,completed:0l,$
Xrange:[0d,0d],Yrange:[0d,0d],Zrange:[0d,0d],Nx:0l,Ny:0l,Nz:0l,dx:0d,dy:0d,dz:0d,$
wX:0l,wY:0l,wL:0l,wXrange:0L,wyrange:0L,wNx:0l,wNy:0l,wNz:0l,wTV_Slice:0l,wInfo:0l,wToolbar:0l,wHideVolume:0l,TaskManager:obj_new(),ImgViewWid:obj_new(),$
wHideScanbox:0l,wHideSun:0l,wSlice:0l,wSliceSelect:0l,wSaveLOS:0l,wSquareFOV:0L,wdx:0l,wdy:0l,$;wAuto:0L,$
ROI:obj_new(),slicer:obj_new(),wParmsTable:0l,wScan:0L,wPause:0L,wAbort:0L,wDebug:0L,wTaskTable:0L,wBridges:0l,wStatusBar:0l,$
renderer:'',wRenderer:0l,wSelectRenderer:0l,pData:ptr_new(),grid:ptr_new(),info:ptr_new(),bridges:obj_new(),$
pause:0b,active:0b,new_view:0b,log:0l,t_start:0d,wPlotLOSOptions:0L,wLOS:0L,wPlotLOS:0L,wModelInfo:0l,profiler:obj_new(),$
Grid2Update:0L,wGrid2Update:0L,wMinVolume:0l,wMaxVolume:0l,wPowerIndexVolume:0l,wResetVolumeScale:0l,$
wSelectEbtel:0l,wEbtelTable:0ll,wParmBase:0l,wArrayParmBase:0l,wNparms:0l,wRparms:0l,$
wUploadFreqList:0l,wFreqList:0l,wUseFreqList:0l,wDelFreqList:0l,wUndoFreqList:0l,$
wResetFreqList:0l,wSaveFreqList:0l,wEditedFreqList:0l,Rsun:(pb0r())[2]*60,wObserver:0L,wL0:0l,wB0:0l,wR:0L}
end