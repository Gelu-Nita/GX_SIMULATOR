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
 self.parms=ptr_new(info.parms)
 self.info=ptr_new(info)
 wControlTab=widget_info(wScanner,/parent)
 wPlotBase=Widget_base(wControlTab,/column,Title='IMAGE PROFILES',/frame)
 self.ImgViewWid=obj_new('gxImgViewWid', wImgBase,info=self.info,renderer=self.renderer,wPlotBase=wPlotBase,/save)
 parms=*self.parms
 row_labels=strarr(n_elements(parms))
 for i=0,n_elements(parms)-1 do row_labels[i]=string(i+1,format='(i3)')  
 wScannerBase=widget_base(wScanner,/column,event_func='gxScanboxHandleEvent',uvalue=self)
 wExecBase=widget_base(wScannerBase,/column)
 base=widget_base(wScannerBase,/column)
 wParmToolbarBase = widget_base(base, /row, /frame,/TOOLBAR)
 self.wSelectRenderer= widget_button( wParmToolbarBase, $
             value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
             /bitmap,tooltip='Select Rendering Method IDL Wrapper')
 edit=1
     
 self.wParmsTable=widget_table(font=!defaults.font,base,xsize=4,ysize=n_elements(*self.parms),$
 y_scroll_size=0,x_scroll_size=0,value=*self.parms,COLUMN_WIDTHS =xscale*[100,100,100,400],$
 edit=edit,format=format,$
 column_labels=['Parameter','Value','Unit','Comments'],/RESIZEABLE_COLUMNS,uname='ParmsTable',$
 row_labels=row_labels,SCR_ySIZE=550*xscale)
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
wEBTELToolbarBase = widget_base(base, /row, /frame,/TOOLBAR,map=0)
self.wSelectEBTEL= widget_button( wEBTELToolbarBase, $
  value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
  /bitmap,tooltip='Select Impulsive heating EBTEL Table')
self.wEBTELTable=widget_text(font=!defaults.font,wEBTELToolbarBase,value=gx_ebtel_path(),SCR_XSIZE=geometry1.SCR_XSIZE-3*geometry2.SCR_XSIZE,/wrap)
  
wEBTELSSToolbarBase = widget_base(base, /row, /frame,/TOOLBAR,map=0)  
self.wSelectEBTELSS= widget_button( wEbtelSSToolbarBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Select Steady-State heating EBTEL Table')  
self.wEBTELSSTable=widget_text(font=!defaults.font,wEBTELSSToolbarBase,value=gx_ebtel_path(/ss),SCR_XSIZE=geometry1.SCR_XSIZE-3*geometry2.SCR_XSIZE,/wrap)
  
  
  main_base=get_tlb(wExecBase)
  self.wScan=widget_info(main_base,find_by_uname='SCAN_START')
  ;self.wPause=widget_info(main_base,find_by_uname='SCAN_PAUSE')
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

function gxScanBox::DefaultRenderer
 which,'gx_simulator',outfile=outfile
 cdir=file_dirname(file_dirname(outfile))
 renderer=cdir+path_sep()+'userslib'+path_sep()+'xray'+path_sep()+'xray_tt.pro'
 if strupcase(renderer) eq strupcase(find_file(renderer)) then return,renderer
 return,self->SelectRenderer()
end 

function gxScanBox::SelectRenderer
 return,dialog_pickfile(filter='*.pro',TITLE='Please select a renderer IDL routine/wrapper',path=gx_findfile(folder='userslib'))
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
 if ptr_valid(self.parms) then begin
   idx=where(strupcase((*self.parms).name) eq strupcase(name),count)
   if count eq 1 then begin
     (*self.parms)[idx].value=value
     widget_control,self.wParmsTable,get_value=parms
     parms[idx].value=value
     widget_control,self.wParmsTable,set_value=parms
   end
 end
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
                if ptr_valid(self.parms) then begin
                  info.parms[0].value=(*self.parms)[0].value
                  info.parms[1].value=(*self.parms)[1].value
                end  
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
                 data=renderer.data
                 sz=size(data)
                 nx=sz[1]
                 ny=sz[2]
                 fovmap=renderer.fovmap
                 rsun=fovmap->Get(/rsun)
                 xrange=fovmap->Get(/xrange)/rsun
                 yrange=fovmap->Get(/yrange)/rsun
                 info=renderer.info
               end       
    else:return        
endcase

if size(info,/tname) eq 'STRUCT' then begin
 widget_control,self.wRenderer,set_value=self.renderer
 ptr_free,self.info
 ptr_free,self.parms
 self.parms=ptr_new(info.parms)
 self.info=ptr_new(info)
 row_labels=strarr(n_elements(info.parms))
 for i=0,n_elements(info.parms)-1 do row_labels[i]=string(i+1,format='(i3)') 
 widget_control,self.wParmsTable,set_value=*self.parms,row_labels=row_labels,table_ysize=n_elements(info.parms)
 self.pData=(self.ImgViewWid)->NewView(self.info,renderer=self.renderer,nx=nx,ny=ny,xrange=xrange,yrange=yrange,data=data,fovmap=fovmap)
 self->MakeGrid
 self->Slice
 self->ResetAllBridges
 widget_control,self.wSliceSelect,set_combobox_select=(where((*self.parms).name eq 'n_0'))[0],set_value=[(*self.parms).name,'B','curlB','divB+','divB-','helB+','helB-','Q0','Q','VoxelID']
 endif else begin
  self.renderer=current
  answ=dialog_message(/error,'Invalid rendering method selected. Operation aborted!')
 end 
end

function gxScanBox::name2idx,name
 return,where(strcompress(strupcase((*self.parms).name),/rem) eq strcompress(strupcase(name),/rem))
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
  widget_control,widget_info(self.wEbtelTable,/parent),map=hasBL
  widget_control,widget_info(self.wEbtelSSTable,/parent),map=hasBL
 endif else begin model_info='NO GX MODEL SELECTED!'
  widget_control,widget_info(widget_info(self.wScan,/parent),/parent),map=0
 end
 widget_control,self.wModelInfo,set_value=model_info
end

function gxScanbox::GetRefModel
 self.ImgViewWid->GetProperty,model=model
 if obj_isa(model,'gxmodel') then return,model else return,obj_new()
end 

function gxScanbox::AutoFOV
 return,widget_info(self.wAuto,/button)
end

pro gxScanbox::ComputeFOV,compute_grid=compute_grid,upload=upload
 if self.active then begin
  answ=dialog_message('There is an active scan in progress.'+string(10b)+$
                      'Any unsaved results will be lost!'+string(10b)+$
                      'Do you want to continue anyway?',/question)
  if strupcase(answ) eq 'NO' then return                    
 end


 auto=widget_info(self.wAuto,/button)

  
 self->GetProperty,parent=oSun
 if ~obj_isa(osun,'gxsun') then return
 all=oSun->Get(/all,count=count,isa='gxmodel')
 for i=0, count-1 do begin
   all[i]->GetProperty,IsROI=IsROI,XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV
   if IsROI then begin
     MOI=all[i]
     MOI->ResetPosition
     hasrange=get_obj_range(MOI,oSun,odestination,range,ignore=['IDLgrLight','IDLgrImage','IDLgrVolume','gxROI']) 
     xrange=reform(range[0,*])
     yrange=reform(range[1,*])
     zrange=reform(range[2,*])
     dx=XCOORD_CONV[1]
     dy=YCOORD_CONV[1]
     dz=ZCOORD_CONV[1]
   endif
 endfor 
 if obj_valid(moi) eq 0 then MOI=obj_new()
 
 if keyword_set(auto) then begin
   if obj_valid(moi) then begin
    if self->SquareFOV() then begin
      fov=max([delta(xrange),delta(yrange)])
      xrange=mean(xrange)+[-1,1]*fov/2
      yrange=mean(yrange)+[-1,1]*fov/2
      data=moi->GetROIBox()
      data=gx_transform(data,moi->GetSTM(),/inv)
    end
   endif else begin
    ;answ=dialog_message('No model of interest selected!',/info)
    xrange=[-1.5,1.5]
    yrange=[-1.5,1.5]
    zrange=[-1.5,1.5]
    self.nz=mean([self.nx,self.ny])
   end 
 endif else begin
  widget_control,self.wXrange,get_value=xfov
  widget_control,self.wYrange,get_value=yfov
  widget_control,self.wX,get_value=xc
  widget_control,self.wY,get_value=yc
  xrange=(xc+[-1,1]*xfov/2)/self.r()
  yrange=(yc+[-1,1]*yfov/2)/self.r()
  if n_elements(zrange) ne 2 then begin
    zrange=[-1.5,1.5]
    self.nz=mean([self.nx,self.ny])
  endif
 endelse
 self->Reset
 self->SetRefModel,MOI
 self->NewGrid,xrange=xrange, yrange=yrange,zrange=zrange,compute_grid=compute_grid,upload=upload
 self->TV_SLICE
end

pro gxScanBox::NewGrid,xrange=xrange,yrange=yrange,zrange=zrange,nx=nx,ny=ny,compute_grid=compute_grid,upload=upload
    self.Grid2Update=1
    newbox=1
    nz=0
    self.ImgViewWid->GetProperty,model=MOI  
    default,xrange,self.xrange
    default,yrange,self.yrange
    default,zrange,self.zrange
    default,nx,self.nx
    default,ny,self.ny
    if keyword_set(upload) and isa(moi,'gxmodel') then begin
        sroi=moi->getroi(/scanbox)
        sdata=sroi->GetScanboxData(/sun)
        dim=sroi->GetDim()
        nx=dim[0]
        ny=dim[1]
        nz=dim[2]
        xrange=minmax(sdata[0,*])
        yrange=minmax(sdata[1,*])
        zrange=minmax(sdata[2,*])
        self.Grid2Update=((moi->GetVolume())->getflags()).newGrid
    endif else begin
      p=dblarr(3,8)
      for i=0,7 do p[*,i] = [xrange[(i AND 1)], yrange[((i/2) AND 1)], zrange[((i/4) AND 1)]]
      index=[0,1,3,1,5,7,5,4,6,4,0,2,3,7,6,2]
      sdata=p[*,index]
        if isa(moi,'gxmodel') then begin
         flags=(moi->GetVolume())->setflags(/newGrid)
         if keyword_set(compute_grid) then begin
           self.Grid2Update=moi->ReplaceScanboxData(gx_transform(sdata,moi->GetSTM()),nx=nx,ny=ny,/compute_grid)
         endif else self.Grid2Update=flags.NewGrid
         sroi=moi->getroi(/scanbox)
         dim=sroi->GetDim()
         nz=dim[2]
      endif
  endelse
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
  widget_control,self.wParmsTable,get_value=table
  idx=self->name2idx('dS')
  dS=self.dx*self.dy*(gx_rsun()^2)
  table[idx].value=dS
  (*self.parms)[idx].value=dS
  idx=self->name2idx('dR')
  dR=self.dz*(gx_rsun())
  table[idx].value=dR
  (*self.parms)[idx].value=dR
  widget_control,self.wParmsTable,set_value=table
  self.pData=(self.ImgViewWid)->NewView(*self.info,nx=self.nx,ny=self.ny,xrange=self.xrange,yrange=self.yrange)
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
   parms:dblarr(self.nx,self.nz,n_elements(*self.parms)),slice:empty_slice})    
               
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
  assigned=lonarr(n_elements(*self.parms))
  
  self->DrawSlicer,column,row
  
  self.ImgViewWid->GetProperty,model=model
  if ~obj_valid(model) then begin
    goto, unassigned
  endif
 void=model->Box2Volume(box2vol=box2vol) 
 grid=model->GetGrid()
 if ptr_valid(grid) then (*self.grid).grid=grid
  if ~ptr_valid(grid) then goto, unassigned

  sz=model->Size(/volume)
  dr=reform((*grid)[0,*,row,*])
  g=reform((*grid)[1:3,*,row,*])
  vol_ind=transpose(reform(g,3,self.nx*self.nz))
  missing=0
  (model->GetVolume())->GetVertexAttributeData,'voxel_id',id
  if n_elements(id) gt 0 then begin
    var=interpolate(id,fix(vol_ind[*,0]),fix(vol_ind[*,1]),fix(vol_ind[*,2]),missing=missing)
    idx=self->name2idx('VoxelID')
    if (size(idx))[0] ne 0 then begin
      (*self.grid).parms[*,*,idx]=ulong(var)
      assigned[idx]=1
    end
  endif
  
  r=model->R(/volume)
  radius=interpolate(temporary(r),vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
  
  ;ASSIGN dz
  idx=self->name2idx('dR')
  if (size(idx))[0] ne 0 then begin
    (*self.grid).parms[*,*,idx]=dr*gx_rsun()
    assigned[idx]=1
  end
  
  n_oculted=0
  ;oculted=where((slice_grid[*,0]^2+slice_grid[*,1]^2 lt 1 and slice_grid[*,2] lt 0) or (slice_grid[*,0]^2+slice_grid[*,1]^2 +slice_grid[*,2]^2 lt 1),n_oculted)
  ;ondisk=where((slice_grid[*,0]^2+slice_grid[*,1]^2 lt 1 and slice_grid[*,2] ge 0) and (slice_grid[*,0]^2+slice_grid[*,1]^2 +slice_grid[*,2]^2 lt 1),n_ondisk)
  

   corona=model->Corona()
   corona->GetProperty,n0=n0,T0=temp,dist_e=dist_e,kappa=kappa,emin=emin,emax=emax,chromo_n=chromo_n,chromo_T=chromo_T,chromo_h=chromo_h,ignore=ignore_corona
   
   tmp=(*self.grid).slice
   
   (*self.grid).slice=corona->GetDensity(radius)
   tmp[*]=radius

    if ~ignore_corona then begin
       ;corona Dist_E
        idx=self->name2idx('Dist_E')
        if (size(idx))[0] ne 0 then begin
          (*self.grid).parms[*,*,idx]=dist_e
          assigned[idx]=1
        end
        
        
       ;corona kappa
        idx=self->name2idx('kappa')
        if (size(idx))[0] ne 0 then begin
          (*self.grid).parms[*,*,idx]=kappa 
          assigned[idx]=1
        end
       
       ;corona Emin
        idx=self->name2idx('Emin')
        if (size(idx))[0] ne 0 then begin
          (*self.grid).parms[*,*,idx]=emin
          assigned[idx]=1
        end  
       
       ;corona Emax
        idx=self->name2idx('Emax')
        if (size(idx))[0] ne 0 then begin
          (*self.grid).parms[*,*,idx]=emax
          assigned[idx]=1 
        end  
    end    

    
    sz=model->Size()
    vol=fltarr(sz[1],sz[2],sz[3])   
    volume=model->GetVolume()   
    ;n_0
    idx=self->name2idx('n_0')
    if (size(idx))[0] ne 0 then begin
      vol=model->Box2Volume('n0')
      if isa(vol)then begin
        (*self.grid).parms[*,*,idx]=interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
        assigned[idx]=1
      end
    end

    ;T_0
    idx=self->name2idx('T_0')
    if (size(idx))[0] ne 0 then begin
      vol=model->Box2Volume('T0')
      (*self.grid).parms[*,*,idx]=interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
      assigned[idx]=1
    end
      
     idx=self->name2idx('bmed')
     if (size(idx))[0] ne 0 then begin
       vol=model->Box2Volume('bmed',/corona)
       if isa(vol)then begin
         (*self.grid).parms[*,*,idx]=interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
          assigned[idx]=1 
        end
     end 
     

     idx=self->name2idx('length')
     if (size(idx))[0] ne 0 then begin
       vol=model->Box2Volume('length',/corona)
       if isa(vol)then begin
         ;vol[*]=1e11
         vol=gx_rsun()*vol/2
         (*self.grid).parms[*,*,idx]=interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
          assigned[idx]=1 
       end 
     end 

    idx=self->name2idx('Q')
    if (size(idx))[0] ne 0 then begin
      vol=model->Box2Volume('Q',/corona)
      if isa(vol)then begin
        (*self.grid).parms[*,*,idx]=interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
        assigned[idx]=1
      end  
    end
    
    idx=self->name2idx('UseDEM')
    if (size(idx))[0] ne 0 then begin
      (*self.grid).parms[*,*,idx]=(volume->getflags()).NTDEM
      assigned[idx]=1
    end
    
    idx=self->name2idx('SS')
    if (size(idx))[0] ne 0 then begin
      (*self.grid).parms[*,*,idx]=(volume->getflags()).NTSSDEM
      assigned[idx]=1
    end
    
    idx=self->name2idx('AddTR')
    if (size(idx))[0] ne 0 then begin
        (*self.grid).parms[*,*,idx]=(volume->getflags()).TRADD
        assigned[idx]=1
    end
    
    idx=self->name2idx('ApplyTRfactor')
    if (size(idx))[0] ne 0 then begin
      (*self.grid).parms[*,*,idx]=(volume->getflags()).TRFACTOR
      assigned[idx]=1
    end
    
    idx=self->name2idx('DEMAVG')
    if (size(idx))[0] ne 0 then begin
       model->GetProperty,wparent=wparent
       id=widget_info(wparent,find_by_uname='GXMODEL:DEMAVG')
       if widget_valid(id) then begin
        widget_control,id,get_value=demavg
        (*self.grid).parms[*,*,idx]=demavg
        assigned[idx]=1
       endif
    end
       
    idx=self->name2idx('hc_angle')
    if (size(idx))[0] ne 0 then begin
      model->GetProperty,ns=ns,ew=ew
      (*self.grid).parms[*,*,idx]=asin(sqrt(total((hel2arcmin(ns,ew,radius=rsun,date=(*(model->Refmaps()))->Get(/time)))^2))/rsun)/!dtor
      assigned[idx]=1
    end
   

    chromo_idx=model->GetVertexData('chromo_idx')
    ; start for backward compatibility Dec 18 2014!!!!
    if n_elements(chromo_idx) eq 0 then chromo_idx=model->GetVertexData('idx')
    ; end for backward compatibility Dec 18 2014!!!!
    if isa(chromo_idx,/number,/array) then begin
     n_htot=model->GetVertexData('n_htot')
     if n_elements(n_htot) eq n_elements(chromo_idx) then begin
      var=temporary(n_htot)
      vol[*]=0
      vol[chromo_idx]=var
      var =interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
      idx=self->name2idx('n_Htot')
      if (size(idx))[0] ne 0 then begin
        (*self.grid).parms[*,*,idx]=var
        assigned[idx]=1
      end
     endif

     n_hi=model->GetVertexData('n_hi')
     if n_elements(n_hi) eq n_elements(chromo_idx)  then begin
       var=temporary(n_hi)
       vol[*]=0
       vol[chromo_idx]=var
       var =interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
       idx=self->name2idx('n_HI')
       if (size(idx))[0] ne 0 then begin
         (*self.grid).parms[*,*,idx]=var
         assigned[idx]=1
       end
     endif

     n_p=model->GetVertexData('n_p')
     if n_elements(n_p) eq n_elements(chromo_idx) then begin
       var=temporary(n_p)
       vol[*]=0
       vol[chromo_idx]=var
       var =interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
       idx=self->name2idx('n_p')
       if (size(idx))[0] ne 0 then begin
         (*self.grid).parms[*,*,idx]=var
         assigned[idx]=1
       end
     endif
   end 
   
    
    ;LOOP OVER FLUXTUBES
    tubes=model->Get(/all,ISA='gxFluxtube',count=tcount)
    bsize=model->Size()
    vol=(tmp=dblarr(bsize[1],bsize[2],bsize[3]))  
      for j=0,tcount-1 do begin
        vol[*]=0
        tubes[j]->GetProperty,T0=T0,eps=eps,kappa=kappa,emin=emin,emax=emax,$
                  Ebreak=e_break,delta1=delta1,delta2=delta2,dist_e=dist_e,dist_ang=dist_ang,centerbase=base
        base->GetVertexAttributeData,'owned',owned  
        base->GetVertexAttributeData,'N_IDX',n_idx      
        ocount=n_elements(owned)      
        if ocount gt 1 then begin
          vol[*]=0
          vol[owned]=1
          (*self.grid).slice=interpolate(vol[box2vol],vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
           slice_owned=where((*self.grid).slice eq 1,comp=cowned,nowned)  
           if nowned gt 0 then begin
            owned2d=array_indices((*self.grid).slice,slice_owned)
            x=reform(owned2d[0,*])
            y=reform(owned2d[1,*])
           end 
           
          parms=['kappa','eps','Emin','Emax','E_break','delta1','delta2','Dist_E','Dist_Ang']
          for k=0,n_elements(parms)-1 do begin
            idx=self->name2idx(parms[k])
            if idx ge 0 then assigned[idx]=1
            if (size(idx))[0] ne 0 then begin
              if nowned gt 0 then begin
               result=execute('data='+parms[k])
               if result eq 1 then begin
                (*self.grid).parms[x,y,replicate(idx,n_elements(x))]=data
               end 
              end
            end 
          end 
      end
    end
    ;______________________________________________________
   
   
   vertex_parms=['n_nth','THETA_C','THETA_B','dMu','a4']
   idx_parms=['n_b','THETA_C','THETA_B','dMu','a_4']
   bsize=model->Size()
   vol=(tmp=dblarr(bsize[1],bsize[2],bsize[3]))
   for k=0,n_elements(idx_parms)-1 do begin
     idx=self->name2idx(idx_parms[k])
     if idx ge 0 then assigned[idx]=1
     if (size(idx))[0] ne 0 then begin    
      ;LOOP OVER FLUXTUBES
      tubes=model->Get(/all,ISA='gxFluxtube',count=tcount)
      vol[*]=0
      for j=0,tcount-1 do begin
        tmp[*]=0
        tubes[j]->GetProperty,centerbase=base
        base->GetVertexAttributeData,vertex_parms[k],data  
        if n_elements(data) gt 0 then begin
          base->GetVertexAttributeData,'owned',owned
          base->GetVertexAttributeData,'N_IDX',n_idx
          if n_elements(owned) gt 1 then begin
            tmp[n_idx]=data
            vol[owned]=tmp[owned]
          end
        end
      end
      (*self.grid).parms[*,*,idx]+=interpolate(vol[box2vol],vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
     endif
   end  
   
 
  
  vol=volume->GetBx(/volume)  
  (*self.grid).B[*,0]=Interpolate(vol, vol_ind[*, 0], vol_ind[*, 1], vol_ind[*, 2], missing=missing)
  vol=volume->GetBy(/volume) 
  (*self.grid).B[*,1]=Interpolate(vol, vol_ind[*, 0], vol_ind[*, 1], vol_ind[*, 2], missing=missing)
  vol=volume->GetBz(/volume) 
  (*self.grid).B[*,2]=Interpolate(vol, vol_ind[*, 0], vol_ind[*, 1], vol_ind[*, 2], missing=missing)
  
  
  btm=model->GetBTM()
  (*self.grid).B=btm##(*self.grid).B

  ;SCALE B
  volume->GetProperty,bscale=bscale
  if n_elements(bscale) ne 0 then begin
    (*self.grid).B=Bscale*(*self.grid).B
  end

   ;COMPUTE B
   B=sqrt(total((*self.grid).B^2,2))
   
   idx=self->name2idx('B')
   if (size(idx))[0] ne 0 then begin
     (*self.grid).parms[*,*,idx]=B
     assigned[idx]=1
   end
   
   idx=self->name2idx('Bx')
   if (size(idx))[0] ne 0 then begin
     (*self.grid).parms[*,*,idx]=(*self.grid).B[*,0]
     assigned[idx]=1
   end
   
   idx=self->name2idx('By')
   if (size(idx))[0] ne 0 then begin
     (*self.grid).parms[*,*,idx]=(*self.grid).B[*,1]
     assigned[idx]=1
   end
   
   idx=self->name2idx('Bz')
   if (size(idx))[0] ne 0 then begin
     (*self.grid).parms[*,*,idx]=(*self.grid).B[*,2]
     assigned[idx]=1
   end
   
   ;COMPUTE PHI
   idx=self->name2idx('phi')
   if (size(idx))[0] ne 0 then begin
     if (size(idx))[0] ne 0 then begin
      (*self.grid).parms[*,*,idx]=atan((*self.grid).B[*,1],(*self.grid).B[*,0])/!dtor;atan(By,Bx)/!dtor
      assigned[idx]=1
     end
   end  
 
   ;COMPUTE THETA
   idx=self->name2idx('theta')
   if (size(idx))[0] ne 0 then begin
     temp=acos((*self.grid).B[*,2]/B);acos(Bz/B)
     good=where(finite(temp) eq 1,comp=comp,ncomp=ncomp)
     if ncomp gt 0 then  temp[comp]=0
     (*self.grid).parms[*,*,idx]=temp/!dtor
     assigned[idx]=1    
   end
   
   ;COMPUTE ETA
   idx=self->name2idx('TRfactor')
   if (size(idx))[0] ne 0 then begin
     (*self.grid).B=(invert(btm)##(*self.grid).B)
     ;theta is the angle between B and Bz (between the field and normal to TR)
     costheta=abs((*self.grid).B[*,2])/B
     ;ez is the box z-axis versor (normal to TR) in the observer coordinate system, where LOS is the z axis
     ;So, phi is the angle betwen the TR normal and LOS, so cosphi is the z component of the ez versor
     ez=btm[*,2]
     dz=sqrt((*self.parms)[self->name2idx('dS')].value);pixel area
     r=gx_rsun(unit='cm')
     mincosphi=sin(0.5*acos(1-dz/R))
     cosphi=(abs(ez[2])>mincosphi)
     tr_factor=(costheta/cosphi)
     good=where(finite(tr_factor) eq 1,comp=comp,ncomp=ncomp)
     if ncomp gt 0 then  tr_factor[comp]=0
;     no_tr=where((ulong((*self.grid).parms[*,*,self->name2idx('VoxelID')]) and 2ul) eq 0,no_tr_count)
;     if no_tr_count gt 0 then tr_factor[no_tr]=0
      (*self.grid).parms[*,*,idx]=tr_factor
     assigned[idx]=1
   end  


if n_oculted gt 0 then self->CleanGrid,oculted=oculted
   
unassigned:   
   ;ASSIGN UNASSIGNED
   for j=0,n_elements(*self.parms)-1 do begin
    if ptr_valid(self.grid) then begin
     if ~assigned[j] then begin
       (*self.grid).parms[*,*,j]=(*self.parms)[j].value
     endif 
    end
   end  
   
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
  self.ImgViewWid->OnStartScan
  self.ImgViewWid->GetProperty,fovmap=fovmap
  ;order matters
  MULTI_SAVE,/new,log,{row:-1L,parms:(*self.grid).parms,grid:transpose(reform((*(*self.grid).grid)[*,*,0,*]),[1,2,0])},file=file, $
     header={renderer:self.renderer ,info:(*self.info),fovmap:fovmap,nx:self.nx,ny:self.ny,xrange:self.xrange,yrange:self.yrange}

  
  for row=0l,self.ny-1 do begin
   self->slice,row
   self->TV_SLICE
   MULTI_SAVE,log,{row:row,parms:(*self.grid).parms,grid:transpose(reform((*(*self.grid).grid)[*,*,row,*]),[1,2,0])}
  end
  close,log
 end
end

pro gxScanbox::UpdateParmsTable, info
 widget_control,self.wParmsTable,get_value=value
 (*self.parms)=value
 if n_elements(info) eq 0 then begin
   (*self.info).parms=value
   (*self.info)=self->RendererInfo(*self.info)
 endif else begin
   ptr_free,self.info
   self.info=ptr_new(self->RendererInfo(info))
 endelse
 self.pData=(self.ImgViewWid)->NewView(*self.info,nx=self.nx,ny=self.ny,xrange=self.xrange,yrange=self.yrange)
 self->MakeGrid
 self->Slice
 self->ResetAllBridges
end



function gxScanbox::HandleEvent,event
subdirectory=['resource', 'bitmaps']
case event.id of   
  self.wParmsTable: self->UpdateParmsTable 
  self.wSquareFOV: begin
                    square=widget_info(self.wSquareFOV,/button_set)
                    autoFov=widget_info(self.wAuto,/button_set)
                    if event.select  then begin
                      self.Ny=self.Nx
                      self.Yrange=self.Xrange
                      widget_control,self.wNx,set_value=self.Nx
                      widget_control,self.wNy,set_value=self.Ny,sensitive=0
                      widget_control,self.wXrange,set_value=delta(self.Xrange)*self.R()
                      widget_control,self.wYrange,set_value=delta(self.YRange)*self.R()
                    endif else widget_control,self.wNy,sensitive=1
                    widget_control,self.wX,sensitive=~autoFov
                    widget_control,self.wY,sensitive=~autoFov
                    widget_control,self.wXrange,sensitive=~autoFov
                    widget_control,self.wYrange,sensitive=~square and ~autoFov
                    auto=autoFov
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
 self.wAuto: Begin
               sensitive=~event.select
               square=widget_info(self.wSquareFOV,/button_set)
               widget_control,self.wXrange,sensitive=sensitive
               widget_control,self.wYrange,sensitive=(sensitive and ~square)
               if square then begin
                self.Yrange=self.Xrange
                widget_control,self.wYrange,set_value=delta(self.Xrange)*self.R()
                widget_control,self.wXrange,set_value=delta(self.YRange)*self.R()
               endif
               widget_control,self.wX,sensitive=sensitive
               widget_control,self.wY,sensitive=sensitive
               auto=event.select
             END                 
  
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
               (container->GetByName('Solar Grid'))->SetProperty,hide=select
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
  self.wSelectEBTELss: begin
                          self->ReplaceEBTELtables,/ss
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
                        volume=(model->GetVolume())
                        volume->Update,select,range=data_range,pwr_idx=pwr_idx,/update;explicitely request volume update
                        widget_control,self.wMinVolume,Set_Value=data_range[0]
                        widget_control,self.wMaxVolume,Set_Value=data_range[1]
                        widget_control,self.wPowerIndexVolume,Set_Value=pwr_idx
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
   self.wResetVolumeScale: goto,update_volume                                                                                                                                   
   self.wTV_Slice: self->TV_SLICE    
   self.wSaveLOS:self->SaveLOS                                                                                          
 else:
 endcase
return, self->Rewrite(event,auto=auto)
END

pro gxScanBox::ReplaceEBTELtables,ss=ss
 path=dialog_pickfile(path=gx_findfile(filename,folder='userslib'+path_sep()+'aia'+path_sep()+'ebtel'),default='.sav')
 if gx_ebtel_valid_path(path) then begin
  widget_control,keyword_set(ss)?self.wEbtelSSTable:self.wEbtelTable,set_value=gx_ebtel_path(path,ss=ss)
  self.ImgViewWid->GetProperty,model=MOI 
  if isa(MOI) then begin
    volume=moi->GetVolume()
    flags=(volume->setflags(newNT=volume->NewNT()))
    volume->Update
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
; bridge->Execute,'.reset'
; if !version.os_family eq 'Windows' then bridge->Execute, '@' + pref_get('IDL_STARTUP') else bridge->Execute, '@gx_startup.pro'
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
;                 widget_control,self.wpause,/set_button
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
     data:make_array([self.nx,1,(*self.info).pixdim],/float)},file=GETENV('IDL_TMPDIR')+GETENV('USER')+'GX_Simulator.log', $
     header={renderer:self.renderer ,info:(*self.info),fovmap:fovmap,nx:self.nx,ny:self.ny,xrange:self.xrange,yrange:self.yrange}
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
   MULTI_SAVE,self.log,{row:long(row),parms:parms,data:(*self.pData)[*,row,*,*,*]},file=GETENV('IDL_TMPDIR')+GETENV('USER')+'GX_Simulator.log', header=(*self.info)
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
   status_message=strcompress(string(self.completed,self.ny,systime(/s)-self.t_start,format="('Progress: ',i4,' rows out of ',i4,' in process or processed in ',f7.3,' seconds')"))
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
  xaxis=findgen(self.nz);(reform((*self.grid).grid[*,2],self.nx,self.nz))[x,*]
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

pro gxScanBox::CreatePanel
 subdirectory=['resource', 'bitmaps']                                                           
 ;xlabelsize=36
 xtextsize=10
 device, get_screen_size=scr
 if not exist(xsize) then xsize = fix (scr[0] * .3)
 if not exist(ysize) then ysize = xsize 
 frame=1
 format='(f7.2)'
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
 self.wSliceSelect= widget_combobox(wSelect, value=[(*self.parms).name,'B','curlB','divB+','divB-','helB+','helB-','Q0','Q','VoxelID'])
 widget_control,self.wSliceSelect,set_combobox_select=(where((*self.parms).name eq 'n_0'))[0]

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
 Widget_Control,self.wx,sensitive=0       
 self.wy=CW_objFIELD(wRow3, UNAME='Y', LABEL=' Yc',$
        XTEXTSIZE=XTEXTSIZE, XLABELSIZE=XLABELSIZE,$
        INCREMENT=10, $
        UNITS='"', $
        VALUE=mean(self.Yrange),Sensitive=1,frame=frame, format=format)    
 Widget_Control,self.wy,sensitive=0                      
 self.wXrange=CW_objFIELD(wRow3, UNAME='Xrange', LABEL=' Xrange',$
        XTEXTSIZE=XTEXTSIZE, XLABELSIZE=XLABELSIZE,$
        INCREMENT=10, $
        UNITS='"', $
        VALUE=delta(self.Xrange),Sensitive=1,frame=frame, format=format)   
 Widget_Control,self.wXrange,sensitive=0        
 self.wYrange=CW_objFIELD(wRow3, UNAME='Yrange', LABEL=' Yrange',$
        XTEXTSIZE=XTEXTSIZE, XLABELSIZE=XLABELSIZE,$
        INCREMENT=10, $
        UNITS='"', $
        VALUE=delta(self.Yrange),Sensitive=1,frame=frame, format=format)             
 Widget_Control,self.wYrange,sensitive=0   
wAutobase=widget_base(wRow3,/row,/nonexclusive)
self.wAuto=widget_button(font=font,wAutobase ,value='Auto FOV',uname='Auto FOV')
      Widget_Control,self.wAuto,Set_Button=1

 self->UpdateFields

 geometry=widget_info(widget_info(self.wInfo,/parent),/geometry)
 
 
 self.wModelInfo=widget_text(wRow1,value='NO GX MODEL SELECTED YET!',/align_center,scr_xsize=geometry.xsize)      
                                                   
end

function gxScanbox::R
  self.ImgViewWid->GetProperty,model=moi
  if obj_isa(moi,'gxmodel') then pbr=pb0r(moi->GetTime()) else pbr=pb0r()
  return,pbr[2]*60
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
  ptr_free,self.parms
  ptr_free,self.pData
  ptr_free,self.info
  obj_destroy,self.bridges
  self->gxWidget::Cleanup
end
 
pro gxScanbox__define
self={gxScanbox,inherits IDLgrModel, inherits gxWidget,row:0L,completed:0l,$
Xrange:[0d,0d],Yrange:[0d,0d],Zrange:[0d,0d],Nx:0l,Ny:0l,Nz:0l,dx:0d,dy:0d,dz:0d,$
wX:0l,wY:0l,wL:0l,wXrange:0L,wyrange:0L,wNx:0l,wNy:0l,wNz:0l,wTV_Slice:0l,wInfo:0l,wToolbar:0l,wHideVolume:0l,TaskManager:obj_new(),ImgViewWid:obj_new(),$
wHideScanbox:0l,wHideSun:0l,wSlice:0l,wSliceSelect:0l,wSaveLOS:0l,wSquareFOV:0L,wdx:0l,wdy:0l,wAuto:0L,$
ROI:obj_new(),slicer:obj_new(),wParmsTable:0l,wScan:0L,wPause:0L,wAbort:0L,wDebug:0L,wTaskTable:0L,wBridges:0l,wStatusBar:0l,$
renderer:'',wRenderer:0l,wSelectRenderer:0l,pData:ptr_new(),grid:ptr_new(),info:ptr_new(),parms:ptr_new(),bridges:obj_new(),$
pause:0b,active:0b,new_view:0b,log:0l,t_start:0d,wPlotLOSOptions:0L,wLOS:0L,wPlotLOS:0L,wModelInfo:0l,profiler:obj_new(),$
Grid2Update:0L,wGrid2Update:0L,wMinVolume:0l,wMaxVolume:0l,wPowerIndexVolume:0l,wResetVolumeScale:0l,$
wSelectEbtel:0l,wEbtelTable:0l,wSelectEbtelSS:0l,wEbtelSSTable:0l}
end