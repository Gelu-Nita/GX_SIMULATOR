function gxROI::Init,model,nx=nx,ny=ny,_extra=_extra
if ~obj_valid(model) then return,-1
if ~obj_isa(model,'gxmodel') then return,-1
model->ResetPosition
model->GetProperty,xcoord_conv=xcoord_conv,ycoord_conv=ycoord_conv,zcoord_conv=zcoord_conv,refmaps=refmaps
sz=model->Size()
time=(*refmaps)[0]->get(/time)
rsun=(*refmaps)[0]->get(/rsun)
if size(rsun,/tname) eq 'STRING' or size(rsun,/tname) eq 'UNDEFINED' then rsun=(pb0r(time))[2]*60
void=self->IDLgrModel::Init(name='ScanBox')
box=model->GetRoiBox()
stm=model->GetSTM()
data=gx_transform(box,stm,/inv)
xrange=minmax(data[0,*])
yrange=minmax(data[1,*])
zrange=minmax(data[2,*])
p=dblarr(3,8)
for i=0,7 do p[*,i] = [xrange[(i AND 1)], yrange[((i/2) AND 1)], zrange[((i/4) AND 1)]]
index=[0,1,3,1,5,7,5,4,6,4,0,2,3,7,6,2]
p=p[*,index]
sdata=gx_transform(p,stm)
self.scanbox=IdlGrRoi(sdata,color=[0,0,255],name='fov:scanbox',xcoord_conv=xcoord_conv,ycoord_conv=ycoord_conv,zcoord_conv=zcoord_conv)
if n_elements(nx) eq 1 then self.nx=nx else self.nx=sz[1]
if n_elements(ny) eq 1 then self.ny=ny else self.ny=sz[2]
xrange=xrange*rsun
yrange=yrange*rsun
xc=mean(xrange)
yc=mean(yrange)
dx=abs(xrange[1]-xrange[0])/(self.nx-1)
dy=abs(yrange[1]-yrange[0])/(self.ny-1)
fovmap=make_map(fltarr(self.nx,self.ny)+255,xc=xc,yc=yc,dx=dx,dy=dy,time=time,rsun=rsun)
;correction needed to avoid double precision math errors
fovmap.xc=round(fovmap.xc*1000d)/1000d
fovmap.yc=round(fovmap.yc*1000d)/1000d
;correction needed to avoid precision math errors
self.fovmap=obj_new('map',name='fov:fovmap')
self.fovmap->set,0,map=fovmap
self.fovimage=IDLgrImage(bytscl(self.fovmap->get(/data)),name='fov:image')
self.fovscreen=idlgrpolygon(sdata[*,[0,1,2,15]],texture_map=self.fovimage,Texture_Coord=[[0,0],[1,0],[1,1],[0,1]],Color=[255,255,255],$
                         xcoord_conv=xcoord_conv,ycoord_conv=ycoord_conv,zcoord_conv=zcoord_conv,name='fov:screen')
self->add,self.fovscreen
self->add,self.scanbox
return,1
end

function gxROI::Refmaps
 return,*(self.parent->refmaps())
end

pro gxROI::DisplayMap,select
 if ~is_number(select) then begin
  self.parent->GetProperty,wParent=wParent
  if widget_valid(wparent) then begin
    wselect=widget_info(wparent,find_by_uname='GXMODEL:BaseMapSelect')
    if widget_valid(wselect) then begin
       selected_map=widget_info(wselect,/combobox_gettext)   
       widget_control,wselect,get_value=list
       select=(where(list eq selected_map))[0]
    endif else select=2<((self->Refmaps())->get(/count)-1)
  endif else select=2<((self->Refmaps())->get(/count)-1)
 endif else select=select<((self->Refmaps())->get(/count)-1)
 ref_map=(self->Refmaps())->get(0,/map)
 map=(self->Refmaps())->get(select,/map)
 if tag_exist(map,'PROJECTION') then CEA=(map.projection eq 'CEA') else CEA=0
 if STRUPCASE(map.xunits) eq 'DEG' then CEA=1
 if strupcase(strmid(map.id,0,4))eq 'BASE' then CEA=1
 self.parent->GetProperty,wParent=wParent
 if widget_valid(wParent) then begin
   hide_fov=widget_info(wparent,Find_By_Uname='GXMODEL:HideFovMap')
   hide_cea=widget_info(wparent,Find_By_Uname='GXMODEL:HideMap')
 end
 if select le 2 or CEA then begin
  sz=size(ref_map.data)
  map=gx_rebin_map(map,sz[1],sz[2],/congrid)
  display=self->GetBaseScreen()
  display->SetProperty,ALPHA_CHANNEL=1,BLEND_FUNCTION = [3, 4],hide=0
  (self->GetFOVscreen())->SetProperty,hide=1
  if widget_valid(wParent) then begin
   if widget_valid(hide_fov) then  widget_control,hide_fov,set_button=1
   if widget_valid(hide_cea) then  widget_control,hide_cea,set_button=0
  end  
 endif else begin
  ;map=gx_remap(map,self.fovmap->get(/xrange),self.fovmap->get(/yrange),[self.nx,self.ny],time=ref_map.time)
  map=drot_map(map,ref=self.fovmap->get(/map),/KEEP_LIMB,/RIGID)
  display=self.fovimage
 (self->GetFOVscreen())->SetProperty,hide=0
 (self->GetBaseScreen())->SetProperty,ALPHA_CHANNEL=0,BLEND_FUNCTION = [3, 4]
 if widget_valid(wParent) then begin
  if widget_valid(hide_fov) then  widget_control,hide_fov,set_button=0
  if widget_valid(hide_cea) then widget_control,hide_cea,set_button=1
 end  
 endelse
 if obj_isa(display,'IDLgrImage') then display->SetProperty,data=bytscl(map.data)
end

function gxROI::ReplaceData,data,nx=nx,ny=ny,compute_grid=compute_grid
  old_nx=self.nx
  old_ny=self.ny
  self.scanbox->GetProperty,data=old_data
  if n_elements(nx) eq 1 then self.nx=nx else nx=self.nx
  if n_elements(ny) eq 1 then self.ny=ny else ny=self.ny
    if n_elements(data) ne 0 then begin
      self.scanbox->ReplaceData,data
      self->ReplaceFovMap
    end  
  if ~keyword_set(compute_grid) then begin
    return,(((self.parent)->GetVolume())->getflags()).newGrid
  endif
  return,self->ComputeGrid()
end  

pro gxROI::ReplaceFovMap
  self.scanbox->GetProperty,data=data
  sdata=gx_transform(data,self.parent->GetSTM(),/inv)
  time=self.parent->GetTime()
  rsun=self.parent->Rsun()
  if size(rsun,/tname) eq 'STRING' then rsun=(pb0r(time))[2]*60
  xrange=minmax(sdata[0,*])*rsun
  yrange=minmax(sdata[1,*])*rsun
  xc=mean(xrange)
  yc=mean(yrange)
  dx=abs(xrange[1]-xrange[0])/self.nx
  dy=abs(yrange[1]-yrange[0])/self.ny
  fovmap=make_map(fltarr(self.nx,self.ny)+255,xc=xc,yc=yc,dx=dx,dy=dy,time=time,rsun=rsun)
  ;correction needed to avoid double precision math errors
  fovmap.xc=round(fovmap.xc*1000d)/1000d
  fovmap.yc=round(fovmap.yc*1000d)/1000d
  ;correction needed to avoid precision math errors
  add_prop,fovmap,b0=self.parent->GetLos(/b0),l0=self.parent->GetLos(/l0),SpaceView=self.parent->SpaceView()
  if ~obj_valid(self.fovmap) then self.fovmap=obj_new('map')
  self.fovmap->set,0,map=fovmap
  fovdata=data[*,[0,1,2,15]]
  self.fovscreen->SetProperty, data=fovdata
end

function gxROI::GetFovPixSize,unit=unit,arcseconds=arcseconds
  ds=[self.fovmap->get(/dx),self.fovmap->get(/dy)]
  if keyword_set(arcseconds) then return,ds
  ds/=self.fovmap->get(/RSUN)
  if keyword_set(unit) then ds=ds*gx_rsun(unit=unit)
  return,ds
end

function gxROI::GetScanboxData,sun=sun
  self.scanbox->GetProperty,data=data
  if keyword_set(sun) then  data=gx_transform(data,self.parent->GetSTM(),/inv)
  return,data
end  

function gxROI::GetFOVData,sun=sun
  self.fovscreen->GetProperty, data=data
  if keyword_set(sun) then  data=gx_transform(data,self.parent->GetSTM(),/inv)
  return,data
end

function gxROI::ComputeGrid
  t0=systime(/s)
  prog_id = gx_progmeter(/INIT,label='Grid Computation Progress',button='Abort')
  nx=self.nx
  ny=self.ny
  self->GetProperty,parent=model
  model->GetProperty,xcoord_conv=xcoord_conv,ycoord_conv=ycoord_conv,zcoord_conv=zcoord_conv
  dx=xcoord_conv[1]
  dy=ycoord_conv[1]
  scale=[[dx,dy,zcoord_conv[1]],[dx,dy,zcoord_conv[1]]] 
  volume=model->GetVolume()
  volume->GetVertexAttributeData,'dz',dz
  dim=(model->size(/volume))[1:3]
  max_size=total(dim)
  grid=dblarr(4,nx,ny,max_size)-1
  if n_elements(dz) eq 0 then begin
    dz=replicate(zcoord_conv[1],dim)
  endif
  sdata=model->GetScanboxData(stm=stm)
  Nlos=nx*ny
  LOSarr=dblarr(3, 2, NLos)
  zrange=minmax(sdata[2,*])
  fovmap=self.fovmap->get(/map)
  ;correction needed to avoid double precision math errors
  fovmap.xc=round(fovmap.xc*1000d)/1000d
  fovmap.yc=round(fovmap.yc*1000d)/1000d
  ;correction needed to avoid precision math errors
  get_map_coord,fovmap,x,y
  x=x/fovmap.rsun
  y=y/fovmap.rsun
  LOSarr[0,0,*]=x
  LOSarr[0,1,*]=x
  LOSarr[1,0,*]=y
  LOSarr[1,1,*]=y
  LOSarr[2,0,*]=min(zrange)
  LOSarr[2,1,*]=max(zrange)
  losarr[*,0,*]=gx_transform(reform(losarr[*,0,*]),stm)
  losarr[*,1,*]=gx_transform(reform(losarr[*,1,*]),stm)
  losarr[0,*,*]=losarr[0,*,*]*dx
  losarr[1,*,*]=losarr[1,*,*]*dy
  losarr[2,*,*]=losarr[2,*,*]*zcoord_conv[1]
  LZ=delta(zrange)
  t1=systime(/s)
  if (gx_progmeter(prog_id,.25) eq 'Cancel') then goto,escape
  volume->GetVertexAttributeData,'voxel_id',voxel_id
  ;erase upper 16 bits of the voxel id
  voxel_id=ishft(ishft(voxel_id,16),-16)
  ;end erase upper 16 bits
  gx_renderirregularmulti, Nlos, dx, dy, dz, LOSarr, $
    Nvoxels, VoxList, ds, $
    x_ind, y_ind, z_ind, $
    entry_point, exit_point, $
    voxel_id=voxel_id
  sz=size(x_ind)
  Nvoxels=reform(NVoxels,nx,ny)
  entry_point=reform(entry_point,3,nx,ny)
  exit_point=reform(exit_point,3,nx,ny)
  ds=reform(ds,sz[1],nx,ny) 
  x_ind=reform(x_ind,sz[1],nx,ny)  
  y_ind=reform(y_ind,sz[1],nx,ny)
  z_ind=reform(z_ind,sz[1],nx,ny)  
  t2=systime(/s) 
  if (gx_progmeter(prog_id,.5) eq 'Cancel') then goto,escape
  n=0
  k=0
  k0=0l
  k1=0l
  n=0l
  for j=0,ny-1 do begin
    for i=0,nx-1 do begin
      ng=nvoxels[i,j]
      if ng gt 0 then begin
        n=max([n,ng])
        LOS_sun_seg=gx_transform([[entry_point[*,i,j]],[exit_point[*,i,j]]]/scale,stm,/inv)
        k=(floor(max_size*(min(LOS_sun_seg[2,*])-min(zrange))/LZ))>0
        k0=min([k,k0])
        k1=max([k+ng-1,k1])
        if k1 ge max_size then begin
          dk=k1-max_size+64
          grid=transpose([transpose(grid),transpose(dblarr(4,nx,ny,dk)-1)])
          max_size+=dk
        endif
        grid[0,i,j,k:k+ng-1]=ds[0:ng-1,i,j] 
        grid[1,i,j,k:k+ng-1]=x_ind[0:ng-1,i,j] 
        grid[2,i,j,k:k+ng-1]=y_ind[0:ng-1,i,j] 
        grid[3,i,j,k:k+ng-1]=z_ind[0:ng-1,i,j] 
      end
    end
  end
  t3=systime(/s) 
  if (gx_progmeter(prog_id,.75) eq 'Cancel') then goto,escape
  grid=grid[*,*,*,(k0-1)>0:(k1-1)<(max_size-1)]
  max_size=n_elements(grid[0,0,0,*])
  for i=0,nx-1 do begin
    for j=0, ny-1 do begin
      full=where(reform(grid[0,i,j,*]) eq -1, count, comp=comp, ncomp=ncomp)
      if ncomp gt 0 then begin
        if lz lt total(grid[0,i,j,comp],/double) then begin
          print,lz,total(grid[0,i,j,comp],/double)
        endif
        if count gt 0 then grid[0,i,j,full]=((Lz-total(grid[0,i,j,comp],/double))/count);>0
      endif else begin
        grid[0,i,j,full]=Lz/count
      endelse
    endfor
  endfor
  t4=systime(/s)
  sz=size(grid)
  self.nz=sz[4]
  ptr_free,self.grid
  self.grid=ptr_new(temporary(grid))
  ptr_free,self.los_bounds
  self.los_bounds=ptr_new({entry_point:temporary(entry_point),exit_point:temporary(exit_point)})
  status = gx_progmeter(prog_id,/DESTROY)
  flags=(self.parent->GetVolume())->setflags(newGrid=0)
  message,strcompress('ComputeScanGrid execution time: '+string(systime(/s)-t0)),/info
  if (gx_progmeter(prog_id,1.0) eq 'Cancel') then goto,escape
  self->DisplayMap
  return,flags.newGrid
  escape:
  status = gx_progmeter(prog_id,/DESTROY)
  message,'Grid computation aborted',/info
  flags=(self.parent->GetVolume())->getflags()
  return,flags.newGrid
end

function gxROI::GetGrid
  return,self.grid
end

function gxROI::Get_LOS_Bounds
  return,self.los_bounds
end

pro gxROI::SetGrid,grid
  self.grid=grid
end

function gxROI::GetDim
  return,[self.nx,self.ny,self.nz]
end

function gxROI::GetFOVmap
  return,self.fovmap
end

function gxROI::GetRefMaps
  return,self.parent->refmaps()
end

function gxROI::GetFOVscreen
  return,self.fovscreen
end

function gxROI::GetBaseScreen
return, self.parent->GetByName('Reference Map')
end

function gxROI::GetScanbox
  return, self.scanbox
end

pro gxROI__define
self={gxROI,inherits IDLgrModel, nx:0l,ny:0l,nz:0l,grid:ptr_new(),los_bounds:ptr_new(),scanbox:IdlGrROI(),fovmap:obj_new(),fovscreen:IDLgrPolygon(),fovimage:IDLgrImage()}
end