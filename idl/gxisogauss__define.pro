function gxisogauss::INIT, verts,freq=freq, s=s,te=te,_extra=_extra
 default,freq,1d; lowest EOVSA observing frequency
 default,s,1; first harmonic
 self.te=ptr_new(te)
 self.freq=freq[0];ignore accidental array input
 self.s=s[0];ignore accidental array input
 b=self.freq/2.8e-3/self.s
 self.name=strcompress(string(b,self.freq,self.s,f='("iso_",i0,"G_",f0.2,"GHz_s",i0)'),/rem)
 return,self->IDLgrPolygon::Init(verts,_extra=_extra)
end

function gxisogauss::GetS
 return,self.s
end

function gxisogauss::GetFreq
  return,self.freq
end

function gxisogauss::GetB
  return,self.freq/2.8e-3/self.s
end

function gxisogauss::GetName
  return,self.name
end

pro gxisogauss::SetName,name
  self.name=name
end

function gxisogauss::GetSelectedSurface,names=names,string=string
 names=['Te','Tb_lcp','Tb_rcp']
 return,keyword_set(string)?names[self.selected]:self.selected
end

pro gxisogauss::Select,select
  if is_number(select) then begin
    self.selected=select>0<2
    return
  endif
  if is_string(select) then begin
    case strlowcase(select) of
      'te':self.select=0
      'tb_lcp':self.select=1
      'tb_rcp':self.select=2
    else:
    endcase
  endif  
end

function gxisogauss::GetT,lcp=lcp,rcp=rcp,te=te,id=id,opacity=opacity,alpha=alpha
  if ~ptr_valid(self.te) then return,!null
  if ~ptr_valid(self.tb_lcp) or ~ptr_valid(self.tb_rcp) then self->Update,/force,/full
  if keyword_set(te) then self.selected=0
  if keyword_set(lcp) then self.selected=1
  if keyword_set(rcp) then self.selected=2
  case self.selected of 
    1:begin
       id='Tb_lcp'
       Tb=*self.tb_lcp
      end
    2:begin
       id='Tb_rcp'
       Tb=*self.tb_rcp
      end
    else:begin
          id='Te'
          Tb=*self.te
         end
  endcase
  if keyword_set(opacity) or keyword_set(alpha) then begin
    opacity=keyword_set(opacity)?opacity:1
    if opacity gt 1 then alpha=1
    Tb=self->GetOpacity(Tb,alpha=alpha)
    id=(keyword_set(alpha)?'Opacity ':'Optical Depth ')+id
  endif
  return,Tb
end

function gxisogauss::ComputeGrid,nz=nz,full=full
  self->getproperty,parent=model
  if ~obj_isa(model,'gxmodel') then begin
    message,'parent is not a valid gxmodel object!',/cont
    return,!null
  endif
  model->ResetPosition
  verts=*self.data
  dx=self.YCOORD_CONV[1]
  dy=self.YCOORD_CONV[1]
  dz=self.ZCOORD_CONV[1]
  nLOS=n_elements(verts[0,*])
  LOSarr=dblarr(3, 2, NLos)
  stm=model->GetSTM()
  lverts=gx_transform(verts,stm,/inv)
  losarr[0,0,*]=lverts[0,*]
  losarr[0,1,*]=lverts[0,*]
  losarr[1,0,*]=lverts[1,*]
  losarr[1,1,*]=lverts[1,*]
  default,nz,1
  if keyword_set(full) then nz=max((model->Size())[[1,2,3]])
  losarr[2,0,*]=lverts[2,*]-dz*nz
  losarr[2,1,*]=lverts[2,*]+dz*nz
  losarr[*,0,*]=gx_transform(reform(losarr[*,0,*]),stm)
  losarr[*,1,*]=gx_transform(reform(losarr[*,1,*]),stm)
  losarr[0,*,*]=losarr[0,*,*]*dx
  losarr[1,*,*]=losarr[1,*,*]*dy
  losarr[2,*,*]=losarr[2,*,*]*dz
  ;erase upper 16 bits of the voxel id
  voxel_id=ishft(ishft(model->GetVertexData('voxel_id'),16),-16)
  ;end erase upper 16 bits
  dz_vol=model->GetVertexData('dz')
  if n_elements(dz_vol) eq 0 then begin
    dim=(model->size(/volume))[1:3]
    dz_vol=replicate(dz,dim)
  endif
  gx_renderirregularmulti, nLOS, dx, dy, dz_vol, LOSarr, $
    Nvoxels, VoxList, dr, $
    x_ind, y_ind, z_ind, $
    entry_point, exit_point, $
    voxel_id=voxel_id
    return,reform(transpose([[[dr]],[[x_ind]],[[y_ind]],[[z_ind]]],[2,1,0]),4,nLOS,1,n_elements(dr[*,0]))
end

pro gxisogauss::update,force_update=force_update,_extra=_extra
  self->getproperty,parent=model
  if ~obj_isa(model,'gxmodel') then begin
    message,'parent is not a valid gxmodel object!',/cont
    return
  endif
  if ~ptr_valid(self.te) or ~ptr_valid(self.tb_lcp) or ~ptr_valid(self.tb_rcp) then force_update=1
  if ~keyword_set(force_update) then return
  message,'Updating the isogauss '+self.name+' surface, it might take a while!',/cont
  dx=self.YCOORD_CONV[1]
  dy=self.YCOORD_CONV[1]
  dz=self.ZCOORD_CONV[1]
  grid=self->ComputeGrid(_extra=_extra)
  renderer=!version.os_family eq 'Windows'?'grff_dem_transfer':'grffdemtransfer'
  info=gx_rendererinfo(renderer)
  ds=dx*dy*(gx_rsun(unit='cm')^2)
  gx_setparm,info,'dS',ds
  gx_setparm,info,'freqlist',self.freq
  gx_setparm,info,'mech_flag',0;2
  gx_setparm,info,'s_max',self.s
  freqlist=info.spectrum.x.axis
  info=gx_rendererinfo(renderer,info=info)
  ;here the gx_rendererinfo automatic settings are replaced
  gx_setparm,info,'DEM_key',1
  gx_setparm,info,'DDM_key',1
  ;end replacement
  sz=size(grid)
  nx=sz[2]
  nvox=sz[4]
  rowdata=make_array([nx,info.pixdim],/float)
  chanlist=info.spectrum.x.axis
  nchan=n_elements(chanlist)
  nparms=long(info.nparms.value)
  nparms[0:2]=[nx,nvox,nchan]
  info.nparms.value=nparms
  rparms=info.rparms.value
  model->Slice,info.parms,0,grid,scanner=scanner
  parms=(*scanner).parms
  result=execute(info.execute)
  coeff=1.4568525e026/ds/(self.freq)^2
  coeff=coeff/2; NORH convention
  ptr_free,self.tb_lcp & self.tb_lcp=ptr_new(coeff*rowdata[*,0,0,0]) ; LCP Brightness teerature
  ptr_free,self.tb_rcp & self.tb_rcp=ptr_new(coeff*rowdata[*,0,1,0]) ; RCP Brightness teerature
  message,'update of '+self.name+' surface completed!',/cont
end

function gxisogauss::GetOpacity,Tb,alpha=alpha
 tau=n_elements(tb) gt 0?alog(*self.te / ((*self.te - 2*Tb)>0.0001)):replicate(100.,n_elements(*self.te ))
 return,keyword_set(alpha)?((n_elements(tb) gt 0 and self.selected ne 0)?byte((tau>0.0)*255):replicate(255b,n_elements(*self.te ))):tau
end

function gxisogauss::IsOpaque
 return,self.opaque
end

pro gxisogauss::SetOpacity,opacity
 self.opaque=is_number(opacity)?opacity:0
end 

function gxisogauss::GetRGB,ct
  if n_elements(ct) eq 3*256 then return,ct
  if n_elements(ct) ne 1 then ct=39
  tvlct,/get,rgb_old
  loadct,ct,/silent
  tvlct,/get,rgb
  tvlct,rgb_old
  return,rgb
end

pro gxisogauss::Display,select,hide=hide,lcp=lcp,rcp=rcp,te=te,opaque=opaque,$
                        force_update=force_update,local_scale=local_scale,ct=ct
 if ~obj_valid(self.parent) then return
 self.hide=keyword_set(hide)
 if self.hide then return
 self->Update,force_update=force_update; update if necessary
 if keyword_set(te) then self.selected=0
 if keyword_set(lcp) and  ~keyword_set(rcp) then self.selected=1
 if keyword_set(rcp) and  ~keyword_set(lcp) then self.selected=2
 if is_string(select) or is_number(select) then self->select,select; if set, select overwrites all of the above!
 case self.selected of
  0:Tb=*self.te
  1:Tb=*self.tb_lcp
  2:Tb=*self.tb_rcp
  else:
 endcase
 if keyword_set(opaque) then self.opaque=1
 alpha=keyword_set(self.opaque)?replicate(255b,n_elements(Tb)):self->GetOpacity(TB,/alpha)
 te_vert=*self.te;keyword_set(tb)?tb:*self.te 
 rgb=self->GetRGB(ct)
 t0 = self.parent->GetVertexData('T0')
 it = byte(te_vert*255/(keyword_set(local_scale)?max(te_vert):max(t0)))
 vcol = transpose([[rgb[it,0]],[rgb[it,1]],[rgb[it,2]],[alpha]])
 self.SetProperty,vert_colors=vcol
 self.hide=0
end

function gxisogauss::GetMap,vertexData,id=id,ObsBeam=ObsBeam,_extra=_extra
  model=self.parent
  if ~obj_isa(model,'gxmodel') then return,!null
  map=(self->GetGridMap())->get(/map)
  grid=map.data
  sz=size(grid)
  data=fltarr(sz[1],sz[2])
  good=where(grid ge 0, count)
  vertexData=(n_elements(vertexData) eq n_elements(*self.te))?vertexdata:self->GetT(id=id,_extra=_extra)
  if count gt 0 then data[good]=vertexData[grid[good]]
  if (size(ObsBeam))[0] eq 2 then data=convol_fft(data, ObsBeam)
  add_prop,map,data=data,/replace
  default,id,'User Supplied Vertex Data'
  map.id=id
  return,keyword_set(structure)?map->get(/map):map
end

function gxisogauss::GetCombinedOpacityMap,th=th,or_operateor=or_operator,_extra=_extra
 default,th,1
 lcp=self->GetT(/lcp,/opacity)
 rcp=self->GetT(/rcp,/opacity)
 opacity= keyword_set(or_operator)?((lcp ge th)  or (rcp ge th)):((lcp ge th)  and (rcp ge th))
 return,self->GetMap(opacity,id=(keyword_set(or_operator)?'LCPorRCP':'LCP&LCP')+' Opacity',_extra=_extra)
end


function gxisogauss::GetGridMap,_extra=_extra
  if ~valid_map(self.GridMap) then self.ComputeGridMap,_extra=_extra
  return,self.GridMap
end

pro gxisogauss::ComputeGridMap,dx=dx,dy=dy,refmap=refmap
  model=self.parent
  if ~obj_isa(model,'gxmodel') then return
  model->ResetPosition
  STM=model->GetSTM()
  self->GetProperty,data=verts
  verts=gx_transform(verts,stm,/inv)*model->Rsun()
  if valid_map(refmap) then begin
    ref=obj_valid(refmap)?refmap->get(/map):refmap
    dx=ref.dx
    dy=ref.dy
    xrange=get_map_xrange(ref)
    yrange=get_map_yrange(ref)
    if valid_map(self.GridMap) then begin
      match=1b
      match =match and (dx eq self.GridMap->get(/dx))
      match =match and (dy eq self.GridMap->get(/dy))
      for k=0,1 do match=match and (xrange[k] eq (self.GridMap->get(/xrange))[k])
      for k=0,1 do match =match and (yrange[k] eq (self.GridMap->get(/yrange))[k])
      if match eq 1b then return
    endif
  endif
  default,dx,ceil(self.xcoord_conv[1]*model->Rsun())
  default,dy,ceil(self.ycoord_conv[1]*model->Rsun())
  default,xrange,[min(verts[0,*],max=max),max]+dx
  default,yrange,[min(verts[1,*],max=max),max]+dy
  hist_x=histogram(verts[0,*],min=xrange[0],max=xrange[1],bin=dx,reverse=rx,loc=x)
  hist_y=histogram(verts[1,*],min=yrange[0],max=yrange[1],bin=dy,reverse=ry,loc=y)
  grid=lonarr(n_elements(x),n_elements(y))
  grid[*]=-1
  dz2=(max(verts[2,*])-verts[2,*])^2
  for i=0,n_elements(x)-1  do begin
    for j=0,n_elements(y)-1 do begin
      if Rx[i] NE Rx[i+1] and Ry[j] NE Ry[j+1] then begin
        x_idx=Rx(Rx[i] : Rx[i+1]-1)
        y_idx=Ry(Ry[j] : Ry[j+1]-1)
        idx=cgSETINTERSECTION(x_idx,y_idx,count=count)
        if count gt 0 then begin
          min_dist2=min((x[i]-verts[0,*])^2+(y[j]-verts[1,*])^2+dz2,k)
          grid[i,j]=k
        endif
      endif  
    endfor
  endfor
  obj_destroy,self.GridMap
  map=make_map(grid,xc=mean(minmax(x)),yc=mean(minmax(y)),$
    dx=dx,dy=dy,time=model->GetTime(),b0=model->GetLos(/b0),l0=model->GetLos(/l0))
  gridmap=obj_new('map')  
  gridmap->setmap,0,map
  self.GridMap=gridmap
end

pro gxisogauss::Cleanup
  ptr_free,self.te
  ptr_free,self.tb_rcp
  ptr_free,self.tb_rcp
  self->IDLgrPolygon::Cleanup
end

pro gxisogauss__define
  self={gxisogauss,inherits IDLgrPolygon,freq:0.0,s:0,te:ptr_new(),tb_rcp:ptr_new(),tb_lcp:ptr_new(),gridmap:obj_new(),selected:0b,opaque:0b}
end