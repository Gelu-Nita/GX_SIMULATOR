function gxModel::INIT,EW=EW,NS=NS,_extra=_extra
 compile_opt hidden
 default,EW,0.0
 default,NS,0.0
 default,wParent,0l
 self.XCOORD_CONV=[0,1]
 self.YCOORD_CONV=[0,1]
 self.ZCOORD_CONV=[0,1]
 self.IsROI=0
 self.FullROI=1
 self.subgridpts=10
 self.steps=50000
 ;Here, the previously unused lock property is internaly repurposed 
 ;to store the newly introduced WinOS property
 self.lock=(!VERSION.OS_FAMILY eq 'Windows')
 return,self->IDLgrModel::Init(_extra=_extra)
end


pro gxModel::CurlB,cx,cy,cz,Bx=Bx,By=By,Bz=Bz
  self->GetProperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz
  dummy=self->GetB(Bx=Bx,By=By,Bz=Bz)
  m=max([dx,dy,dz])
  dx=dx[1]/m
  dy=dy[1]/m
  dz=dz[1]/m
  curl,bx,by,bz,cx,cy,cz,order=3, dx=dx, dy=dy, dz=dz
  du=m*gx_rsun()
  cx/=du
  cy/=du
  cz/=du
end

function gxModel::GetBaseIndex
 base_index=!null
 if ptr_valid(self.refmaps) then begin
  map=(*self.refmaps)[0]
  if obj_valid(map) then begin
    map=map->get(0,/map)
    if tag_exist(map,'index') then base_index=map.index
  endif
 endif
 return,base_index
end

function gxModel::GetB,p,Bx=Bx,By=By,Bz=Bz,volume=volume
;p is expressed in self's box fractional index coordinates
if obj_valid(self.volume) then return,self.volume->GetB(p,Bx=Bx,By=By,Bz=Bz,volume=volume) else return,[0,0,0]*!Values.f_nan
end

function gxModel::GetBLine,p,dxdydz,subgridpts=subgridpts,tr_height=tr_height,no_tr=no_tr,full=full,use_idl=use_idl,_extra=_extra
id=self.volume->VoxelId()
tr_layer=where((id and 1) gt 0,count)
if count gt 0 then begin
  tr_idx=max((array_indices(id,tr_layer))[2,*])+1
endif else tr_idx=0
sx=self.size[1]
sy=self.size[2]
sz=self.size[3]
if n_elements(p) eq 0 then p,randomu(seed,3)*[sx,sy,sz]-1

;if ~self.lock  then begin
if keyword_set(use_idl)  then begin
  default,subgridpts,self.subgridpts
  default,dxdydz,[1., 1., 1.]
  void=self->GetB(Bx=Bx,By=By,Bz=Bz)
  dx=self.YCOORD_CONV[1]
  dy=self.YCOORD_CONV[1]
  dz=self.ZCOORD_CONV[1]
  x=p[0]
  y=p[1]
  z=p[2]
  x0=x
  y0=y
  z0=z
  top=[x,y,z]
  s_top=0
  lb=[interpolate(bx,x,y,z),interpolate(by,x,y,z),interpolate(bz,x,y,z)]
  ls=0.0
  s=0.0
  btop=norm(lb)
  lx=x
  ly=y
  lz=z
  iter=0l
  fin=1
  while iter lt self.steps and x ge 0 and x le sx-1 and y ge 0 and y le sy-1 and z ge 0 and z le sz-1 and fin eq 1 do begin
   if ~keyword_set(no_tr) then $
    n=n_elements(chromo_layers) gt 0?((z le max(tr_idx))?chromo_layers:subgridpts):subgridpts $
   else n=subgridpts
   iter+=1
   bb=[interpolate(bx,x,y,z),interpolate(by,x,y,z),interpolate(bz,x,y,z)]
   absb=norm(bb)
   dr=bb/absb/n
   dr=dr*min([dx, dy, dz])/[dx, dy, dz] ;*****
   if z lt tr_idx then dr=[0,0,dr[2]/abs(dr[2])]
   ds=norm(dr*[dx,dy,dz])
   fin=finite(ds)
   if fin then begin
   s=s+ds
   x=(x+dr[0]);>0<(sx-1)
   y=(y+dr[1]);>0<(sy-1)
   z=(z+dr[2]);>0<(sy-1)
   dxdydz_current=dxdydz
  
   voxel_test=(abs(x-x0) gt dxdydz_current[0] or abs(y-y0)gt dxdydz_current[1] or abs(z-z0) gt dxdydz_current[2]) 
   if voxel_test or keyword_set(full) then begin
    if absb lt btop then begin
     btop=absb
     top=[x,y,z]
     s_top=s
    end
    lx=[lx,x]
    ly=[ly,y]
    lz=[lz,z]
    ls=[ls,s]
    lb=[[[lb]],[bb]]
    x0=x
    y0=y
    z0=z
   endif 
   end
  end
  x=p[0]
  y=p[1]
  z=p[2]
  x0=x
  y0=y
  z0=z
  s=0.0
  fin=1
  iter=0l
  while iter lt self.steps and x ge 0 and x le sx-1 and y ge 0 and y le sy-1 and z ge 0 and z le sz-1 and fin eq 1 do begin
   if ~keyword_set(no_tr) then $
    n=n_elements(chromo_layers) gt 0?((z le max(tr_idx))?chromo_layers:subgridpts):subgridpts $
   else n=subgridpts
   iter+=1
   bb=[interpolate(bx,x,y,z),interpolate(by,x,y,z),interpolate(bz,x,y,z)]
   absb=norm(bb)
   dr=-bb/absb/n
   dr=dr*min([dx, dy, dz])/[dx, dy, dz] ;*****
   if z lt tr_idx then dr=[0,0,dr[2]/abs(dr[2])]
   ds=-norm(dr*[dx,dy,dz])
   fin=finite(ds)
   if fin then begin
   s=s+ds
   x=(x+dr[0]);>0<(sx-1)
   y=(y+dr[1]);>0<(sy-1)
   z=(z+dr[2]);>0<(sy-1)
   dxdydz_current=dxdydz
  
   voxel_test=(abs(x-x0) gt dxdydz_current[0] or abs(y-y0)gt dxdydz_current[1] or abs(z-z0) gt dxdydz_current[2]) 
   if voxel_test or keyword_set(full) then begin
    if absb lt btop then begin
     btop=absb
     top=[x,y,z]
     s_top=s
    end
    lx=[x,lx]
    ly=[y,ly]
    lz=[z,lz]
    ls=[s,ls]
    lb=[[bb],[[lb]]]
    x0=x
    y0=y
    z0=z
   endif 
   end
  end
   if n_elements(lx) lt 2 then begin
    return,obj_new()
   end
   lx=lx>0<(sx-1)
   ly=ly>0<(sy-1)
   lz=lz>0<(sz-1)
   m=min((lx-top[0])^2+(ly-top[1])^2+(lz-top[2])^2,itop)
   lx[itop]=top[0]
   ly[itop]=top[1]
   lz[itop]=top[2]
   ls[itop]=s_top
   ls=ls-s_top
   line=OBJ_NEW('gxBline',lx>0<(sx-1),ly>0<(sy-1),lz>0<(sz-1),top=top,_extra=_extra,tr_height=tr_height)
   line->SetProperty,XCOORD_CONV=self.XCOORD_CONV,YCOORD_CONV=self.YCOORD_CONV,ZCOORD_CONV=self.ZCOORD_CONV
   line->SetVertexAttributeData,'B',lb
   line->SetVertexAttributeData,'s',ls
 endif else begin
   line=self->ComputeBlines(p,tr_height_km=0)
 endelse
 
 return,line
END


pro gxModel::ComputeCoronalModel,compute_lines=compute_lines,zstop=zstop,closed=closed,tr_height=tr_height,center_vox=center_vox
  self.volume->GetVertexAttributeData,'bmed',bmed
  if keyword_set(compute_lines) or n_elements(bmed) eq 0 then begin
    ;compute global alpha
    self.volume->GetVertexAttributeData,'Bx',Bx
    self.volume->GetVertexAttributeData,'By',By
    self.volume->GetVertexAttributeData,'Bz',Bz
    scale=self->GetScale()
    dx=scale.YCOORD_CONV[1]
    dy=scale.YCOORD_CONV[1]
    dz=scale.ZCOORD_CONV[1]
    if n_elements(tr_height) eq 0 then tr_height=gx_tr_height()
    tr_height=tr_height/dz
    du=max([dx,dy,dz])
    curl,bx,by,bz,cx,cy,cz,order=3, dx=dx/du, dy=dy/du, dz=dz/du
    global_alpha=(bx*cx+by*cy+bz*cz)/(bx*bx+by*by+bz*bz)/(du*gx_rsun())
    global_curlb=sqrt(cx*cx+cy*cy+cz*cz)/(du*gx_rsun())
    ;end global alpha computation
    default,MAX_STEPSIZE,1/self.subgridpts
    default,MAX_ITERATIONS,self.STEPS
    t0=systime(/s)
    size=self.size
    nx=size[1]
    ny=size[2]
    nz=size[3]
    bmed=fltarr(nx,ny,nz)
    length=bmed
    alpha=bmed
    curlb=bmed
    foot1=lonarr(nx,ny,nz)
    foot2=lonarr(nx,ny,nz)
    closed=bytarr(nx,ny,nz)
    hit=closed

    obmed=fltarr(nx,ny,nz)
    olength=bmed
    oalpha=bmed
    ocurlb=bmed
    ofoot1=lonarr(nx,ny,nz)
    ofoot2=lonarr(nx,ny,nz)
    open=bytarr(nx,ny,nz)

    sz=self->Size()
    self->getproperty,zcoord_conv=zcoord_conv
    dz=zcoord_conv[1]
    z=-1
    bcube=fltarr(3,nx,ny,nz)
    bcube[0,*,*,*]=temporary(Bx)
    bcube[1,*,*,*]=temporary(By)
    bcube[2,*,*,*]=temporary(Bz)
    inverse_bcube=-bcube
    if keyword_set(center_vox) and ~keyword_set(zstop) then zstop=nz-1
    if keyword_set(zstop) then zmax=zstop else zmax=0
    repeat begin
      z+=1
      print,systime(/s)-t0,z,'/', zmax,$
        '  %hit: ',100*double(n_elements(where(hit)))/n_elements((hit)),$
        '  %closed: ',100*double(n_elements(where(closed)))/n_elements((hit))
      for x=0,nx-1 do begin
        for y=0, ny-1 do begin
          if closed[x,y,z] eq 1 then begin
            goto, skip
          end
          seed=[x,y,z]
          seed_normal=norm(seed) gt 0 ? seed/norm(seed):[0,0,1]
          PARTICLE_TRACE, bcube, seed, Verts_up, Conn_up, $
           max_stepsize=max_stepsize, max_iterations=max_iterations,seed_normal=seed_normal,anisotropy=dr,_extra=_extra
          size_up=size(verts_up)
          up=size_up[0] eq 1?0:1
          PARTICLE_TRACE, inverse_bcube, seed, Verts_down, Conn_down, $
           max_stepsize=max_stepsize, max_iterations=max_iterations,seed_normal=-seed_normal,anisotropy=dr,_extra=_extra
          size_down=size(verts_down)
          down=size_down[0] eq 1?0:1
          case down+ishft(up,1) of
            1: data=verts_down
            2: data=verts_up
            3: data=[[size_down[2] eq 2?verts_down[*,1:*]:reverse(verts_down[*,1:*],2)],[verts_up]]
            else:data=reform([-1,-1,-1],3,1)
          endcase
          if keyword_set(center_vox) then begin
            xx=x
            yy=y
            zz=z
          endif else begin
            xx=data[0,*]
            yy=data[1,*]
            zz=data[2,*]
          endelse
          closed_line=0
          bm=0
          lm=0
          sz=size(data)
          npts=sz[2]
          f1=data[0,0]+data[1,0]*nx+data[2,0]*nx*ny
          f2=data[0,npts-1]+data[1,npts-1]*nx+data[2,npts-1]*nx*ny
          if sz[0] eq 2 then begin
            idx=where(data[2,*] ge tr_height, nverts)
            if nverts gt 0 then begin
               closed_line=(data[2,0] lt tr_height) and (data[2,sz[2]-1] lt tr_height)
               verts_above=data[*,idx]
               lm=nverts eq 1?sqrt(total([dx,dy,dz]^2)):total(sqrt(total(((((verts_above-shift(verts_above,[0,1])))[*,1:*])*([dx,dy,dz]#replicate(1,nverts-1)))^2,1)))
               bline=interpolate(bcube,verts_above[0,*],verts_above[1,*],verts_above[2,*])
               bm=mean(sqrt(total(bline^2,1)))
               hit[data[0,*],data[1,*],data[2,*]]=1
            end 
          end    
            if closed_line eq 1 then begin
                if ~keyword_set(zstop) then zmax=max(data[2,*])>zmax
                bmed[xx,yy,zz]=bm
                length[xx,yy,zz]=lm
                alpha[xx,yy,zz]=mean(global_alpha[xx,yy,zz])
                curlb[xx,yy,zz]=mean(global_curlb[xx,yy,zz])
                closed[xx,yy,zz]=1
                foot1[xx,yy,zz]=f1
                foot2[xx,yy,zz]=f2
            endif else begin
                obmed[xx,yy,zz]=bm
                olength[xx,yy,zz]=lm
                oalpha[xx,yy,zz]=mean(global_alpha[xx,yy,zz])
                ocurlb[xx,yy,zz]=mean(global_curlb[xx,yy,zz])
                open[xx,yy,zz]=1
                ofoot1[xx,yy,zz]=f1
                ofoot2[xx,yy,zz]=f2
            endelse
          skip:
        end
      end
    endrep until floor(zmax) eq z
    print,systime(/s)-t0,x,y
    cidx=where(closed eq 1,count)
    if count gt 0 then begin
      self.volume->SetVertexAttributeData,'bmed',bmed[cidx]
      self.volume->SetVertexAttributeData,'length',length[cidx]
      self.volume->SetVertexAttributeData,'alpha',alpha[cidx]
      self.volume->SetVertexAttributeData,'curlb',curlb[cidx]
      self.volume->SetVertexAttributeData,'foot1',foot1[cidx]
      self.volume->SetVertexAttributeData,'foot2',foot2[cidx]
      self.volume->SetVertexAttributeData,'idx',cidx
    end
    oidx=where(open eq 1 and closed eq 0,ocount)
    if ocount gt 0 then begin
      self.volume->SetVertexAttributeData,'obmed',obmed[oidx]
      self.volume->SetVertexAttributeData,'olength',olength[oidx]
      self.volume->SetVertexAttributeData,'oalpha',oalpha[oidx]
      self.volume->SetVertexAttributeData,'ocurlb',ocurlb[oidx]
      self.volume->SetVertexAttributeData,'ofoot1',ofoot1[oidx]
      self.volume->SetVertexAttributeData,'ofoot2',ofoot2[oidx]
      self.volume->SetVertexAttributeData,'oidx',oidx
    end
  end
end

pro gxModel::GetCoronalModelAttributes,idx,bmed,length,open=open
 if ~keyword_set(open) then begin
  self.volume->GetVertexAttributeData,'idx',idx
  self.volume->GetVertexAttributeData,'length',length
  self.volume->GetVertexAttributeData,'bmed',bmed
 endif else begin
   self.volume->GetVertexAttributeData,'oidx',idx
   self.volume->GetVertexAttributeData,'olength',length
   self.volume->GetVertexAttributeData,'obmed',bmed
 endelse
end

pro gxModel::ComputeCoronalLines,bcube,inverse_bcube,seed=seed,Bmed=Bmed,Length=Length,closed=closed,verts=verts,tr_height=tr_height,_extra=_extra
 dx=self.xcoord_conv[1]
 dy=self.xcoord_conv[1]
 dz=self.xcoord_conv[1]
 default,tr_height,gx_tr_height()/dz
 dr=[dx,dy,dz]/min([dx,dy,dz])
 void=self->GetB(Bx=Bx,By=By,Bz=Bz)
 if n_elements(bcube) eq 0 then begin
   dim=size(Bx,/dim)
   bcube=fltarr(3,dim[0],dim[1],dim[2])
   bcube[0,*,*,*]=temporary(Bx)
   bcube[1,*,*,*]=temporary(By)
   bcube[2,*,*,*]=temporary(Bz)
   inverse_bcube=-bcube
 end
 dim=self.size
 default,seed,[dim[0],dim[1],dim[2]]/2;center of the box
 default,MAX_STEPSIZE,1/self.subgridpts
 default,MAX_ITERATIONS,self.STEPS
 sz=size(seed)
 if sz[0] gt 1 then seed=seed[*,0]
  seed_normal=norm(seed) gt 0 ? seed/norm(seed):[0,0,1]
  PARTICLE_TRACE, bcube, seed, Verts_up, Conn_up, $
    max_stepsize=max_stepsize, max_iterations=max_iterations,seed_normal=seed_normal,anisotropy=dr,_extra=_extra
    size_up=size(verts_up)
    up=size_up[0] eq 1?0:1
  PARTICLE_TRACE, inverse_bcube, seed, Verts_down, Conn_down, $
    max_stepsize=max_stepsize, max_iterations=max_iterations,seed_normal=-seed_normal,anisotropy=dr,_extra=_extra
    size_down=size(verts_down)
    down=size_down[0] eq 1?0:1
    case down+ishft(up,1) of
    1: verts=verts_down
    2: verts=verts_up
    3: verts=[[size_down[2] eq 2?verts_down[*,1:*]:reverse(verts_down[*,1:*],2)],[verts_up]]
    else:verts=reform([-1,-1,-1],3,1)
    end
    closed=0
    length=0
    bmed=0
    sz=size(verts)
    if sz[0] eq 2 then begin
      idx=where(verts[2,*] ge tr_height, nverts)
      if nverts gt 0 then begin
        closed=(verts[2,0] lt tr_height) and (verts[2,sz[2]-1] lt tr_height)
        verts_above=verts[*,idx]
        length=nverts eq 1?sqrt(total([dx,dy,dz]^2)):total(sqrt(total(((((verts_above-shift(verts_above,[0,1])))[*,1:*])*([dx,dy,dz]#replicate(1,nverts-1)))^2,1)))
        bline=interpolate(bcube,verts_above[0,*],verts_above[1,*],verts_above[2,*])
        bmed=mean(sqrt(total(bline^2,1)))
      end
    end
end

pro gxModel::CreateContour,select
 default,select,3
 if not ptr_valid(self.refmaps) then return
 if select gt n_elements(*self.refmaps)+1 then return
 data=(*self.refmaps)[select]->get(/data)
 o = OBJ_NEW('IDLgrContour', data, COLOR=[0,0,200], C_LINESTYLE=0, /PLANAR, GEOMZ=0, C_VALUE=INDGEN(20),XCOORD_CONV=self.XCOORD_CONV,YCOORD_CONV=self.YCOORD_CONV)
 self->add,o
end

pro gxModel::UpdateRoi,roi=roi,replace=replace
 all=self->Get(/all,isa='IDLgrImage',count=count)
 if count gt 0 then begin
  self->Remove,all
  obj_destroy,all
 endif

 roi=self->scanbox()
 if keyword_set(replace) then begin
  self->remove,roi
  obj_destroy,roi
 endif
 if ~obj_valid(roi) then begin
   roi=gxroi(self)
   self->add,roi
 end
 self->remove,self.volume
 sz=self->Size()
 data=dblarr(sz[1],sz[2])
 map=obj_new('IDLgrImage',data,TRANSFORM_MODE=1,DEPTH_TEST_DISABLE=2,name='Reference Map')
 map->SetProperty,XCOORD_CONV=self.XCOORD_CONV,YCOORD_CONV=self.YCOORD_CONV
 self->add,map
 self->add,self.volume
end

function gxModel::Rsun
  pbr=pb0r(self->GetTime())
  return,pbr[2]*60
end

function gxModel::SetFOV,xc=xc,yc=yc,xfov=xfov, yfov=yfov,_extra=_extra
 self->ResetPosition
 rsun=self->Rsun()
 fovmap=self->GetFovMap()
 default,xc,fovmap->get(/xc)
 default,yc,fovmap->get(/yc)
 default, xfov,delta(get_map_xrange(fovmap->get(/map),/edge))
 default, yfov,delta(get_map_yrange(fovmap->get(/map),/edge))
 xrange=(xc+[-0.5d,0.5d]*xfov)/rsun
 yrange=(yc+[-0.5d,0.5d]*yfov)/rsun
 box=self->GetRoiBox()
 stm=self->GetSTM()
 data=gx_transform(box,stm,/inv)
 zrange=minmax(data[2,*])
 boxdata=gx_getboxedges(xrange=xrange,yrange=yrange,zrange=zrange)
 sdata=gx_transform(boxdata,stm)
 return,self->ReplaceScanboxData(sdata,_extra=_extra)
end

function gxModel::GetScanboxData,stm=stm
  rsun=self->Rsun()
  fovmap=self->GetFovMap()
  xc=fovmap->get(/xc)
  yc=fovmap->get(/yc)
  xfov=delta(get_map_xrange(fovmap->get(/map),/edge))
  yfov=delta(get_map_yrange(fovmap->get(/map),/edge))
  xrange=(xc+[-0.5d,0.5d]*xfov)/rsun
  yrange=(yc+[-0.5d,0.5d]*yfov)/rsun
  self->ResetPosition
  box=self->GetRoiBox()
  stm=self->GetSTM()
  boxdata=gx_transform(box,stm,/inv)
  zrange=minmax(boxdata[2,*])
  return,gx_getboxedges(xrange=xrange,yrange=yrange,zrange=zrange)
end


function gxModel::ReplaceScanboxData,sdata,nx=nx,ny=ny,compute_grid=compute_grid
 return,(self->scanbox())->ReplaceData(sdata,nx=nx,ny=ny,compute_grid=compute_grid)
end

pro gxModel::UpdateDef
  ;Here we upgrade combo_bodel if necessary
  self->upgrade_combo_model
  
  ;Here we update the WinOS flag depending on the platform on which the model is restored 
  ;the repurposed lock property is used to store this flag
  self.lock=(!VERSION.OS_FAMILY eq 'Windows')
  
  ;Here we solve a backward compatibility issue related with very old models using "closed" instead of "idx"
  self.volume->GetVertexAttributeData,'idx',idx
  if n_elements(idx) eq 0 then begin
    self.volume->GetVertexAttributeData,'closed',idx
    if n_elements(idx) ne 0 then begin
      self.volume->SetVertexAttributeData,'closed',0
      self.volume->SetVertexAttributeData,'idx',idx
    endif 
  endif
  ;done with closed-idx
  
  ;Here we set scale factors and model rotation correction, if not already defined
  self.volume->GetProperty,nscale=nscale,tscale=tscale,bscale=bscale
  if bscale eq 0 or nscale eq 0 or tscale then begin
     self.volume->GetVertexAttributeData,'Tscale',tscale
     default,tscale,1
     self.volume->GetVertexAttributeData,'Nscale',nscale
     default,nscale,1
     self.volume->GetVertexAttributeData,'Bscale',bscale
     default,bscale,1
     self.volume->SetProperty,bscale=bscale,nscale=nscale,tscale=tscale
  endif
  
  self.volume->GetVertexAttributeData,'gyro',gyro
  if n_elements(gyro) ne 0 then self.volume->SetProperty,gyro=gyro

  ;finished with scale factors and model rotation correction angle
    
    ;Here set default flags if not already defined
    flags=self.volume->getflags()
    has_flags=0l
    for i=0, n_tags(flags)-1 do has_flags+=flags.(i)
    if has_flags eq 0 then flags=self.volume->setflags(/NTstored,/TRadd,/TRMask,/TRfactor)
    ;Here check wheter the volume has Bmed-Lenght pair computed
    
    flags=self.volume->setflags(hasBL=self.volume->hasBL())
    flags=self.volume->setflags(hasNT=self.volume->hasNT())
 
    if flags.hasBL or flags.hasNT then begin
      ;Here compute alpha and curl if not already computed
      self.volume->GetVertexAttributeData,'alpha',alpha
      self.volume->GetVertexAttributeData,'curlb',curlb
      self.volume->GetVertexAttributeData,'idx',idx
      if n_elements(alpha) ne n_elements(idx) or n_elements(curlb) ne n_elements(idx) then begin
        dx=self.xcoord_conv[1]
        dy=self.ycoord_conv[1]
        dz=self.zcoord_conv[1]
        m=max([dx,dy,dz])
        dummmy=self->GetB(Bx=Bx,By=By,Bz=Bz)
        curl,bx,by,bz,cx,cy,cz,order=3, dx=dx/m, dy=dy/m, dz=dz/m
        alpha=bx*cx+by*cy+bz*cz
        alpha=alpha/(bx*bx+by*by+bz*bz)
        curlb=sqrt(cx*cx+cy*cy+cz*cz)
        alpha=alpha[idx]
        curlb=curlb[idx]
        self.volume->SetVertexAttributeData,'alpha',alpha
        self.volume->SetVertexAttributeData,'curlb',curlb
      endif
    endif

  
  ;Here e add corona object if missing from old models
  corona=self->Corona()
  if ~obj_valid(corona) then begin
    corona=obj_new('gxCorona',name='Corona')
    self->add,corona
  endif
  
  
  ;Add voxel IDs if missing
  self.volume->GetVertexAttributeData,'voxel_id',id
  sz=self->Size(/volume)
  if n_elements(id) ne sz[1]*sz[2]*sz[3] then begin
    ;Here we define a default volume with no chromosphere and no TR
    id=ulonarr(sz[1],sz[2],sz[3])+4
    self.volume->SetVertexAttributeData,'voxel_id',id
    self.volume->UpdateVoxelId,/force
    self.volume->GetVertexAttributeData,'voxel_id',id
    ;done with default
  endif else self.volume->SetVertexAttributeData,'voxel_id',ulong(id)
  
  
  if min(id) eq 4 then self.volume->UpdateVoxelId,/force
  
  ;Update fluxtubes properties if needed
  fluxtubes=self->get(/all,is='gxfluxtube',count=count)
  for k=0,count-1 do begin
    if ~isa(fluxtubes[k]->GetVertexData('nr_nth'),/number) then fluxtubes[k]->Update_N_NTH
  endfor
  ;
  
  ;Update maps if needed
  maps=*self.refmaps
  if n_elements(maps) eq 1 then goto,skip
  refmaps=obj_new('Map')
  sz=size(maps)
  if sz[0] eq 3 then begin
    omaps=objarr(sz[3])
    for i=0, sz[3]-1 do begin
      case i of
        0:id='Bx'
        1:id='By'
        2:id='Bz'
        else:id=string(i-2,format="('Refmap',i2)")
      endcase
      map=make_map(reform(maps[*,*,i]),id=id)
      refmaps->set,i,map=map
      refmaps->set,i,id=id
    endfor
 endif else for i=0, sz[3]-1 do refmaps->set,i,map=maps[i]->get(0,/map)
 ptr_free,self.refmaps
 self.refmaps=ptr_new(refmaps)
skip:
 self->UpdateRoi 
end

pro gxModel::AddMap,newmap,id=id
 if size(newmap,/tname) eq 'STRUCT'  then begin
  omap=obj_new('map')
  omap->setmap,0,newmap
  newmap=omap
 endif
 if obj_isa(newmap,'map') then begin
  added=newmap->get(/count)
  for k=0,added-1 do begin
    (*self.refmaps)->set,(*self.refmaps)->get(/count),map=newmap->get(k,/map)
    id=k eq 0?newmap->get(k,/id)+newmap->get(k,/time):[id,newmap->get(k,/id)+newmap->get(k,/time)]
  endfor
  return
 endif
 if valid_map(newmap) then (*self.refmaps)->set,(*self.refmaps)->get(/count),map=newmap
 id=newmap.id+' '+newmap.time
end

pro gxModel::ComputeTRmask,type=type,threshold=threshold,trmap=trmap,test=test
 default,type,'Bz'
 default,threshold,0
 case strupcase(type) of
  'BZ/B':begin
           map=(*self.refmaps)->Get(2,/map)
           bx=(*self.refmaps)->Get(0,/data)
           by=(*self.refmaps)->Get(1,/data)
           bz=(*self.refmaps)->Get(2,/data)
           b=sqrt(bx^2+by^2+bz^2)
           good=where(asin(abs(bz)/b)/!dtor ge abs(threshold),count,comp=comp,ncomp=ncomp)
           unit='deg'
         end
  else:begin
    map=(*self.refmaps)->Get(2,/map)
    good=where(abs(map.data) ge abs(threshold),count,comp=comp,ncomp=ncomp)
    unit='gauss'
    end
 endcase
 map.data[*]=0
 if count gt 0 then map.data[good]=1
 if keyword_set(test) then begin
  self->DisplayTRmask,trmap=map
  return
 endif
 map.id='BASE TR_Mask'
 map=create_struct(map,'tr_type',type,'tr_threshold',threshold,'tr_threshold_unit',unit)
 if ~tag_exist(map,'PROJECTION') then map=create_struct(map,'PROJECTION','CEA')
 match=-1
 for i=0, (*self.refmaps)->Get(/count)-1 do if (*self.refmaps)->Get(i,/id) eq map.id then match=i
 trmap=map
 if match eq -1 then begin
  self->AddMap,map 
  match=(*self.refmaps)->Get(/count)-1
 endif else (*self.refmaps)->set,match, map=map
 self->DisplayMap,match
 widget_control,widget_info(self.wparent,find_by_uname='GXMODEL:BaseMapSelect'),SET_COMBOBOX_SELECT=match
end

pro gxModel::DisplayTRmask,trmap=trmap
if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'
if n_elements(trmap) gt 0 then begin
  wTRDraw=widget_info(self.wparent,find_by_uname='GXMODEL:TRpreview')
endif else begin
  wTRDraw=widget_info(self.wparent,find_by_uname='GXMODEL:TR Mask')
  wTRTypeDisplay=widget_info(self.wparent,find_by_uname='GXMODEL:TR_TypeDisplay')
  wTRThresholdDisplay=widget_info(self.wparent,find_by_uname='GXMODEL:TR_ThresholdDisplay')
  count=(*self.refmaps)->get(/count)
  for i=0,count-1 do if (*self.refmaps)->get(i,/id) eq 'BASE TR_Mask' then trmap= (*self.refmaps)->get(i,/map)
  if n_elements(trmap) eq 0 then self->ComputeTRmask,trmap=trmap
endelse
if widget_valid(wTRDraw) then begin
   widget_control,wTRdraw,get_value=trwin,get_uvalue=xysize
   wset,trwin
   tvscl,congrid(trmap.data,xysize[0],xysize[1])
endif
if widget_valid(wTRTypeDisplay) then widget_control, wTRTypeDisplay, set_value=tag_exist(trmap,'tr_type')?trmap.tr_type:'Custom Unknown'
if widget_valid(wTRThresholdDisplay) then widget_control, wTRThresholdDisplay, $
  set_value=tag_exist(trmap,'tr_threshold')?strcompress(string(trmap.tr_threshold),/rem)+(tag_exist(trmap,'tr_threshold_unit')?' '+trmap.tr_threshold_unit:'')$
  :'Custom Unknown'
end

pro gxModel::ReplaceTRMask,event
  base=widget_info(event.id,/parent)
  wSettings=widget_info(base,find_by_uname='TR_MaskSettings')
  wMenu=widget_info(base,find_by_uname='GXMODEL:TRMaskMenu')
  widget_control,wMenu,get_uvalue=xsize
  if widget_valid(wSettings) then widget_control,wSettings,/destroy
  if event.index eq 0 then return
  wSettings=widget_base(base,/column,unam='TR_MaskSettings')
  wTRpreview=widget_draw(wSettings,scr_xsize=xsize,scr_ysize=xsize,uname='GXMODEL:TRpreview',uvalue=[1,1]*xsize)
 case event.index of 
  1: begin
         default_threshold=0
         threshold=cw_objfield(wSettings,xtextsize=20,xlabelsize=16,value=default_threshold,min=0,unit=' gauss',label='abs(Bz)>',increment=10,/dynamic,uname='GXMODEL:TR_BZ_MASK_THRESHOLD')
         button_base=widget_base(wSettings,/row)
         uname='GXMODEL:TR_BZ_MASK_OK'
         self.ComputeTRmask,type='Bz',threshold=default_threshold,/test   
        end
  2: begin
          default_threshold=0
          threshold=cw_objfield(wSettings,xtextsize=20,xlabelsize=16,value=default_threshold,min=0,unit=' deg',label='asin(abs(Bz)/B)>',increment=1,/dynamic,uname='GXMODEL:TR_THETA_MASK_THRESHOLD')
          button_base=widget_base(wSettings,/row)
          uname='GXMODEL:TR_THETA_MASK_OK
          self.ComputeTRmask,type='Bz/B',threshold=default_threshold,/test
      end      
  else:
 endcase
 button_base=widget_base(wSettings,/row)
 wOK=widget_button(button_base,value='OK',font=!defaults.font,uname=uname,uvalue=threshold)
 wCancel=widget_button(button_base,value='Cancel',font=!defaults.font,uname='GXMODEL:TR_MASK_CANCEL')
end

pro gxModel::SetBaseMap,select,volume=volume,newmap=newmap,refmaps=refmaps,time=time,xc=xc,yc=yc,dx=dx,dy=dy
  default,select,2
  default,refmaps,obj_new()
  map=self->GetByName('Reference Map')
  if ~obj_isa(map,'IDLgrImage')then begin
   if keyword_set(volume) then begin
    if obj_isa(volume,'gxVolume') then  void=volume->GetB(Bx=Bx,By=By,Bz=Bz) else void=self->GetB(Bx=Bx,By=By,Bz=Bz)
   endif else void=self->GetB(Bx=Bx,By=By,Bz=Bz)
   if obj_isa(refmaps[0],'MAP') then begin
    omaps=refmaps
    if refmaps[0]->get(/id) ne 'Bx' then omaps=[refmaps[0]->clone(),refmaps[0]->clone(),refmaps[0]->clone(),omaps]
    map=omaps[0]->get(/map)
    map.data=bx[*,*,0]
    omaps[0]->set,0,map=map
    omaps[0]->set,id='Bx'
    map.data=by[*,*,0]
    omaps[1]->set,0,map=map
    omaps[1]->set,id='By'
    map.data=bz[*,*,0]
    omaps[2]->set,0,map=map
    omaps[2]->set,id='Bz'
   endif else begin
     maps=[make_map(bx[*,*,0],id='Bx',time=time,xc=xc,yc=yc,dx=dx,dy=dy),$
           make_map(by[*,*,0],id='By',time=time,xc=xc,yc=yc,dx=dx,dy=dy),$
           make_map(bz[*,*,0],id='Bz',time=time,xc=xc,yc=yc,dx=dx,dy=dy)]
     omaps=objarr(3)
     for i=0,2 do begin
      omaps[i]=obj_new('map')
      omaps[i]->set,map=maps[i]
      omaps[i]->set,id=maps[i].id
     end  
   endelse             
   ptr_free,self.refmaps
   self.refmaps=ptr_new(omaps)
   map=obj_new('IDLgrImage',bytscl(omaps[2]->get(/data)),TRANSFORM_MODE=1,DEPTH_TEST_DISABLE=2,name='Reference Map')
   map->SetProperty,XCOORD_CONV=self.XCOORD_CONV,YCOORD_CONV=self.YCOORD_CONV
   self->add,map
  end
  if is_class(newmap,'map') then begin
     omaps=*self.refmaps
     ref_map=omaps[0]->get(/map)
     newmap=newmap->drotate(ref_map=ref_map)
     ptr_free,self.refmaps
     omaps=[omaps,newmap]
     self.refmaps=ptr_new(omaps)
     select=(size(omaps))[3]-1
  end
  if is_struct(newmap) then begin
    if valid_map(newmap) then begin
       omaps=*self.refmaps
       omap=obj_new('map')
       omap->setmap,0,newmap
       newmap=omap->drotate(ref_map=omaps[0]->get(/map))
       obj_destroy,omap
       ptr_free,self.refmaps
       omaps=[omaps,newmap]
       self.refmaps=ptr_new(omaps)
       select=(size(omaps))[3]-1
    end
  end
end

pro gxmodel::DisplayMap,select
 (self->scanbox())->DisplayMap,select
end

function gxmodel::GetFovMap
  return,(self->scanbox())->GetFovMap()
end

pro gxmodel::RemoveMap,select
refmaps=obj_new('map')
k=0
for i=0, (*self.refmaps)->get(/count)-1 do begin
  if select ne i then begin
    refmaps->set,k,map=(*self.refmaps)->get(i,/map)
    k+=1
  endif
endfor
obj_destroy,*self.refmaps
ptr_free,self.refmaps
self.refmaps=ptr_new(refmaps)
end

pro gxModel::Get,key,data
 self.Volume->GetVertexAttributeData,key,data
end

pro gxModel::UpdateVolume,select,_extra=_extra;data=data,force=force,_extra=_extra
 if keyword_set(force) then self->RequestVolumeUpdate
 self.Volume->Update,select,_extra=_extra;,data=data,force=force,_extra=_extra
end

pro gxModel::RequestVolumeUpdate,_extra=_extra
  self.Volume->RequestVolumeUpdate,_extra=_extra
end


function gxModel::Size,volume=volume,chromo_layers=chromo_layers, corona_base=corona_base
 sz=self.size
 chromo_layers=self->GetVertexData('chromo_layers')
 if ~isa(chromo_layers,/number) then begin
  if isa(self->corona(),'gxcorona') then (self->corona())->GetProperty,chromo_h=chromo_layers else chromo_layers=0
 endif else corona_base=self->GetVertexData('corona_base')
 if ~isa(corona_base,/number) then corona_base=chromo_layers
 if keyword_set(volume) and corona_base gt 0 then sz[3]=sz[3]-corona_base+chromo_layers
 return,sz
end

function gxModel::IsCombo,bsize=bsize,csize=csize,_ref_extra=extra
  bsize=self->Size(_extra=extra)
  csize=self->Size(/volume)
  return,~array_equal(bsize[1:3],csize[1:3])
end

function gxModel::GetVertexData,var
  return,self.volume->GetVertexData(var)
end

pro gxModel::SetVertexData,key,var
  self.volume->SetVertexAttributeData,key,var
end


function gxModel::Box2Volume,data,idx,box2vol=box2vol,bsize=bsize,csize=csize,corona_only=corona_only,recompute=recompute
  isa_combo=(self->IsCombo(bsize=bsize,csize=csize))
  box_idx=lindgen(bsize[1],bsize[2],bsize[3])
  if isa_combo then begin
    box2chromo=self->GetVertexData('box2chromo')
    chromo_layers=self->GetVertexData('chromo_layers')
    corona_base=self->GetVertexData('corona_base')
    dim=size(box2chromo,/dim)
    compute=1
    if (n_elements(dim) eq 3) and ~keyword_set(recompute) then begin
      if array_equal(dim,[csize[1:2],chromo_layers]) then begin
        compute=0
      endif
    endif
    if compute then begin
      h=self->R(/volume)-1
      box2chromo=lonarr(csize[1],csize[2],chromo_layers)    
      c_idx=floor(h/self.zcoord_conv[1])
      for i=0,csize[1]-1 do begin
        for j=0, csize[2]-1 do begin
          for k=0,chromo_layers-1 do begin
           box2chromo[i,j,k]=box_idx[i,j,c_idx[i,j,k]]
          endfor
        endfor
      endfor
      self.volume->SetVertexAttributeData,'box2chromo',box2chromo
    endif 
    box2vol=lonarr(csize[1],csize[2],csize[3])
    box2vol[*,*,0:chromo_layers-1]=box2chromo
    box2vol[*,*,chromo_layers:*]=box_idx[*,*,corona_base:*]
  endif else box2vol=box_idx
  
  if (size(data,/tname) eq 'STRING') then data=self.GetVertexData(data)
  if isa(data,/number,/array) then begin
   data_size=size(data)
   if array_equal(data_size[1:3],csize[1:3]) then return,data
  endif
  if ~isa(idx) then idx=self.GetVertexData('idx')
  if (size(idx,/tname) eq 'STRING') then idx=self.GetVertexData(idx)
  if isa(idx,/number) and isa(data,/number) and (n_elements(idx) eq n_elements(data)) then begin
    vol=replicate(data[0]*0,bsize[1],bsize[2],bsize[3])
    vol[idx]=data
    vol=vol[box2vol]
    if keyword_set(corona_only) then begin
       chromo_idx=self->GetVertexData('chromo_idx')
       if isa(chromo_idx,/number,/array) then vol[chromo_idx]=0
    endif
    return,vol
  endif else return,!null
end

function gxModel::GetScale
 return,{XCOORD_CONV:self.XCOORD_CONV,YCOORD_CONV:self.YCOORD_CONV,ZCOORD_CONV:self.ZCOORD_CONV}
end

function gxModel::GetVolume
 return,self.volume
end

function gxModel::GetVoxelId
  self.volume->GetVertexAttributeData,'voxel_id',voxel_id
  return,voxel_id
end

function gxModel::GetTubeId
  return,(ishft(self->GetVertexData('voxel_id'),-8) and 255b)
end

pro gxModel::SetRoi
 if self.FullROI then goto, skip
 all=self->Get(/all,isa=['gxFluxtube','gxBline'],count=count)
 tubes=0
 for i=0,count -1 do begin
  if obj_isa(all[i],'gxFluxtube') then $
    all[i]->GetBounds,xrange=xr, yrange=yr,zrange=zr $
    else all[i]->GetProperty,xrange=xr,yrange=yr,zrange=zr
    if i eq 0 then begin
      xrange=xr
      yrange=yr
      zrange=zr
    endif else begin
    xrange[0]=min([xrange,xr],max=max) & xrange[1]=max
    yrange[0]=min([yrange,yr],max=max) & yrange[1]=max
    zrange[0]=min([zrange,zr],max=max) & zrange[1]=max
    endelse
 end
 if count ne 0 then begin
  xrange[0]=floor(xrange[0])>0 & xrange[1]=ceil(xrange[1])
  yrange[0]=floor(yrange[0])>0 & yrange[1]=ceil(yrange[1])
  zrange[0]=floor(zrange[0])>0 & zrange[1]=ceil(zrange[1])
 end 
 skip:
  p=self->GetROIBox(xrange=xrange,yrange=yrange,zrange=zrange)
  if ~obj_isa(self.roi,'IDLgrRoi') then begin
   self.roi=obj_new('IDLgrRoi',p,name='Box',$
   color=self.IsROI?[255,0,0]:[0,255,255],linestyle=0,xcoord_conv=self.xcoord_conv,ycoord_conv=self.ycoord_conv,zcoord_conv=self.zcoord_conv)
   self->Add,self.roi
  endif else self.roi->ReplaceData,p
end


function gxModel::GetROIBox,xrange=xrange,yrange=yrange,zrange=zrange
  if n_elements(xrange) eq 0 or n_elements(yrange) eq 0 or n_elements(zrange) eq 0 then $
    self.volume->GetProperty,xrange=xrange,yrange=yrange,zrange=zrange
  return,gx_getboxedges(xrange=xrange,yrange=yrange,zrange=zrange)
end

function gxModel::GetGrid
 scanbox=self->GetROI(/scanbox)
 if ~obj_valid(scanbox) then return,ptr_new() else return, scanbox->GetGrid()
end

pro gxModel::SetGrid,grid
  scanbox=self->GetROI(/scanbox)
  if obj_valid(scanbox) then scanbox->SetGrid,grid
end

function gxModel::MakeScanboxGrid,parms
 if ~isa(parms) then return,ptr_new()
 grid=self->GetGrid()
 if ~ptr_valid(grid) then begin
  scanbox=self->getroi(/scanbox)
  newgrid=scanbox->ComputeGrid()
  grid=self->GetGrid()
 endif
 if ~ptr_valid(grid) then return,ptr_new()
 dim=size(*grid,/dim)
 nx=dim[1]
 ny=dim[2]
 nz=dim[3]
 empty_slice=fltarr(nx,nz)
 grid=ptr_new({grid:grid,B:dblarr(nx*nz,3),$
   parms:dblarr(nx,nz,n_elements(parms)),slice:empty_slice})
 return,grid
end

function gxModel::concatenate_aparms
  catch, error_status
  if error_status ne 0 then begin
    message, !error_state.msg,/cont
    return,!null
  endif
 fluxtubes=self->get(/all,is='gxfluxtube',count=count)
 for i=0,count-1 do begin
  case n_elements(aparms) of
    0:begin 
       aparms=fluxtubes[i]->interpolate_fparms()
       if n_elements(aparms) ne 0 then begin
         f_arr=transpose(aparms.f_arr,[2,0,1])
         e_arr=aparms.e_arr 
         mu_arr=aparms.mu_arr
         spine_arr=(size(f_arr))[1]
         time_arr=aparms.time
       end  
      end 
    else: begin
            aparms=fluxtubes[i]->interpolate_fparms()
            if n_elements(aparms) ne 0 then begin
              if ~array_equal(aparms.e_arr,e_arr) then begin
                message,'Energy arrays must be identical for all fluxtubes in a model!'
                return,!null
              endif
              if ~array_equal(aparms.mu_arr,mu_arr) then begin
                message,'Energy arrays must be identical for all fluxtubes in a model!'
                return,!null
              endif
              f_arr=[f_arr,transpose(aparms.f_arr,[2,0,1])]
              spine_arr=[spine_arr,(size(f_arr))[1]]
              time_arr=[time_arr,aparms.time]
            end   
          end
  endcase 
 endfor
 f_arr=transpose(f_arr,[1,2,0])
 return,{f_arr:f_arr,e_arr:e_arr,mu_arr:mu_arr,spine_arr:spine_arr,time_arr:time_arr}
end

pro gxModel::Slice,parms,row,scanner=scanner
  if ~ptr_valid(scanner) then scanner=self->MakeScanboxGrid(parms)
  void=self->Box2Volume(box2vol=box2vol)
  grid=self->GetGrid()
  if ptr_valid(grid) then (*scanner).grid=grid
  assigned=lonarr(n_elements((*scanner).parms))
  if ~ptr_valid(grid) then goto, unassigned
  dim=size(*grid,/dim)
  nx=dim[1]
  ny=dim[2]
  nz=dim[3]
  sz=self->Size(/volume)
  dr=reform((*grid)[0,*,row,*])
  g=reform((*grid)[1:3,*,row,*])
  vol_ind=transpose(reform(g,3,nx*nz))+1e-6;added epilon correction to fix borderline numerical errors
  missing=0
  (self->GetVolume())->GetVertexAttributeData,'voxel_id',id 
  if n_elements(id) gt 0 then begin
    var=interpolate(id,fix(vol_ind[*,0]),fix(vol_ind[*,1]),fix(vol_ind[*,2]),missing=missing)
    idx=gx_name2idx(parms,'VoxelID')
    if (size(idx))[0] ne 0 then begin
      (*scanner).parms[*,*,idx]=var
      assigned[idx]=1
    end
    idx=gx_name2idx(parms,'TubeID')
    if (size(idx))[0] ne 0 then begin
      (*scanner).parms[*,*,idx]=(ishft(ulong(var),-8) and 255b)
      assigned[idx]=1
    end
  endif

  r=self->R(/volume)
  radius=interpolate(temporary(r),vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)

  ;ASSIGN dz
  idx=gx_name2idx(parms,'dR')
  if (size(idx))[0] ne 0 then begin
    (*scanner).parms[*,*,idx]=dr*gx_rsun()
    assigned[idx]=1
  end
  
  ;ASSIGN z
  idx=gx_name2idx(parms,'z')
  if (size(idx))[0] ne 0 then begin
    vol=(self->R(/volume)-1)*gx_rsun(unit='km')
    (*scanner).parms[*,*,idx]=interpolate(vol,fix(vol_ind[*,0]),fix(vol_ind[*,1]),fix(vol_ind[*,2]),missing=missing)
    assigned[idx]=1
  end

  corona=self->Corona()
  corona->GetProperty,n0=n0,T0=temp,dist_e=dist_e,kappa=kappa,emin=emin,emax=emax,chromo_n=chromo_n,chromo_T=chromo_T,chromo_h=chromo_h,ignore=ignore_corona

  tmp=(*scanner).slice

  (*scanner).slice=corona->GetDensity(radius)
  tmp[*]=radius

  if ~ignore_corona then begin
    ;corona Dist_E
    idx=gx_name2idx(parms,'Dist_E')
    if (size(idx))[0] ne 0 then begin
      (*scanner).parms[*,*,idx]=dist_e
      assigned[idx]=1
    end


    ;corona kappa
    idx=gx_name2idx(parms,'kappa')
    if (size(idx))[0] ne 0 then begin
      (*scanner).parms[*,*,idx]=kappa
      assigned[idx]=1
    end

    ;corona Emin
    idx=gx_name2idx(parms,'Emin')
    if (size(idx))[0] ne 0 then begin
      (*scanner).parms[*,*,idx]=emin
      assigned[idx]=1
    end

    ;corona Emax
    idx=gx_name2idx(parms,'Emax')
    if (size(idx))[0] ne 0 then begin
      (*scanner).parms[*,*,idx]=emax
      assigned[idx]=1
    end
  end


  sz=self->Size()
  vol=fltarr(sz[1],sz[2],sz[3])
  volume=self->GetVolume()
  
  ;n_0
  idx=gx_name2idx(parms,'n_0')
  if (size(idx))[0] ne 0 then begin
    vol=self->Box2Volume('n0')
    if isa(vol)then begin
      (*scanner).parms[*,*,idx]=interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
      assigned[idx]=1
    end
  end

  ;T_0
  idx=gx_name2idx(parms,'T_0')
  if (size(idx))[0] ne 0 then begin
    vol=self->Box2Volume('T0')
    (*scanner).parms[*,*,idx]=interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
    assigned[idx]=1
  end

  idx=gx_name2idx(parms,'bmed')
  if (size(idx))[0] ne 0 then begin
    vol=self->Box2Volume('bmed',/corona)
    if isa(vol)then begin
      (*scanner).parms[*,*,idx]=interpolate(vol,fix(vol_ind[*,0]),fix(vol_ind[*,1]),fix(vol_ind[*,2]),missing=missing)
      assigned[idx]=1
    end
  end


  idx=gx_name2idx(parms,'length')
  if (size(idx))[0] ne 0 then begin
    vol=self->Box2Volume('length',/corona)
    if isa(vol)then begin
      vol=gx_rsun()*vol/2
      (*scanner).parms[*,*,idx]=interpolate(vol,fix(vol_ind[*,0]),fix(vol_ind[*,1]),fix(vol_ind[*,2]),missing=missing)
      assigned[idx]=1
    end
  end

  idx=gx_name2idx(parms,'Q')
  if (size(idx))[0] ne 0 then begin
    vol=self->Box2Volume('Q',/corona)
    if isa(vol)then begin
      (*scanner).parms[*,*,idx]=interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
      assigned[idx]=1
    end
  end

  idx=gx_name2idx(parms,'UseDEM')
  if (size(idx))[0] ne 0 then begin
    (*scanner).parms[*,*,idx]=(volume->getflags()).NTDEM
    assigned[idx]=1
  end

  idx=gx_name2idx(parms,'AddTR')
  if (size(idx))[0] ne 0 then begin
    (*scanner).parms[*,*,idx]=(volume->getflags()).TRADD
    assigned[idx]=1
  end

  idx=gx_name2idx(parms,'ApplyTRfactor')
  if (size(idx))[0] ne 0 then begin
    (*scanner).parms[*,*,idx]=(volume->getflags()).TRFACTOR
    assigned[idx]=1
  end

  idx=gx_name2idx(parms,'DEMAVG')
  if (size(idx))[0] ne 0 then begin
    self->GetProperty,wparent=wparent
    if widget_valid(wparent)  then begin
     id=widget_info(wparent,find_by_uname='GXMODEL:DEMAVG')
     if widget_valid(id) then begin
      widget_control,id,get_value=demavg
     endif
    endif 
    default,demavg,0
    (*scanner).parms[*,*,idx]=demavg
    assigned[idx]=1
  end

  idx=gx_name2idx(parms,'hc_angle')
  if (size(idx))[0] ne 0 then begin
    self->GetProperty,ns=ns,ew=ew
    (*scanner).parms[*,*,idx]=asin(sqrt(total((hel2arcmin(ns,ew,radius=rsun,date=(*(self->Refmaps()))->Get(/time)))^2))/rsun)/!dtor
    assigned[idx]=1
  end


  chromo_idx=self->GetVertexData('chromo_idx')
  ; start for backward compatibility Dec 18 2014!!!!
  if n_elements(chromo_idx) eq 0 then chromo_idx=self->GetVertexData('idx')
  ; end for backward compatibility Dec 18 2014!!!!
  if isa(chromo_idx,/number,/array) then begin
    n_htot=self->GetVertexData('n_htot')
    if n_elements(n_htot) eq n_elements(chromo_idx) then begin
      var=temporary(n_htot)
      vol[*]=0
      vol[chromo_idx]=var
      var =interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
      idx=gx_name2idx(parms,'n_Htot')
      if (size(idx))[0] ne 0 then begin
        (*scanner).parms[*,*,idx]=var
        assigned[idx]=1
      end
    endif

    n_hi=self->GetVertexData('n_hi')
    if n_elements(n_hi) eq n_elements(chromo_idx)  then begin
      var=temporary(n_hi)
      vol[*]=0
      vol[chromo_idx]=var
      var =interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
      idx=gx_name2idx(parms,'n_HI')
      if (size(idx))[0] ne 0 then begin
        (*scanner).parms[*,*,idx]=var
        assigned[idx]=1
      end
    endif

    n_p=self->GetVertexData('n_p')
    if n_elements(n_p) eq n_elements(chromo_idx) then begin
      var=temporary(n_p)
      vol[*]=0
      vol[chromo_idx]=var
      var =interpolate(vol,vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
      idx=gx_name2idx(parms,'n_p')
      if (size(idx))[0] ne 0 then begin
        (*scanner).parms[*,*,idx]=var
        assigned[idx]=1
      end
    endif
  end


  ;LOOP OVER FLUXTUBES
  tubes=self->Get(/all,ISA='gxFluxtube',count=tcount)
  bsize=self->Size()
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
      (*scanner).slice=interpolate(vol[box2vol],vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
      slice_owned=where((*scanner).slice eq 1,comp=cowned,nowned)
      if nowned gt 0 then begin
        owned2d=array_indices((*scanner).slice,slice_owned)
        x=reform(owned2d[0,*])
        y=reform(owned2d[1,*])
      end

      tube_parms=['kappa','eps','Emin','Emax','E_break','delta1','delta2','Dist_E','Dist_Ang']
      for k=0,n_elements(tube_parms)-1 do begin
        idx=gx_name2idx(parms,tube_parms[k])
        if idx ge 0 then assigned[idx]=1
        if (size(idx))[0] ne 0 then begin
          if nowned gt 0 then begin
            result=execute('data='+tube_parms[k])
            if result eq 1 then begin
              (*scanner).parms[x,y,replicate(idx,n_elements(x))]=data
            end
          end
        end
      end
    end
  end
  ;______________________________________________________


  vertex_parms=['n_nth','THETA_C','THETA_B','dMu','a4','nr_nth','C_IDX','C_IDX']
  idx_parms=['n_b','THETA_C','THETA_B','dMu','a_4','SpineR','SpineS','HasArr']
  bsize=self->Size()
  vol=(tmp=dblarr(bsize[1],bsize[2],bsize[3]))
  for k=0,n_elements(idx_parms)-1 do begin
    idx=gx_name2idx(parms,idx_parms[k])
    if idx ge 0 then assigned[idx]=1
    if (size(idx))[0] ne 0 then begin
      ;LOOP OVER FLUXTUBES
      tubes=self->Get(/all,ISA='gxFluxtube',count=tcount)
      vol[*]=0
      offset=0
      for j=0,tcount-1 do begin
        tmp[*]=0
        tubes[j]->GetProperty,centerbase=base
        data=tubes[j]->GetVertexData(vertex_parms[k])
        if n_elements(data) gt 0 then begin
          owned=tubes[j]->GetVertexData('owned')
          n_idx=tubes[j]->GetVertexData('N_IDX')
          if idx_parms[k] eq 'SpineS' then begin
            data+=offset
            aparms=tubes[j]->interpolate_fparms()
            if n_elements(aparms) gt 0 then offset=n_elements(tubes[j]->GetVertexData('s'))
          endif
          if idx_parms[k] eq 'HasArr' then begin
            aparms=tubes[j]->interpolate_fparms()
            data[*]=(n_elements(aparms) gt 0 )
          endif
          if n_elements(owned) gt 0 then begin
            tmp[n_idx]=data
            vol[owned]=tmp[owned]
          end
        end
      end
      if (idx_parms[k] eq 'SpineS') or (idx_parms[k] eq 'HasArr') or $
          (idx_parms[k] eq 'SpineR') or (idx_parms[k] eq 'dummy_n_b')then begin
        (*scanner).parms[*,*,idx]+=interpolate(vol[box2vol],fix(vol_ind[*,0]),fix(vol_ind[*,1]),fix(vol_ind[*,2]),missing=missing)
      endif else (*scanner).parms[*,*,idx]+=interpolate(vol[box2vol],vol_ind[*,0],vol_ind[*,1],vol_ind[*,2],missing=missing)
    endif
  end



  vol=volume->GetBx(/volume)
  (*scanner).B[*,0]=Interpolate(vol, vol_ind[*, 0], vol_ind[*, 1], vol_ind[*, 2], missing=missing)
  vol=volume->GetBy(/volume)
  (*scanner).B[*,1]=Interpolate(vol, vol_ind[*, 0], vol_ind[*, 1], vol_ind[*, 2], missing=missing)
  vol=volume->GetBz(/volume)
  (*scanner).B[*,2]=Interpolate(vol, vol_ind[*, 0], vol_ind[*, 1], vol_ind[*, 2], missing=missing)


  btm=self->GetBTM()
  (*scanner).B=btm##(*scanner).B

  ;SCALE B
  volume->GetProperty,bscale=bscale
  if n_elements(bscale) ne 0 then begin
    (*scanner).B=Bscale*(*scanner).B
  end

  ;COMPUTE B
  B=sqrt(total((*scanner).B^2,2))

  idx=gx_name2idx(parms,'B')
  if (size(idx))[0] ne 0 then begin
    (*scanner).parms[*,*,idx]=B
    assigned[idx]=1
  end

  idx=gx_name2idx(parms,'Bx')
  if (size(idx))[0] ne 0 then begin
    (*scanner).parms[*,*,idx]=(*scanner).B[*,0]
    assigned[idx]=1
  end

  idx=gx_name2idx(parms,'By')
  if (size(idx))[0] ne 0 then begin
    (*scanner).parms[*,*,idx]=(*scanner).B[*,1]
    assigned[idx]=1
  end

  idx=gx_name2idx(parms,'Bz')
  if (size(idx))[0] ne 0 then begin
    (*scanner).parms[*,*,idx]=(*scanner).B[*,2]
    assigned[idx]=1
  end

  ;COMPUTE PHI
  idx=gx_name2idx(parms,'phi')
  if (size(idx))[0] ne 0 then begin
    if (size(idx))[0] ne 0 then begin
      (*scanner).parms[*,*,idx]=atan((*scanner).B[*,1],(*scanner).B[*,0])/!dtor;atan(By,Bx)/!dtor
      assigned[idx]=1
    end
  end

  ;COMPUTE THETA
  idx=gx_name2idx(parms,'theta')
  if (size(idx))[0] ne 0 then begin
    temp=acos((*scanner).B[*,2]/B);acos(Bz/B)
    good=where(finite(temp) eq 1,comp=comp,ncomp=ncomp)
    if ncomp gt 0 then  temp[comp]=0
    (*scanner).parms[*,*,idx]=temp/!dtor
    assigned[idx]=1
  end

  ;COMPUTE ETA
  idx=gx_name2idx(parms,'TRfactor')
  if (size(idx))[0] ne 0 then begin
    (*scanner).B=(invert(btm)##(*scanner).B)
    ;theta is the angle between B and Bz (between the field and normal to TR)
    costheta=abs((*scanner).B[*,2])/B
    ;ez is the box z-axis versor (normal to TR) in the observer coordinate system, where LOS is the z axis
    ;So, phi is the angle betwen the TR normal and LOS, so cosphi is the z component of the ez versor
    ez=btm[*,2]
    dr=self->GetFovPixSize(unit='cm')
    dz=dr[0]
    r=gx_rsun(unit='cm')
    mincosphi=sin(0.5*acos(1-dz/R))
    cosphi=(abs(ez[2])>mincosphi)
    tr_factor=(costheta/cosphi)
    good=where(finite(tr_factor) eq 1,comp=comp,ncomp=ncomp)
    if ncomp gt 0 then  tr_factor[comp]=0
    (*scanner).parms[*,*,idx]=tr_factor
    assigned[idx]=1
  end

  unassigned:
  ;ASSIGN UNASSIGNED
  for j=0,n_elements(parms)-1 do begin
    if ptr_valid(scanner) then begin
      if ~assigned[j] then begin
        (*scanner).parms[*,*,j]=(parms)[j].value
      endif
    end
  end

end


pro gxModel::CreateBline,xyz,any=any
 if ~ keyword_set(any) then xyz[2]=0
 bline=self->GetBLine(xyz,name='reference Bline')
 if obj_valid(bline) then self->Add,bline
end

function gxModel::BBOX,volume=volume
  dummy=self->GetB(Bx=Bx,By=By,Bz=Bz,volume=volume)
  self->GetProperty,dr=dr
  return,{Bx:temporary(Bx),by:temporary(By),bz:temporary(Bz),dr:dr}
end

function gxModel::GenerateSeeds, x0=x0,y0=y0,z0=z0,dx=dx,dy=dy,dz=dz,nx=nx,ny=ny,nz=nz
  default,x0,0
  default,y0,0
  default,z0,0
  default,dx,1
  default,dy,1
  default,dz,1
  default,nx,1
  default,ny,1
  default,nz,1
  maxx=(self.size())[1]
  maxy=(self.size())[2]
  maxz=(self.size())[3]
  nx = nx>1<maxx
  ny = ny>1<maxy
  nz = nz>1<maxz
  dx = dx>0<maxx
  dy = dy>0<maxy
  dz = dz>0<maxz
  x0 = x0>0<maxx
  y0 = y0>0<maxy
  z0 = z0>0<maxz
  seeds=!null
  desc = [ '1, BASE,, ROW', $
    string(x0,format="('0, FLOAT,', g0,', LABEL_LEFT=x0:, WIDTH=12, TAG=x0')"), $
    string(y0,format="('0, FLOAT,', g0,', LABEL_LEFT=y0:, WIDTH=12, TAG=y0')"), $
    string(z0,format="('2, FLOAT,', g0,', LABEL_LEFT=z0:, WIDTH=12, TAG=z0')"), $
    '1, BASE,, ROW', $
    string(dx,format="('0, FLOAT,', g0,', LABEL_LEFT=dx:, WIDTH=12, TAG=dx')"), $
    string(dy,format="('0, FLOAT,', g0,', LABEL_LEFT=dy:, WIDTH=12, TAG=dy')"), $
    string(dz,format="('2, FLOAT,', g0,', LABEL_LEFT=dz:, WIDTH=12, TAG=dz')"), $
    '1, BASE,, ROW', $
    string(nx,format="('0, FLOAT,', g0,', LABEL_LEFT=nx:, WIDTH=12, TAG=nx')"), $
    string(ny,format="('0, FLOAT,', g0,', LABEL_LEFT=ny:, WIDTH=12, TAG=ny')"), $
    string(nz,format="('2, FLOAT,', g0,', LABEL_LEFT=nz:, WIDTH=12, TAG=nz')"), $
    '1, BASE,, ROW', $
    '0, BUTTON, OK, QUIT,' $
    + 'TAG=OK', $
    '2, BUTTON, Cancel, QUIT,TAG=Quit']
  a = CW_FORM(desc, /COLUMN,Title='Magnetic Field Line Seeds')
  if a.ok then begin
    nx = a.nx>1<maxx
    ny = a.ny>1<maxy
    nz = a.nz>1<maxz
    dx = a.dx>0<maxx
    dy = a.dy>0<maxy
    dz = a.dz>0<maxz
    x0 = ((nx gt 1)?a.x0-nx*dx/2:a.x0)>0<maxx
    y0 = ((ny gt 1)?a.y0-ny*dy/2:a.y0)>0<maxy
    z0 = ((nz gt 1)?a.z0-nz*dz/2:a.z0)>0<maxz
    seeds = dblarr(3, nx*ny*nz)
    cnt = 0
    for ix = 0, nx-1 do begin
      for iy = 0, ny-1 do begin
        for iz = 0, nz-1 do begin
          seeds[*, cnt++] = [(x0+ix*dx)>0<maxx, (y0+iy*dy)>0<maxy, (z0+iz*dz)>0<maxz]
        endfor
      endfor
    endfor
  end
  return,seeds
end

function gxModel::ComputeBlines,inputSeeds,tr_height_km=tr_height_km,_extra=_extra
  if n_elements(inputSeeds)/3 lt 1 then return, obj_new()
  nSeeds=n_elements(inputSeeds)/3
  maxLength=self.steps*nSeeds>1000000L
  reduce_passed=0
  box=self->BBOX()
  default,tr_height_km,0
  chromo_level=tr_height_km

  dll_path=gx_findfile('WWNLFFFReconstruction.dll',folder='gxbox')

  nonStored = gx_box_calculate_lines(dll_path, box $
                           , status = status, physLength = physLength, avField = avField $
                           , startIdx = startIdx, endIdx = endIdx, seedIdx = seedIdx $
                           , inputSeeds = inputSeeds, maxLength = maxLength $
                           , totalLength = totalLength, nLines = nLines, nPassed = nPassed $
                           , coords = coords, linesPos = linesPos, linesLength = linesLength, linesIndex = linesIndex $
                           , codes = codes $
                           , version_info = version_info $
                           , reduce_passed = reduce_passed, n_processes = n_processes, chromo_level = chromo_level $
                           )
if n_elements(nlines) eq 0 then return, obj_new()
if nLines eq 0 then return, obj_new()
lines=objarr(nLines)
linesB= [ interpolate(box.bx,coords[0,*],coords[1,*],coords[2,*]) $
         , interpolate(box.by,coords[0,*],coords[1,*],coords[2,*]) $
         , interpolate(box.bz,coords[0,*],coords[1,*],coords[2,*]) $
         ]
 absB=sqrt(total(linesB^2,1))
 status=status[LinesIndex]
 for i=0,nLines-1 do begin
   lb=linesB[*,linesPos[i]:linesPos[i]+linesLength[i]-1]
   data=coords[0:2,linesPos[i]:linesPos[i]+linesLength[i]-1]
   ls=coords[3,linesPos[i]:linesPos[i]+linesLength[i]-1]*box.dr[0]
   imin=where(ls eq 0)
   top=data[*,imin]
   line=OBJ_NEW('gxBline',data[0,*],data[1,*],data[2,*],top=top,status=status[i],_extra=_extra,tr_height=tr_height)
   line->SetProperty,XCOORD_CONV=self.XCOORD_CONV,YCOORD_CONV=self.YCOORD_CONV,ZCOORD_CONV=self.ZCOORD_CONV
   line->SetVertexAttributeData,'B',lb
   line->SetVertexAttributeData,'s',ls
   lines[i]=line
 endfor
return,lines
end

pro gxModel::AddBLines,_extra=_extra
 inputSeeds=self->GenerateSeeds(_extra=_extra)
 nSeeds=n_elements(inputSeeds)/3 
 if nSeeds lt 1 then return
;      if self.lock then begin
        lines=self->ComputeBlines(inputSeeds)
        good=where(obj_valid(lines) eq 1,count)
        if count gt 0 then self->add,lines[good]
;      endif else begin
;        for i=0, nSeeds-1 do self->CreateBline,InputSeeds[*,i],/any
;      endelse
end  

pro gxModel::RemoveBlines
  lines=self->get(/all,isa='gxbline')
  if obj_valid(lines[0]) then begin
    for k=0,n_elements(lines)-1 do begin
        lines[k]->GetProperty,lock=lock
        if ~lock then obj_destroy,lines[k]
    endfor
  endif else message,'No field lines found in this model, nothing to delete!',/info
end

pro gxModel::ResetPosition, unlock=unlock,top=top
 self->Reset
 maps=*(self->Refmaps())
 time=maps->get(2,/time)
 pb0r=pb0r(time)
 b0=pb0r[1]
 rsun=pb0r[2]*60
 self->Translate,self.xrange[0],self.yrange[0],1
 if ~keyword_set(top) then begin
 if widget_valid(self.wparent) then begin
   wTopView=widget_info(self.wparent,find_by_uname='GXMODEL:TopView')
   if widget_valid(wTopView) then begin
    topview=widget_info(wTopView,/button_set)
   endif else topview=0
 endif else topview=0
 endif else topview=1
 if (~topview) or keyword_set(unlock) then begin
   self->GetProperty,gyro=gyro
   if n_elements(gyro) ne 1 then gyro=0
   if gyro ne 0 then begin
     self->Rotate,[0,0,1],gyro
   end
   
   self->Rotate,[1d,0,0],-self.NS
   self->Rotate,[0,1d,0],self.EW
   self->Rotate,[1d,0,0],b0
   
 endif else begin
   xy=60*hel2arcmin(self.NS,self.EW,date=time)/rsun
   self->Translate,xy[0],xy[1],0
 endelse
end


Pro gxModel::CreateFluxtube,centerline
 count=self.FluxTubeCount+1
 name=string(count,format="('Flux Tube',i2)")
 fluxtube=Obj_NEW('GXFLUXTUBE',centerline=centerline,name=name)
 if ~obj_valid(fluxtube) then message,'Flux tube creation failed!'
 self->Add,fluxtube
 self.FluxTubeCount+=1
 wComponentTab=widget_info(self.wparent,find_by_uname='GXMODEL:COMPONENTTAB')
 void=obj_new('gxWidget',Widget_Base(wComponentTab,title=name,uname=name),fluxtube,_extra=_extra)
 widget_control,wComponentTab,SET_TAB_CURRENT=widget_info(wComponentTab,/N_CHILDREN)-1
 self->SetRoi
 fluxtube->DisplayB2B0ratio
 fluxtube->Update_N_nth
 fluxtube->Update_N_th
 fluxtube->Update_Theta_c
 fluxtube->Update_Theta_b
 fluxtube->Update_dMu
 fluxtube->Update_a4
end

Function gxModel::FullROI
 return,self.FullROI
end

PRO gxModel::CLEANUP
 ptr_free,self.refmaps
 self->IDLgrModel::CLEANUP
END

function gxModel::refmaps
  return,self.refmaps
end

function gxModel::GetTime,seconds=seconds
 time=(*self.refmaps)->get(2,/time)
 if keyword_set(seconds) then time=anytim(time)
 return,time
end

function GXModel::UpdateEUVinfo,info
  volume=self->GetVolume()
  self->GetProperty,wparent=wparent
  if widget_valid(wparent) then begin
    wDEMavg=widget_info(wParent,find_by_uname='GXMODEL:DEMAVG')
    if widget_valid(wDEMavg) then widget_control,wDEMAvg,get_value=DEMavg else DEMavg=0
    gx_setparm,info, 'DEMavg',DEMavg
  endif
  gx_setparm,info, 'UseDEM',(volume->getflags()).NTDEM
  gx_setparm,info, 'AddTR',(volume->getflags()).TRADD
  gx_setparm,info, 'ApplyTRfactor',(volume->getflags()).TRFACTOR
  gx_setparm,info, 'AIA_response_date',self->GetTime(/sec)
  return,info
end  

PRO gxModel::GetProperty,NS=NS,EW=EW,ROI=ROI,FLUXTUBECOUNT=FLUXTUBECOUNT,$
 XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV,$
 XRANGE=XRANGE,YRANGE=YRANGE,ZRANGE=ZRANGE,ISROI=ISROI,FULLROI=FULLROI,wPARENT=wPARENT,$
 subgridpts=subgridpts,steps=steps,refmaps=refmaps,Bscale=Bscale,dr=dr,_ref_extra=extra,scanbox=scanbox,gyro=gyro,WinOS=WinOS
 NS=self.NS
 EW=self.EW
 ROI=self.ROI
 refmaps=self.refmaps
 FLUXTUBECOUNT=self.FLUXTUBECOUNT
 XCOORD_CONV=self.XCOORD_CONV
 YCOORD_CONV=self.YCOORD_CONV
 ZCOORD_CONV=self.ZCOORD_CONV
 dr=[XCOORD_CONV[1],YCOORD_CONV[1],ZCOORD_CONV[1]]
 XRANGE=self.XRANGE
 YRANGE=self.YRANGE
 ZRANGE=self.ZRANGE
 ISROI=self.ISROI
 FULLROI=self.FULLROI
 wParent=self.wParent
 subgridpts=self.subgridpts
 steps=self.steps
 WinOS=self.lock
 ;this arg_present check is needed to bypass an IDL bug when restoring the model
 if arg_present(bscale) then self.volume->GetProperty,bscale=bscale
 if arg_present(gyro) then self.volume->GetProperty,gyro=gyro
 self->IDLgrModel::GetProperty,_extra=extra
end

pro gxModel::SetIsROI,IsROI
  self.ISROI=ISROI
  self.Roi->SetProperty,color=isROI?[255,0,0]:[0,255,255]
  if widget_valid(self.wParent) then begin
    subdirectory=['resource', 'bitmaps']
    wIsROI=widget_info(self.wparent,find_by_uname='GXMODEL:IsROI')
    if widget_valid(wIsROI) then $
    WIDGET_CONTROL,wIsROI,/bitmap,set_value=IsROI?$
    gx_bitmap(filepath('image.bmp', subdirectory=subdirectory)):$
    gx_bitmap(filepath('eye_closed.bmp', subdirectory=subdirectory)),SET_BUTTON=IsROI 
  endif
end

PRO gxModel::SetProperty,NS=NS,EW=EW,ROI=ROI,FLUXTUBECOUNT=FLUXTUBECOUNT,$
 XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV,$
 XRANGE=XRANGE,YRANGE=YRANGE,ZRANGE=ZRANGE,ISROI=ISROI,FULLROI=FULLROI,wPARENT=wPARENT,$
 subgridpts=subgridpts,steps=steps,refmaps=refmaps,bscale=bscale,volume=volume,size=size,gyro=gyro,winOS=winOS,_extra=extra
 if exist(NS) then self.NS=NS
 if exist(EW) then self.EW=EW
 if exist(ROI) then self.ROI=ROI
 if exist(FLUXTUBECOUNT) then self.FLUXTUBECOUNT=FLUXTUBECOUNT
 if exist(XCOORD_CONV) then self.XCOORD_CONV=XCOORD_CONV
 if exist(YCOORD_CONV) then self.YCOORD_CONV=YCOORD_CONV
 if exist(ZCOORD_CONV) then self.ZCOORD_CONV=ZCOORD_CONV
 if exist(XRANGE) then self.XRANGE=XRANGE
 if exist(YRANGE) then self.YRANGE=YRANGE
 if exist(ZRANGE) then self.ZRANGE=ZRANGE
 if exist(ISROI) then self->SetIsROI,IsROI
 if exist(FULLROI) then self.FULLROI=FULLROI
 if exist(wParent) then self.wParent=wParent
 if exist(subgridpts) then self.subgridpts=subgridpts
 if exist(steps) then self.steps=steps
 if exist(size) then self.size=size
 if exist(refmaps) then begin
  ptr_free,self.refmaps
  self.refmaps=ptr_new(refmaps)
 end
 if exist(bscale) then begin
  self.volume->SetProperty,bscale=bscale
 end
 if exist(gyro) then begin
   self.volume->SetProperty,gyro=gyro
 end
 if exist(WinOS) then begin
   self.lock=WinOS
 end
 if isa(volume,'gxvolume') then begin
  self->Remove,self.volume
  obj_destroy,self.volume
  self.volume=volume
  self->add,self.volume
 end
 self->IDLgrModel::SetProperty,_extra=extra
end

function gxModel::Grid,_extra=_extra
 sz=self->Size( _extra=_extra)
 return,float(transpose(array_indices(sz[1:3],lindgen(sz[1]*sz[2]*sz[3]),/dim)))
end

function gxModel::R,volume=volume,_extra=_extra
 dim=(self->size(volume=volume,_extra=_extra))[1:3]
 if keyword_set(volume) then (self.volume)->GetVertexAttributeData,'dz',dz
 if n_elements(dz) eq 0 then begin
   dz=replicate(self.zcoord_conv[1],dim)
 endif  
 dz=[[[dblarr(dim[0],dim[1])]],[[dz]]]
 h=(total(dz,3,/cum))[*,*,0:dim[2]-1]
 return,1+h
end

function gxModel::Corona
 return,self->GetByName('Corona')
end

function gxModel::GetRoi,scanbox=scanbox
 if keyword_set(scanbox) then return,self->GetByName('ScanBox')
 return,self.roi
end

function gxModel::Scanbox
 return,self->GetByName('ScanBox')
end

function gxModel::GetFovPixSize,_extra=_extra
  return,(self->GetByName('ScanBox'))->GetFovPixSize(_extra=_extra)
end

function gxModel::GetSTM,mscale=mscale,tm=tm
  ;this function returns the SCANBOX transform matrix from solar coordinates to model box coordinates
  dx=self.XCOORD_CONV[1]
  dy=self.YCOORD_CONV[1]
  dz=self.ZCOORD_CONV[1]
  Mscale = DBLARR(4,4)
  Mscale[0,0]=1d/dx
  Mscale[1,1]=1d/dy
  Mscale[2,2]=1d/dz
  Mscale[3,3]=1
  self->GetProperty,transform=tm
  return,MSCALE##invert(tm)
end  

function gxModel::GetDirections, TopViewCorrection=TopViewCorrection
  btm=self->GetBTM(TopViewCorrection=TopViewCorrection)
  vx=btm[*,0]
  vy=btm[*,1]
  vz=btm[*,2]
  return,{x:vx,y:vy,z:vz}
end

function gxModel::EstimateOptimalImageGridResolution
 btm=self->GetBTM()
 pbr=pb0r(self->GetTime())*60
 self.GetProperty,XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV
 return,[(btm##[xcoord_conv[1]*pbr[2],0,0])[0],(btm##[0,ycoord_conv[1]*pbr[2],0])[1]]
end

function gxModel::GetBTM, TopViewCorrection=TopViewCorrection
  ;This functions returns the transformation matrix needed to convert B from box coordinates to LOS coordinates
  
  ;This section seems to have no effect
  if ~keyword_set(TopViewCorrection) then begin
    wTopViewCorrection= widget_valid(self.wparent)?widget_info(self.wparent,find_by_uname='GXMODEL:TopViewCorrection'):0
    if widget_valid( wTopViewCorrection) then begin
      TopViewCorrection=widget_info(wTopViewCorrection,/button_set)
    endif else TopViewCorrection=0
  end

  if TopViewCorrection then begin
    ;Here we compute the true LOS orientaion disregarding TopView selection or not
    tm=self->getSTM(mscale=mscale)
    self->ResetPosition,/unlock
    self->GetProperty,transform=tm
    tm=MSCALE##invert(tm)
  endif
  ;**********************************
  
  
  self->getproperty,transform=ctm
  
  btm=ctm[0:2,0:2];invert(ctm[0:2,0:2])

  ;This section seems to have no effect
  if TopViewCorrection then begin
    ;Here we recover the TopView or rotated position
    self->ResetPosition
    self->GetProperty,transform=tm
    tm=MSCALE##invert(tm)
  endif
  ;**********************************

  return,btm
end


function gxModel::IsRoi
  return,self.IsRoi
end

function gxModel::GetName
 return,self.name
end

pro gxModel::SetName, name
  self->SetProperty,name=name
end

function gxModel::GetID
  self->GetProperty,id=id
  return,id
end

pro gxModel::SetID, id
  self->SetProperty,id=id
end

pro gxModel::upgrade_combo_model,verbose=verbose
  ;this routine is called automatical by gxModel::Upgrade
  ;for the purpose of converting ald format combo models (resolved chromosphere+corobal part)
  ;to the updated format that display a true chromosphere height/ corona height scale and
  ;supports proper handling of fluxtubes added to such models.
  ;15-May-2020
  IsCombo=self->IsCombo(bsize=bsize,csize=csize,chromo_layers=chromo_layers, corona_base=corona_base)
  if IsCombo then begin
    if keyword_set(verbose) then message,'This is a properly formated combo model, nothing to be upgraded!',/cont
    return
  endif
  if chromo_layers eq 0 then begin
    if keyword_set(verbose) then message,'This is a not a combo model, nothing to be upgraded!',/cont
    return
  endif else begin
    dz=self->GetVertexData('dz')
    if n_elements(dz) eq 0 then begin
      if keyword_set(verbose) then message,'This is a not a combo model, nothing to be upgraded!',/cont
      return
    endif
  endelse
  answ=dialog_message(['This combo model has an outdated format!', $
  'In order to take advantage of all current GX Simulator combo models functionalities, you must choose to convert it to the newst format.'+$
  'However, the conversion might result in slighly modified volume properties or in missing some previously defined properties, ' +$
  'which will be reset to the GX Simulator default values..',$
  'Do you want to upgrade this model, or to keep its original format? '],/question)
  if strupcase(answ) eq 'NO' then begin
     message,'Keeping the old combo model format!',/cont
     return
  endif
  message,'Upgrading this combo model the new combo model format!',/cont
  box=self->BBOX()
  dz=self->GetVertexData('dz')
  self.ZCOORD_CONV[1]=dz[0,0,bsize[3]-1]
  chromo_bcube=fltarr(bsize[1],bsize[2],chromo_layers,3)
  chromo_bcube[*,*,*,0]=box.Bx[*,*,0:chromo_layers-1]
  chromo_bcube[*,*,*,1]=box.By[*,*,0:chromo_layers-1]
  chromo_bcube[*,*,*,2]=box.Bz[*,*,0:chromo_layers-1]
  h=(self->R(/volume)-1)
  corona_base=floor(h[0,0,chromo_layers+1]/self.zcoord_conv[1])-1
  self.size[3]=self.size[3]-chromo_layers+corona_base
  bx=(by=(bz=fltarr(self.size[1:3])))
  bx[*,*,corona_base:*]=box.bx[*,*,chromo_layers:*]
  by[*,*,corona_base:*]=box.by[*,*,chromo_layers:*]
  bz[*,*,corona_base:*]=box.bz[*,*,chromo_layers:*]
  box_h=(Self->R()-1)[*,*,0:corona_base-1]
  for i=0,bsize[1]-1 do begin
    for j=0,bsize[2]-1 do begin
      for k=0,corona_base-1 do begin
        m=min(abs(h[i,j,*]-box_h[i,j,k]),kmin)
        bx[i,j,k]=box.bx[i,j,kmin]
        by[i,j,k]=box.by[i,j,kmin]
        bz[i,j,k]=box.bz[i,j,kmin]
      endfor
    endfor
  endfor
  old_volume=self.volume
  self->remove,old_volume
  data=bytarr(self.size[1:3])
  self.volume=obj_new('gxVolume',data,name='Volume',XCOORD_CONV=self.XCOORD_CONV,YCOORD_CONV=self.YCOORD_CONV,ZCOORD_CONV=self.ZCOORD_CONV,/interpolate,hints=2)
  self.volume->SetProperty,data0=data,XCOORD_CONV=self.XCOORD_CONV,YCOORD_CONV=self.YCOORD_CONV,ZCOORD_CONV=self.ZCOORD_CONV
  self.volume->SetVertexAttributeData,'Bx',bx
  self.volume->SetVertexAttributeData,'By',by
  self.volume->SetVertexAttributeData,'Bz',bz
  self.volume->SetVertexAttributeData,'chromo_bcube',chromo_bcube
  self.volume->SetVertexAttributeData,'corona_base',corona_base
  self.volume->SetVertexAttributeData,'chromo_layers',chromo_layers
  self.volume->SetVertexAttributeData,'dz',dz
  volume_tags=['chromo_idx','n_htot','n_hi','n_p','chromo_n','chromo_T','tr']
  for k=0,n_elements(volume_tags)-1 do begin
    data=old_volume->GetVertexData(volume_tags[k])
    if isa(data) then self.volume->SetVertexAttributeData,volume_tags[k],data
  endfor
  
  idx=old_volume->GetVertexData('idx')
  void=self->Box2Volume(box2vol=box2vol)
  chromo_idx=self->GetVertexData('chromo_idx')
  if isa(idx,/number,/array) then begin
   vol=dblarr(csize[1:3])
   box_vol=dblarr(bsize[1:3])
   box_vol[*]=0
   vol[*]=0
   vol[idx]=idx
   if isa(chromo_idx,/number,/array) then vol[chromo_idx]=0
   box_vol[box2vol]=vol
   box_idx=where(box_vol ne 0)
   self.volume->SetVertexAttributeData,'idx',box_idx
   volume_tags=['bmed','length', 'n', 'T','foot1', 'foot2']
     
     for k=0, n_elements(volume_tags)-1 do begin
       data=old_volume->GetVertexData(volume_tags[k])
       if isa(data) then begin
         box_vol[*]=0
         vol[*]=0
         vol[idx]=data
         if isa(chromo_idx,/number,/array) then vol[chromo_idx]=0
         box_vol[box2vol]=vol
         data=box_vol[box_idx]
         self.volume->SetVertexAttributeData,volume_tags[k],data
       end
     endfor
  endif
  
  oidx=old_volume->GetVertexData('oidx')
  if isa(oidx,/number,/array) then begin
   vol=dblarr(csize[1:3])
   box_vol=dblarr(bsize[1:3])
   box_vol[*]=0
   vol[*]=0
   vol[oidx]=oidx
   if isa(chromo_idx,/number,/array) then vol[chromo_idx]=0
   box_vol[box2vol]=vol
   box_oidx=where(box_vol ne 0)
   self.volume->SetVertexAttributeData,'oidx',box_oidx
   volume_tags=['obmed','olength', 'ofoot1', 'ofoot2']
     for k=0, n_elements(volume_tags)-1 do begin
       data=old_volume->GetVertexData(volume_tags[k])
       if isa(data) then begin
         box_vol[*]=0
         vol[*]=0
         vol[oidx]=data
         if isa(chromo_idx,/number,/array) then vol[chromo_idx]=0
         box_vol[box2vol]=vol
         data=box_vol[box_oidx]
         self.volume->SetVertexAttributeData,volume_tags[k],data
       end
     endfor
  endif
  
  obj_destroy,old_volume
  self->add,self.volume
  self->UpdateRoi,/replace
end

pro gxModel__define
self={gxModel,inherits gxComponent,$
NS:0d,EW:0d,volume:obj_new(),ROI:obj_new(),refmaps:ptr_new(),$
Size:lonarr(4),$
FluxTubeCount:0l,XCOORD_CONV:[0d,0d],YCOORD_CONV:[0d,0d],ZCOORD_CONV:[0d,0d],$
xrange:[0d,0d],yrange:[0d,0d],zrange:[0d,0d],IsROI:0l,FullROI:0l,wParent:0l,subgridpts:0l,steps:0l,lock:0l}
end