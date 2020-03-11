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

function gxModel::GetB,p,Bx=Bx,By=By,Bz=Bz
;p is expressed in self's box fractional index coordinates
if obj_valid(self.volume) then return,self.volume->GetB(p,Bx=Bx,By=By,Bz=Bz) else return,[0,0,0]*!Values.f_nan
end

function gxModel::GetBLine,p,dxdydz,subgridpts=subgridpts,tr_height=tr_height,no_tr=no_tr,full=full,_extra=_extra
id=self.volume->VoxelId()
tr_layer=where((id and 1) gt 0,count)
if count gt 0 then begin
  tr_idx=max((array_indices(id,tr_layer))[2,*])+1
endif else tr_idx=0
default,subgridpts,self.subgridpts
default,dxdydz,[1., 1., 1.]
void=self->GetB(Bx=Bx,By=By,Bz=Bz)
dx=self.YCOORD_CONV[1]
dy=self.YCOORD_CONV[1]
dz=self.ZCOORD_CONV[1]
sx=self.size[1]
sy=self.size[2]
sz=self.size[3]

if n_elements(p) eq 0 then p,randomu(seed,3)*[sx,sy,sz]-1
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
  n=n_elements(chromo_layers) gt 0?((z le max(tr))?chromo_layers:subgridpts):subgridpts $
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
  n=n_elements(chromo_layers) gt 0?((z le max(tr))?chromo_layers:subgridpts):subgridpts $
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
 return,line
END

function gxModel::GetBLineM,p,dxdydz,subgridpts=subgridpts,tr_height=tr_height,no_tr=no_tr,full=full,_extra=_extra
common blinem, blm_bx, blm_by, blm_bz, blm_id ;#####
tr_layer=where((blm_id and 1) gt 0,count)     ;#####
if count gt 0 then begin
  tr_idx=max((array_indices(blm_id,tr_layer))[2,*])+1 ;#####
endif else tr_idx=0
default,subgridpts,self.subgridpts
default,dxdydz,[1., 1., 1.]
dx=self.YCOORD_CONV[1]
dy=self.YCOORD_CONV[1]
dz=self.ZCOORD_CONV[1]
sx=self.size[1]
sy=self.size[2]
sz=self.size[3]

if n_elements(p) eq 0 then p,randomu(seed,3)*[sx,sy,sz]-1
x=p[0]
y=p[1]
z=p[2]
x0=x
y0=y
z0=z
top=[x,y,z]
s_top=0
lb=[interpolate(blm_bx,x,y,z),interpolate(blm_by,x,y,z),interpolate(blm_bz,x,y,z)] ;#####
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
  n=n_elements(chromo_layers) gt 0?((z le max(tr))?chromo_layers:subgridpts):subgridpts $
 else n=subgridpts
 iter+=1
 bb=[interpolate(blm_bx,x,y,z),interpolate(blm_by,x,y,z),interpolate(blm_bz,x,y,z)] ;#####
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
  n=n_elements(chromo_layers) gt 0?((z le max(tr))?chromo_layers:subgridpts):subgridpts $
 else n=subgridpts
 iter+=1
 bb=[interpolate(blm_bx,x,y,z),interpolate(blm_by,x,y,z),interpolate(blm_bz,x,y,z)] ;#####
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

function gxModel::ReplaceScanboxData,sdata,nx=nx,ny=ny,compute_grid=compute_grid
 return,(self->scanbox())->ReplaceData(sdata,nx=nx,ny=ny,compute_grid=compute_grid)
end

pro gxModel::UpdateDef

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
        self.volume->GetVertexAttributeData,'Bx',Bx
        self.volume->GetVertexAttributeData,'By',By
        self.volume->GetVertexAttributeData,'Bz',Bz
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
  if n_elements(id) ne self.size[1]*self.size[2]*self.size[3] then begin
    ;Here we define a default volume with no chromosphere and no TR
    id=ulonarr(self.size[1],self.size[2],self.size[3])+4
    self.volume->SetVertexAttributeData,'voxel_id',id
    self.volume->UpdateVoxelId,/force
    self.volume->GetVertexAttributeData,'voxel_id',id
    ;done with default
  endif else self.volume->SetVertexAttributeData,'voxel_id',ulong(id)
  
  
  if min(id) eq 4 then self.volume->UpdateVoxelId,/force
  
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

pro gxModel::UpdateVolume,select,data=data,force=force,_extra=_extra
 if keyword_set(force) then self->RequestVolumeUpdate
 self.Volume->Update,select,data=data,force=force,_extra=_extra
end

pro gxModel::RequestVolumeUpdate,_extra=_extra
  self.Volume->RequestVolumeUpdate,_extra=_extra
end


function gxModel::Size
 return,self.size
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
  if n_elements(xrange) eq 0 or n_elements(yrange) eq 0 or n_elements(yrange) eq 0 then $
    self.volume->GetProperty,xrange=xrange,yrange=yrange,zrange=zrange
  p=dblarr(3,8)
  for i=0,7 do p[*,i] = [xrange[(i AND 1)], yrange[((i/2) AND 1)], zrange[((i/4) AND 1)]]
  index=[0,1,3,1,5,7,5,4,6,4,0,2,3,7,6,2]
  p=p[*,index]
  return,p
end

function gxModel::GetGrid
 scanbox=self->GetROI(/scanbox)
 if ~obj_valid(scanbox) then return,ptr_new() else return, scanbox->GetGrid()
end

pro gxModel::SetGrid,grid
  scanbox=self->GetROI(/scanbox)
  if obj_valid(scanbox) then scanbox->SetGrid,grid
end

pro gxModel::CreateBline,xyz,any=any
 if ~ keyword_set(any) then xyz[2]=0
 bline=self->GetBLine(xyz,name='reference Bline')
 if obj_valid(bline) then self->Add,bline
end

pro gxModel::AddBLines
  desc = [ '1, BASE,, ROW', $
    '0, FLOAT, 0, LABEL_LEFT=xstep:, WIDTH=6, TAG=x', $
    '0, FLOAT, 0, LABEL_LEFT=ystep:, WIDTH=6, TAG=y', $
    '2, FLOAT, 0, LABEL_LEFT=zlayer:, WIDTH=6, TAG=z', $
    '1, BASE,, ROW', $
    '0, BUTTON, OK, QUIT,' $
    + 'TAG=OK', $
    '2, BUTTON, Cancel, QUIT']
  a = CW_FORM(desc, /COLUMN,Title='x,y step at layer z')
  if a.ok then begin
    void=self->GetB(Bx=Bx)
    sz=size(bx)
    if (a.x lt sz[1]) and (a.y lt sz[2]) and (a.z lt sz[3]) and (a.x ge 1) and (a.y ge 1) and (a.z ge 0) then begin
      if sz[1] MOD a.x ne 0 then nx=sz[1]/a.x else nx=sz[1]/a.x-1 ; define x,y blines number inside volume
      if sz[2] MOD a.y ne 0 then ny=sz[2]/a.y else ny=sz[2]/a.y-1 ;
      for i = 1, nx do begin
        for j = 1, ny do begin
          self->CreateBline,[a.x*i,a.y*j,a.z],/any
        endfor
      endfor
    endif else answ=dialog_message('Coordinates provided are outside the datacube range',/error)
  end
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

pro gxModel::ResetPosition, unlock=unlock,n=n
 self->Reset
 maps=*(self->Refmaps())
 time=maps->get(2,/time)
 pb0r=pb0r(time)
 b0=pb0r[1]
 rsun=pb0r[2]*60
 self->Translate,self.xrange[0],self.yrange[0],1
 if widget_valid(self.wparent) then begin
   wTopView=widget_info(self.wparent,find_by_uname='GXMODEL:TopView')
   if widget_valid(wTopView) then begin
    lock=widget_info(wTopView,/button_set)
   endif else lock=0
 endif else lock=0
 if (~lock) or keyword_set(unlock) then begin
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

function gxModel::GetTime
 return,(*self.refmaps)->get(2,/time)
end

PRO gxModel::GetProperty,NS=NS,EW=EW,ROI=ROI,FLUXTUBECOUNT=FLUXTUBECOUNT,$
 XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV,$
 XRANGE=XRANGE,YRANGE=YRANGE,ZRANGE=ZRANGE,ISROI=ISROI,FULLROI=FULLROI,wPARENT=wPARENT,$
 subgridpts=subgridpts,steps=steps,refmaps=refmaps,Bscale=Bscale,dr=dr,_ref_extra=extra,scanbox=scanbox,gyro=gyro
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
 subgridpts=subgridpts,steps=steps,refmaps=refmaps,bscale=bscale,volume=volume,size=size,gyro=gyro,_extra=extra
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
 if isa(volume,'gxvolume') then begin
  self->Remove,self.volume
  obj_destroy,self.volume
  self.volume=volume
  self->add,self.volume
 end
 self->IDLgrModel::SetProperty,_extra=extra
end

function gxModel::Grid
 sz=self->Size()
 return,float(transpose(array_indices(sz[1:3],lindgen(sz[1]*sz[2]*sz[3]),/dim)))
end

function gxModel::R
; (self.volume)->GetVertexAttributeData,'dz',dz
; if n_elements(dz) eq 0 then begin
 grid=self->Grid()
 grid[*,0]=(grid[*,0]+0.5)*self.xcoord_conv[1]+self.xrange[0]
 grid[*,1]=(grid[*,1]+0.5)*self.ycoord_conv[1]+self.yrange[0]
 grid[*,2]=(grid[*,2])*self.zcoord_conv[1]+self.zrange[0]+1
 ;return,sqrt(total(grid^2,2));grid[*,2];
 return,grid[*,2];
; endif else return,total(dz,3,/double)+dz[*,*,0]/2+1
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
  
  ;This section seems to have no effect in this version
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
;  self->getproperty,transform=ctm
;  vx=gx_transform([1,0,0],ctm)-gx_transform([0,0,0],ctm)
;  vy=gx_transform([0,1,0],ctm)-gx_transform([0,0,0],ctm)
;  vz=gx_transform([0,0,1],ctm)-gx_transform([0,0,0],ctm)
;  vx/=norm(vx)
;  vy/=norm(vy)
;  vz/=norm(vz)
  self->getproperty,transform=ctm
  btm=invert(ctm[0:2,0:2])
  vx=btm[*,0]
  vy=btm[*,1]
  vz=btm[*,2]

  ;This section seems to have no effect in this version
  if TopViewCorrection then begin
    ;Here we recover the TopView or rotated position
    self->ResetPosition
    self->GetProperty,transform=tm
    tm=MSCALE##invert(tm)
  endif
  ;**********************************

  
  return,{x:vx,y:vy,z:vz}
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
  
  btm=invert(ctm[0:2,0:2])

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
self->GetProperty,id=name
 return,name
end
pro gxModel__define
self={gxModel,inherits gxComponent,$
NS:0d,EW:0d,volume:obj_new(),ROI:obj_new(),refmaps:ptr_new(),$
Size:lonarr(4),$
FluxTubeCount:0l,XCOORD_CONV:[0d,0d],YCOORD_CONV:[0d,0d],ZCOORD_CONV:[0d,0d],$
xrange:[0d,0d],yrange:[0d,0d],zrange:[0d,0d],IsROI:0l,FullROI:0l,wParent:0l,subgridpts:0l,steps:0l,lock:0l}
end