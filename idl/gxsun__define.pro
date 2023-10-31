function gxSUN::INIT,UT,grid_spacing=grid_spacing,_extra=_extra
    default,time,''
    default,grid_spacing,10
    self.r=1
    self.time=time  
    num_points=100
    theta = FINDGEN(num_points) * 2*!DPI/ (num_points - 1)
    radius=(self.r+0.005)
    y = dblarr(num_points)
    z = radius*COS(theta)
    x = radius*SIN(theta)
    thelimb=OBJ_NEW('IDLgrPolyline',x,y,z,color=[0,0,255],thick=2,name='Solar Limb seen from Earth')
    grid = replicate(self.r,361, 181)
    mesh_obj, 4, verts, polys, grid, /closed,p1=0,p2=0
    sphere = obj_new('IDLgrPolygon',data=verts, polygons=polys, color=[255,255,0], style=2,name='Solar Surface')
    grid = replicate(self.r,360/grid_spacing+1, 180/grid_spacing+1)
    grid+=0.005
    mesh_obj, 4, verts, polys, grid, /closed,p1=0,p2=0
    thegrid = obj_new('IDLgrPolygon',data=verts, polygons=polys, style=1, thick=1,name="Observer's Solar Grid")
    grid = obj_new('IDLgrModel',name='Solar Grid')
    grid->Add, sphere
    grid->Add, thegrid
    grid->Rotate,[1,0,0],90
    limb = obj_new('IDLgrModel',name='Solar Limb')
    limb->Add,thelimb
    limb->Rotate,[1,0,0],90
    init=self->IDLgrModel::INIT(SELECT_TARGET=0,_extra=_extra,name='Sun')
    self->Add,grid
    self->Add,limb

 print,'gxSUN Object Initiliazed'
 return,1
end

pro gxSun::SetTime,time
  default,time,''
  self.time=time
  self->Reset
end

function gxSun::GetTime,time
  return,self.time
end

pro gxSun::Reset,b0=b0,l0=l0
 self->IDLgrModel::Reset
 scanbox=self->Get(isa='gxScanbox')
 if obj_valid(scanbox) then begin
  l0=scanbox->getLos(/l0)
  b0=scanbox->getLos(/b0)
  moi=scanbox->getMOI()
  if obj_isa(moi,'gxmodel') then begin
    moi->GetProperty,gyro=gyro
  endif
 endif else begin
  default,l0,0d
  default,b0,0d
 endelse
 limb =self->GetByName('Solar Limb')
 limb->Reset
 limb->Rotate,[1,0,0],90
 if l0 ne 0 then begin
  limb->Rotate,[1,0,0],-(pb0r(self.time))[1]
  limb->Rotate,[0,1,0],-l0
 endif
 grid =self->GetByName('Solar Grid')
 grid->Reset
 grid->Rotate,[1,0,0],90
 if b0 ne 0 then begin
   grid->Rotate,[1,0,0],b0
 endif
 if keyword_set(gyro) then begin
   grid->Rotate,[0,0,1],gyro
 endif
end

pro gxSun__define
self={gxSun,inherits IDLgrModel,r:1d,time:''}
end