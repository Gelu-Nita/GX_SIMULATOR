function gxSUN::INIT,UT,grid_spacing=grid_spacing,_extra=_extra
 default,time,''
 default,grid_spacing,10
 self.r=1
 self.time=time
 lon=grid_spacing*findgen(360./grid_spacing+1)*!dtor & nlat=n_elements(lon)
 lat=grid_spacing*findgen(180./grid_spacing+1)*!dtor & nlon=n_elements(lat)
 x=findgen(nlat,nlon)
 y=findgen(nlat,nlon)
 z=findgen(nlat,nlon)
 for i=0, nlon-1 do begin 
   z[*,i]=self.r*sin(lat[i])*cos(lon)
   x[*,i]=self.r*sin(lat[i])*sin(lon)
   y[*,i]=self.r*cos(lat[i])
 end   
    grid = replicate(self.r,361, 181)
    mesh_obj, 4, verts, polys, grid, /closed,p1=0,p2=0
    sphere = obj_new('IDLgrPolygon',data=verts, polygons=polys, color=[255,255,0], style=2);,diffuse=[255,0,0],emission=[255,0,0])
    grid = replicate(self.r,360/grid_spacing+1, 180/grid_spacing+1)
    grid+=0.005
    mesh_obj, 4, verts, polys, grid, /closed,p1=0,p2=0
    thegrid = obj_new('IDLgrPolygon',data=verts, polygons=polys, style=1, thick=1)
    omodel = obj_new('IDLgrModel',name='Solar Grid')
    omodel->Add, sphere
    omodel->Add, thegrid
    omodel->Rotate,[1,0,0],90
    init=self->IDLgrModel::INIT(SELECT_TARGET=0,_extra=_extra,name='Sun')
    self->Add,omodel

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

pro gxSun::Reset
 self->IDLgrModel::Reset
 grid =self.GetByName('Solar Grid')
 grid->Reset
 grid->Rotate,[1,0,0],90
 scanbox=self->Get(isa='gxScanbox')
 grid->Rotate,[1d,0,0],scanbox->getLos(/b0)
end

pro gxSun__define
self={gxSun,inherits IDLgrModel,r:1d,time:''}
end