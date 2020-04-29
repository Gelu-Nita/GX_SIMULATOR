FUNCTION gxBline:: INIT,X,Y,Z, top=top,status=status,_extra=_extra,tr_height=tr_height
  default,tr_height,gx_tr_height()
  success=self->IDLgrPolyline::Init(X,Y,Z,_extra=_extra)
  if success then begin
  sz=size(z)
  
  if ~keyword_set(status) then begin
  ;June 8 2014 GN: Redefined what an open field line is 
  self.open=~((floor(z[0]) le tr_height) and (floor(z[sz[1]-1]) le tr_height))
  endif else begin
  ;April 10 2020 GN: introduced the status optional input, as returned by the WWNLFFFReconstruction.dll
  self.open=((status and 4L) eq 0) 
  endelse
  
  self->SetProperty,color=self.open?[255,255,0]:[0,255,0]
   if n_elements(top) eq 3 then self.top=top
  end
  return,success
END

PRO gxBline::SetProperty,lock=lock,center=center,top=top,_extra=extra
 if exist(lock) then self.lock=keyword_set(lock)
 if exist(center) then self.center=keyword_set(center)
 if exist(top) then self.top=top
 self->IDLgrPolyline::SetProperty,thick=self.lock?2:1,color=self.center?[255,0,0]:self.open?[255,255,0]:[0,255,0]
 self->IDLgrPolyline::SetProperty,_extra=extra
END

PRO gxBline::GetProperty,lock=lock,open=open,center=center,top=top,_ref_extra=extra
 lock=self.lock
 open=self.open
 center=self.center
 top=self.top
 self->IDLgrPolyline::GetProperty,_extra=extra
END


FUNCTION gxBline::GetBmed,above_tr=above_tr,tr_height=tr_height
 default,tr_height,gx_tr_height()
 self->GetVertexAttributeData,'B',B
 if keyword_set(above_tr) then begin
   dz=self.ZCOORD_CONV[1]
   self->GetProperty,data=data
   z=data[2,*]*dz
   above=where(z ge tr_height,acount)
   if acount gt 0 then begin
    B=B[*,above]
   endif else B=B*0
 end
 RETURN,mean(sqrt(total(B^2,1)))
END

FUNCTION gxBline::GetLength,open=open,above_tr=above_tr,tr_height=tr_height
 default,tr_height,gx_tr_height()
 self->GetVertexAttributeData,'s',s
 open=self.open
 if keyword_set(above_tr) then begin
   dz=self.ZCOORD_CONV[1]
   self->GetProperty,data=data
   z=data[2,*]*dz
   above=where(z ge tr_height,acount)
   if acount gt 0 then begin
    s=s[above]
   endif else begin
    s=s*0
    open=1
   end 
 end
 RETURN,max(s,min=min)-min
END

FUNCTION gxBline::GetAlpha,local=local
 self.GetProperty,data=line
 if keyword_set(local) then begin
   self.parent->getproperty,subgridpts=npts
   dummy=self.parent->GetB(Bx=Bx,By=By,Bz=Bz)
   scale=self.parent->GetScale()
   dx=scale.YCOORD_CONV[1]
   dy=scale.YCOORD_CONV[1]
   dz=scale.ZCOORD_CONV[1]
   du=max([dx,dy,dz])
   sz=size(line)
   count=sz[2]
   n=7L
   indices,bytarr(n,n,n),xoffsets,yoffsets,zoffsets,center=[0,0,0]
   npts=double(npts)
   xoffsets/=npts
   yoffsets/=npts
   zoffsets/=npts
   alpha=fltarr(sz[2])
   x=dblarr(n,n,sz[2]*n)
   y=x
   z=x
  for i=0L,sz[2]-1 do begin
    x[*,*,i*n:(i+1)*n-1]=line[0,i]+xoffsets
    y[*,*,i*n:(i+1)*n-1]=line[1,i]+yoffsets
    z[*,*,i*n:(i+1)*n-1]=line[2,i]+zoffsets
  endfor
     bbx=reform(interpolate(bx,x,y,z),n,n,sz[2]*n)
     bby=reform(interpolate(by,x,y,z),n,n,sz[2]*n)
     bbz=reform(interpolate(bz,x,y,z),n,n,sz[2]*n)
     curl,bbx,bby,bbz,cx,cy,cz,order=3, dx=dx/du, dy=dy/du, dz=dz/du
     a=bbx*cx+bby*cy+bbz*cz
     a=npts*a/(bbx*bbx+bby*bby+bbz*bbz)
     alpha=reform(a[(n-1)/2,(n-1)/2,lindgen(sz[2])*n+3])/(du*gx_rsun())
 endif else begin  
   (self.parent->GetVolume())->GetVertexAttributeData,'global_alpha',alpha
   if n_elements(alpha) eq 0 then begin
     dummy=self.parent->GetB(Bx=Bx,By=By,Bz=Bz)
     scale=self.parent->GetScale()
     dx=scale.YCOORD_CONV[1]
     dy=scale.YCOORD_CONV[1]
     dz=scale.ZCOORD_CONV[1]
     du=max([dx,dy,dz])
     curl,bx,by,bz,cx,cy,cz,order=3, dx=dx/du, dy=dy/du, dz=dz/du
     alpha=(bx*cx+by*cy+bz*cz)/(bx*bx+by*by+bz*bz)/(du*gx_rsun())
    (self.parent->GetVolume())->SetVertexAttributeData,'global_alpha',alpha
   end
   alpha=reform(interpolate(alpha,line[0,*],line[1,*],line[2,*]))
 endelse 
 return,alpha
END

pro gxBline__define
 self={gxBline,inherits IDLgrPolyline,lock:0l,open:0l,center:0l,top:[0.0,0.0,0.0]}
end