FUNCTION gxBline:: INIT,X,Y,Z, top=top, status = status, physLength = physLength, avField = avField, $
                               startIdx = startIdx, endIdx = endIdx, seedIdx = seedIdx,$
                               connectivity=connectivity,boxsize=boxsize,_extra=_extra,tr_height=tr_height
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
  
  ;Oct-2-2024 GN: Added useful properties as returned by the WWNLFFFReconstruction.dll
  if keyword_set(status) then self.status=status
  if keyword_set(physLength) then self.physLength=physLength
  if keyword_set(avField) then self.avField=avField
  if keyword_set(startIdx) then self.startIdx=startIdx
  if keyword_set(endIdx) then self.endIdx=endIdx
  if keyword_set(seedIdx) then self.seedIdx=seedIdx
  if keyword_set(boxsize) then self.boxsize=boxsize
  if keyword_set(connectivity) then self.connectivity=connectivity
  return,success
END

PRO gxBline::SetProperty,lock=lock,center=center,top=top,_extra=extra
 if exist(lock) then self.lock=keyword_set(lock)
 if exist(center) then self.center=keyword_set(center)
 if exist(top) then self.top=top
 self->IDLgrPolyline::SetProperty,thick=self.lock?2:1,color=self.center?[255,0,0]:self.open?[255,255,0]:[0,255,0]
 self->IDLgrPolyline::SetProperty,_extra=extra
END

PRO gxBline::GetProperty,lock=lock,open=open,center=center,top=top,status = status, physLength = physLength, avField = avField, $
 startIdx = startIdx, endIdx = endIdx, seedIdx = seedIdx,connectivity=connectivity,boxsize=boxsize,_ref_extra=extra
 lock=self.lock
 open=self.open
 center=self.center
 top=self.top
 status=self.status
 physLength=self.physLength
 avField=self.avField
 startIdx=self.startIdx
 endIdx=self.endIdx
 seedtIdx=self.seedIdx
 connectivity=self.connectivity
 boxsize=self.boxsize
 self->IDLgrPolyline::GetProperty,_extra=extra
END

FUNCTION gxBline::GetB
 self->GetVertexAttributeData,'B',B
 return,b
END

FUNCTION gxBline::GetS
  self->GetVertexAttributeData,'s',s
  return,s
END

FUNCTION gxBline::GetBmed,above_tr=above_tr,tr_height=tr_height
 if self.avField eq 0 then begin 
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
 self.avField=mean(sqrt(total(B^2,1)))
 end
 return,self.avField
END

FUNCTION gxBline::GetLength,open=open,above_tr=above_tr,tr_height=tr_height
 if self.physLength eq 0 then begin 
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
   self.physLength=max(s,min=min)-min
 end
 return,self.physLength
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

function gxBline::GetModel
 self->GetProperty,parent=model
 if ~obj_valid(model) then return,model
 if obj_isa(model,'gxmodel') then return, model; this is a line of the model
 model->GetProperty, model=model; this is a line of a fluxtube
 return, model
end

function gxBline::GetInfo
 if self.startIDX eq 0 and self.endIDX eq 0 then begin
  model=self->GetModel()
  if obj_isa(model,'gxmodel') then begin
  dim=size((*self.data),/dim)
  self.boxsize=model->Size(/dim)
  self.startIDX=gx_grid2idx(long((*self.data)[*,0]),self.boxsize)
  self.endIDX=gx_grid2idx(long((*self.data)[*,dim[1]-1]),self.boxsize)
  end
 endif
 if self.connectivity[0] eq 0 and self.connectivity[1] eq 0 then begin
   model=self->GetModel()
   self.boxsize=model->Size(/dim)
   refmaps=model->RefMaps()
   if ptr_valid(refmaps) then begin
     refmaps=(*refmaps)[0]
     ref_count=refmaps->Get(/count)
     for i=0,ref_count-1 do begin
       if strupcase(refmaps->Get(i,/id)) eq strupcase('BASE CHROMO_MASK') or $
         strupcase(refmaps->Get(i,/id)) eq strupcase('CEA-CHROMO MASK') then begin
         mask=refmaps->Get(i,/data)
         goto,mask_exit
       endif
     endfor
   end
   mask_exit:
   startidx=array_indices(model->Size(/dim),/dim,self.startidx)
   endidx=array_indices(model->Size(/dim),/dim,self.endidx)
   if self.open then begin
    if startidx[2] lt 1 then self.connectivity[0]=mask[startidx[0],startidx[1]]
    if endidx[2] lt 1 then self.connectivity[1]=mask[endidx[0],endidx[1]]
   endif else self.connectivity=[mask[startidx[0],startidx[1]],mask[endidx[0],endidx[1]]]
 endif
 return,{length:self->GetLength(),bmed:self->GetBmed(),$
         startIDX:array_indices(self.boxsize,/dim,self.startidx),$
         endIDX:array_indices(self.boxsize,/dim,self.endidx),$
         s:reform(self.getS()),b:self.getB(),connectivity:self.connectivity,boxsize:self.boxsize}
end

pro gxBline::ShowInfo
 info=self->GetInfo()
 items=[]
 case self.connectivity[0] of
   0:c1='OPEN'
   1:c1='IN'
   2:c1='NW'
   3:c1='ENW'
   4:c1='PL'
   5:c1='FAC'
   6:c1='PEN'
   7:c1='UMB'
   else:c1='OPEN'
 endcase
 case self.connectivity[1] of
   0:c2='OPEN'
   1:c2='IN'
   2:c2='NW'
   3:c2='ENW'
   4:c2='PL'
   5:c2='FAC'
   6:c2='PEN'
   7:c2='UMB'
   else:c2='OPEN'
 endcase
 items=[items,string(c1,info.s[0]/info.length,c2,info.s[n_elements(info.s)-1]/info.length,$
 format="(a0,' (s1=',f0.2,'L) - ',a0,' (s2=',f0.2,'L)')")]
 
 items=[items,string(info.length*gx_rsun(unit='cm'),format="('L= ',g0,' cm')")]
 items=[items,string(info.bmed,format="('Bmed= ',g0,' G')")]
 b=sqrt(total(info.b^2,1))
 bstart=b[0]
 bend=b[n_elements(b)-1]
 btop=min(b,s_top)

items=[items,strcompress(string(arr2str(info.startidx,/compress),bstart,c1,format="('B1[',a0,']= ',g0,' G (',a0,')')"))]
if ~self.open then items=[items,strcompress(string(arr2str(long(self.top),/compress),btop,format="('B0[',a0,']= ',g0,' G')"))]
items=[items,strcompress(string(arr2str(info.endidx,/compress),bend,c2,format="('B2[',a0,']= ',g0,' G (',a0,')')"))]

 
 
 answ=dialog_message(items,/info)
end

pro gxBline__define
 self={gxBline,inherits IDLgrPolyline,lock:0l,open:0l,center:0l,top:[0.0,0.0,0.0],status:0L, physLength:0d, avField:0d, $
                                      startIdx:0l, endIdx:0l, seedIdx:0L, connectivity:[0l,0l],boxsize:[0l,0l,0l]}
end