function chromo_model,chromo,pbox,refmaps
csize=size(chromo.nh)
dx=(*pbox).dr[0]
dy=(*pbox).dr[1]
dz=(*pbox).dr[2]
h=[[[dblarr((size(chromo.dh))[1],( size(chromo.dh))[2])]],[[total(chromo.dh[*,*,1:*],3,/cum,/double)]]]
idx=where(chromo.nne ne 0 and chromo.temp ne 0 )
idx_idz=array_indices(chromo.nne,idx)
z=h/696000./dz
c0size=size(z)
tr_cube=dblarr(c0size[1],c0size[2],c0size[3])
tr_cube[idx]=chromo.dh[idx]/696000/dz
izmax=max(idx_idz[2,*])+1
z=z[*,*,0:izmax]
z[*,*,izmax]=ceil(max(z))
csize=size(z)
deltaz=dz*(max(z)-min(z))/csize[3]
bcube=fltarr(csize[1],csize[2],csize[3],3)
tr=dblarr(csize[1],csize[2])
dh=dblarr(csize[1],csize[2],csize[3])
dh[*]=chromo.dh[*,*,0:izmax]/696000
total_dh=total(dh[*,*,0:izmax-1],3,/double)
hmax=(z[0,0,izmax])*dz
dh[*,*,izmax]=hmax-total_dh
tr=total(tr_cube,3,/double)
for i=0,csize[1]-1 do begin
  for j=0,csize[2]-1 do begin    
      bcube[i,j,*,0]=interpolate((*pbox).bcube[i,j,*,0],z[i,j,*],c=-0.5)
      bcube[i,j,*,1]=interpolate((*pbox).bcube[i,j,*,1],z[i,j,*],c=-0.5)
      bcube[i,j,*,2]=interpolate((*pbox).bcube[i,j,*,2],z[i,j,*],c=-0.5)
  end
end

t=float(idx)
t=chromo.temp[idx]
n=float(idx)
n=chromo.nne[idx]
nh=float(idx)
nh=chromo.nh[idx]
nhi=float(idx)
nhi=chromo.nhi[idx]
np=float(idx)
np=chromo.np[idx]


box={bcube:bcube,refmaps:ptr_new(*(*pbox).refmaps),dr:[dx,dy,deltaz],idx:idx,n:n,t:t,n_htot:nh,n_hi:nhi,n_p:np,dz:dh,tr:tr,chromo_layers:csize[3]}

return,box
end