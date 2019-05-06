function CheckCrossPixel, xl, xr, yb, yt, x1, x2, y1, y2
;A supplementary function; 
;checks whether a part of the specified straight line falls within the specified rectangle (2D).
;
;Input parameters:
; xl, xr - left and right boundaries of the rectangle
; yb, yt - bottom and top boundaries of the rectangle
; x1, x2, y1, y2 - specify the line endpoints: (x1, y1)-(x2, y2)
;
;Returns: 
; 1 if either the line intersects the rectangle or one (or both) of its endpoints lie within the rectangle,
; 0 otherwise

 if (x1 gt xl) && (x1 le xr) && (y1 gt yb) && (y1 le yt) then return, 1 ;1st point inside
 
 if (x2 gt xl) && (x2 le xr) && (y2 gt yb) && (y2 le yt) then return, 1 ;2nd point inside
 
 bx=x2-x1
 if bx eq 0 then if (x1 gt xl) && (x1 le xr) && ((y1<y2) le yb) && ((y1>y2) ge yt) $
  then return, 1 $ ;vertical crossing
  else return, 0
 
 by=y2-y1
 if by eq 0 then if (y1 gt yb) && (y1 le yt) && ((x1<x2) le xl) && ((x1>x2) ge xr) $
  then return, 1 $ ;horizontal crossing
  else return, 0
 
 yc=y1+(xl-x1)*by/bx
 if finite(yc) then if (yc ge (y1<y2)) && (yc le (y1>y2)) && (yc gt yb) && (yc le yt) $
  then return, 1 ;crosses the left boundary
 
 yc=y1+(xr-x1)*by/bx
 if finite(yc) then if (yc ge (y1<y2)) && (yc le (y1>y2)) && (yc gt yb) && (yc le yt) $
  then return, 1 ;crosses the right boundary
 
 xc=x1+(yb-y1)*bx/by
 if finite(xc) then if (xc ge (x1<x2)) && (xc le (x1>x2)) && (xc gt xl) && (xc le xr) $
  then return, 1 ;crosses the bottom boundary
 
 xc=x1+(yt-y1)*bx/by
 if finite(xc) then if (xc ge (x1<x2)) && (xc le (x1>x2)) && (xc gt xl) && (xc le xr) $
  then return, 1 ;crosses the top boundary
 
 return, 0
end 

pro gx_RenderIrregular_old, dx, dy, dz, xx, yy, zz, $
                        Nvoxels, VoxList, ds, $
                        x_ind, y_ind, z_ind, $
                        entry_point, exit_point, $
                        voxel_id=voxel_id, oneD=oneD, lib=lib
;Returns the voxels intersected by a straight line of sight in an irregularly-spaced data cube.
;The data cube is assumed to be evenly spaced in X and Y directions (dx=const and dy=const),
;but has an irregular spacing in Z direction (dz may depend on both X, Y, and Z).
;The data cube left, front and bottom boundaries are assumed to be at (X=0, Y=0, Z=0). Hence, the center of the
;first voxel [0, 0, 0] is at [dx/2, dy/2, dz[0, 0, 0]/2], etc.
;The line of sight is given by two endpoints: (x1, y1, z1)-(x2, y2, z2), with the ray direction 
;from the 1st to the 2nd point. The voxels are sorted accordingly.
; 
;Input parameters:
; dx - voxel size in X direction (scalar)
; dy - voxel size in Y direction (scalar)
; dz - voxel sizes in Z direction (3D array)
; xx=[x1, x2],
; yy=[y1, y2],
; zz=[z1, z2] - three 2-element arrays specifying the line of sight.
;
;Optional input parameters:
; voxel_id - 3D array (with the same size as dz) specifying the types of 
;            individual voxels. If present, then some x_ind, y_ind, z_ind
;            values (see below) are truncated to the nearest integer. This 
;            happens if:
;            a) the voxel belongs to the chromosphere (specified as
;            (voxel_id[x_ind, y_ind, z_ind] and 1) ne 0) or
;            b) the adjacent voxels have different types.
; /oneD - if set, then up to 3 adjacent voxels along the line of sight are
;         considered in the above algorithm;
;         if not set, then up to 3*3*3=27 adjacent voxels in three cartesian
;         directions are considered in the above algorithm.
;
;Output parameters:
; Nvoxels - number of the voxels intersected by the line of sight (may be zero if the line passes beyond the cube)
; VoxList - 1D indices of the intersected voxels (1D array with Nvoxels elements)
; ds - projected depths of the intersected voxels (1D array with Nvoxels elements), i.e.
;      ds[i] is the length of the fragment of the line of sight falling within the i-th voxel
; x_ind, y_ind, z_ind - three 1D arrays (with Nvoxels elements) representing the 'fractional' indices of the
;      line-of-sight midpoints within each of the intersected voxels.
;      The integer part of an index is the integer index of the corresponding voxel 
;      and the fractional part is relative to the length, width, or height of that voxel.
; entry_point, exit_point - 3-element arrays (x, y, z) specifying the points where the line of sight enters
;                           and exits the data cube 
;Modification history
;30Aug-2017 GN: added lib keyword to presrve lib path between subsequent calls if DLL is used on Windows

if !version.OS_Family eq 'Windows' then begin
  gx_RenderIrregular_dll, dx, dy, dz, xx, yy, zz, $
    Nvoxels, VoxList, ds, $
    x_ind, y_ind, z_ind, $
    entry_point, exit_point, $
    voxel_id=voxel_id, oneD=oneD, lib=lib
return  
endif

 s=size(dz)
 Nx=s[1]
 Ny=s[2]
 Nz=s[3]
 
 x1=xx[0]
 x2=xx[1]
 y1=yy[0]
 y2=yy[1]
 z1=zz[0]
 z2=zz[1]
 L=sqrt((x2-x1)^2+(y2-y1)^2+(z2-z1)^2)
 
 bx=x2-x1
 by=y2-y1
 bz=z2-z1 
 
 tcmin= 1d100
 tcmax=-1d100
 
 Nvoxels=0L
 VoxList=lonarr(64)
 ds=dblarr(64)
 x_ind=dblarr(64)
 y_ind=dblarr(64)
 z_ind=dblarr(64)
 tmidarr=dblarr(64)
 Nvox_max=64L
 
 imin=floor((x1<x2)/dx)>0
 imax=ceil((x1>x2)/dx)<(Nx-1)
 jmin=floor((y1<y2)/dy)>0
 jmax=ceil((y1>y2)/dy)<(Ny-1)
 
 if bx eq 0 then imin=(imin-1)>0
 if by eq 0 then jmin=(jmin-1)>0 
 
 vox_on=keyword_set(voxel_id) ? 1 : 0
 oneD_on=keyword_set(oneD) ? 1 : 0
 
 if (imax ge imin) && (jmax ge jmin) then begin
  iarr=intarr(64)
  jarr=intarr(64)
  Nxy_max=64
  Nxy=0
 
  for i=1L*imin, imax do begin
   xl=dx*i
   xr=dx*(i+1)
  
   for j=1L*jmin, jmax do begin
    yb=dy*j
    yt=dy*(j+1)
   
    if CheckCrossPixel(xl, xr, yb, yt, x1, x2, y1, y2) then begin
     iarr[Nxy]=i
     jarr[Nxy]=j
     Nxy+=1
    
     if Nxy ge Nxy_max then begin
      iarr=[iarr, intarr(64)]
      jarr=[jarr, intarr(64)]
      Nxy_max+=64
     endif
    endif
   endfor
  endfor 
 
  if Nxy gt 0 then begin
   for m=0, Nxy-1 do begin
    i=iarr[m]
    j=jarr[m]
    
    xl=dx*i
    xr=dx*(i+1)
    yb=dy*j
    yt=dy*(j+1)
    
    zbarr=dblarr(Nz+1)
    for k=1, Nz do zbarr[k]=zbarr[k-1]+dz[i, j, k-1]
    
    Ncrossings=intarr(Nz)
    tarr=dblarr(Nz, 2)
    
    if bx ne 0 then begin
     tc=(xl-x1)/bx
     yc=y1+tc*by
     zc=z1+tc*bz
     
     if (yc gt yb) && (yc le yt) && (zc ge zbarr[0]) && (zc le zbarr[Nz]) then begin
      r=value_locate(zbarr, zc)
      if (r ge 0) && (r lt Nz) then begin
       if Ncrossings[r] lt 2 then tarr[r, Ncrossings[r]]=tc
       Ncrossings[r]+=1
       tcmin=(tcmin<tc)
       tcmax=(tcmax>tc)       
      endif
     endif
     
     tc=(xr-x1)/bx
     yc=y1+tc*by
     zc=z1+tc*bz
     
     if (yc gt yb) && (yc le yt) && (zc ge zbarr[0]) && (zc le zbarr[Nz]) then begin
      r=value_locate(zbarr, zc)
      if (r ge 0) && (r lt Nz) then begin
       if Ncrossings[r] lt 2 then tarr[r, Ncrossings[r]]=tc
       Ncrossings[r]+=1
       tcmin=(tcmin<tc)
       tcmax=(tcmax>tc)
      endif
     endif 
    endif
    
    if by ne 0 then begin
     tc=(yb-y1)/by
     xc=x1+tc*bx
     zc=z1+tc*bz 
     
     if (xc gt xl) && (xc le xr) && (zc ge zbarr[0]) && (zc le zbarr[Nz]) then begin
      r=value_locate(zbarr, zc)
      if (r ge 0) && (r lt Nz) then begin
       if Ncrossings[r] lt 2 then tarr[r, Ncrossings[r]]=tc
       Ncrossings[r]+=1
       tcmin=(tcmin<tc)
       tcmax=(tcmax>tc)       
      endif
     endif
     
     tc=(yt-y1)/by
     xc=x1+tc*bx
     zc=z1+tc*bz 
     
     if (xc gt xl) && (xc le xr) && (zc ge zbarr[0]) && (zc le zbarr[Nz]) then begin
      r=value_locate(zbarr, zc)
      if (r ge 0) && (r lt Nz) then begin
       if Ncrossings[r] lt 2 then tarr[r, Ncrossings[r]]=tc
       Ncrossings[r]+=1
       tcmin=(tcmin<tc)
       tcmax=(tcmax>tc)       
      endif
     endif
    endif
    
    if bz ne 0 then for k=0, Nz do begin
     tc=(zbarr[k]-z1)/bz
     xc=x1+tc*bx
     yc=y1+tc*by 
     
     if (xc gt xl) && (xc le xr) && (yc gt yb) && (yc le yt) then $
      for r=(k-1)>0, k<(Nz-1) do begin
       if Ncrossings[r] lt 2 then tarr[r, Ncrossings[r]]=tc
       Ncrossings[r]+=1
       tcmin=(tcmin<tc)
       tcmax=(tcmax>tc)       
      endfor
    endfor
    
    for r=0L, Nz-1 do if Ncrossings[r] eq 2 then begin
     VoxList[Nvoxels]=r*Nx*Ny+j*Nx+i
     ds[Nvoxels]=abs(tarr[r, 0]-tarr[r, 1])*L
     tmidarr[Nvoxels]=(tarr[r, 0]+tarr[r, 1])/2
     
     xmid=x1+tmidarr[Nvoxels]*bx
     ymid=y1+tmidarr[Nvoxels]*by
     zmid=z1+tmidarr[Nvoxels]*bz
     x_ind[Nvoxels]=xmid/dx
     y_ind[Nvoxels]=ymid/dy
     z_ind[Nvoxels]=1d0*r+(zmid-zbarr[r])/(zbarr[r+1]-zbarr[r]) 
     
     if vox_on && (~oneD_on) then if voxel_id[i, j, r] and 1 then begin
      x_ind[Nvoxels]=i
      y_ind[Nvoxels]=j
      z_ind[Nvoxels]=r 
     endif else begin
      ue=where(voxel_id[(i-1)>0 : (i+1)<(Nx-1), $
                        (j-1)>0 : (j+1)<(Ny-1), $
                        (r-1)>0 : (r+1)<(Nz-1)] ne voxel_id[i, j, r], ke)
      if ke ne 0 then begin
       x_ind[Nvoxels]=i
       y_ind[Nvoxels]=j
       z_ind[Nvoxels]=r 
      endif            
     endelse  

     Nvoxels+=1
     if Nvoxels ge Nvox_max then begin
      VoxList=[VoxList, lonarr(64)]
      ds=[ds, dblarr(64)]
      tmidarr=[tmidarr, dblarr(64)]
      x_ind=[x_ind, dblarr(64)]
      y_ind=[y_ind, dblarr(64)]
      z_ind=[z_ind, dblarr(64)]
      Nvox_max+=64
     endif
    endif
   endfor
  endif
 endif
 
 if Nvoxels gt 0 then begin
  VoxList=VoxList[0 : Nvoxels-1]
  ds=ds[0 : Nvoxels-1]
  tmidarr=tmidarr[0 : Nvoxels-1]
  
  u=sort(tmidarr)
  VoxList=VoxList[u]
  ds=ds[u]
  x_ind=x_ind[u]
  y_ind=y_ind[u]
  z_ind=z_ind[u]
  
  if vox_on && oneD_on then for i=0, Nvoxels-1 do if voxel_id[VoxList[i]] and 1 then begin
   x_ind[i]=floor(x_ind[i])
   y_ind[i]=floor(y_ind[i])
   z_ind[i]=floor(z_ind[i])
  endif else begin
   ue=where(voxel_id[VoxList[(i-1)>0 : (i+1)<(Nvoxels-1)]] ne voxel_id[VoxList[i]], ke)
   if ke ne 0 then begin
    x_ind[i]=floor(x_ind[i])
    y_ind[i]=floor(y_ind[i])
    z_ind[i]=floor(z_ind[i])
   endif  
  endelse  
  
  entry_point=[x1+tcmin*bx, y1+tcmin*by, z1+tcmin*bz]
  exit_point= [x1+tcmax*bx, y1+tcmax*by, z1+tcmax*bz]
 endif 
end