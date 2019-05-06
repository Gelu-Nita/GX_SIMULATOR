function TriLinear, Arr, x, y, z, dx, dy, dz
;Performs trilinear interpolation on a regular rectangular
;uniform / non-uniform grid.

;Input parameters:
; Arr - a 3D array.
; dx, dy, dz - voxel sizes in x, y, z directions respectively;
;              dx, dy, dz must be either scalars or 1D arrays 
;              with the same size as the respective dimension of the Arr array.
; x, y, z - the coordinates of the point where the value is to be found;
;           the values in the Arr array are assumed to correspond to the 
;           centers of the voxels;
;           if x, y, z are arrays, they must have the same dimensions.
;Returns: the interpolated value.

;In a uniform case (dz is scalar), the voxel centers zc[i] are located at:
; zc[0]=0, zc[1]=dz, zc[2]=2*dz, ... zc[i]=i*dz
;In a non-uniform case (dz is vector), the voxel centers are located at:
; zc[0]=0, zc[1]=(dz[0]+dz[1])/2, zc[2]=zc[1]+(dz[1]+dz[2])/2, ... 
; zc[i]=zc[i-1]+(dz[i-1]+dz[i])/2

 s=size(Arr)
 
 if s[0] ne 3 then stop, 'The input array must be 3D!'
 
 Nx=s[1]
 Ny=s[2]
 Nz=s[3]
 
 if (Nx lt 2) || (Ny lt 2) || (Nz lt 2) then $
  stop, 'Array must have at least 2 nodes in each direction!'
  
 if min([dx, dy, dz]) le 0 then stop, 'dx, dy, dz must be positive!' 
  
 if n_elements(dx) eq 1 then begin
  i1=(fix(x/dx)>0)<(Nx-2)
  i2=i1+1
  delta_x=x/dx-i1
 endif else if n_elements(dx) eq Nx then begin
  xarr=dblarr(Nx)
  for i=1, Nx-1 do xarr[i]=xarr[i-1]+(dx[i-1]+dx[i])/2
  i1=(value_locate(xarr, x)>0)<(Nx-2)
  i2=i1+1
  delta_x=(x-xarr[i1])/(xarr[i2]-xarr[i1])   
 endif else stop, 'Incompatible dimensions (x)!'
 
 if n_elements(dy) eq 1 then begin
  j1=(fix(y/dy)>0)<(Ny-2)
  j2=j1+1
  delta_y=y/dy-j1
 endif else if n_elements(dy) eq Ny then begin
  yarr=dblarr(Ny)
  for j=1, Ny-1 do yarr[j]=yarr[j-1]+(dy[j-1]+dy[j])/2
  j1=(value_locate(yarr, y)>0)<(Ny-2)
  j2=j1+1
  delta_y=(y-yarr[j1])/(yarr[j2]-yarr[j1])   
 endif else stop, 'Incompatible dimensions (y)!'
 
 if n_elements(dz) eq 1 then begin
  k1=(fix(z/dz)>0)<(Nz-2)
  k2=k1+1
  delta_z=z/dz-k1
 endif else if n_elements(dz) eq Nz then begin
  zarr=dblarr(Nz)
  for k=1, Nz-1 do zarr[k]=zarr[k-1]+(dz[k-1]+dz[k])/2
  k1=(value_locate(zarr, z)>0)<(Nz-2)
  k2=k1+1
  delta_z=(z-zarr[k1])/(zarr[k2]-zarr[k1])   
 endif else stop, 'Incompatible dimensions (z)!'
 
 return, Arr[i1, j1, k1]*(1d0-delta_x)*(1d0-delta_y)*(1d0-delta_z)+$
         Arr[i2, j1, k1]*delta_x*(1d0-delta_y)*(1d0-delta_z)+$
         Arr[i1, j2, k1]*(1d0-delta_x)*delta_y*(1d0-delta_z)+$
         Arr[i1, j1, k2]*(1d0-delta_x)*(1d0-delta_y)*delta_z+$
         Arr[i2, j1, k2]*delta_x*(1d0-delta_y)*delta_z+$
         Arr[i1, j2, k2]*(1d0-delta_x)*delta_y*delta_z+$
         Arr[i2, j2, k1]*delta_x*delta_y*(1d0-delta_z)+$
         Arr[i2, j2, k2]*delta_x*delta_y*delta_z   
end