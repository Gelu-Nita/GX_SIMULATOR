pro gx_RenderIrregularMulti, NLOS, dx, dy, dz, LOSarr, $
                             Nvoxels, VoxList, ds, $
                             x_ind, y_ind, z_ind, $
                             entry_point, exit_point, $
                             voxel_id=voxel_id, oneD=oneD
;Returns the voxels intersected by a number of straight lines of sight in an irregularly-spaced data cube.
;The data cube is assumed to be evenly spaced in X and Y directions (dx=const and dy=const),
;but has an irregular spacing in Z direction (dz may depend on both X, Y, and Z).
;The data cube left, front and bottom boundaries are assumed to be at (X=0, Y=0, Z=0). 
;Hence, the center of the first voxel [0, 0, 0] is at [dx/2, dy/2, dz[0, 0, 0]/2], etc.
;A line of sight is given by two endpoints: (x1, y1, z1)-(x2, y2, z2), with the ray direction 
;from the 1st to the 2nd point. The voxels are sorted accordingly.
; 
;Input parameters:
; NLOS - number of the lines of sight;
; dx - voxel size in X direction (scalar);
; dy - voxel size in Y direction (scalar);
; dz - voxel sizes in Z direction (3D array);
; LOSarr - 3D array (3, 2, NLOS) specifying the lines of sight by their endpoints:
;          j-th line of sight is assumed to be from LOSarr[*, 0, j] to LOSarr[*, 1, j].
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
; Nvoxels - 1D array (NLOS) specifying the numbers of voxels intersected by each line of sight 
;           (the number may be zero if the line passes beyond the cube);
; VoxList - 2D array (max(Nvoxels), NLOS) specifying 1D indices of the intersected voxels;
; ds - 2D array (max(Nvoxels), NLOS) specifying projected depths of the intersected voxels, i.e.
;      ds[i, j] is the length of the fragment of j-th line of sight falling within i-th voxel;
; x_ind, y_ind, z_ind - three 2D arrays (max(Nvoxels), NLOS) representing the 'fractional' indices of the
;      line-of-sight midpoints within each of the intersected voxels.
;      The integer part of an index is the integer index of the corresponding voxel 
;      and the fractional part is relative to the length, width, or height of that voxel.
; entry_point, exit_point - two 2D arrays (3, NLOS) specifying the points where each line of sight enters
;                           and exits the data cube.

;------------------ input
 NLOS=long(NLOS)

 s=size(dz)
 Nx=s[1]
 Ny=s[2]
 Nz=s[3]

 x1=reform(LOSarr[0, 0, *])
 x2=reform(LOSarr[0, 1, *])
 y1=reform(LOSarr[1, 0, *])
 y2=reform(LOSarr[1, 1, *])
 z1=reform(LOSarr[2, 0, *])
 z2=reform(LOSarr[2, 1, *])
 
 VoxOn=keyword_set(voxel_id) ? 1 : 0
 OnedOn=keyword_set(oneD) ? 1 : 0
 
 plh=0
 
;------------------ output
 N=1000

 Nvoxels=lonarr(NLOS)
 VoxList=lonarr(N, NLOS)
 ds=dblarr(N, NLOS)
 x_ind=dblarr(N, NLOS)
 y_ind=dblarr(N, NLOS)
 z_ind=dblarr(N, NLOS)
 entry_point=dblarr(3, NLOS)
 exit_point=dblarr(3, NLOS)
 
;------------------ main code
 lib=gx_libpath('Render') 
 res=call_external(lib, 'RENDER_MULTI', $
                   NLOS, Nx, Ny, Nz, dx, dy, dz, $
                   x1, x2, y1, y2, z1, z2, $
                   VoxOn, OnedOn, VoxOn ? voxel_id : plh, $
                   Nvoxels, VoxList, ds, $
                   x_ind, y_ind, z_ind, entry_point, exit_point)  

 if res then print, 'Number of voxels along a line-of-sight exceeds the allowed limit (1000).' $
 else begin
  Nmax=max(Nvoxels)
  if Nmax gt 0 then begin
   VoxList=VoxList[0 : Nmax-1, *]
   ds=ds[0 : Nmax-1, *]
   x_ind=x_ind[0 : Nmax-1, *]
   y_ind=y_ind[0 : Nmax-1, *]
   z_ind=z_ind[0 : Nmax-1, *]
  endif
 endelse             
end