pro gx_RenderIrregular, dx, dy, dz, xx, yy, zz, $
  Nvoxels, VoxList, ds, $
  x_ind, y_ind, z_ind, $
  entry_point, exit_point, $
  voxel_id=voxel_id, oneD=oneD
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

  ;------------------ input
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

  VoxOn=keyword_set(voxel_id) ? 1 : 0
  OnedOn=keyword_set(oneD) ? 1 : 0

  plh=0

  ;------------------ output
  N=1000

  Nvoxels=0L
  VoxList=lonarr(N)
  ds=dblarr(N)
  x_ind=dblarr(N)
  y_ind=dblarr(N)
  z_ind=dblarr(N)
  entry_point=dblarr(3)
  exit_point=dblarr(3)

  ;------------------ main code
  lib=gx_libpath('grid')
  res=call_external(lib, 'RENDER', $
    Nx, Ny, Nz, dx, dy, dz, $
    x1, x2, y1, y2, z1, z2, $
    VoxOn, OnedOn, VoxOn ? voxel_id : plh, $
    Nvoxels, VoxList, ds, $
    x_ind, y_ind, z_ind, entry_point, exit_point)

  if res then print, 'Number of voxels along the line-of-sight exceeds the allowed limit (1000).' $
  else if Nvoxels gt 0 then begin
    VoxList=VoxList[0 : Nvoxels-1]
    ds=ds[0 : Nvoxels-1]
    x_ind=x_ind[0 : Nvoxels-1]
    y_ind=y_ind[0 : Nvoxels-1]
    z_ind=z_ind[0 : Nvoxels-1]
  endif
end