;+
; PURPOSE:
;  This procedure computes the curl of a 3d vector field sampled on a
;  regular grid. 
;
; INPUTS:
;  x: The x component of the vector field
;  y: The y component of the vector field
;  z: The z component of the vector field
;
; KEYWORD PARAMETERS:
;  order: Sets the precision of the lagrange interpolation when
;  computing partial derivatives. see pdiv.pro.
;  dx: Grid size in the x direction (default: 1)
;  dy: Grid size in the y direction (default: 1)
;  dz: Grid size in the z direction (default: 1)
;
; OUTPUTS:
;  cx: The x component of the curl
;  cy: The y component of the curl
;  cz: The z component of the curl
;
; MODIFICATION HISTORY:
;  2010-08-03: Written by Chris Beaumont
;  2012-10-16: dx, dy and dz keywords added by Alexey Kuznetsov
;-
pro curl, x, y, z, cx, cy, cz, order = order, dx = dx, dy = dy, dz = dz
  ;- check inputs
  np = n_params()
  if np lt 3 then begin
     print, 'calling sequence:'
     print, ' curl, x, y, z, cx, cy, cz, [order = order, dx = dx, dy = dy, dz= dz]'
     return
  endif
  nx = n_elements(x) & ny = n_elements(y) & nz = n_elements(z)
  if nx ne ny || nx ne nz then $
     message, 'x, y, and z must be the same size'
  if size(x, /n_dim) ne 3 || size(y, /n_dim) ne 3 || $
     size(z, /n_dim) ne 3 then $
        message, 'x, y, and z must be 3D arrays'

  if ~keyword_set(dx) then dx=1
  if ~keyword_set(dy) then dy=1
  if ~keyword_set(dz) then dz=1

  ;- compute the curl
  xy = pdiv(x, 2, order = order)/dy
  xz = pdiv(x, 3, order = order)/dz
  
  yx = pdiv(y, 1, order = order)/dx
  yz = pdiv(y, 3, order = order)/dz
  
  zx = pdiv(z, 1, order = order)/dx
  zy = pdiv(z, 2, order = order)/dy
  
  cx = zy - yz
  cy = xz - zx
  cz = yx - xy     
  return
end

pro test
  im = fltarr(10, 10, 10)
  indices, im, x, y, z

  ;-curl free field
  print, 'should be 0 0'
  curl, x, y, z, cx, cy, cz
  print, minmax(cx > cy > cz)

  ;- divergence free field.
  ;- cz = 1
  print, 'should be 0, 0 // 0, 0 // 1, 1'
  curl, -y, y, z, cx, cy, cz
  print, minmax(cx), minmax(cy), minmax(cz)
end
