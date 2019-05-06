;+
; PURPOSE:
;  This procedure computes the divergence of a 3d vector field sampled on a
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
; OUTPUT:
;  D: The divergence
;
; MODIFICATION HISTORY:
;  2012-10-16: Written by Alexey Kuznetsov
;-
pro div, x, y, z, D, order = order, dx = dx, dy = dy, dz = dz
  ;- check inputs
  np = n_params()
  if np lt 3 then begin
     print, 'calling sequence:'
     print, ' div, x, y, z, D, [order = order, dx = dx, dy = dy, dz= dz]'
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

  ;- compute the divergence
  D = pdiv(x, 1, order = order)/dx + $
      pdiv(y, 2, order = order)/dy + $
      pdiv(z, 3, order = order)/dz

  return
end