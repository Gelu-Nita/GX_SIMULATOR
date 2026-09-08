;+
; :Description:
;    Method A integrated-flux sample stddev: s_F of F_k = scale * total(I_k)
;    under IMG_MASK on a remapped cube. Not SEM (no /sqrt(M)).
;-
function gx_ref_cube_sf, cube_r, img_mask, scale
  compile_opt idl2
  sz = size(cube_r)
  if sz[0] lt 3 then return, !values.d_nan
  nframe = long(sz[3])
  if nframe lt 2 then return, !values.d_nan
  if n_elements(scale) eq 0 then scale = 1d
  msk = n_elements(img_mask) eq n_elements(cube_r[*, *, 0]) ? byte(img_mask) : $
    byte(cube_r[*, *, 0] * 0) + 1b
  bad = where(~msk, nbad)
  Fk = dblarr(nframe)
  for t = 0L, nframe - 1 do begin
    sl = cube_r[*, *, t]
    if nbad gt 0 then sl[bad] = 0d
    Fk[t] = total(sl, /nan) * double(scale)
  endfor
  Fbar = total(Fk, /nan) / nframe
  return, sqrt(total((Fk - Fbar)^2, /nan) / (nframe - 1d))
end
