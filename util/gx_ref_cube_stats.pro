;+
; :Description:
;    Time-mean and unbiased (M-1) per-pixel sample SDEV of a [nx,ny,M] cube.
;-
pro gx_ref_cube_stats, cube, mean_data, sdev_data, nframe=nframe
  compile_opt idl2
  sz = size(cube)
  if sz[0] lt 3 then begin
    mean_data = double(cube)
    sdev_data = mean_data * 0d
    nframe = 1L
    return
  endif
  nframe = long(sz[3])
  c = double(cube)
  s1 = total(c, 3, /nan)
  s2 = total(c^2, 3, /nan)
  mean_data = s1 / nframe
  if nframe lt 2 then sdev_data = mean_data * 0d $
  else sdev_data = sqrt(((s2 - s1^2 / nframe) / (nframe - 1d)) > 0)
end
