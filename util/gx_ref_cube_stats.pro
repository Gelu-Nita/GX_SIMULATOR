;+
; :Description:
;    Time-mean and unbiased per-pixel sample SDEV of a [nx,ny,M] cube.
;    NaNs (e.g. after remap/crop) are omitted per pixel: mean uses the finite
;    count, sample σ uses that count minus one where at least two samples exist.
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
  cnt = total(finite(c), 3)
  s1 = total(c, 3, /nan)
  s2 = total(c^2, 3, /nan)
  mean_data = make_array(sz[1], sz[2], /double, value=!values.d_nan)
  sdev_data = mean_data
  ok = where(cnt ge 1, nok)
  if nok gt 0 then mean_data[ok] = s1[ok] / cnt[ok]
  ok2 = where(cnt ge 2, nk2)
  if nk2 gt 0 then $
    sdev_data[ok2] = sqrt(((s2[ok2] - s1[ok2]^2 / cnt[ok2]) / (cnt[ok2] - 1d)) > 0)
end
