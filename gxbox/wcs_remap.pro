
;+
  ; :Description:
  ;    Remaps data from one World Coordinate System (WCS) to anothe
  ;
  ; :Params:
  ;    data_from - data to be remapped
  ;    wcs_from - WCS structure describing coordinate system of the given
  ;    wcs_to - WCS structure describing coordinate system where the data will be remapped to
  ;    
  ; :Keywords:
  ;    sample - set to use nearest neighbour interpolation instead of the cubic one
  ;
  ;
  ; :Author: Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
  ;-
function wcs_remap, data_from, wcs_from, wcs_to, sample = sample, ssaa = ssaa, missing = missing
  if keyword_set(ssaa) then return, wcs_remap_ssaa(data_from, wcs_from, wcs_to, missing = missing)
  cc_to = wcs_get_coord(wcs_to)
  wcs_convert_from_coord,wcs_to,cc_to,'HG', lon, lat, /carrington
  wcs_convert_to_coord,wcs_from,cc_from,'HG', lon, lat, /carrington
  pix = wcs_get_pixel(wcs_from, cc_from)
  if keyword_set(sample) then begin
    sz = size(data_from)
    limx = sz[1] - 1
    limy = sz[2] - 1
    return, reform(data_from[round(pix[0,*,*]>0<limx), round(pix[1,*,*]>0<limx)])
  endif
  return, reform(interpolate(data_from,pix[0,*,*], pix[1,*,*], cubic=-0.5, missing = missing))
end