;+
; :Description:
;    FOV-integrate a map (and optional SDEV map) over all pixels.
;
;    Microwave (FREQ tag present and >0): convert Tb [K] to flux [sfu] via
;    gx_tb2sfu before summing:
;      S = total(Tb) * gx_tb2sfu(dx*dy, freq)
;    EUV / other (CHAN or no FREQ): intensity integral
;      S = total(data) * dx * dy
;
;    Spectral uncertainty of the sum assumes independent pixels:
;      S_sdev = sqrt(total(sdev^2)) * (same scale factor as S)
;
; :Params:
;    map - map structure, in
;
; :Keywords:
;    sdev - optional sdev map structure
;    s_sdev - out, integrated uncertainty (if sdev provided and valid)
;    R - optional solar radius [arcsec] for Tb<->sfu (default: map.rsun or gx_sfu2tb default)
;
; :Returns:
;    Double scalar FOV integral (sfu for MW Tb maps; intensity*area otherwise)
;-
function gx_fov_integral_map, map, sdev=sdev, s_sdev=s_sdev, R=R
  if ~valid_map(map) then begin
    message,'Valid map structure required for FOV integral',/info
    return, !values.d_nan
  endif
  dx = double(map.dx)
  dy = double(map.dy)
  ds = dx * dy

  use_tb2sfu = 0b
  if tag_exist(map, 'freq') then begin
    freq = map.freq
    if is_number(freq) then if freq gt 0 then use_tb2sfu = 1b
  endif

  if use_tb2sfu then begin
    if n_elements(R) eq 0 and tag_exist(map, 'rsun') then R = map.rsun
    scale = gx_tb2sfu(ds, freq, R=R)
    s = total(double(map.data), /nan) * scale
    if arg_present(s_sdev) then begin
      if valid_map(sdev) then $
        s_sdev = sqrt(total(double(sdev.data)^2, /nan)) * scale $
      else s_sdev = !values.d_nan
    endif
  endif else begin
    s = total(double(map.data), /nan) * ds
    if arg_present(s_sdev) then begin
      if valid_map(sdev) then begin
        sdx = double(sdev.dx)
        sdy = double(sdev.dy)
        s_sdev = sqrt(total(double(sdev.data)^2, /nan)) * sdx * sdy
      endif else s_sdev = !values.d_nan
    endif
  endelse
  return, s
end
