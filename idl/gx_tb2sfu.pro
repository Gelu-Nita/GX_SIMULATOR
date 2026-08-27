;+
; :Description:
;    Brightness-temperature to flux density conversion coefficient.
;    Companion / inverse of gx_sfu2tb for a given frequency.
;
;    With ds = dx*dy in arcsec^2 (same convention as gx_sfu2tb):
;      sfu = gx_tb2sfu(ds, freq) * Tb
;      Tb  = gx_sfu2tb(ds) * sfu / freq^2
;         = sfu / gx_tb2sfu(ds, freq)
;
; :Params:
;    ds - pixel solid angle in arcsec^2 (typically dx*dy)
;    freq - frequency in GHz (scalar or array)
;
; :Keywords:
;    R - apparent solar radius in arcsec (passed through to gx_sfu2tb)
;
; :Returns:
;    Coefficient(s) such that sfu = result * Tb
;-
function gx_tb2sfu, ds, freq, R=R
  if n_elements(freq) eq 0 then begin
    message,'freq (GHz) is required for Tb to sfu conversion',/info
    return, !values.d_nan
  endif
  return, (double(freq)^2) / gx_sfu2tb(ds, R=R)
end
