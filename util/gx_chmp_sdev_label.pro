;+
; Short plot/legend phrase for sdev_method 'A' or 'B'.
; A = stdev of the live-ROI integrated light curve (s_F).
; B = independent-pixel quadrature of the ROI (gx_fov_integral_map).
; The letter is kept so the title still matches sdev_method=.
;-
function gx_chmp_sdev_label, method
  compile_opt idl2
  if n_elements(method) eq 0 then return, ''
  key = strupcase(strcompress(string(method[0]), /rem))
  if key eq 'METHODA' or key eq '1' then key = 'A'
  if key eq 'METHODB' or key eq '2' then key = 'B'
  case key of
    'A': return, 'sdev: light-curve (A)'
    'B': return, 'sdev: pixel-quad (B)'
    else: return, 'sdev: ' + strtrim(string(method[0]), 2)
  endcase
end
