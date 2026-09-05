;+
; :Description:
;    Resolve sdev_method: 'auto'|'A'|'B' → 'A' or 'B'.
;
;    auto + cube + spectrum → A
;    auto + cube + image    → B
;    auto + no cube         → B
;    A without a cube, or A in image mode, is an error (err_msg set, '').
;-
function gx_chmp_sdev_method, sdev_method=sdev_method, search_mode=search_mode, $
  has_cube=has_cube, err_msg=err_msg
  compile_opt idl2
  err_msg = ''
  default, search_mode, 'image'
  smode = strlowcase(strcompress(string(search_mode[0]), /rem))
  raw = 'auto'
  if n_elements(sdev_method) gt 0 then raw = strtrim(string(sdev_method[0]), 2)
  key = strupcase(strcompress(raw, /rem))
  if key eq '' then key = 'AUTO'
  if key eq 'METHODA' or key eq '1' then key = 'A'
  if key eq 'METHODB' or key eq '2' then key = 'B'

  want = ''
  case key of
    'AUTO': if keyword_set(has_cube) and (smode eq 'spectrum') then want = 'A' else want = 'B'
    'A': want = 'A'
    'B': want = 'B'
    else: begin
      err_msg = "sdev_method must be 'auto', 'A', or 'B' (got '" + raw + "')"
      return, ''
    end
  endcase

  if want eq 'A' then begin
    if ~keyword_set(has_cube) then begin
      err_msg = "sdev_method='A' requires a time-series cube reference (not [mean,SDEV] snapshots)"
      return, ''
    endif
    if smode eq 'image' then begin
      err_msg = "sdev_method='A' is an integrated-spectrum uncertainty; not valid for search_mode='image'"
      return, ''
    endif
  endif
  return, want
end
