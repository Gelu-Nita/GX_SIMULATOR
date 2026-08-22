;+
; :Description:
;    If a map (or map array) is missing restoring-beam tags, copy standard
;    FITS beam keywords from HEADER onto the map in a form gx_ref2chmp accepts.
;
;    Recognized header keys (via fxpar): BMAJ, BMIN, BPA
;      FITS convention: BMAJ/BMIN in degrees → stored on the map as FWHM [arcsec].
;    If the map already has a_beam/b_beam, bmaj/bmin, or bmaj_bmin_bpa, leave it
;    unchanged unless /overwrite is set.
;
; :Params:
;    map - map structure or array of maps (replaced/updated on return)
;    header - FITS header string array (or index struct with bmaj/bmin/bpa tags)
;
; :Keywords:
;    overwrite - replace existing beam tags
;    degrees - if set (default), treat header BMAJ/BMIN as degrees; if 0, as arcsec
;    status - out, 1 if beam tags were written, 0 if skipped/missing
;    loud - print why recovery was skipped or what was written
;-
pro gx_map_add_beam_from_fits, map, header, overwrite=overwrite, degrees=degrees, $
                               status=status, loud=loud
  status = 0
  if n_elements(map) eq 0 then begin
    if keyword_set(loud) then message, 'no map provided', /info
    return
  endif
  if size(map, /tname) ne 'STRUCT' then begin
    if keyword_set(loud) then message, 'map is not a structure (got '+size(map,/tname)+')', /info
    return
  endif
  default, degrees, 1

  bmaj = !null
  bmin = !null
  bpa = 0d

  if size(header, /tname) eq 'STRUCT' then begin
    if tag_exist(header, 'bmaj') then bmaj = double(header.bmaj)
    if tag_exist(header, 'bmin') then bmin = double(header.bmin)
    if tag_exist(header, 'bpa') then bpa = double(header.bpa)
  endif else if size(header, /tname) eq 'STRING' then begin
    if n_elements(header) gt 0 then begin
      bj = fxpar(header, 'BMAJ', count=cj)
      bn = fxpar(header, 'BMIN', count=cn)
      bp = fxpar(header, 'BPA', count=cp)
      if cj gt 0 and cn gt 0 then begin
        bmaj = double(bj)
        bmin = double(bn)
        if cp gt 0 then bpa = double(bp) else bpa = 0d
      endif
    endif
  endif

  if n_elements(bmaj) eq 0 or n_elements(bmin) eq 0 then begin
    if keyword_set(loud) then message, 'header has no BMAJ/BMIN', /info
    return
  endif
  if ~(finite(bmaj) and finite(bmin)) then begin
    if keyword_set(loud) then message, 'BMAJ/BMIN not finite', /info
    return
  endif
  if bmaj eq 0 or bmin eq 0 then begin
    if keyword_set(loud) then message, 'BMAJ/BMIN are zero', /info
    return
  endif

  if keyword_set(degrees) then begin
    bmaj_arcsec = bmaj * 3600d
    bmin_arcsec = bmin * 3600d
    ; If conversion looks absurd (>1 deg on sky) but raw values look like arcsec, keep raw
    if (bmaj_arcsec gt 3600d) and (bmaj lt 3600d) and (bmaj gt 0.05d) then begin
      bmaj_arcsec = bmaj
      bmin_arcsec = bmin
      if keyword_set(loud) then message, 'BMAJ/BMIN treated as arcsec (not degrees)', /info
    endif
  endif else begin
    bmaj_arcsec = bmaj
    bmin_arcsec = bmin
  endelse

  beam_str = string(bmaj_arcsec, bmin_arcsec, bpa, $
    format='(F0.4,", ",F0.4,", ",F0.2)')

  nmap = n_elements(map)
  n_written = 0L
  for i = 0L, nmap - 1 do begin
    mi = map[i]
    has_beam = tag_exist(mi, 'a_beam') or tag_exist(mi, 'b_beam') $
      or tag_exist(mi, 'bmaj') or tag_exist(mi, 'bmin') $
      or tag_exist(mi, 'bmaj_bmin_bpa')
    if has_beam and ~keyword_set(overwrite) then begin
      if keyword_set(loud) and i eq 0 then $
        message, 'map already has beam tags; use /overwrite to replace', /info
      if i eq 0 then out = mi else out = [out, mi]
      continue
    endif
    add_prop, mi, bmaj = bmaj_arcsec, /replace
    add_prop, mi, bmin = bmin_arcsec, /replace
    add_prop, mi, bpa = bpa, /replace
    add_prop, mi, bmaj_bmin_bpa = beam_str, /replace
    if i eq 0 then out = mi else out = [out, mi]
    n_written++
  endfor
  map = out
  status = n_written gt 0
  if keyword_set(loud) and status then $
    message, 'set bmaj/bmin/bpa = '+beam_str+' [arcsec, arcsec, deg]', /info
end
