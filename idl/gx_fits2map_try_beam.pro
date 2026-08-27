;+
; Attempt beam recovery from a FITS header or index struct onto MAP.
;-
pro gx_fits2map_try_beam, map, header, status=status, loud=loud
  status = 0
  if size(map, /tname) ne 'STRUCT' then return
  if size(header, /tname) eq 'STRUCT' then begin
    gx_map_add_beam_from_fits, map, header, status=status, loud=loud
  endif else if size(header, /tname) eq 'STRING' then begin
    if n_elements(header) gt 0 then $
      gx_map_add_beam_from_fits, map, header, status=status, loud=loud
  endif
end
