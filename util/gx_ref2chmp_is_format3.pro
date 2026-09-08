;+
; :Description:
;    True if ITEM is a CHMP format-3 snapshot {MAPS:[mean,sdev], ...}, not a
;    time cube. MAPS[1] may be a real SDEV or a placeholder copy of MAPS[0].
;-
function gx_ref2chmp_is_format3, item
  compile_opt idl2
  if size(item, /tname) ne 'STRUCT' then return, 0b
  if n_elements(item) gt 1 then return, 0b
  ; Promoted cube wrappers also have MAPS=[mean,sdev]; they are not snapshots.
  if tag_exist(item, 'time_cube') then $
    if size(item.time_cube, /tname) eq 'POINTER' then $
      if ptr_valid(item.time_cube) then return, 0b
  if tag_exist(item, 'has_cube') then if keyword_set(item.has_cube) then return, 0b
  if ~tag_exist(item, 'maps') then return, 0b
  if ~valid_map(item.maps) then return, 0b
  nm = n_elements(item.maps)
  if nm lt 1 or nm gt 2 then return, 0b
  sz = size(item.maps[0].data)
  if sz[0] ne 2 then return, 0b
  if nm eq 1 then return, 1b
  ; Two planes: SDEV id, or a CHMP wrapper with beam tags (legacy [mean,sdev]
  ; may not prefix the SDEV map ID)
  if gx_ref_is_sdev_map(item.maps[1]) then return, 1b
  if tag_exist(item, 'a_beam') or tag_exist(item, 'b_beam') then return, 1b
  return, 0b
end
