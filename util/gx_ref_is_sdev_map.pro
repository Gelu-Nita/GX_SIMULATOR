;+
; :Description:
;    True if a map looks like a CHMP SDEV plane (ID starts with SDEV), not a
;    time-series intensity frame.
;-
function gx_ref_is_sdev_map, map
  compile_opt idl2
  if ~valid_map(map) then return, 0b
  id = ''
  if tag_exist(map[0], 'id') then id = strupcase(strtrim(map[0].id, 2))
  if id eq '' then return, 0b
  return, strpos(id, 'SDEV') eq 0
end
