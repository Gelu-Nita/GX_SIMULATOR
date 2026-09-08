;+
; :Description:
;    Return 1b if REF is a CHMP map object or map structure that carries a
;    time-series cube (HAS_CUBE + valid TIME_CUBE pointer).
;
;    For an objarr, checks the first object (use per-element for mixed sets).
;-
function gx_ref_has_cube, ref
  compile_opt idl2
  if n_elements(ref) eq 0 then return, 0b
  if size(ref, /tname) eq 'OBJREF' then begin
    if ~obj_valid(ref[0]) then return, 0b
    m = ref[0]->get(0, /map)
    if ~valid_map(m) then return, 0b
    return, gx_ref_has_cube(m)
  endif
  if ~valid_map(ref) then return, 0b
  r = ref[0]
  if ~tag_exist(r, 'has_cube') then return, 0b
  if ~keyword_set(r.has_cube) then return, 0b
  if ~tag_exist(r, 'time_cube') then return, 0b
  if size(r.time_cube, /tname) ne 'POINTER' then return, 0b
  if ~ptr_valid(r.time_cube) then return, 0b
  sz = size(*r.time_cube)
  if sz[0] lt 3 then return, 0b
  if sz[3] lt 2 then return, 0b
  return, 1b
end
