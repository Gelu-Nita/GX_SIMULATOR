;+
; :Description:
;    Copy HAS_CUBE / TIME_CUBE tags from a wrapper or source map onto CHMP
;    Data (map 0) of OBJ. Call after gx_ref2chmp_one so MAPS=[mean,sdev]
;    stay tag-compatible.
;-
pro gx_ref_cube_bind_ref, obj, src
  compile_opt idl2
  if size(obj, /tname) ne 'OBJREF' then return
  if ~obj_valid(obj) then return
  cube = !null
  template = !null
  ; tag_exist on a map/struct array returns a byte array; do not && it.
  src0 = src
  if size(src, /tname) eq 'STRUCT' and n_elements(src) ge 1 then src0 = src[0]
  if size(src0, /tname) eq 'STRUCT' and n_elements(src0) eq 1 then begin
    if tag_exist(src0, 'time_cube') then $
      if size(src0.time_cube, /tname) eq 'POINTER' then $
        if ptr_valid(src0.time_cube) then cube = *src0.time_cube
    if tag_exist(src0, 'time_template') then $
      if valid_map(src0.time_template) then template = src0.time_template
    if n_elements(cube) eq 0 and tag_exist(src0, 'maps') then begin
      if gx_ref_has_cube(src0.maps) then begin
        cube = *src0.maps[0].time_cube
        if tag_exist(src0.maps[0], 'time_template') then template = src0.maps[0].time_template
      endif
    endif
  endif
  if n_elements(cube) eq 0 and gx_ref_has_cube(src) then begin
    if size(src, /tname) eq 'OBJREF' then m = src[0]->get(0, /map) else m = src[0]
    cube = *m.time_cube
    if tag_exist(m, 'time_template') then template = m.time_template
  endif
  if n_elements(cube) eq 0 then return
  dmap = obj->get(0, /map)
  if ~valid_map(dmap) then return
  if ~valid_map(template) then template = dmap
  gx_ref_cube_attach, dmap, cube, template
  obj->setmap, 0, dmap
end
