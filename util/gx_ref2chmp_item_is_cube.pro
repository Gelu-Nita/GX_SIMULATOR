;+
; :Description:
;    Classify a gx_ref2chmp load item as a time cube (1b) vs snapshot (0b).
;
;    Cubes: 3-D .data, map array with M>=2 intensity frames, or RMAPS-style
;    arrays. Format-3 {MAPS:[mean,sdev]} is never a cube. A 2-element map
;    array whose second plane is SDEV is the legacy Data/SDEV pair, not a cube.
;-
function gx_ref2chmp_item_is_cube, item, err_msg=err_msg
  compile_opt idl2
  err_msg = ''
  if n_elements(item) eq 0 then return, 0b
  if size(item, /tname) eq 'OBJREF' then return, 0b
  if size(item, /tname) ne 'STRUCT' then return, 0b

  ; Already-promoted cube wrapper (time_cube / has_cube on the struct)
  if n_elements(item) eq 1 then begin
    if tag_exist(item, 'time_cube') then $
      if size(item.time_cube, /tname) eq 'POINTER' then $
        if ptr_valid(item.time_cube) then return, 1b
    if tag_exist(item, 'has_cube') then if keyword_set(item.has_cube) then return, 1b
  endif

  if gx_ref2chmp_is_format3(item) then begin
    ; Format-3 with a 3-D mean would still be unusual; treat as snapshot
    return, 0b
  endif

  maps = !null
  it = item[0]
  if n_elements(item) eq 1 then begin
    if tag_exist(it, 'maps') then if valid_map(it.maps) then maps = it.maps
    if ~valid_map(maps) and tag_exist(it, 'rmaps') then $
      if valid_map(it.rmaps) then maps = it.rmaps
    if ~valid_map(maps) and valid_map(it) then maps = item
  endif else if valid_map(item) then maps = item

  if ~valid_map(maps) then return, 0b

  if n_elements(maps) eq 1 then begin
    sz = size(maps[0].data)
    return, sz[0] eq 3 && sz[3] ge 2
  endif

  if n_elements(maps) eq 2 and gx_ref_is_sdev_map(maps[1]) then return, 0b

  ; M>=2 intensity frames, same 2-D size
  if n_elements(maps) ge 2 then begin
    sz = size(maps[0].data)
    if sz[0] eq 3 && sz[3] ge 2 then return, 1b
    if sz[0] ne 2 then return, 0b
    nsd = 0L
    for t = 0L, n_elements(maps) - 1 do if gx_ref_is_sdev_map(maps[t]) then nsd++
    if nsd gt 0 then return, 0b
    return, 1b
  endif
  return, 0b
end
