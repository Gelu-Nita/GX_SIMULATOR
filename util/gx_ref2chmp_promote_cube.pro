;+
; :Description:
;    Convert one load item to a cube wrapper if it is a time series; otherwise
;    return the item unchanged.
;-
function gx_ref2chmp_promote_cube, item, err_msg=err_msg
  compile_opt idl2
  err_msg = ''
  if ~gx_ref2chmp_item_is_cube(item, err_msg=em) then return, item
  maps = !null
  wrap = !null
  if size(item, /tname) eq 'STRUCT' then begin
    if n_elements(item) gt 1 and valid_map(item) then maps = item $
    else begin
      it = item[0]
      if tag_exist(it, 'maps') then if valid_map(it.maps) then begin
        maps = it.maps
        wrap = item
      endif else if tag_exist(it, 'rmaps') then if valid_map(it.rmaps) then begin
        maps = it.rmaps
        wrap = item
      endif else if valid_map(it) then maps = item
    endelse
  endif
  if ~valid_map(maps) then return, item
  out = gx_ref2chmp_maps2cube(maps, wrap=wrap, err_msg=err_msg)
  if ~isa(out, 'STRUCT') then begin
    if err_msg eq '' then err_msg = em
    return, !null
  endif
  return, out
end
