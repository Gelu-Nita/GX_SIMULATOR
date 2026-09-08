;+
; :Description:
;    FREQ or CHAN axis value for a gx_ref2chmp load item (format-3 struct,
;    map array, or single map). Returns NaN if none found.
;-
function gx_ref_item_axis, item, is_chan=is_chan, err_msg=err_msg
  compile_opt idl2
  err_msg = ''
  is_chan = 0b
  nan = !values.d_nan
  if n_elements(item) eq 0 then return, nan

  if size(item, /tname) eq 'OBJREF' then begin
    if ~obj_valid(item[0]) then return, nan
    rf = item[0]->get(0, /freq)
    rc = item[0]->get(0, /chan)
    if n_elements(rf) gt 0 && finite(rf[0]) then begin
      is_chan = 0b
      return, double(rf[0])
    endif
    if n_elements(rc) gt 0 && finite(rc[0]) then begin
      is_chan = 1b
      return, double(rc[0])
    endif
    return, nan
  endif

  if size(item, /tname) ne 'STRUCT' then return, nan

  it = item[0]
  if tag_exist(it, 'freq') then begin
    if finite(double(it.freq[0])) then begin
      is_chan = 0b
      return, double(it.freq[0])
    endif
  endif
  if tag_exist(it, 'chan') then begin
    if finite(double(it.chan[0])) then begin
      is_chan = 1b
      return, double(it.chan[0])
    endif
  endif

  m = !null
  if tag_exist(it, 'maps') && valid_map(it.maps) then m = it.maps[0] $
  else if valid_map(it) then m = it
  if ~valid_map(m) then return, nan

  if tag_exist(m, 'freq') && finite(double(m.freq[0])) then begin
    is_chan = 0b
    return, double(m.freq[0])
  endif
  if tag_exist(m, 'chan') && finite(double(m.chan[0])) then begin
    is_chan = 1b
    return, double(m.chan[0])
  endif
  if tag_exist(m, 'id') then begin
    toks = strsplit(m.id, /extract)
    last = toks[n_elements(toks) - 1]
    if valid_num(last) then begin
      is_chan = 1b
      return, double(last)
    endif
  endif
  return, nan
end
