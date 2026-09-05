;+
; :Description:
;    Build a format-3-like CHMP ref struct from a time series of maps:
;    MAPS=[mean, sample-SDEV] plus HAS_CUBE payload on the mean map.
;
;    MAPS_IN: map array, or one map with 3-D .data (time in dim 3).
;    Wrapper tags (A_BEAM, CHAN, …) are copied from WRAP if provided.
;-
function gx_ref2chmp_maps2cube, maps_in, wrap=wrap, err_msg=err_msg
  compile_opt idl2
  err_msg = ''
  if isa(maps_in, 'LIST') then begin
    n = maps_in.count()
    if n lt 1 then begin
      err_msg = 'gx_ref2chmp_maps2cube: empty list'
      return, !null
    endif
    m0 = maps_in[0]
    if ~valid_map(m0) then begin
      err_msg = 'gx_ref2chmp_maps2cube: list item 0 is not a valid map'
      return, !null
    endif
    sz = size(m0.data)
    if sz[0] ne 2 then begin
      err_msg = 'gx_ref2chmp_maps2cube: expected 2-D frames in the list'
      return, !null
    endif
    cube = dblarr(sz[1], sz[2], n)
    template = m0[0]
    for t = 0L, n - 1 do begin
      mt = maps_in[t]
      if ~valid_map(mt) then begin
        err_msg = 'gx_ref2chmp_maps2cube: invalid frame at t=' + strtrim(t, 2)
        return, !null
      endif
      if gx_ref_is_sdev_map(mt) then begin
        err_msg = 'gx_ref2chmp_maps2cube: SDEV plane found in a time-series list'
        return, !null
      endif
      dsz = size(mt[0].data)
      if dsz[0] ne 2 or dsz[1] ne sz[1] or dsz[2] ne sz[2] then begin
        err_msg = 'gx_ref2chmp_maps2cube: frame size mismatch at t=' + strtrim(t, 2)
        return, !null
      endif
      cube[*, *, t] = double(mt[0].data)
    endfor
    goto, have_cube
  endif

  if ~valid_map(maps_in) then begin
    err_msg = 'gx_ref2chmp_maps2cube: input is not a valid map'
    return, !null
  endif

  cube = !null
  template = maps_in[0]
  if n_elements(maps_in) eq 1 then begin
    sz = size(maps_in[0].data)
    if sz[0] eq 3 then begin
      cube = double(maps_in[0].data)
      d2 = maps_in[0]
      d2.data = reform(cube[*, *, 0])
      template = d2
    endif else if sz[0] eq 2 then begin
      err_msg = 'gx_ref2chmp_maps2cube: single 2-D map is not a time cube'
      return, !null
    endif
  endif
  if n_elements(cube) eq 0 then begin
    n = n_elements(maps_in)
    sz = size(maps_in[0].data)
    if sz[0] ne 2 then begin
      err_msg = 'gx_ref2chmp_maps2cube: expected 2-D frames in the map array'
      return, !null
    endif
    cube = dblarr(sz[1], sz[2], n)
    for t = 0L, n - 1 do begin
      if ~valid_map(maps_in[t]) then begin
        err_msg = 'gx_ref2chmp_maps2cube: invalid frame at t=' + strtrim(t, 2)
        return, !null
      endif
      if gx_ref_is_sdev_map(maps_in[t]) then begin
        err_msg = 'gx_ref2chmp_maps2cube: SDEV plane found in a time-series array'
        return, !null
      endif
      dsz = size(maps_in[t].data)
      if dsz[0] ne 2 or dsz[1] ne sz[1] or dsz[2] ne sz[2] then begin
        err_msg = 'gx_ref2chmp_maps2cube: frame size mismatch at t=' + strtrim(t, 2)
        return, !null
      endif
      cube[*, *, t] = double(maps_in[t].data)
    endfor
    template = maps_in[0]
  endif

  have_cube:

  gx_ref_cube_stats, cube, mean_data, sdev_data, nframe=nframe
  if nframe lt 2 then begin
    err_msg = 'gx_ref2chmp_maps2cube: need M>=2 frames'
    return, !null
  endif

  dmap = template
  dmap.data = mean_data
  smap = template
  smap.data = sdev_data
  sid = tag_exist(smap, 'id') ? smap.id : ''
  smap.id = 'SDEV ' + sid
  ; Do not add cube tags onto dmap here: [dmap,smap] must share one struct def.
  maps = [dmap, smap]

  ax = !values.d_nan
  is_chan = 0b
  if isa(maps_in, 'LIST') then ax = gx_ref_item_axis(maps_in[0], is_chan=is_chan) $
  else ax = gx_ref_item_axis(maps_in, is_chan=is_chan)
  if ~finite(ax) and n_elements(wrap) eq 1 then $
    ax = gx_ref_item_axis(wrap, is_chan=is_chan)

  ref = {maps: maps, has_cube: 1b, nframe: nframe, $
    time_cube: ptr_new(cube), time_template: template}
  if finite(ax) then begin
    if keyword_set(is_chan) then ref = create_struct(ref, 'chan', ax) $
    else ref = create_struct(ref, 'freq', ax)
  endif
  if n_elements(wrap) eq 1 then begin
    if size(wrap, /tname) eq 'STRUCT' then begin
      if tag_exist(wrap, 'a_beam') then ref = create_struct(ref, 'a_beam', wrap.a_beam)
      if tag_exist(wrap, 'b_beam') then ref = create_struct(ref, 'b_beam', wrap.b_beam)
      if tag_exist(wrap, 'phi_beam') then ref = create_struct(ref, 'phi_beam', wrap.phi_beam)
      if tag_exist(wrap, 'corr_beam') then ref = create_struct(ref, 'corr_beam', wrap.corr_beam)
      if tag_exist(wrap, 'chan') and ~tag_exist(ref, 'chan') then $
        ref = create_struct(ref, 'chan', wrap.chan)
      if tag_exist(wrap, 'freq') and ~tag_exist(ref, 'freq') then $
        ref = create_struct(ref, 'freq', wrap.freq)
    endif
  endif
  return, ref
end
