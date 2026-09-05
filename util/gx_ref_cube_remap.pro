;+
; :Description:
;    Remap a CHMP ref's native time cube onto TARGET (model FOV map) with
;    inter_map, matching gx_maps2spectrum.
;
;    Cache is a session HASH keyed by the unaligned model FOV (orig_xc/orig_yc
;    if gx_align_map already shifted TARGET). Do not put the cache on the map
;    object: SSW setmap drops TIME_CUBE pointers.
;
;    gx_align_map changes xc/yc every Q, so a cache keyed on the aligned
;    target would miss and re-run M native-grid inter_map calls per Q.
;    Native frames are cropped to the FOV before inter_map.
;
; :Params:
;    ref    - CHMP map object, or Data map struct with TIME_CUBE
;    target - map structure (aligned model FOV)
;
; :Keywords:
;    sdev_map - out, sample per-pixel σ (M-1) on the target grid
;    err_msg
;-
function gx_ref_cube_remap, ref, target, sdev_map=sdev_map, err_msg=err_msg
  compile_opt idl2
  common gx_ref_cube_rmap_cache, cache
  forward_function inter_map
  err_msg = ''
  sdev_map = !null
  is_obj = (size(ref, /tname) eq 'OBJREF') && obj_valid(ref[0])
  if is_obj then dmap = ref[0]->get(0, /map) $
  else if valid_map(ref) then dmap = ref[0] $
  else begin
    err_msg = 'gx_ref_cube_remap: ref is not a cube-bearing map or object'
    return, !null
  endelse
  if ~gx_ref_has_cube(dmap) then begin
    err_msg = 'gx_ref_cube_remap: no time cube on ref'
    return, !null
  endif
  if ~valid_map(target) then begin
    err_msg = 'gx_ref_cube_remap: invalid target map'
    return, !null
  endif

  tsz = size(target.data, /dimensions)
  nx = tsz[0]
  ny = tsz[1]
  ; Unaligned FOV: gx_align_map stores orig_xc/orig_yc then shifts xc/yc.
  xc0 = double(target.xc)
  yc0 = double(target.yc)
  if tag_exist(target, 'orig_xc') then xc0 = double(target.orig_xc)
  if tag_exist(target, 'orig_yc') then yc0 = double(target.orig_yc)
  hid = 0L
  if is_obj then hid = obj_valid(ref[0], /get_heap_identifier)
  ckey = strtrim(string(hid, format='(i0)'), 2) + ':' + $
    strjoin(string([xc0, yc0, double(target.dx), double(target.dy), $
    double(nx), double(ny)], format='(g16.8)'), ',')

  if ~isa(cache, 'HASH') then cache = hash()
  if cache.haskey(ckey) then begin
    cube0 = cache[ckey]
  endif else begin
    cube = *dmap.time_cube
    csz = size(cube)
    nframe = long(csz[3])
    tmpl = dmap
    if tag_exist(dmap, 'time_template') then $
      if valid_map(dmap.time_template) then tmpl = dmap.time_template[0]
    tgt0 = target
    tgt0.xc = xc0
    tgt0.yc = yc0
    xr = get_map_xrange(tgt0, /edge)
    yr = get_map_yrange(tgt0, /edge)
    pad = 5d * (abs(double(tgt0.dx)) > abs(double(tgt0.dy)))
    xw = [xr[0] - pad, xr[1] + pad]
    yw = [yr[0] - pad, yr[1] + pad]
    message, string(nframe, nx, ny, $
      format="('gx_ref_cube_remap: once-only interpolate M=',i0,' native frames -> ',i0,'x',i0)"), /info
    cube0 = dblarr(nx, ny, nframe)
    for t = 0L, nframe - 1 do begin
      fr = tmpl
      fr.data = reform(cube[*, *, t])
      frs = fr
      ; Crop to the model FOV so inter_map does not build full-disk xp/yp grids.
      sub_map, fr, frs, xrange=xw, yrange=yw
      if ~valid_map(frs) then frs = fr
      fr_r = inter_map(frs, tgt0)
      cube0[*, *, t] = double(fr_r.data)
    endfor
    cache[ckey] = cube0
  endelse

  ; Same pixel size, only a gx_align_map xc/yc shift: resample the small cube.
  cube_r = cube0
  if (abs(double(target.xc) - xc0) gt 1d-6) or (abs(double(target.yc) - yc0) gt 1d-6) then begin
    hdr = target
    hdr.xc = xc0
    hdr.yc = yc0
    nframe = (size(cube0))[3]
    cube_r = dblarr(nx, ny, nframe)
    for t = 0L, nframe - 1 do begin
      fr = hdr
      fr.data = reform(cube0[*, *, t])
      fr_r = inter_map(fr, target)
      cube_r[*, *, t] = double(fr_r.data)
    endfor
  endif

  gx_ref_cube_stats, cube_r, mean_r, sdev_r
  sdev_map = target
  sdev_map.data = sdev_r
  sid = tag_exist(sdev_map, 'id') ? sdev_map.id : ''
  sdev_map.id = 'SDEV ' + sid
  return, cube_r
end
