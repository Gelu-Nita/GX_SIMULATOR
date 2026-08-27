;+
; :Description:
;    Build an FOV-integrated model spectrum from a multi-map model object by
;    selecting maps at requested FREQ or CHAN values, optionally convolving
;    each with the corresponding restoring beam, then FOV-integrating.
;
; :Params:
;    mapobj - map object containing one map per frequency/channel
;    axis - dblarr(n) requested frequencies (GHz) or channels
;
; :Keywords:
;    is_chan - if set, match CHAN tags instead of FREQ
;    refs - optional objarr(n) of CHMP refs (for per-channel beams)
;    corr_beam - beam scale factor (default 1)
;    resize - optional [nx,ny] rebin of model maps
;    tol - relative axis match tolerance (default 1e-3)
;    err_msg - out
;    mod_maps - out, objarr(n) map objects each holding one convolved model map
;
; :Returns:
;    Structure {S_mod:dblarr(n), modidx:lonarr(n)}
;-
function gx_mapobj2fovspectrum, mapobj, axis, is_chan=is_chan, refs=refs, $
  corr_beam=corr_beam, resize=resize, tol=tol, err_msg=err_msg, mod_maps=mod_maps

  default, tol, 1d-3
  default, corr_beam, 1d0
  err_msg = ''
  if ~obj_valid(mapobj) then begin
    err_msg = 'Invalid model map object'
    return, !null
  endif
  n = n_elements(axis)
  if n eq 0 then begin
    err_msg = 'Empty spectrum axis'
    return, !null
  endif

  nmap = mapobj->get(/count)
  tag_axis = dblarr(nmap)
  for k=0, nmap-1 do begin
    if keyword_set(is_chan) then v = mapobj->get(k, /chan) else v = mapobj->get(k, /freq)
    if n_elements(v) eq 0 then begin
      err_msg = 'Model map #'+strtrim(k,2)+' missing FREQ/CHAN tag'
      return, !null
    endif
    tag_axis[k] = double(v[0])
  endfor

  if n_elements(resize) ne 0 then begin
    if n_elements(resize) eq 1 then resize = [resize, resize]
    for k=0, nmap-1 do begin
      rmap = gx_rebin_map(mapobj->get(k, /map), resize[0], resize[1], $
        total=keyword_set(is_chan))
      rmap.id = 'rebinned_'+rmap.id
      mapobj->setmap, k, rmap
    endfor
  endif

  S_mod = dblarr(n)
  modidx = lonarr(n)
  mod_maps = objarr(n)

  for k=0, n-1 do begin
    m = min(abs(tag_axis - axis[k]), ii)
    thr = (tol * abs(axis[k])) > 1d-6
    if m gt thr then begin
      err_msg = string(axis[k], keyword_set(is_chan)?'CHAN':'FREQ', $
        format="('Model maps missing requested ',a0,'=',g0)")
      return, !null
    endif
    modidx[k] = ii
    modI = mapobj->get(ii, /map)

    ; beam convolution (same path as image mode)
    if n_elements(refs) gt k then begin
      if obj_valid(refs[k]) then begin
        a_beam = refs[k]->get(0, /a_beam)
        b_beam = refs[k]->get(0, /b_beam)
        phi_beam = refs[k]->get(0, /phi_beam)
        cb = refs[k]->get(0, /corr_beam)
        if ~is_number(cb) then cb = corr_beam
        if isa(a_beam) and isa(b_beam) and isa(phi_beam) then begin
          dx = modI.dx
          dy = modI.dy
          width = size(modI.data, /dimensions)
          if width[0] mod 2 eq 0 then width[0] += 1
          if width[1] mod 2 eq 0 then width[1] += 1
          psf = gx_psf(cb*[a_beam, b_beam]/[dx, dy], phi_beam, width)
          modI.data = convol_fft(modI.data, psf)
        endif
      endif
    endif

    S_mod[k] = gx_fov_integral_map(modI)
    o = obj_new('map')
    o->setmap, 0, modI
    mod_maps[k] = o
  endfor

  return, {S_mod:S_mod, modidx:modidx}
end
