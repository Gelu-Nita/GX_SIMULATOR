;+
; :Description:
;    Prepare observed and model spectra for gx_metrics_spectrum from an
;    objarr of standard CHMP image refs and a multi-map model object,
;    using the same ROI mask semantics as the image search path
;    (mask / levels / apply2 via gx_metrics_image).
;
; :Params:
;    mapobj - model map object (one map per freq/chan)
;    refs - OBJREF or objarr of CHMP refs (Data/SDEV/BEAM each)
;
; :Keywords:
;    mask, apply2 - same meaning as gx_metrics_image / gx_metrics_map
;    resize - optional [nx,ny] rebin of model maps
;    corr_beam - beam scale (default from each ref)
;    tol - axis match tolerance
;    err_msg - out
;    mod_maps - out, objarr of map objects holding convolved model maps
;
; :Returns:
;    Structure:
;      n, axis, is_chan, S_obs, S_mod, S_sdev, has_sdev, refs
;-
forward_function gx_fov_integral_map, gx_psf, gx_metrics_image, gx_rebin_map

function gx_maps2spectrum, mapobj, refs, mask=mask, apply2=apply2, $
  resize=resize, corr_beam=corr_beam, tol=tol, err_msg=err_msg, mod_maps=mod_maps

  default, tol, 1d-3
  default, corr_beam, 1d0
  err_msg = ''

  if ~obj_valid(mapobj) then begin
    err_msg = 'gx_maps2spectrum: invalid model map object'
    return, !null
  endif
  if size(refs, /tname) ne 'OBJREF' then begin
    err_msg = 'gx_maps2spectrum: refs must be CHMP map object(s)'
    return, !null
  endif

  n = n_elements(refs)
  if n lt 1 then begin
    err_msg = 'gx_maps2spectrum: empty refs'
    return, !null
  endif

  ; Model axis tags
  nmap = mapobj->get(/count)
  if n_elements(resize) ne 0 then begin
    if n_elements(resize) eq 1 then resize = [resize, resize]
  endif

  axis = dblarr(n)
  is_chan_vec = bytarr(n)
  for k = 0L, n - 1 do begin
    if ~obj_valid(refs[k]) then begin
      err_msg = 'gx_maps2spectrum: invalid ref at ' + strtrim(k, 2)
      return, !null
    endif
    rf = refs[k]->get(0, /freq)
    rc = refs[k]->get(0, /chan)
    if n_elements(rf) gt 0 && finite(rf[0]) then begin
      axis[k] = double(rf[0])
      is_chan_vec[k] = 0b
    endif else if n_elements(rc) gt 0 && finite(rc[0]) then begin
      axis[k] = double(rc[0])
      is_chan_vec[k] = 1b
    endif else begin
      err_msg = 'gx_maps2spectrum: ref missing FREQ/CHAN at ' + strtrim(k, 2)
      return, !null
    endelse
  endfor
  if n_elements(uniq(is_chan_vec, sort(is_chan_vec))) gt 1 then begin
    err_msg = 'gx_maps2spectrum: mixed FREQ/CHAN refs'
    return, !null
  endif
  is_chan = is_chan_vec[0]

  tag_axis = dblarr(nmap)
  for j = 0, nmap - 1 do begin
    if keyword_set(is_chan) then v = mapobj->get(j, /chan) else v = mapobj->get(j, /freq)
    if n_elements(v) eq 0 then begin
      err_msg = 'Model map #' + strtrim(j, 2) + ' missing FREQ/CHAN'
      return, !null
    endif
    tag_axis[j] = double(v[0])
  endfor

  if n_elements(resize) ne 0 then begin
    for j = 0, nmap - 1 do begin
      rmap = gx_rebin_map(mapobj->get(j, /map), resize[0], resize[1], $
        total=keyword_set(is_chan))
      rmap.id = 'rebinned_' + rmap.id
      mapobj->setmap, j, rmap
    endfor
  endif

  S_obs = dblarr(n)
  S_mod = dblarr(n)
  S_sdev = dblarr(n)
  has_sdev = bytarr(n)
  mod_maps = objarr(n)

  for k = 0L, n - 1 do begin
    m = min(abs(tag_axis - axis[k]), ii)
    thr = (tol * abs(axis[k])) > 1d-6
    if m gt thr then begin
      err_msg = string(axis[k], keyword_set(is_chan) ? 'CHAN' : 'FREQ', $
        format="('Model maps missing requested ',a0,'=',g0)")
      return, !null
    endif

    modI = mapobj->get(ii, /map)
    obsI = refs[k]->get(0, /map)
    obsIsdev = refs[k]->get(1, /map)

    ; Beam convolution (same as image path)
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
      psf = gx_psf(cb * [a_beam, b_beam] / [dx, dy], phi_beam, width)
      modI.data = convol_fft(modI.data, psf)
    endif

    ; Align / remap obs onto model grid (same as gx_metrics_map)
    gx_align_map, modI, obsI
    map_ref = inter_map(obsI, modI)
    if valid_map(obsIsdev) then map_sdev = inter_map(obsIsdev, modI) else map_sdev = !null

    ; EUV flux conservation: if still needed after inter_map, rebin totals
    if keyword_set(is_chan) then begin
      ; inter_map already remapped; keep as-is
    endif

    ; ROI mask identical to image metrics
    tmp = gx_metrics_image(modI.data, map_ref.data, $
      valid_map(map_sdev) ? map_sdev.data : !null, $
      mask=mask, apply2=apply2)
    if ~isa(tmp, 'STRUCT') or ~tag_exist(tmp, 'mask_img') then begin
      err_msg = 'gx_maps2spectrum: failed to build ROI mask at axis index ' + strtrim(k, 2)
      return, !null
    endif
    img_mask = tmp.mask_img

    ; Integrate under mask by zeroing outside ROI
    mod_m = modI
    obs_m = map_ref
    bad = where(~img_mask, nbad)
    if nbad gt 0 then begin
      mod_m.data[bad] = 0
      obs_m.data[bad] = 0
    endif
    S_mod[k] = gx_fov_integral_map(mod_m)
    if valid_map(map_sdev) then begin
      sd_m = map_sdev
      if nbad gt 0 then sd_m.data[bad] = 0
      S_obs[k] = gx_fov_integral_map(obs_m, sdev=sd_m, s_sdev=ss)
      if finite(ss) then begin
        S_sdev[k] = ss
        has_sdev[k] = 1b
      endif else begin
        S_sdev[k] = !values.d_nan
        has_sdev[k] = 0b
      endelse
    endif else begin
      S_obs[k] = gx_fov_integral_map(obs_m)
      S_sdev[k] = !values.d_nan
      has_sdev[k] = 0b
    endelse

    o = obj_new('map')
    o->setmap, 0, modI
    mod_maps[k] = o
  endfor

  return, {n:n, axis:axis, is_chan:is_chan, S_obs:S_obs, S_mod:S_mod, $
    S_sdev:S_sdev, has_sdev:has_sdev, refs:refs}
end
