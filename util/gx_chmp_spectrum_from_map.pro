;+
; Rebuild the full (all-channel) ROI spectrum from a winning .map and refs.
; WHICH=0 uses res2_best_file; nonzero uses chi2_best_file.
;
; Beam comes from the reference maps (gx_ref2chmp / each ref's a_beam, b_beam,
; phi_beam, corr_beam). gx_maps2spectrum convolves the model with that beam.
; Caller a_beam/b_beam/phi_beam/corr_beam are optional overrides for gx_ref2chmp
; only when refs are loaded from REF DATAPATH, not when REFS_ALL is passed.
;
; Last-resort helper for savs that did not store spec_allmetrics spectra or
; channel_image_metrics. Replot should use the saved spectra when they exist.
;
; CHAN_METRICS: optional objarr of gx_metrics_map, one per spec.axis channel.
;-
pro gx_chmp_spectrum_from_map, ri, which, spec, refs_all=refs_all, $
  a_beam=a_beam, b_beam=b_beam, phi_beam=phi_beam, corr_beam=corr_beam, $
  chan_metrics=chan_metrics

  compile_opt idl2
  forward_function gx_ref2chmp, gx_maps2spectrum, gx_metrics_map, gx_rebin_map
  spec = !null
  chan_metrics = !null
  if ~isa(ri, 'STRUCT') then return
  if ~tag_exist(ri, 'modDir') or ~tag_exist(ri, 'res2_best_file') then return
  fn = (which ne 0) ? ri.chi2_best_file : ri.res2_best_file
  f = ri.modDir + path_sep() + fn
  if ~file_test(f) then begin
    message, 'Full spectrum rebuild: map not found: ' + f, /info
    return
  endif
  restore, f
  if ~obj_valid(map) then begin
    message, 'Full spectrum rebuild: no map object in ' + f, /info
    return
  endif

  refs_use = !null
  local_refs = 0b
  if size(refs_all, /tname) eq 'OBJREF' then $
    if obj_valid(refs_all[0]) then refs_use = refs_all
  if size(refs_use, /tname) ne 'OBJREF' then begin
    if ~tag_exist(ri, 'refdatapath') then begin
      obj_destroy, map
      message, 'Full spectrum rebuild: result has no refdatapath.', /info
      return
    endif
    if ri.refdatapath eq '' then begin
      obj_destroy, map
      message, 'Full spectrum rebuild: empty refdatapath.', /info
      return
    endif
    ; Beam stays on the ref maps unless the caller passed explicit overrides.
    if n_elements(a_beam) gt 0 then begin
      refs_use = gx_ref2chmp(ri.refdatapath, a_beam=a_beam, b_beam=b_beam, $
        phi_beam=phi_beam, corr_beam=corr_beam, err_msg=em, /quiet)
    endif else begin
      refs_use = gx_ref2chmp(ri.refdatapath, err_msg=em, /quiet)
    endelse
    if size(refs_use, /tname) ne 'OBJREF' then begin
      obj_destroy, map
      msg = (size(em, /tname) eq 'STRING') ? strjoin(em, ' ') : 'gx_ref2chmp failed'
      message, 'Full spectrum rebuild: ' + msg, /info
      return
    endif
    local_refs = 1b
  endif

  msk = tag_exist(ri, 'mask') ? ri.mask : 12
  spec = gx_maps2spectrum(map, refs_use, mask=msk, apply2=3, err_msg=em, $
    mod_maps=mod_maps)
  obj_destroy, map
  if ~isa(spec, 'STRUCT') then begin
    spec = !null
    if n_elements(mod_maps) gt 0 then obj_destroy, mod_maps
    if local_refs then obj_destroy, refs_use
    msg = (size(em, /tname) eq 'STRING') ? em : 'gx_maps2spectrum failed'
    message, 'Full spectrum rebuild: ' + msg, /info
    return
  endif
  if arg_present(chan_metrics) then begin
    n_all = n_elements(spec.axis)
    chan_metrics = objarr(n_all)
    for kk = 0L, n_all - 1 do begin
      if ~obj_valid(mod_maps[kk]) or ~obj_valid(refs_use[kk]) then continue
      modI = mod_maps[kk]->get(0, /map)
      obsI = refs_use[kk]->get(0, /map)
      obsIsdev = refs_use[kk]->get(1, /map)
      if keyword_set(spec.is_chan) then begin
        sub_map, obsI, obsI, ref=modI
        sub_map, obsIsdev, obsIsdev, ref=modI
        sz = size(modI.data)
        obsI = gx_rebin_map(obsI, sz[1], sz[2], /total)
        obsIsdev = gx_rebin_map(obsIsdev, sz[1], sz[2], /total)
      endif
      chan_metrics[kk] = gx_metrics_map(modI, obsI, obsIsdev, mask=msk, $
        apply2=3, /no_renorm)
    endfor
  endif
  if n_elements(mod_maps) gt 0 then obj_destroy, mod_maps
  if local_refs then obj_destroy, refs_use
end
