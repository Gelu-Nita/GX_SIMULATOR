;+
; Color key for model / data / ROI-threshold contours. Same data-coordinate
; placement as the R=/Q0= map annotations (those already show). Hershey font
; plus a black halo so the words survive both dark linear and bright /log
; images. Compiled with gx_plot_chmp_chanmaps so Best of Bests does not
; depend on a second file.
;-
pro gx_plot_chmp_contour_legend, charsize=charsize, mask=mask
  compile_opt idl2
  cs = 1.2
  if n_elements(charsize) eq 1 then if charsize gt 0 then cs = charsize
  oldfont = !p.font
  !p.font = -1
  xr = !x.crange
  yr = !y.crange
  dx = max(xr, min=xmin) - xmin
  dy = max(yr, min=ymin) - ymin
  x0 = xmin + 0.04 * dx
  x1 = xmin + 0.16 * dx
  xt = xmin + 0.18 * dx
  ; Mid-left: above Mask_Npix (~10%) and below (a; b) (~60%).
  items = ['model', 'data']
  lcol = [0, 200]
  tcol = [255, 200]
  yfr = [0.40, 0.32]
  if keyword_set(mask) then begin
    items = [items, 'threshold']
    lcol = [lcol, 100]
    tcol = [tcol, 100]
    yfr = [yfr, 0.24]
  endif
  for i = 0, n_elements(items) - 1 do begin
    y = ymin + yfr[i] * dy
    plots, [x0, x1], [y, y], color=lcol[i], thick=4, noclip=1
    xyouts, xt, y, ' ' + items[i], color=0, charsize=cs, charthick=4, noclip=1
    xyouts, xt, y, ' ' + items[i], color=tcol[i], charsize=cs, charthick=1, $
      noclip=1
  endfor
  !p.font = oldfont
end

;+
; :Description:
;    2x3 gallery of per-channel model I maps for one Q sample: observed
;    percentile contours overlaid, plus the ROI mask if it is a 2-D array.
;    Titles include the axis value and "(in search)" / "(not in search)"
;    relative to SPEC_AXIS (the metric subset). Starts a new page.
;    Extra channels wrap to further 2x3 pages.
;
; :Params:
;    cim - objarr of gx_metrics_map objects, parallel to AXIS_ALL
;    axis_all - freq [GHz] or chan [A] for each CIM entry
;    spec_axis - channels used in the spectral Q search
;
; :Keywords:
;    header - prepended to each title (e.g. RES2 Q=...)
;    levels, charsize, is_chan
;    _extra - /log is translated to plot_map's LOG_SCALE (extra tag LOG
;             does not match LOG_SCALE). /ylog is not forwarded.
;-
pro gx_chmp_cell_chanmaps, ri, which, spec_axis, levels=levels, charsize=charsize, $
  is_chan=is_chan, refs_all=refs_all, _extra=_extra

  compile_opt idl2
  if ~isa(ri, 'STRUCT') then return
  qv = (which ne 0) ? ri.q_chi2_best : ri.q_res2_best
  hdr = (which ne 0) ? $
    string(qv, format="('CHI!U2!N Q=',g0)") : $
    string(qv, format="('RES!U2!N Q=',g0)")
  cim = (which ne 0) ? ri.chi2_best_metrics : ri.res2_best_metrics
  ax = spec_axis
  if ptr_valid(ri.spec_allmetrics) then begin
    sam = *ri.spec_allmetrics
    void = min(abs(double(sam.q) - double(qv)), iq)
    if tag_exist(sam, 'channel_image_metrics') then begin
      cimk = sam[iq].channel_image_metrics
      if n_elements(cimk) gt 0 then cim = cimk
    endif
    if tag_exist(sam, 'spec_axis_all') then begin
      ax_all = sam[iq].spec_axis_all
      if n_elements(cim) eq n_elements(ax_all) then ax = ax_all
    endif
  endif
  have_cim = 0b
  for ic = 0L, n_elements(cim) - 1 do $
    if obj_valid(cim[ic]) then have_cim = 1b
  if ~have_cim then begin
    gx_chmp_spectrum_from_map, ri, which, specf, chan_metrics=cim2, refs_all=refs_all
    if isa(specf, 'STRUCT') then if n_elements(cim2) gt 0 then begin
      cim = cim2
      ax = specf.axis
    endif
  endif
  gx_plot_chmp_chanmaps, cim, ax, spec_axis, header=hdr, levels=levels, $
    charsize=charsize, is_chan=is_chan, _extra=_extra
end

pro gx_plot_chmp_chanmaps, cim, axis_all, spec_axis, $
  header=header, levels=levels, charsize=charsize, is_chan=is_chan, _extra=_extra

  compile_opt idl2
  default, header, ''
  default, levels, [12, 20, 30, 50, 80]
  default, charsize, !p.charsize
  ; plot_map's keyword is LOG_SCALE. Tags in _EXTRA are not abbreviated, so
  ; {log:1} never enables log scaling — pass LOG_SCALE by name.
  want_log = 0b
  if isa(_extra, 'STRUCT') then begin
    if tag_exist(_extra, 'log_scale') then want_log = keyword_set(_extra.log_scale) $
    else if tag_exist(_extra, 'log') then want_log = keyword_set(_extra.log)
  endif
  n = n_elements(cim)
  if n eq 0 then return
  if n_elements(axis_all) ne n then begin
    if n_elements(spec_axis) eq n then axis_all = spec_axis $
    else if n_elements(axis_all) gt n then axis_all = axis_all[0:n-1] $
    else axis_all = dindgen(n)
  endif
  gx_chmp_axis_selmask, axis_all, spec_axis, sel
  ; Drop invalid map objects so the 2x3 layout has no blank panels.
  good = where(obj_valid(cim), ng)
  if ng eq 0 then return
  if ng lt n then begin
    cim = cim[good]
    axis_all = axis_all[good]
    sel = sel[good]
    n = ng
  endif
  !p.multi = [0, 2, 3, 0, 1]
  !p.font = -1
  for kk = 0, n - 1 do begin
    objm = cim[kk]
    if ~obj_valid(objm) then continue
    modI = objm->get(0, /map)
    obsI = objm->get(1, /map)
    if keyword_set(is_chan) then $
      axlab = string(axis_all[kk], format="(g0,' A')") $
    else $
      axlab = string(axis_all[kk], format="(g0,' GHz')")
    in_s = (kk lt n_elements(sel)) && (sel[kk] ne 0)
    srch = in_s ? 'in search' : 'not in search'
    tit = strtrim(header, 2)
    if tit ne '' then tit += '  '
    tit += axlab + ' (' + srch + ')'
    plot_map, modI, charsize=charsize, title=tit, log_scale=want_log
    plot_map, modI, /over, levels=levels, /perc, color=0, thick=3
    plot_map, obsI, /over, levels=levels, /perc, color=200, thick=3
    drew_mask = 0b
    nmap = objm->get(/count)
    for im = 0, nmap - 1 do begin
      mm = objm->get(im, /map)
      if ~valid_map(mm) then continue
      if ~tag_exist(mm, 'uname') then continue
      if strupcase(strtrim(mm.uname, 2)) ne 'ROI:NPIX' then continue
      if n_elements(mm.data) eq n_elements(modI.data) then begin
        plot_map, mm, /over, levels=1, color=100, thick=4
        drew_mask = 1b
      endif
      break
    endfor
    gx_plot_chmp_contour_legend, charsize=charsize, mask=drew_mask
  endfor
end
