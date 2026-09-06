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
; Pull channel-map metrics for one cell at the RES2 or CHI2 winning Q
; and draw Data | Model | residual (gx_plot_chmp_chanmaps).
;-
pro gx_chmp_cell_chanmaps, ri, which, spec_axis, levels=levels, charsize=charsize, $
  is_chan=is_chan, refs_all=refs_all, _extra=_extra

  compile_opt idl2
  if ~isa(ri, 'STRUCT') then return
  qv = (which ne 0) ? ri.q_chi2_best : ri.q_res2_best
  ; Own-metric fallback only. Companion comes from smetrics/allmetrics at qv
  ; (chi2_best is CHI2 at q_chi2, not at q_res2, and vice versa).
  r2q = !values.d_nan
  c2q = !values.d_nan
  if which eq 0 then begin
    if tag_exist(ri, 'res2_best') then r2q = ri.res2_best
  endif else begin
    if tag_exist(ri, 'chi2_best') then c2q = ri.chi2_best
  endelse
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
    if tag_exist(sam, 'smetrics') then begin
      sm = sam[iq].smetrics
      if tag_exist(sm, 'res2_norm') then r2q = sm.res2_norm
      if tag_exist(sm, 'chi2') then c2q = sm.chi2
    endif
  endif else if ptr_valid(ri.allmetrics) then begin
    am = *ri.allmetrics
    void = min(abs(double(am.q) - double(qv)), iq)
    r2q = am.res2[iq]
    c2q = am.chi2[iq]
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
  gx_plot_chmp_chanmaps, cim, ax, spec_axis, q=qv, res2=r2q, chi2=c2q, $
    min_chi2=(which ne 0), levels=levels, charsize=charsize, is_chan=is_chan, $
    _extra=_extra
end

; Overlay model (black) and data (color 200) percentile contours, plus ROI.
; /roi_only: just the ROI outline in black (normalized-residual panel).
pro gx_chmp_chanmap_contours, objm, modI, obsI, levels=levels, mask=drew_mask, $
  roi_only=roi_only
  compile_opt idl2, hidden
  default, levels, [20, 50, 80]
  if ~keyword_set(roi_only) then begin
    plot_map, modI, /over, levels=levels, /perc, color=0, thick=3
    plot_map, obsI, /over, levels=levels, /perc, color=200, thick=3
  endif
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
end

; Magenta-white-green divergent table. Index 0 is black, 255 is white.
; Data indices 1-254 run -1 (magenta) through 0 (white) to +1 (green).
pro gx_chmp_divergent_rgb, r, g, b
  compile_opt idl2, hidden
  r = bytarr(256)
  g = bytarr(256)
  b = bytarr(256)
  r[255] = 255
  g[255] = 255
  b[255] = 255
  ; ColorBrewer PiYG endpoints: #8e0152 and #276419.
  rm = 142
  gm = 1
  bm = 82
  rg = 39
  gg = 100
  bg = 25
  for i = 0, 253 do begin
    t = i / 253d
    if t le 0.5 then begin
      u = t / 0.5d
      r[i + 1] = byte(round(rm + u * (255 - rm)))
      g[i + 1] = byte(round(gm + u * (255 - gm)))
      b[i + 1] = byte(round(bm + u * (255 - bm)))
    endif else begin
      u = (t - 0.5d) / 0.5d
      r[i + 1] = byte(round(255 + u * (rg - 255)))
      g[i + 1] = byte(round(255 + u * (gg - 255)))
      b[i + 1] = byte(round(255 + u * (bg - 255)))
    endelse
  endfor
end

; (Data-Model)/(Data+Model) in [-1, 1]. Same WCS as obsI.
pro gx_chmp_normres_map, obsI, modI, rmap
  compile_opt idl2, hidden
  rmap = !null
  if ~valid_map(obsI) or ~valid_map(modI) then return
  d = double(obsI.data)
  m = double(modI.data)
  if ~array_equal(size(d, /dim), size(m, /dim)) then return
  den = d + m
  rd = make_array(dimension=size(d, /dim), /double, value=0d)
  ok = where(finite(d) and finite(m) and (abs(den) gt 0d), nok)
  if nok gt 0 then rd[ok] = (d[ok] - m[ok]) / den[ok]
  rd = -1d > rd < 1d
  rmap = obsI
  rmap.data = rd
  rmap.id = 'Norm. residual'
  gx_chmp_divergent_rgb, rr, gg, bb
  add_prop, rmap, red=rr, /replace
  add_prop, rmap, green=gg, /replace
  add_prop, rmap, blue=bb, /replace
end

; Horizontal colorbar above a map. TV + xyouts only: no plot_map /cbar
; (SSW PS bug) and no PLOT, so !p.multi is left alone. Sits high in the
; top margin so plot_map's title still has a slot.
pro gx_chmp_map_cbar, vmin, vmax, map=map, charsize=charsize, bottom=bottom, $
  ncolors=ncolors, log=log
  compile_opt idl2, hidden
  if n_elements(vmin) eq 0 or n_elements(vmax) eq 0 then return
  lo = float(vmin[0])
  hi = float(vmax[0])
  if ~finite(lo) or ~finite(hi) then return
  if hi eq lo then hi = lo + (abs(lo) gt 0 ? 0.01 * abs(lo) : 1.)
  default, bottom, 0
  default, ncolors, !d.table_size < 256
  x0 = float(!x.window[0])
  x1 = float(!x.window[1])
  y1 = float(!y.window[1])
  if (x1 - x0) le 0 then return
  tvlct, sav_r, sav_g, sav_b, /get
  if valid_map(map) then begin
    have = 0b
    if tag_exist(map, 'red') then if tag_exist(map, 'green') then $
      if tag_exist(map, 'blue') then have = 1b
    if have then if n_elements(map.red) ge 256 then $
      tvlct, map.red, map.green, map.blue
  endif
  yb0 = y1 + 0.032
  yb1 = yb0 + 0.010
  nc = long(ncolors) > 2
  bar = reform(bindgen(nc) + byte(bottom), nc, 1)
  tv, bar, x0, yb0, xsize=(x1 - x0), ysize=(yb1 - yb0) > 1d-4, /normal
  plots, [x0, x1, x1, x0, x0], [yb0, yb0, yb1, yb1, yb0], /normal, color=0, thick=1
  cs = 0.45
  if n_elements(charsize) eq 1 then if charsize gt 0 then cs = 0.45 * charsize
  yl = yb1 + 0.001
  if keyword_set(log) and (lo gt 0) and (hi gt 0) then $
    mid = 10d^(0.5d * (alog10(lo) + alog10(hi))) $
  else $
    mid = 0.5 * (lo + hi)
  xyouts, x0, yl, strtrim(string(lo, format='(g0)'), 2), /normal, charsize=cs, $
    align=0.0, color=0
  xyouts, 0.5 * (x0 + x1), yl, strtrim(string(mid, format='(g0)'), 2), /normal, $
    charsize=cs, align=0.5, color=0
  xyouts, x1, yl, strtrim(string(hi, format='(g0)'), 2), /normal, charsize=cs, $
    align=1.0, color=0
  tvlct, sav_r, sav_g, sav_b
end

; Display range matching plot_map (positive-only if /log).
pro gx_chmp_plot_drange, map, vmin, vmax, log=log
  compile_opt idl2, hidden
  vmin = !values.f_nan
  vmax = !values.f_nan
  if ~valid_map(map) then return
  pic = map.data
  if keyword_set(log) then begin
    ok = where(finite(pic) and (pic gt 0), n)
    if n eq 0 then return
    vmin = min(pic[ok], max=vmax, /nan)
  endif else begin
    vmin = min(pic, max=vmax, /nan)
  endelse
end

; Q, then the minimized metric, then the other metric at this Q in
; parentheses (context, not the quantity that was minimized).
pro gx_chmp_chanmap_qleg, q, res2, chi2, charsize=charsize, min_chi2=min_chi2, $
  extra_top=extra_top
  compile_opt idl2, hidden
  items = !null
  if n_elements(extra_top) gt 0 then if strlen(strtrim(extra_top[0], 2)) gt 0 then $
    items = [items, extra_top[0]]
  if n_elements(q) gt 0 then if finite(q[0]) then $
    items = [items, string(q[0], format="('Q=',g0)")]
  if keyword_set(min_chi2) then begin
    if n_elements(chi2) gt 0 then if finite(chi2[0]) then $
      items = [items, string(chi2[0], format="('Chi!U2!N=',g0)")]
    if n_elements(res2) gt 0 then if finite(res2[0]) then $
      items = [items, string(res2[0], format="('(RES!S!U2!N=',g0,')')")]
  endif else begin
    if n_elements(res2) gt 0 then if finite(res2[0]) then $
      items = [items, string(res2[0], format="('RES!S!U2!N=',g0)")]
    if n_elements(chi2) gt 0 then if finite(chi2[0]) then $
      items = [items, string(chi2[0], format="('(Chi!U2!N=',g0,')')")]
  endelse
  if n_elements(items) eq 0 then return
  cs = 0.65
  if n_elements(charsize) eq 1 then if charsize gt 0 then cs = 0.65 * charsize
  gx_chmp_al_legend, items, /top, /left, charsize=cs, spacing=1.25 * cs, $
    box=1, back='grey'
end

; Small grey contour key, one per map panel.
pro gx_chmp_chanmap_key, drew_mask, charsize=charsize
  compile_opt idl2, hidden
  cs = 0.55
  if n_elements(charsize) eq 1 then if charsize gt 0 then cs = 0.55 * charsize
  litems = ['model', 'data']
  lcol = [0, 200]
  if keyword_set(drew_mask) then begin
    litems = [litems, 'ROI']
    lcol = [lcol, 100]
  endif
  n = n_elements(litems)
  gx_chmp_al_legend, litems, /bottom, /left, charsize=cs, spacing=1.25 * cs, $
    box=1, back='grey', psym=intarr(n), linestyle=intarr(n), $
    colors=byte(lcol), textcolors=byte(lcol)
end

; Per-channel Data | Model | normalized residual. Same percentile contours
; on Data and Model; residual is (D-M)/(D+M) in [-1, 1] with a magenta-white-
; green table. Up to 3 channels per page on a 3x3 grid.
; Several channels: column-major — each column is one channel (Data, Model,
; Residual top to bottom). One channel: row-major — Data | Model | Residual
; on the first row (two rows empty). Short titles (Data/Model/Residual +
; channel + in-search). Top-left legend: Q, minimized metric, then the other
; metric at this Q in parentheses (header= is ignored). /min_chi2 marks a
; CHI2-minimized page.
pro gx_plot_chmp_chanmaps, cim, axis_all, spec_axis, $
  header=header, levels=levels, charsize=charsize, is_chan=is_chan, $
  q=q, res2=res2, chi2=chi2, min_chi2=min_chi2, _extra=_extra

  compile_opt idl2
  default, header, ''
  default, charsize, !p.charsize
  if n_elements(q) eq 0 and isa(_extra, 'STRUCT') then $
    if tag_exist(_extra, 'q') then q = _extra.q
  if n_elements(res2) eq 0 and isa(_extra, 'STRUCT') then $
    if tag_exist(_extra, 'res2') then res2 = _extra.res2
  if n_elements(chi2) eq 0 and isa(_extra, 'STRUCT') then $
    if tag_exist(_extra, 'chi2') then chi2 = _extra.chi2
  if n_elements(min_chi2) eq 0 and isa(_extra, 'STRUCT') then $
    if tag_exist(_extra, 'min_chi2') then min_chi2 = _extra.min_chi2
  if n_elements(levels) eq 0 and isa(_extra, 'STRUCT') then $
    if tag_exist(_extra, 'levels') then levels = _extra.levels
  default, levels, [20, 50, 80]
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
  good = where(obj_valid(cim), ng)
  if ng eq 0 then return
  if ng lt n then begin
    cim = cim[good]
    axis_all = axis_all[good]
    sel = sel[good]
    n = ng
  endif
  !p.font = -1
  ip = 0L
  while ip lt n do begin
    nthis = (n - ip) < 3
    ; One channel: fill the first row. Several: one channel per column.
    if nthis eq 1 then !p.multi = [0, 3, 3, 0, 1] $
    else !p.multi = [0, 3, 3, 0, 0]
    for jthis = 0, nthis - 1 do begin
    kk = ip + jthis
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
    obs_id = tag_exist(obsI, 'id') ? strtrim(string(obsI.id), 2) : ''
    if obs_id eq '' then obs_id = axlab
    dtitle = 'Data: ' + obs_id + '  (' + srch + ')'
    mtitle = 'Model: ' + axlab + '  (' + srch + ')'
    rtitle = 'Normalized Residual: (Data-Model)/(Data+Model)'
    plot_map, obsI, charsize=charsize, title=dtitle, log_scale=want_log, $
      ymargin=[4, 8]
    gx_chmp_chanmap_contours, objm, modI, obsI, levels=levels, mask=drew_mask
    gx_chmp_plot_drange, obsI, dlo, dhi, log=want_log
    gx_chmp_map_cbar, dlo, dhi, map=obsI, charsize=charsize, log=want_log
    gx_chmp_chanmap_qleg, q, res2, chi2, charsize=charsize, min_chi2=min_chi2
    gx_chmp_chanmap_key, drew_mask, charsize=charsize
    plot_map, modI, charsize=charsize, title=mtitle, log_scale=want_log, $
      ymargin=[4, 8]
    gx_chmp_chanmap_contours, objm, modI, obsI, levels=levels, mask=drew_mask
    gx_chmp_plot_drange, modI, mlo, mhi, log=want_log
    gx_chmp_map_cbar, mlo, mhi, map=modI, charsize=charsize, log=want_log
    gx_chmp_chanmap_qleg, q, res2, chi2, charsize=charsize, min_chi2=min_chi2
    gx_chmp_chanmap_key, drew_mask, charsize=charsize
    gx_chmp_normres_map, obsI, modI, rmap
    if valid_map(rmap) then begin
      plot_map, rmap, charsize=charsize, title=rtitle, dmin=-1., dmax=1., $
        bottom=1, top=254, ymargin=[4, 8]
      gx_chmp_chanmap_contours, objm, modI, obsI, levels=levels, /roi_only, $
        mask=drew_mask
      gx_chmp_map_cbar, -1., 1., map=rmap, charsize=charsize, bottom=1, ncolors=254
      gx_chmp_chanmap_qleg, q, res2, chi2, charsize=charsize, min_chi2=min_chi2
    endif else begin
      plot, [0, 1], [0, 1], /nodata, title=rtitle, charsize=charsize
    endelse
    endfor
    ip += nthis
  endwhile
end

