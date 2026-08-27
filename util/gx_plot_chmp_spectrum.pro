;+
; :Description:
;    Draw the two CHMP spectrum-mode comparison panels (res2-best Q and
;    chi2-best Q). Assumes the PS/X device is already open and !P.MULTI
;    is set for two stacked panels.
;
;    Full synthesized / reference axis: dotted lines through every channel.
;    Metric subset: diamonds (obs) and triangles (model) connected with lines.
;    Channels not used in the Q search: squares (obs) and triangles (model),
;    no connecting line. A legend is drawn in each panel.
;
; :Params:
;    spec_axis - channel (A) or frequency (GHz) vector
;    S_obs_res2, S_sdev_res2, S_mod_res2 - spectra at the RES2-best Q
;    S_obs_chi2, S_sdev_chi2, S_mod_chi2 - spectra at the CHI2-best Q
;      (if omitted, the RES2-best obs/sdev are reused)
;
; :Keywords:
;    aval, bval, q_res2_best, res2_best, q_chi2_best, chi2_best
;      EBTEL a,b heating indices (not abbreviated a=/b= — those clash with
;      a_beam and best_of_bests under IDL keyword rules). aval/bval may be
;      2-element (one value per panel) for Best of Bests
;    is_chan - set for EUV/CHAN axis labels
;    ylog - logarithmic Y axis (also inherited via _extra)
;    cell_res2 - if set, fill spectra/labels from this cell's result struct
;    cell_chi2 - optional second cell (Best of Bests CHI2 winner)
;    best_of_bests - per-panel a,b,Q labels; titles name the global winner
;    charsize
;    samp_res2, samp_chi2 - spec_allmetrics samples (full-axis *_all tags)
;    refs_all - unused (kept for call compatibility); spectra come from RESULT
;    _extra - /ylog; avoid a_beam/b_beam tags here
;-
pro gx_chmp_result_spectra, ri, spec_axis, $
  S_obs_res2, S_sdev_res2, S_mod_res2, $
  S_obs_chi2, S_sdev_chi2, S_mod_chi2, is_chan=is_chan
  compile_opt idl2
  ; Spectra at this cell's RES2-best and CHI2-best Q (same helpers as replot).
  spec_axis = ri.spec_axis
  is_chan = max(spec_axis) ge 50
  S_obs_res2 = ri.S_obs
  S_sdev_res2 = ri.S_sdev
  S_mod_res2 = ri.S_mod_res2_best
  S_obs_chi2 = ri.S_obs
  S_sdev_chi2 = ri.S_sdev
  S_mod_chi2 = ri.S_mod_chi2_best
  if ptr_valid(ri.spec_allmetrics) then begin
    sam = *ri.spec_allmetrics
    void = min(abs(sam.q - ri.q_res2_best), ir)
    void = min(abs(sam.q - ri.q_chi2_best), ic)
    S_obs_res2 = sam[ir].S_obs
    S_sdev_res2 = sam[ir].S_sdev
    S_mod_res2 = sam[ir].S_mod
    S_obs_chi2 = sam[ic].S_obs
    S_sdev_chi2 = sam[ic].S_sdev
    S_mod_chi2 = sam[ic].S_mod
  endif
end

pro gx_plot_chmp_ebars, x, y, sdev, color=color, thick=thick
  compile_opt idl2
  ; Do not use SSW ERRPLOT: it emits one clipped polyline per bar, so bars
  ; whose caps sit on the axis vanish. Draw stem + caps with /DATA instead.
  ok = where(finite(x) and finite(y) and finite(sdev) and (sdev gt 0), n)
  if n eq 0 then return
  default, color, 0
  default, thick, 3
  xr = !x.crange
  if !x.type ne 0 then xr = 10d^xr
  wid = 0.006 * abs(xr[1] - xr[0])
  for j = 0, n - 1 do begin
    i = ok[j]
    xx = x[i]
    lo = y[i] - sdev[i]
    hi = y[i] + sdev[i]
    if !y.type ne 0 then begin
      ycr = 10d^!y.crange
      lo = lo > ycr[0]
      hi = hi > ycr[0]
      if hi le lo then continue
    endif
    plots, [xx, xx], [lo, hi], /data, thick=thick, color=color, noclip=0
    plots, [xx - wid, xx + wid], [lo, lo], /data, thick=thick, color=color, noclip=0
    plots, [xx - wid, xx + wid], [hi, hi], /data, thick=thick, color=color, noclip=0
  endfor
end

pro gx_plot_chmp_spec_legend, has_ignored, charsize=charsize
  compile_opt idl2
  default, charsize, 1.0
  cs = 0.8 * ((charsize gt 0) ? charsize : 1.0)
  xw = !x.window
  yw = !y.window
  x0 = xw[0] + 0.50 * (xw[1] - xw[0])
  xl = x0 + 0.07 * (xw[1] - xw[0])
  y = yw[0] + 0.90 * (yw[1] - yw[0])
  dy = 0.065 * (yw[1] - yw[0])
  xm = 0.5 * (x0 + xl)
  if keyword_set(has_ignored) then begin
    plots, [x0, xl], [y, y], /normal, color=0, thick=1, linesty=1
    xyouts, xl + 0.012, y, 'all channels', /normal, color=0, charsize=cs, align=0
    y -= dy
  endif
  plots, [x0, xl], [y, y], /normal, color=0, thick=2
  plots, xm, y, psym=4, /normal, color=0, thick=2, symsize=1.1
  xyouts, xl + 0.012, y, 'obs (in search)', /normal, color=0, charsize=cs, align=0
  y -= dy
  if keyword_set(has_ignored) then begin
    plots, xm, y, psym=6, /normal, color=0, thick=2, symsize=1.1
    xyouts, xl + 0.012, y, 'obs (not in search)', /normal, color=0, charsize=cs, align=0
    y -= dy
  endif
  plots, [x0, xl], [y, y], /normal, color=250, thick=2
  plots, xm, y, psym=5, /normal, color=250, thick=2, symsize=1.1
  xyouts, xl + 0.012, y, 'model (in search)', /normal, color=250, charsize=cs, align=0
  y -= dy
  if keyword_set(has_ignored) then begin
    plots, xm, y, psym=5, /normal, color=250, thick=2, symsize=1.1
    xyouts, xl + 0.012, y, 'model (not in search)', /normal, color=250, charsize=cs, align=0
  endif
end

pro gx_plot_chmp_spectrum, spec_axis, $
  S_obs_res2, S_sdev_res2, S_mod_res2, $
  S_obs_chi2, S_sdev_chi2, S_mod_chi2, $
  aval=aval, bval=bval, q_res2_best=q_res2_best, res2_best=res2_best, $
  q_chi2_best=q_chi2_best, chi2_best=chi2_best, $
  is_chan=is_chan, charsize=charsize, ylog=ylog, $
  best_of_bests=best_of_bests, cell_res2=cell_res2, cell_chi2=cell_chi2, $
  samp_res2=samp_res2, samp_chi2=samp_chi2, refs_all=refs_all, _extra=_extra

  compile_opt idl2
  if isa(_extra, 'STRUCT') then begin
    if tag_exist(_extra, 'ylog') then ylog = keyword_set(_extra.ylog)
  endif
  default, charsize, !p.charsize
  if isa(cell_res2, 'STRUCT') then begin
    gx_chmp_result_spectra, cell_res2, spec_axis, $
      S_obs_res2, S_sdev_res2, S_mod_res2, $
      S_obs_chi2, S_sdev_chi2, S_mod_chi2, is_chan=is_chan
    if n_elements(aval) eq 0 then aval = cell_res2.a
    if n_elements(bval) eq 0 then bval = cell_res2.b
    if n_elements(q_res2_best) eq 0 then q_res2_best = cell_res2.q_res2_best
    if n_elements(res2_best) eq 0 then res2_best = cell_res2.res2_best
    if isa(cell_chi2, 'STRUCT') then begin
      gx_chmp_result_spectra, cell_chi2, spec_axis, $
        S_obs_r, S_sdev_r, S_mod_r, $
        S_obs_chi2, S_sdev_chi2, S_mod_chi2, is_chan=is_chan
      if n_elements(aval) lt 2 then aval = [cell_res2.a, cell_chi2.a]
      if n_elements(bval) lt 2 then bval = [cell_res2.b, cell_chi2.b]
      if n_elements(q_chi2_best) eq 0 then q_chi2_best = cell_chi2.q_chi2_best
      if n_elements(chi2_best) eq 0 then chi2_best = cell_chi2.chi2_best
    endif else begin
      if n_elements(q_chi2_best) eq 0 then q_chi2_best = cell_res2.q_chi2_best
      if n_elements(chi2_best) eq 0 then chi2_best = cell_res2.chi2_best
    endelse
  endif
  if n_elements(S_obs_chi2) eq 0 then S_obs_chi2 = S_obs_res2
  if n_elements(S_sdev_chi2) eq 0 then S_sdev_chi2 = S_sdev_res2
  if n_elements(S_mod_chi2) eq 0 then S_mod_chi2 = S_mod_res2

  ; Use spectra stored on the result. Prefer spec_allmetrics *_all; else
  ; top-level S_*_all / spec_axis_all; else the search-subset vectors.
  if isa(samp_res2, 'STRUCT') then if tag_exist(samp_res2, 'spec_axis_all') then begin
    if n_elements(samp_res2.spec_axis_all) gt 0 then $
      if total(finite(samp_res2.spec_axis_all)) gt 0 then begin
        axis_all_r = samp_res2.spec_axis_all
        S_obs_all_r = samp_res2.S_obs_all
        S_sdev_all_r = samp_res2.S_sdev_all
        S_mod_all_r = samp_res2.S_mod_all
      endif
  endif
  if isa(samp_chi2, 'STRUCT') then if tag_exist(samp_chi2, 'spec_axis_all') then begin
    if n_elements(samp_chi2.spec_axis_all) gt 0 then $
      if total(finite(samp_chi2.spec_axis_all)) gt 0 then begin
        axis_all_c = samp_chi2.spec_axis_all
        S_obs_all_c = samp_chi2.S_obs_all
        S_sdev_all_c = samp_chi2.S_sdev_all
        S_mod_all_c = samp_chi2.S_mod_all
      endif
  endif
  if n_elements(axis_all_r) eq 0 and isa(cell_res2, 'STRUCT') then begin
    if ptr_valid(cell_res2.spec_allmetrics) then begin
      sam = *cell_res2.spec_allmetrics
      if tag_exist(sam, 'spec_axis_all') then begin
        void = min(abs(sam.q - cell_res2.q_res2_best), ir)
        if n_elements(sam[ir].spec_axis_all) gt 0 then $
          if total(finite(sam[ir].spec_axis_all)) gt 0 then begin
            axis_all_r = sam[ir].spec_axis_all
            S_obs_all_r = sam[ir].S_obs_all
            S_sdev_all_r = sam[ir].S_sdev_all
            S_mod_all_r = sam[ir].S_mod_all
          endif
      endif
    endif
    if n_elements(axis_all_r) eq 0 then if tag_exist(cell_res2, 'spec_axis_all') then begin
      if n_elements(cell_res2.spec_axis_all) gt 0 then $
        if total(finite(cell_res2.spec_axis_all)) gt 0 then begin
          axis_all_r = cell_res2.spec_axis_all
          S_obs_all_r = cell_res2.S_obs_all
          S_sdev_all_r = cell_res2.S_sdev_all
          S_mod_all_r = cell_res2.S_mod_res2_best_all
        endif
    endif
  endif
  if n_elements(axis_all_c) eq 0 and isa(cell_chi2, 'STRUCT') then begin
    if ptr_valid(cell_chi2.spec_allmetrics) then begin
      samc = *cell_chi2.spec_allmetrics
      if tag_exist(samc, 'spec_axis_all') then begin
        void = min(abs(samc.q - cell_chi2.q_chi2_best), ic)
        if n_elements(samc[ic].spec_axis_all) gt 0 then $
          if total(finite(samc[ic].spec_axis_all)) gt 0 then begin
            axis_all_c = samc[ic].spec_axis_all
            S_obs_all_c = samc[ic].S_obs_all
            S_sdev_all_c = samc[ic].S_sdev_all
            S_mod_all_c = samc[ic].S_mod_all
          endif
      endif
    endif
    if n_elements(axis_all_c) eq 0 then if tag_exist(cell_chi2, 'spec_axis_all') then begin
      if n_elements(cell_chi2.spec_axis_all) gt 0 then $
        if total(finite(cell_chi2.spec_axis_all)) gt 0 then begin
          axis_all_c = cell_chi2.spec_axis_all
          S_obs_all_c = cell_chi2.S_obs_all
          S_sdev_all_c = cell_chi2.S_sdev_all
          S_mod_all_c = cell_chi2.S_mod_chi2_best_all
        endif
    endif
  endif
  if n_elements(axis_all_c) eq 0 and isa(cell_res2, 'STRUCT') and $
    (isa(cell_chi2, 'STRUCT') eq 0) then begin
    if ptr_valid(cell_res2.spec_allmetrics) then begin
      samc = *cell_res2.spec_allmetrics
      if tag_exist(samc, 'spec_axis_all') then begin
        void = min(abs(samc.q - cell_res2.q_chi2_best), ic)
        if n_elements(samc[ic].spec_axis_all) gt 0 then $
          if total(finite(samc[ic].spec_axis_all)) gt 0 then begin
            axis_all_c = samc[ic].spec_axis_all
            S_obs_all_c = samc[ic].S_obs_all
            S_sdev_all_c = samc[ic].S_sdev_all
            S_mod_all_c = samc[ic].S_mod_all
          endif
      endif
    endif
    if n_elements(axis_all_c) eq 0 then if tag_exist(cell_res2, 'spec_axis_all') then begin
      if n_elements(cell_res2.spec_axis_all) gt 0 then $
        if total(finite(cell_res2.spec_axis_all)) gt 0 then begin
          axis_all_c = cell_res2.spec_axis_all
          S_obs_all_c = cell_res2.S_obs_all
          S_sdev_all_c = cell_res2.S_sdev_all
          S_mod_all_c = cell_res2.S_mod_chi2_best_all
        endif
    endif
  endif
  if n_elements(axis_all_r) eq 0 then begin
    axis_all_r = spec_axis
    S_obs_all_r = S_obs_res2
    S_sdev_all_r = S_sdev_res2
    S_mod_all_r = S_mod_res2
  endif
  if n_elements(axis_all_c) eq 0 then begin
    axis_all_c = spec_axis
    S_obs_all_c = S_obs_chi2
    S_sdev_all_c = S_sdev_chi2
    S_mod_all_c = S_mod_chi2
  endif

  xtit = keyword_set(is_chan) ? 'Channel' : 'Frequency (GHz)'
  ytit = keyword_set(is_chan) ? 'ROI integral' : 'ROI flux [sfu]'
  ylo = [S_obs_all_r - S_sdev_all_r, S_obs_all_c - S_sdev_all_c, S_mod_all_r, S_mod_all_c]
  yhi = [S_obs_all_r + S_sdev_all_r, S_obs_all_c + S_sdev_all_c, S_mod_all_r, S_mod_all_c]
  if keyword_set(ylog) then begin
    ypos = [S_obs_all_r, S_obs_all_c, S_mod_all_r, S_mod_all_c, yhi]
    ypos = ypos[where(finite(ypos) and (ypos gt 0), npos)]
    if npos eq 0 then begin
      message, 'ylog: no positive spectral values; using linear axis.', /info
      ylog = 0
    endif else begin
      yrange = [min(ypos), max(ypos)]
      ylo_pos = ylo[where(finite(ylo) and (ylo gt 0), nlo)]
      if nlo gt 0 then yrange[0] = min([yrange[0], min(ylo_pos)])
      yrange = [yrange[0] / 1.5d, yrange[1] * 1.5d]
    endelse
  endif
  if ~keyword_set(ylog) then begin
    yrange = [min(ylo, /nan), max(yhi, /nan)]
    if ~finite(yrange[0]) or ~finite(yrange[1]) or (yrange[0] eq yrange[1]) then $
      yrange = yrange + [-1, 1]
    dy = yrange[1] - yrange[0]
    yrange = yrange + [-0.08, 0.08] * dy
  endif

  nbar = total(finite(S_sdev_res2) and (S_sdev_res2 gt 0))
  if nbar eq 0 then $
    message, 'No finite S_sdev > 0; spectrum error bars will be omitted.', /info

  n_ab = n_elements(aval) < n_elements(bval)
  for ip = 0, 1 do begin
    ia = (n_ab gt 1) ? ip : 0
    aa = (n_ab gt 0) ? aval[ia] : 0d
    bb = (n_elements(bval) gt ia) ? bval[ia] : ((n_elements(bval) gt 0) ? bval[0] : 0d)
    abq_line = string(aa, bb, format="('a=',f0.2,'  b=',f0.2)")
    res2_line = string(q_res2_best, res2_best, $
      format="('Q!Dres2!N=',g0,'  RES!S!U2!N=',g0)")
    chi2_line = string(q_chi2_best, chi2_best, $
      format="('Q!Dchi2!N=',g0,'  Chi!U2!N=',g0)")
    if ip eq 0 then begin
      axis_all = axis_all_r
      Sobs_all = S_obs_all_r
      Ssdev_all = S_sdev_all_r
      Smod_all = S_mod_all_r
      tit = 'Spectrum (res2-best Q)'
      if keyword_set(best_of_bests) then $
        tit = string(aa, bb, format="('Best of Bests RES!U2!N (a=',f0.2,', b=',f0.2,')')")
    endif else begin
      axis_all = axis_all_c
      Sobs_all = S_obs_all_c
      Ssdev_all = S_sdev_all_c
      Smod_all = S_mod_all_c
      tit = 'Spectrum (chi2-best Q)'
      if keyword_set(best_of_bests) then $
        tit = string(aa, bb, format="('Best of Bests CHI!U2!N (a=',f0.2,', b=',f0.2,')')")
    endelse
    gx_chmp_axis_selmask, axis_all, spec_axis, sel
    srt = sort(axis_all)
    xa = axis_all[srt]
    yo = Sobs_all[srt]
    ym = Smod_all[srt]
    ys = Ssdev_all[srt]
    sm = sel[srt]
    i_sel = where(sm, ns)
    i_ign = where(sm eq 0, ni)
    xr = minmax(xa)
    if xr[0] eq xr[1] then xr = xr + [-1, 1]
    dxr = xr[1] - xr[0]
    plot, xa, yo, /nodata, xtitle=xtit, ytitle=ytit, title=tit, $
      yrange=yrange, ystyle=1, xrange=xr + [-0.04, 0.04] * dxr, xstyle=1, $
      ylog=keyword_set(ylog), charsize=1.2 * charsize, thick=2
    ; Full synthesized/data spectrum, then metric subset, then ignored symbols.
    oplot, xa, yo, thick=1, color=0, linesty=1
    oplot, xa, ym, thick=1, color=250, linesty=1
    if ni gt 0 then begin
      oplot, xa[i_ign], yo[i_ign], psym=6, color=0, thick=2, symsize=1.3
      oplot, xa[i_ign], ym[i_ign], psym=5, color=250, thick=2, symsize=1.3
      gx_plot_chmp_ebars, xa[i_ign], yo[i_ign], ys[i_ign], color=0, thick=2
    endif
    if ns gt 0 then begin
      oplot, xa[i_sel], yo[i_sel], psym=-4, color=0, thick=2, symsize=1.4
      gx_plot_chmp_ebars, xa[i_sel], yo[i_sel], ys[i_sel], color=0, thick=3
      oplot, xa[i_sel], ym[i_sel], psym=-5, color=250, thick=2, symsize=1.4
    endif
    gx_plot_chmp_spec_legend, ni gt 0, charsize=charsize
    gx_plot_label, 0.01, 0.20, abq_line, charsize=charsize, ylog=keyword_set(ylog)
    if keyword_set(best_of_bests) then begin
      if ip eq 0 then $
        gx_plot_label, 0.01, 0.12, res2_line, charsize=charsize, ylog=keyword_set(ylog) $
      else $
        gx_plot_label, 0.01, 0.12, chi2_line, charsize=charsize, ylog=keyword_set(ylog)
    endif else begin
      gx_plot_label, 0.01, 0.12, res2_line, charsize=charsize, ylog=keyword_set(ylog)
      gx_plot_label, 0.01, 0.04, chi2_line, charsize=charsize, ylog=keyword_set(ylog)
    endelse
  endfor
end
