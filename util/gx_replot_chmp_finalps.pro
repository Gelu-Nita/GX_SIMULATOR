;+
; :Description:
;    Rewrite each cell's set_a*b*_final.ps from a saved gx_search4bestq
;    RESULT array. No rendering and no extra Q samples. Spectrum-mode
;    only; image-mode results are ignored.
;
;    Page 1: Q vs RES2 / CHI2 from ALLMETRICS.
;    Page 2: ROI spectra with 1-sigma S_sdev bars (gx_plot_chmp_spectrum).
;    Page 3+: per-channel model I maps (obs contours, ROI mask) at each
;    winning Q, 2x3 layout. Titles say (in search) / (not in search).
;
; :Params:
;    result - array of structs restored from the search .sav
;    psDir  - output directory (default: result[0].psDir)
;
; :Keywords:
;    charsize
;    levels - percentile contours on the channel-map pages (same default and
;             meaning as gx_plotbestmwmodels_ebtel)
;    _extra - /ylog to gx_plot_chmp_spectrum; /log to channel-map images
;             (passed as plot_map LOG_SCALE)
;
; :Example:
;    restore, tmpDir + path_sep() + 'result_3x3_aia_spectrum.sav'
;    gx_replot_chmp_finalps, result, psDir          ; finals only
;    gx_plotbestmwmodels_ebtel, result, psDir       ; Best of Bests only
;    gx_plotbestmwmodels_ebtel, result, psDir, /replot_final  ; both
;-
pro gx_replot_chmp_finalps, result, psDir, charsize=charsize, levels=levels, $
  refs_all=refs_all, _extra=_extra

  compile_opt idl2
  resolve_routine, 'gx_plot_chmp_chanmaps', /compile_full_file, /either
  if ~isa(result) or n_elements(result) eq 0 then return
  if ~tag_exist(result, 'search_mode') then return
  if strlowcase(strcompress(result[0].search_mode, /rem)) ne 'spectrum' then return
  if ~tag_exist(result, 'spec_axis') then return

  default, charsize, !p.charsize
  default, levels, [12, 20, 30, 50, 80]
  if n_elements(refs_all) eq 0 and isa(_extra, 'STRUCT') then $
    if tag_exist(_extra, 'refs_all') then refs_all = _extra.refs_all
  if n_elements(psDir) eq 0 then begin
    if tag_exist(result, 'psDir') then psDir = result[0].psDir $
    else psDir = curdir() + path_sep() + 'psDir'
  endif
  if ~file_test(psDir, /directory) then file_mkdir, psDir

  thisDevice = !d.name
  tvlct, rgb, /get
  loadct, 39
  cd, psDir, current=oldcwd
  set_plot, 'ps'

  for i = 0, n_elements(result) - 1 do begin
    ri = result[i]
    if tag_exist(ri, 'psfile') then $
      psname = file_basename(ri.psfile) $
    else psname = strcompress(string(ri.a, ri.b, $
      format="('set_a',g0,'b',g0,'_final.ps')"), /rem)
    filename = psDir + path_sep() + psname

    ; FSC_PSConfig Filepath() prepends the IDL cwd if FILENAME is a full
    ; path, so pass a basename here after cd into psDir.
    psObject = obj_new('FSC_PSConfig', /color, /times, /bold, filename=psname, $
      directory=psDir, xoffset=0.5, yoffset=0.25, xsize=6.4, ysize=9.5, $
      landscape=0, bits=8)
    psKeys = psObject->GetKeywords()
    obj_destroy, psObject
    device, filename=psname, _extra=psKeys

    spec_axis = ri.spec_axis
    is_chan = max(spec_axis) ge 50
    ax0 = min(spec_axis, max=ax1)
    if is_chan then $
      metrics_title = string(n_elements(spec_axis), ax0, ax1, $
        format="('ROI spectrum, ',i0,' channels (',g0,'–',g0,' A)')") $
    else $
      metrics_title = string(n_elements(spec_axis), ax0, ax1, $
        format="('ROI spectrum, ',i0,' frequencies (',g0,'–',g0,' GHz)')")

    ;----- page 1: Q search curves -----
    !p.multi = [0, 1, 2]
    !p.font = 2
    if ptr_valid(ri.allmetrics) then begin
      am = *ri.allmetrics
      q = am.q
      res2 = am.res2
      chi2 = am.chi2
      yrange = [0, max(res2, /nan)]
      plot, q, res2, psym=-4, xstyle=0, ystyle=1, xticks=4, yrange=yrange, $
        xtitle='!18Q!3', ytitle='!17 RES!S!U2!N!R!Dnorm!N!3', thick=2, $
        charsize=1.2*charsize, title=metrics_title
      oplot, ri.q_res2_best[[0, 0]], !y.crange, color=250, thick=3, $
        linesty=ri.res2_done ? 0 : 2
      oplot, ri.q_res2_range[[0, 0]], !y.crange, color=250, thick=3, linesty=1
      oplot, ri.q_res2_range[[1, 1]], !y.crange, color=250, thick=3, linesty=1
      gx_plot_label, 0.01, 0.9, string(ri.a, ri.b, format="('a=',f5.2,'; ','b=',f5.2)"), $
        charsize=charsize
      gx_plot_label, 0.01, 0.2, string(ri.res2_best, format="('RES!S!U2!N!R!Dnorm!N = ',g0)"), $
        charsize=charsize

      yrange = [0, max(chi2, /nan)]
      plot, q, chi2, psym=-4, xstyle=0, ystyle=1, xticks=4, yrange=yrange, $
        xtitle='!18Q!3', ytitle='!17 Chi!U2!N!3', thick=2, $
        charsize=1.2*charsize, title=metrics_title
      oplot, ri.q_chi2_best[[0, 0]], !y.crange, color=250, thick=3, $
        linesty=ri.chi2_done ? 0 : 2
      oplot, ri.q_chi2_range[[0, 0]], !y.crange, color=250, thick=3, linesty=1
      oplot, ri.q_chi2_range[[1, 1]], !y.crange, color=250, thick=3, linesty=1
      gx_plot_label, 0.01, 0.9, string(ri.a, ri.b, format="('a=',f5.2,'; ','b=',f5.2)"), $
        charsize=charsize
      gx_plot_label, 0.01, 0.2, string(ri.chi2_best, format="('Chi!U2!N=',g0)"), $
        charsize=charsize
    endif
    !p.font = -1

    ;----- page 2: spectra -----
    !p.multi = [0, 1, 2]
    gx_plot_chmp_spectrum, cell_res2=ri, charsize=charsize, _extra=_extra

    ;----- page 3+: stored channel maps at RES2-best Q, then CHI2-best Q -----
    for kq = 0, 1 do $
      gx_chmp_cell_chanmaps, ri, kq, spec_axis, levels=levels, $
        charsize=charsize, is_chan=is_chan, refs_all=refs_all, _extra=_extra

    device, /close
    print, 'Wrote ', filename
  endfor

  tvlct, rgb
  set_plot, thisDevice
  cd, oldcwd
end
