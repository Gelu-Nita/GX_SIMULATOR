;+
; Two stacked Q-search panels (RES2 then CHI2 vs Q) for one CHMP result cell.
; Consumes two !P.MULTI slots (call twice with column-major 2x2 for Best of Bests).
;-
pro gx_plot_chmp_qsearch, ri, charsize=charsize, header=header

  compile_opt idl2
  default, charsize, !p.charsize
  default, header, ''
  if ~isa(ri, 'STRUCT') then return
  if ~ptr_valid(ri.allmetrics) then begin
    plot, [0, 1], [0, 1], /nodata, title=header, charsize=charsize
    gx_plot_label, 0.1, 0.5, 'No allmetrics', charsize=charsize
    plot, [0, 1], [0, 1], /nodata, charsize=charsize
    return
  endif

  am = *ri.allmetrics
  q = am.q
  res2 = am.res2
  chi2 = am.chi2
  spec_mode = tag_exist(ri, 'search_mode') && $
    strlowcase(strcompress(ri.search_mode, /rem)) eq 'spectrum'
  ytit_r2 = spec_mode ? '!17 RES!S!U2!N!R!Dnorm!N!3' : '!17 RES!U2!N!3'
  ab = string(ri.a, ri.b, format="('a=',f0.2,', b=',f0.2)")
  if header ne '' then tit0 = header + '  ' + ab else tit0 = ab

  yrange = [0, max(res2, /nan)]
  plot, q, res2, psym=-4, xstyle=0, ystyle=1, xticks=4, yrange=yrange, $
    xtitle='!18Q!3', ytitle=ytit_r2, thick=2, $
    charsize=1.2 * charsize, title=tit0
  oplot, ri.q_res2_best[[0, 0]], !y.crange, color=250, thick=3, $
    linesty=ri.res2_done ? 0 : 2
  oplot, ri.q_res2_range[[0, 0]], !y.crange, color=250, thick=3, linesty=1
  oplot, ri.q_res2_range[[1, 1]], !y.crange, color=250, thick=3, linesty=1
  gx_plot_label, 0.02, 0.90, string(ri.res2_best, format="('RES!S!U2!N=',g0)"), $
    charsize=charsize
  gx_plot_label, 0.02, 0.78, string(ri.q_res2_best, format="('Q!Dres2!N=',g0)"), $
    charsize=charsize

  yrange = [0, max(chi2, /nan)]
  plot, q, chi2, psym=-4, xstyle=0, ystyle=1, xticks=4, yrange=yrange, $
    xtitle='!18Q!3', ytitle='!17 Chi!U2!N!3', thick=2, $
    charsize=1.2 * charsize, title=tit0
  oplot, ri.q_chi2_best[[0, 0]], !y.crange, color=250, thick=3, $
    linesty=ri.chi2_done ? 0 : 2
  oplot, ri.q_chi2_range[[0, 0]], !y.crange, color=250, thick=3, linesty=1
  oplot, ri.q_chi2_range[[1, 1]], !y.crange, color=250, thick=3, linesty=1
  gx_plot_label, 0.02, 0.90, string(ri.chi2_best, format="('Chi!U2!N=',g0)"), $
    charsize=charsize
  gx_plot_label, 0.02, 0.78, string(ri.q_chi2_best, format="('Q!Dchi2!N=',g0)"), $
    charsize=charsize
end
