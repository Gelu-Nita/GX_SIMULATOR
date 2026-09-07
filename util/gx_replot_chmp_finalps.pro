;+
; Compatibility wrapper: rewrite cell set_a*b*_final.ps from a saved RESULT
; (image or spectrum). Prefer gx_plotbestchmpmodels_ebtel, /plot_all, plot_best=0.
;
; Default of the top-level plotter is Best of Bests only; this wrapper
; writes the cell files and skips Best of Bests.
;-
pro gx_replot_chmp_finalps, result, psDir, charsize=charsize, levels=levels, $
  refs_all=refs_all, overwrite=overwrite, debug=debug, _extra=_extra

  compile_opt idl2
  message, 'gx_replot_chmp_finalps: use gx_plotbestchmpmodels_ebtel, /plot_all, plot_best=0', /info
  gx_plotbestchmpmodels_ebtel, result, psDir, /plot_all, plot_best=0, charsize=charsize, $
    levels=levels, overwrite=overwrite, debug=debug, refs_all=refs_all, _extra=_extra
end
