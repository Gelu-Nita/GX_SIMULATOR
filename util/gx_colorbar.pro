pro gx_colorbar, prange, bottom, ncolors, cb_title=cb_title, log=log, $
  charsize=charsize, cposition=cposition, color=color,cformat=cformat, _extra=_extra
  ;this is a customization of the sww plot_map_colorbar routine that offers convenient means
  ;to properly position it on any device type
  colorbar = obj_new('colorbar2', title=cb_title,_extra=_extra)
  default, charsize, 1.
;  default,cposition,[.15,.96, .88,.99]
  format = '(f8.1)'
  if keyword_set(log) then format='(g12.2)' else begin
    if max(abs(prange)) gt 9999. then format='(i6)'
    if max(abs(prange)) gt 99999. then format='(g9.2)'
    if max(abs(prange)) lt 100. then format='(f8.2)'
    if max(abs(prange)) lt 1. then format='(g9.2)'
  endelse
  datarange = float(prange)
  if abs(datarange[1]-datarange[0]) lt 1.e-6  then datarange[1] = datarange[0] + .001
  colorbar -> setproperty, range=datarange,position=cposition, $
    bottom=bottom, ncolors=ncolors, ticklen=-.2, format=format, log=log, color=color
  ytitle_sav = !y.title
  ; colorbar draw uses xcharsize which is a scaling factor on !p.charsize, so don't
  ; pass charsize in through set - if !p.charsize is already set, characters will be huge
  pcharsize_sav = !p.charsize
  !y.title = ''
  !p.charsize = .8 * charsize
  colorbar -> draw
  !y.title = ytitle_sav
  !p.charsize = pcharsize_sav
  obj_destroy, colorbar


end