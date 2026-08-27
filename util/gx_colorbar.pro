pro gx_colorbar, prange, bottom, ncolors, cb_title=cb_title, log=log, $
  charsize=charsize, cposition=cposition, color=color,cformat=cformat, $
  vertical=vertical, _extra=_extra
  ;this is a customization of the sww plot_map_colorbar routine that offers convenient means
  ;to properly position it on any device type
  compile_opt idl2
  default, charsize, 1.
  default, color, !p.color
  default, cb_title, ''
;  default,cposition,[.15,.96, .88,.99]
  format = '(f8.1)'
  if keyword_set(log) then format='(g12.2)' else begin
    if max(abs(prange)) gt 9999. then format='(i6)'
    if max(abs(prange)) gt 99999. then format='(g9.2)'
    if max(abs(prange)) lt 100. then format='(f8.2)'
    if max(abs(prange)) lt 1. then format='(g9.2)'
  endelse
  if n_elements(cformat) gt 0 then format = cformat
  datarange = float(prange)
  if abs(datarange[1]-datarange[0]) lt 1.e-6  then datarange[1] = datarange[0] + .001

  ; Vertical bar: ticks and title on the right. !P.MULTI is cleared only
  ; while drawing so this PLOT cannot steal a subplot or start a new PS page.
  if keyword_set(vertical) and n_elements(cposition) eq 4 then begin
    bang_p = !p
    bang_x = !x
    bang_y = !y
    !p.multi = 0
    default, bottom, 0
    nc = (n_elements(ncolors) gt 0) ? long(ncolors[0]) : (!d.table_size < 256)
    bar = reform(bindgen(nc) + bottom, 1, nc)
    x0 = 0.0 > float(cposition[0]) < 0.97
    y0 = 0.0 > float(cposition[1]) < 0.98
    x1 = (x0 + 0.008) > float(cposition[2]) < 0.99
    y1 = (y0 + 0.02) > float(cposition[3]) < 0.995
    tv, bar, x0, y0, xsize=(x1 - x0) > 1d-4, ysize=(y1 - y0) > 1d-4, /normal
    plot, [0, 1], datarange, /nodata, /noerase, position=[x0, y0, x1, y1], $
      xstyle=5, ystyle=4, yrange=datarange, color=color
    axis, yaxis=1, yrange=datarange, ystyle=1, ytitle=cb_title, $
      charsize=0.7 * ((charsize gt 0) ? charsize : 1.0), $
      yticklen=-0.35, ytickformat=format, color=color, ylog=keyword_set(log)
    !p = bang_p
    !x = bang_x
    !y = bang_y
    return
  endif

  colorbar = obj_new('colorbar2', title=cb_title,_extra=_extra)
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