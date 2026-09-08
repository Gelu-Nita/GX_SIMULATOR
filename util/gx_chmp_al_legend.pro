;+
; al_legend wrapper for CHMP PostScript.
; Increases line spacing (al_legend default is 1.2*charsize) and restores
; the indexed color table / decomposed state. Coyote cgcolor (used by
; al_legend) otherwise leaves PS in decomposed mode so later color=250
; lines are no longer loadct 39 red.
;
; Placement/styling keywords used at CHMP call sites are declared (not only
; absorbed in _extra) so they are valid in the wrapper signature.
;-
pro gx_chmp_al_legend, items, charsize=charsize, spacing=spacing, $
  top=top, bottom=bottom, left=left, right=right, center=center, $
  box=box, back=back, psym=psym, linestyle=linestyle, colors=colors, $
  textcolors=textcolors, _extra=_extra

  compile_opt idl2
  if n_elements(items) eq 0 then return
  tvlct, rr, gg, bb, /get
  pfont = !p.font
  dec = 0
  dn = strupcase(!d.name)
  if (dn eq 'PS') or (dn eq 'X') or (dn eq 'WIN') or (dn eq 'Z') then $
    device, get_decomposed=dec
  cs = 1.0
  if n_elements(charsize) gt 0 then $
    if finite(charsize[0]) and (charsize[0] gt 0) then cs = float(charsize[0])
  default, spacing, 2.0 * cs
  fnt = pfont
  if (fnt ne -1) and (fnt ne 0) and (fnt ne 1) then fnt = 1
  al_legend, items, charsize=cs, spacing=spacing, font=fnt, $
    top=top, bottom=bottom, left=left, right=right, center=center, $
    box=box, back=back, psym=psym, linestyle=linestyle, colors=colors, $
    textcolors=textcolors, _extra=_extra
  tvlct, rr, gg, bb
  !p.font = pfont
  if (dn eq 'PS') or (dn eq 'X') or (dn eq 'WIN') or (dn eq 'Z') then $
    device, decomposed=dec
end
