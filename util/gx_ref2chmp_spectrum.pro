;+
; :Description:
;    Thin convenience wrapper: load refs via gx_ref2chmp and optionally
;    subset/order by freq= or chan= (gx_ref_select_axis).
;
;    Prefer calling gx_ref2chmp + gx_ref_select_axis directly from the
;    spectrum search path. Spectra for metrics are prepared at run time by
;    gx_maps2spectrum (mask-aware); this routine no longer returns FOV S_obs.
;
; :Returns:
;    OBJREF or objarr of CHMP map objects (same as gx_ref2chmp / select)
;-
forward_function gx_ref2chmp, gx_ref_select_axis

function gx_ref2chmp_spectrum, reflist, freq=freq, chan=chan, $
  err_msg=err_msg, quiet=quiet, tol=tol, _extra=_extra

  err_msg = ''
  refs = gx_ref2chmp(reflist, err_msg=em, quiet=quiet, _extra=_extra)
  if size(refs, /tname) ne 'OBJREF' then begin
    err_msg = size(em, /tname) eq 'STRING' ? em : 'gx_ref2chmp failed'
    if ~keyword_set(quiet) then begin
      message, '', /info
      box_message, err_msg
    endif
    return, !null
  endif

  out = gx_ref_select_axis(refs, freq=freq, chan=chan, tol=tol, err_msg=em)
  if size(out, /tname) ne 'OBJREF' then begin
    err_msg = size(em, /tname) eq 'STRING' ? em : 'gx_ref_select_axis failed'
    if ~keyword_set(quiet) then begin
      message, '', /info
      box_message, err_msg
    endif
    return, !null
  endif
  return, out
end
