;+
; :Description:
;    Select / order an objarr of CHMP reference map objects by FREQ or CHAN.
;    If freq= or chan= is omitted, all refs are kept (sorted by axis).
;    If freq=/chan= is set, each requested value must match a ref (within tol).
;
; :Params:
;    refs - OBJREF or objarr of CHMP map objects from gx_ref2chmp
;
; :Keywords:
;    freq - optional metric frequencies [GHz]
;    chan - optional metric channels
;    tol - relative match tolerance (default 1e-3)
;    err_msg - out
;    is_chan - out, 1b if channel axis
;    axis - out, dblarr of selected axis values
;
; :Returns:
;    objarr(n) of selected refs (or single OBJREF if n=1), sorted by axis
;-
function gx_ref_select_axis, refs, freq=freq, chan=chan, tol=tol, $
  err_msg=err_msg, is_chan=is_chan, axis=axis

  default, tol, 1d-3
  err_msg = ''
  if size(refs, /tname) ne 'OBJREF' then begin
    err_msg = 'gx_ref_select_axis: refs must be a map object or objarr'
    return, !null
  endif
  n_all = n_elements(refs)
  if n_all lt 1 then begin
    err_msg = 'gx_ref_select_axis: empty refs'
    return, !null
  endif

  ax = dblarr(n_all)
  ic = bytarr(n_all)
  for i = 0L, n_all - 1 do begin
    if ~obj_valid(refs[i]) then begin
      err_msg = 'gx_ref_select_axis: invalid ref at index ' + strtrim(i, 2)
      return, !null
    endif
    rf = refs[i]->get(0, /freq)
    rc = refs[i]->get(0, /chan)
    if n_elements(rf) gt 0 && finite(rf[0]) then begin
      ax[i] = double(rf[0])
      ic[i] = 0b
    endif else if n_elements(rc) gt 0 && finite(rc[0]) then begin
      ax[i] = double(rc[0])
      ic[i] = 1b
    endif else begin
      err_msg = 'gx_ref_select_axis: ref missing FREQ/CHAN at index ' + strtrim(i, 2)
      return, !null
    endelse
  endfor

  if n_elements(uniq(ic, sort(ic))) gt 1 then begin
    err_msg = 'gx_ref_select_axis: mixed FREQ and CHAN refs not allowed'
    return, !null
  endif
  is_chan = ic[0]

  if keyword_set(is_chan) then begin
    if n_elements(freq) gt 0 then $
      message, 'WARNING: freq= ignored for CHAN reference set', /info
    if n_elements(chan) eq 0 then begin
      sel = sort(ax)
    endif else begin
      req = double(chan)
      sel = lonarr(n_elements(req))
      for k = 0, n_elements(req) - 1 do begin
        m = min(abs(ax - req[k]), ii)
        thr = (tol * abs(req[k])) > 1d-6
        if m gt thr then begin
          err_msg = string(req[k], format="('Requested CHAN=',g0,' not found in reference set')")
          return, !null
        endif
        sel[k] = ii
      endfor
    endelse
  endif else begin
    if n_elements(chan) gt 0 then $
      message, 'WARNING: chan= ignored for FREQ reference set', /info
    if n_elements(freq) eq 0 then begin
      sel = sort(ax)
    endif else begin
      req = double(freq)
      sel = lonarr(n_elements(req))
      for k = 0, n_elements(req) - 1 do begin
        m = min(abs(ax - req[k]), ii)
        thr = (tol * abs(req[k])) > 1d-6
        if m gt thr then begin
          err_msg = string(req[k], format="('Requested FREQ=',g0,' GHz not found in reference set')")
          return, !null
        endif
        sel[k] = ii
      endfor
    endelse
  endelse

  n = n_elements(sel)
  out = objarr(n)
  axis = dblarr(n)
  for k = 0, n - 1 do begin
    out[k] = refs[sel[k]]
    axis[k] = ax[sel[k]]
  endfor
  if n eq 1 then return, out[0]
  return, out
end
