;+
; :Description:
;    Build a multi-frequency / multi-channel CHMP reference container for
;    spectrum-mode searches. Each element is produced by gx_ref2chmp and must
;    contain Data/SDEV/BEAM maps. Existing single-ref gx_ref2chmp is unchanged.
;
; :Params:
;    reflist - one of:
;      1) STRING path to a .sav restoring `ref` (array) or `refs`
;      2) STRING path to a directory of .sav files
;      3) STRING array of .sav paths
;      4) OBJREF / STRUCT array already CHMP-valid (or list thereof)
;
; :Keywords:
;    freq - optional dblarr of requested frequencies (GHz); filter/match refs
;    chan - optional array of requested EUV channels; filter/match refs
;    err_msg - out, diagnostic messages
;    quiet - suppress box_message
;    tol - relative axis match tolerance (default 1e-3)
;
; :Returns:
;    Structure:
;      n, axis, is_chan, refs (objarr), S_obs, S_sdev, has_sdev,
;      ref0 (first ref, for FOV/geometry)
;-
function gx_ref2chmp_spectrum, reflist, freq=freq, chan=chan, $
  err_msg=err_msg, quiet=quiet, tol=tol, _extra=_extra

  default, tol, 1d-3
  err_msg = ''
  items = list()

  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    err_msg = !ERROR_STATE.MSG
    goto, exit_fail
  END

  ;---------- normalize reflist into a list of candidates ----------
  case size(reflist, /tname) of
    'STRING': begin
      if n_elements(reflist) eq 1 then begin
        path0 = reflist[0]
        if file_test(path0, /directory) then begin
          files = file_search(path0, '*.sav', count=nfc)
          if nfc eq 0 then begin
            err_msg = 'No .sav files found in directory: '+path0
            goto, exit_fail
          endif
          for i=0, nfc-1 do items.add, files[i]
        endif else if file_exist(path0) then begin
          sObj = OBJ_NEW('IDL_Savefile', path0)
          sNames = sObj->Names()
          restored_ref = !null
          restored_refs = !null
          for k=0, n_elements(sNames)-1 do begin
            sObj->Restore, sNames[k]
            void = execute('tmp=temporary('+sNames[k]+')')
            if strupcase(sNames[k]) eq 'REFS' then restored_refs = temporary(tmp) $
            else if strupcase(sNames[k]) eq 'REF' then restored_ref = temporary(tmp) $
            else if ~isa(restored_ref) then restored_ref = temporary(tmp)
          endfor
          obj_destroy, sObj
          if isa(restored_refs) then begin
            for i=0, n_elements(restored_refs)-1 do items.add, restored_refs[i]
          endif else if isa(restored_ref) then begin
            for i=0, n_elements(restored_ref)-1 do items.add, restored_ref[i]
          endif else begin
            err_msg = 'Save file did not contain ref/refs: '+path0
            goto, exit_fail
          endelse
        endif else begin
          err_msg = 'Reference path not found: '+path0
          goto, exit_fail
        endelse
      endif else begin
        for i=0, n_elements(reflist)-1 do items.add, reflist[i]
      endelse
    end
    'OBJREF': begin
      for i=0, n_elements(reflist)-1 do items.add, reflist[i]
    end
    'STRUCT': begin
      for i=0, n_elements(reflist)-1 do items.add, reflist[i]
    end
    else: begin
      if isa(reflist, 'LIST') then begin
        foreach r, reflist do items.add, r
      endif else begin
        err_msg = 'Unsupported reflist type for gx_ref2chmp_spectrum'
        goto, exit_fail
      endelse
    end
  endcase

  if items.count() eq 0 then begin
    err_msg = 'Empty reference list for spectrum mode'
    goto, exit_fail
  endif

  ;---------- convert each item via gx_ref2chmp ----------
  refs = list()
  axis_all = list()
  is_chan_all = list()
  foreach item, items do begin
    if size(item, /tname) eq 'STRING' then begin
      r = gx_ref2chmp(item, err_msg=em, /quiet, _extra=_extra)
    endif else begin
      r = gx_ref2chmp(item, err_msg=em, /quiet, _extra=_extra)
    endelse
    if ~obj_valid(r) then begin
      err_msg = 'Failed to interpret a spectrum reference entry: '+em
      goto, exit_fail
    endif
    rf = r->get(0, /freq)
    rc = r->get(0, /chan)
    if n_elements(rf) gt 0 then begin
      if finite(rf[0]) then begin
        axis_all.add, double(rf[0])
        is_chan_all.add, 0b
      endif else begin
        err_msg = 'Reference entry has non-finite FREQ'
        goto, exit_fail
      endelse
    endif else if n_elements(rc) gt 0 then begin
      if finite(rc[0]) then begin
        axis_all.add, double(rc[0])
        is_chan_all.add, 1b
      endif else begin
        err_msg = 'Reference entry has non-finite CHAN'
        goto, exit_fail
      endelse
    endif else begin
      err_msg = 'Reference entry missing FREQ or CHAN'
      goto, exit_fail
    endelse
    refs.add, r
  endforeach

  n_all = refs.count()
  axis = dblarr(n_all)
  is_chan_vec = bytarr(n_all)
  for i=0, n_all-1 do begin
    axis[i] = axis_all[i]
    is_chan_vec[i] = is_chan_all[i]
  endfor

  ; all must share domain
  if n_elements(uniq(is_chan_vec, sort(is_chan_vec))) gt 1 then begin
    err_msg = 'Mixed FREQ and CHAN references are not allowed in spectrum mode'
    goto, exit_fail
  endif
  is_chan = is_chan_vec[0]

  ;---------- select / match requested axis ----------
  if is_chan then begin
    if n_elements(chan) eq 0 then begin
      sel = sort(axis)
      req = axis[sel]
    endif else begin
      req = double(chan)
      sel = lonarr(n_elements(req))
      for k=0, n_elements(req)-1 do begin
        m = min(abs(axis - req[k]), ii)
        thr = (tol * abs(req[k])) > 1d-6
        if m gt thr then begin
          err_msg = string(req[k], format="('Requested CHAN=',g0,' not found in spectrum reference set')")
          goto, exit_fail
        endif
        sel[k] = ii
      endfor
    endelse
  endif else begin
    if n_elements(freq) eq 0 then begin
      sel = sort(axis)
      req = axis[sel]
    endif else begin
      req = double(freq)
      sel = lonarr(n_elements(req))
      for k=0, n_elements(req)-1 do begin
        m = min(abs(axis - req[k]), ii)
        thr = (tol * abs(req[k])) > 1d-6
        if m gt thr then begin
          err_msg = string(req[k], format="('Requested FREQ=',g0,' GHz not found in spectrum reference set')")
          goto, exit_fail
        endif
        sel[k] = ii
      endfor
    endelse
  endelse

  n = n_elements(sel)
  if n lt 1 then begin
    err_msg = 'Spectrum reference set is empty after selection'
    goto, exit_fail
  endif

  out_refs = objarr(n)
  out_axis = dblarr(n)
  S_obs = dblarr(n)
  S_sdev = dblarr(n)
  has_sdev = bytarr(n)

  for k=0, n-1 do begin
    r = refs[sel[k]]
    out_refs[k] = r
    out_axis[k] = axis[sel[k]]
    data = r->get(0, /map)
    sdev = r->get(1, /map)
    S_obs[k] = gx_fov_integral_map(data, sdev=sdev, s_sdev=ss)
    if finite(ss) then begin
      S_sdev[k] = ss
      has_sdev[k] = 1b
    endif else begin
      S_sdev[k] = !values.d_nan
      has_sdev[k] = 0b
    endelse
  endfor

  ; destroy unused refs not selected
  for i=0, n_all-1 do begin
    keep = where(sel eq i, nk)
    if nk eq 0 then begin
      r = refs[i]
      if obj_valid(r) then obj_destroy, r
    endif
  endfor

  return, {n:n, axis:out_axis, is_chan:is_chan, refs:out_refs, $
    S_obs:S_obs, S_sdev:S_sdev, has_sdev:has_sdev, ref0:out_refs[0], $
    search_mode:'spectrum'}

  exit_fail:
  if ~keyword_set(quiet) then begin
    message, '', /info
    box_message, err_msg
  endif
  return, !null
end
