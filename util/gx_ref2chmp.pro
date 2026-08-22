;+
; :Description:
;    Interpret various reference data formats and return a map object containing
;    three map structures: Data, SDEV, and BEAM (DATA carries restoring-beam tags).
;
;    Path inputs:
;      - One .sav or .fits/.fts/.fit file → one map object
;      - Directory of those files, or a string array of paths, with 2+ items →
;        objarr(n) of map objects sorted by FREQ then CHAN
;      - Directory with exactly one convertible file → one map object
;
; :Keywords:
;    freq, chan, a_beam, b_beam, phi_beam, corr_beam - optional overrides
;    data, sdev - optional outputs (single-ref path only)
;    err_msg - out diagnostic / warning text
;    quiet - suppress box_message
;    help - print accepted formats
;-
forward_function gx_ref2chmp_list_ref_files, gx_ref2chmp_one

function gx_ref2chmp, refdata, freq=freq, chan=chan, $
  a_beam=a_beam, b_beam=b_beam, phi_beam=phi_beam, corr_beam=corr_beam, $
  data=data, sdev=sdev, err_msg=err_msg, help=help, quiet=quiet, _extra=_extra

  ; WARNING: if any of a_beam,b_beam,phi_beam,corr_beam are provided as input,
  ; they replace any such parameters that the reference data might contain.
  if keyword_set(help) then begin
    err_msg = ['Expected reference data formats:', $
      '1) A map object containing up to 2 relevant map structures:', $
      '   a) data=map->get(0,/map)', $
      '   b) sdev=map->get(1,/map)', $
      '2) An array of up to 2 relevant map structures:', $
      '   a) maps[0]:  data map', $
      '   b) maps[1]:  sdev map', $
      'The DATA map must also contain the following tags:', $
      '      FREQ or CHAN    ; Frequency (GHz) or Wavelength (A)', $
      '', $
      '      BMAJ or A_BEAM  ; FWHM or Gauss Sigma major beam axis, arcsecs', $
      '      BMIN or B_BEAM  ; FWHM or Gauss Sigma minor beam axis, arcsecs', $
      '      BPA or PHI_BEAM ; Major axis polar angle in degrees,', $
      'OR', $
      '      BMAJ_BMIN_BPA   ; comma separated string list of the above beam parameters', $
      '3) An IDL structure having the following tags:', $
      '      A_BEAM          Gauss Sigma A, arcsecs', $
      '      B_BEAM          Gauss Sigma B, arcsecs', $
      '      PHI_BEAM        A-axis rotation angle, degrees', $
      '      CORR_BEAM       correction factor for A and B (optional)', $
      '      MAPS            [Data] or [Data, SDEV] array of map structures', $
      '      FREQ or CHAN    Frequency (GHz) or Wavelength (A)', $
      '      NOTE: Alternatively, FREQ or CHAN may be tags of the Data map', $
      '4) STRING path inputs:', $
      '   a) One .sav file (as above) or one FITS file (via gx_fits2map)', $
      '   b) Directory of .sav and/or FITS files → objarr of CHMP refs (sorted)', $
      '   c) String array of .sav/FITS paths → same as (b) when 2+ files']
    goto, exit_fail
  endif
  if n_elements(refdata) eq 0 then begin
    err_msg = ['gx_ref2chmp: no input provided (undefined or empty refdata).', $
      'Example:', $
      "  refdir='.../refs'", $
      '  rm=gx_ref2chmp(refdir)', $
      'Use gx_ref2chmp(/help) for accepted formats.']
    goto, exit_fail
  endif

  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    err_msg = !ERROR_STATE.MSG
    goto, exit_fail
  END

  ;---------- STRING: directory / file list / single path ----------
  if size(refdata, /tname) eq 'STRING' then begin
    files = !null
    if n_elements(refdata) gt 1 then begin
      files = refdata
    endif else begin
      path0 = refdata[0]
      if file_test(path0, /directory) then begin
        files = gx_ref2chmp_list_ref_files(path0, count=nfc)
        if nfc eq 0 then begin
          err_msg = 'No .sav or FITS reference files found in: ' + path0
          goto, exit_fail
        endif
      endif else if file_exist(path0) then begin
        files = [path0]
      endif else begin
        err_msg = 'Reference path not found: ' + path0
        goto, exit_fail
      endelse
    endelse

    ; Resolve each file into one or more single-ref inputs, then build
    items = list()
    for i = 0L, n_elements(files) - 1 do begin
      gx_ref2chmp_load_file, files[i], items, err_msg=em
      if n_elements(em) gt 0 then begin
        if size(em, /tname) eq 'STRING' then begin
          if em[0] ne '' then begin
            err_msg = em
            goto, exit_fail
          endif
        endif
      endif
    endfor

    nitem = items.count()
    if nitem eq 0 then begin
      err_msg = 'No convertible reference items found'
      goto, exit_fail
    endif

    refs = objarr(nitem)
    axis = dblarr(nitem)
    is_chan_vec = bytarr(nitem)
    n_ok = 0L
    for i = 0L, nitem - 1 do begin
      r = gx_ref2chmp_one(items[i], freq=freq, chan=chan, $
        a_beam=a_beam, b_beam=b_beam, phi_beam=phi_beam, corr_beam=corr_beam, $
        err_msg=em, /quiet, _extra=_extra)
      if ~obj_valid(r) then begin
        err_msg = 'Failed to build CHMP reference from an input item: ' + $
          (size(em, /tname) eq 'STRING' ? strjoin(em, ' ') : 'unknown error')
        goto, exit_fail
      endif
      rf = r->get(0, /freq)
      rc = r->get(0, /chan)
      if n_elements(rf) gt 0 && finite(rf[0]) then begin
        axis[n_ok] = double(rf[0])
        is_chan_vec[n_ok] = 0b
      endif else if n_elements(rc) gt 0 && finite(rc[0]) then begin
        axis[n_ok] = double(rc[0])
        is_chan_vec[n_ok] = 1b
      endif else begin
        err_msg = 'Built reference is missing FREQ or CHAN'
        goto, exit_fail
      endelse
      refs[n_ok] = r
      n_ok++
    endfor

    if n_ok eq 0 then begin
      err_msg = 'No valid CHMP references were built'
      goto, exit_fail
    endif
    if n_ok lt nitem then begin
      refs = refs[0:n_ok-1]
      axis = axis[0:n_ok-1]
      is_chan_vec = is_chan_vec[0:n_ok-1]
    endif

    ; Sort by axis (freq or chan value)
    ord = sort(axis)
    refs = refs[ord]
    if n_ok eq 1 then return, refs[0]
    return, refs
  endif

  ;---------- Non-string: single-ref path ----------
  return, gx_ref2chmp_one(refdata, freq=freq, chan=chan, $
    a_beam=a_beam, b_beam=b_beam, phi_beam=phi_beam, corr_beam=corr_beam, $
    data=data, sdev=sdev, err_msg=err_msg, quiet=quiet, _extra=_extra)

  exit_fail:
  if ~keyword_set(quiet) then begin
    message, '', /info
    if n_elements(err_msg) eq 0 then err_msg = 'gx_ref2chmp failed'
    box_message, err_msg
  endif
  return, !null
end
