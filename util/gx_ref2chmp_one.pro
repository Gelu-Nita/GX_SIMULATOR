;+
; Build one CHMP map object (Data/SDEV/BEAM) from a non-path refdata item.
;-
forward_function gx_fwhm2sigma, gx_psf

function gx_ref2chmp_one, refdata, freq=freq, chan=chan, $
  a_beam=a_beam, b_beam=b_beam, phi_beam=phi_beam, corr_beam=corr_beam, $
  data=data, sdev=sdev, err_msg=err_msg, quiet=quiet, _extra=_extra

  error = 0
  delvarx, data, sdev
  ; Local copies so caller keyword overrides are not clobbered across multi-loop
  freq_use = n_elements(freq) gt 0 ? freq : !null
  chan_use = n_elements(chan) gt 0 ? chan : !null
  a_use = n_elements(a_beam) gt 0 ? a_beam : !null
  b_use = n_elements(b_beam) gt 0 ? b_beam : !null
  phi_use = n_elements(phi_beam) gt 0 ? phi_beam : !null
  corr_use = n_elements(corr_beam) gt 0 ? corr_beam : !null

  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    err_msg = !ERROR_STATE.MSG
    goto, exit_fail
  END

  case size(refdata, /tname) of
    'STRUCT': ref = refdata
    'OBJREF': ref = obj_clone(refdata)
    else: error = 1
  endcase

  case size(ref, /tname) of
    'OBJREF': begin
      if valid_map(ref) then begin
        data = ref->get(0, /map)
        if ref->get(/count) ge 2 then sdev = ref->get(1, /map)
      endif else error = 1
    end
    'STRUCT': begin
      if valid_map(ref) then begin
        data = ref[0]
        if n_elements(ref) gt 1 then sdev = ref[1]
      endif else begin
        if tag_exist(ref, 'maps') then begin
          if valid_map(ref.maps) then begin
            data = ref.maps[0]
            if n_elements(ref.maps) ge 2 then sdev = ref.maps[1] else sdev = data
          endif else error = 1
        endif
        if tag_exist(ref, 'a_beam') and tag_exist(ref, 'b_beam') then begin
          a_use = n_elements(a_use) ne 0 ? a_use : ref.a_beam
          b_use = n_elements(b_use) ne 0 ? b_use : ref.b_beam
          phi_use = n_elements(phi_use) ne 0 ? phi_use : $
            (tag_exist(ref, 'phi_beam') ? ref.phi_beam : 0.0)
          corr_use = n_elements(corr_use) ne 0 ? corr_use : $
            (tag_exist(ref, 'corr_beam') ? ref.corr_beam : 1)
        endif
        if tag_exist(ref, 'BMAJ') and tag_exist(ref, 'BMIN') then begin
          a_use = n_elements(a_use) ne 0 ? a_use : gx_fwhm2sigma(ref[0].bmaj)
          b_use = n_elements(b_use) ne 0 ? b_use : gx_fwhm2sigma(ref[0].bmin)
          phi_use = n_elements(phi_use) ne 0 ? phi_use : $
            (tag_exist(ref, 'BPA') ? ref.bpa : 0.0)
          corr_use = n_elements(corr_use) ne 0 ? corr_use : $
            (tag_exist(ref, 'corr_beam') ? ref.corr_beam : 1)
        endif
        if tag_exist(ref, 'freq') and n_elements(freq_use) eq 0 then freq_use = ref.freq
        if tag_exist(ref, 'chan') and n_elements(chan_use) eq 0 then chan_use = ref.chan
      endelse
    end
    else: error = 1
  endcase

  if keyword_set(error) or ~valid_map(data) then begin
    err_msg = ['Unexpected reference data format:', $
      'You may use', $
      'IDL> void=gx_ref2chmp(/help)', $
      'to list all acceptable reference data formats!']
    goto, exit_fail
  endif

  ; Required FREQ or CHAN
  if n_elements(freq_use) eq 0 and n_elements(chan_use) eq 0 then begin
    if ~tag_exist(data, 'freq') then begin
      if ~tag_exist(data, 'chan') then begin
        strarr = str2arr(data.id, del='\')
        if n_elements(strarr) gt 1 then chan_use = float(strarr[1])
        strarr = str2arr(data.id, del='_')
        if n_elements(strarr) gt 1 and n_elements(chan_use) eq 0 then $
          chan_use = float(strarr[1])
      endif else chan_use = data.chan
    endif else freq_use = data.freq
    if n_elements(freq_use) eq 0 and n_elements(chan_use) eq 0 then begin
      err_msg = ['Required FREQ or CHAN parameters have not been found', $
        'in the provided reference data structure!', $
        'You may use', $
        'IDL> void=gx_ref2chmp(/help)', $
        'to list all acceptable reference data formats!']
      goto, exit_fail
    endif
  endif

  if n_elements(a_use) eq 0 then if tag_exist(data, 'a_beam') then a_use = data.a_beam
  if n_elements(a_use) eq 0 and tag_exist(data, 'BMAJ') then a_use = gx_fwhm2sigma(data.bmaj)
  if n_elements(b_use) eq 0 then if tag_exist(data, 'b_beam') then b_use = data.b_beam
  if n_elements(b_use) eq 0 and tag_exist(data, 'BMIN') then b_use = gx_fwhm2sigma(data.bmin)
  if n_elements(phi_use) eq 0 then if tag_exist(data, 'phi_beam') then phi_use = data.phi_beam
  if n_elements(phi_use) eq 0 then if tag_exist(data, 'BPA') then phi_use = data.bpa else phi_use = 0.0
  if n_elements(corr_use) eq 0 then if tag_exist(data, 'corr_beam') then corr_use = data.corr_beam else corr_use = 1
  if n_elements(a_use) eq 0 or n_elements(b_use) eq 0 then begin
    if tag_exist(data, 'BMAJ_BMIN_BPA') then begin
      bparms = float(str2arr(data.BMAJ_BMIN_BPA))
      a_use = bparms[0]
      b_use = bparms[1]
      phi_use = bparms[2]
    endif
  endif
  if n_elements(a_use) eq 0 or n_elements(b_use) eq 0 then begin
    err_msg = ['Required beam parameters have not been found', $
      'in the provided reference data structure!', $
      'You may use', $
      'IDL> void=gx_ref2chmp(/help)', $
      'to list all acceptable reference data formats!']
    goto, exit_fail
  endif

  add_prop, data, a_beam=a_use, b_beam=b_use, phi_beam=phi_use, corr_beam=corr_use, $
    freq=freq_use, chan=chan_use, /replace
  if ~valid_map(sdev) then begin
    sdev = data
    if tag_exist(data, 'rms') then begin
      sdev.data[*, *] = data.rms
      sdev.id = 'SDEV ' + sdev.id
    endif else err_msg = ['WARNING!', 'No SDEV information found in the reference data,', $
      'the reference data map was used as a SDEV placeholder!']
  endif

  width = size(data.data, /dimensions) < 10 * max([a_use, b_use])
  if width[0] mod 2 eq 0 then width[0] += 1
  if width[1] mod 2 eq 0 then width[1] += 1
  beam = data
  add_prop, beam, data=gx_psf(corr_use * [a_use, b_use] / [data.dx, data.dy], phi_use, width), $
    id='Restoring beam ' + data.id, /rep
  ref = obj_new('map')
  if valid_map(data) then ref->setmap, 0, data
  if valid_map(sdev) then ref->setmap, 1, sdev
  if valid_map(beam) then ref->setmap, 2, beam
  if n_elements(err_msg) ne 0 and ~keyword_set(quiet) then begin
    message, '', /info
    box_message, err_msg
  endif
  return, ref

  exit_fail:
  if ~keyword_set(quiet) then begin
    message, '', /info
    if n_elements(err_msg) eq 0 then err_msg = 'gx_ref2chmp_one failed'
    box_message, err_msg
  endif
  return, !null
end
