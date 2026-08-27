;+
; :Description:
;    Build a temporary CHMP search-result array suitable for legacy display
;    routines (gx_plotbestchmpmodels_ebtel, gx_chmp2grid, GUI) by replacing
;    RES2_BEST_METRICS / CHI2_BEST_METRICS with the image-metric objects for
;    one spectrum channel extracted from SPEC_ALLMETRICS.
;
;    Q winners (q_*_best, *_best_file, scalar *_best) are left unchanged —
;    those remain the spectral-search optima. Only the map objects used by
;    plotters/grids are swapped to the selected channel at those same Qs.
;
; :Params:
;    result - array of structs from gx_search4bestq / gx_processmodels_ebtel
;             (e.g. restored from a .sav). Unchanged on return.
;
; :Keywords:
;    index   - 0-based index into SPEC_AXIS / channel_image_metrics
;    freq    - select channel whose SPEC_AXIS matches this frequency (GHz)
;    chan    - select channel whose SPEC_AXIS matches this wavelength (A)
;    err_msg - out diagnostic string
;    axis_value - out: SPEC_AXIS value for the selected channel
;    quiet - suppress informational messages
;
; :Returns:
;    New result array (same tags). Image-mode or non-spectrum results are
;    returned as a shallow copy with no metrics swap.
;
; :Example:
;    restore, 'mysolution.sav'   ; -> result
;    r94 = gx_result_select_channel(result, chan=94)
;    gx_plotbestchmpmodels_ebtel, r94, psDir, /plot_res, /plot_chi
;    ; or feed r94 into the CHMP GUI / gx_chmp2grid
;-
function gx_result_select_channel, result, index=index, freq=freq, chan=chan, $
  err_msg=err_msg, axis_value=axis_value, quiet=quiet

  err_msg = ''
  axis_value = !null

  if ~isa(result) or n_elements(result) eq 0 then begin
    err_msg = 'gx_result_select_channel: empty or undefined result'
    message, err_msg, /info
    return, !null
  endif

  if size(result, /tname) ne 'STRUCT' then begin
    err_msg = 'gx_result_select_channel: result must be a structure array'
    message, err_msg, /info
    return, !null
  endif

  ; Image-mode / old sav without spectrum tags: nothing to swap
  if ~tag_exist(result[0], 'spec_allmetrics') or ~tag_exist(result[0], 'spec_axis') then begin
    if ~keyword_set(quiet) then $
      message, 'No spec_allmetrics on result; returning input unchanged (image-mode or pre-spectrum sav).', /info
    return, result
  endif

  if ~ptr_valid(result[0].spec_allmetrics) then begin
    if ~keyword_set(quiet) then $
      message, 'spec_allmetrics is null; returning input unchanged.', /info
    return, result
  endif

  spec_axis = result[0].spec_axis
  n_chan = n_elements(spec_axis)
  if n_chan lt 1 then begin
    err_msg = 'gx_result_select_channel: empty spec_axis'
    message, err_msg, /info
    return, !null
  endif

  sam0 = *result[0].spec_allmetrics
  axis_cim = spec_axis
  if n_elements(sam0) gt 0 then if tag_exist(sam0, 'spec_axis_all') then $
    if n_elements(sam0[0].channel_image_metrics) eq n_elements(sam0[0].spec_axis_all) then $
      axis_cim = sam0[0].spec_axis_all

  ; Resolve the requested axis value, then match it onto the stored CIM axis
  ; (spec_axis_all after all-channel storage; spec_axis on older savs).
  if n_elements(index) gt 0 then begin
    ich = long(index[0])
    if (ich lt 0) or (ich ge n_chan) then begin
      err_msg = string(ich, n_chan, format="('gx_result_select_channel: channel index ',i0,' out of range [0..',i0,']')")
      message, err_msg, /info
      return, !null
    endif
    want = spec_axis[ich]
  endif else if n_elements(freq) gt 0 then begin
    d = abs(double(axis_cim) - double(freq[0]))
    want = axis_cim[(where(d eq min(d)))[0]]
  endif else if n_elements(chan) gt 0 then begin
    d = abs(double(axis_cim) - double(chan[0]))
    want = axis_cim[(where(d eq min(d)))[0]]
  endif else begin
    err_msg = 'gx_result_select_channel: set index=, freq=, or chan='
    message, err_msg, /info
    return, !null
  endelse

  axis_value = want
  if ~keyword_set(quiet) then $
    message, string(axis_value, format="('Selecting spectrum channel spec_axis=',g0)"), /info

  n = n_elements(result)
  out = !null

  for i = 0L, n - 1 do begin
    ri = result[i]

    if ~ptr_valid(ri.spec_allmetrics) then begin
      err_msg = string(i, format="('gx_result_select_channel: result[',i0,'].spec_allmetrics is null')")
      message, err_msg, /info
      return, !null
    endif

    sam = *ri.spec_allmetrics
    if n_elements(sam) eq 0 then begin
      err_msg = string(i, format="('gx_result_select_channel: empty spec_allmetrics at result[',i0,']')")
      message, err_msg, /info
      return, !null
    endif

    ; Closest sampled Q to each spectral winner
    dq_r = abs(double(sam.q) - double(ri.q_res2_best))
    dq_c = abs(double(sam.q) - double(ri.q_chi2_best))
    jr = (where(dq_r eq min(dq_r)))[0]
    jc = (where(dq_c eq min(dq_c)))[0]

    cim_r = sam[jr].channel_image_metrics
    cim_c = sam[jc].channel_image_metrics
    ax = spec_axis
    if tag_exist(sam, 'spec_axis_all') then $
      if n_elements(cim_r) eq n_elements(sam[jr].spec_axis_all) then ax = sam[jr].spec_axis_all
    void = min(abs(double(ax) - double(want)), ich)

    if n_elements(cim_r) le ich or n_elements(cim_c) le ich then begin
      err_msg = string(i, format="('gx_result_select_channel: channel_image_metrics too short at result[',i0,']')")
      message, err_msg, /info
      return, !null
    endif

    if ~obj_valid(cim_r[ich]) or ~obj_valid(cim_c[ich]) then begin
      err_msg = string(i, ich, format="('gx_result_select_channel: invalid metrics object at result[',i0,'], channel ',i0)")
      message, err_msg, /info
      return, !null
    endif

    ; Replace only the legacy display slots (clone so callers may destroy temps)
    ri = rep_tag_value(ri, obj_clone(cim_r[ich]), 'res2_best_metrics', /duplicate)
    ri = rep_tag_value(ri, obj_clone(cim_c[ich]), 'chi2_best_metrics', /duplicate)

    if ~isa(out) then out = ri else out = [out, ri]
  endfor

  return, out
end
