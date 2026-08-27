
;+
; :Description:
;    Quantitatively compare a modeled spectrum with an observed one.
;    CHI2 tags are present only when data_sdev is provided and matches the
;    spectrum length. CHMP spectrum mode minimizes res2_norm and chi2.
;
; :Params:
;    data_model - dblarr(n), in,  synthetic spectrum (ROI-integrated S_mod)
;    data_obs - dblarr(n), in,  reference observed spectrum (S_obs)
;    data_sdev - dblarr(n), in,  reference spectrum standard deviation (S_sdev).
;      Built by gx_maps2spectrum via gx_fov_integral_map: independent pixels,
;      S_sdev = sqrt(total(sdev^2)) times the same per-pixel scale as S_obs.
;      If omitted or size-mismatched, CHI2 tags are not computed.
;
; :Keywords:
;   range_idx - selects which spectral points enter the metrics (1-D analogue
;       of the image ROI). This is NOT the image mask= keyword used by
;       gx_metrics_image / gx_metrics_map / gx_maps2spectrum.
;       Legacy behavior is fully preserved when weights is omitted.
;   range_idx - lonarr, default:lindgen(n_elements(data_model)), in, compare
;       only selected spectral points. Note: if n_elements(range_idx) eq 2 and
;       the values are integer/LONG, they are treated as a contiguous index
;       span [min:max], not as two discrete points — pass a longer index list
;       or pre-slice the spectra when selecting non-contiguous channels.
;   OR
;   range_idx - dblarr(2), data-value range for which the comparison should
;       be performed (float/double endpoints).
;
;   weights - optional dblarr(n), same length as data_model. Non-negative
;       per-point weights applied AFTER range_idx. Points with weight <= 0
;       (or outside range_idx) do not enter the averaged metrics.
;       RES2_NORM = sum(w*r^2)/sum(w), CHI2 = sum(w*chi^2)/(sum(w)-n_free)
;       with r=(model-obs)/obs and chi=(model-obs)/sdev.
;       Binary 0/1 weights are the soft analogue of hard channel selection.
;       If omitted, metrics match the historical unweighted range_idx path
;       exactly (including the chi2_spec=1 fill outside the mask).
;       Size mismatch → warning and weights ignored (legacy path).
;
;   n_free - number of degrees of freedom, default 0, used only for CHI2
;
; :Return value:
;     Structure fields:
;       R - Pearson correlation coefficient
;       res_spec = data_model - data_obs
;       res = total(res_spec[mask_idx])   ; unweighted path
;           or total(w*res_spec)          ; weighted path (over active points)
;       res_spec_norm = res_spec/data_obs
;       res_norm = mean or weighted mean of res_spec_norm
;       res2_spec = res_spec^2
;       res2, res2_spec_norm, res2_norm - as documented historically;
;         weighted path uses sum(w*·)/sum(w) for the averaged scalars
;       mask_spec - byte mask from range_idx (unchanged meaning)
;       weights - effective weights used (range_idx ∩ weights); only present
;         when the weighted path ran
;
;       chi_spec = res_spec/data_sdev
;       chi, chi2_spec, chi2 - unweighted or weighted as above
;
; :Author: Gelu Nita (gnita@njit.edu) 7/26/20
;-

function gx_metrics_spectrum, data_model, data_obs, data_sdev, $
  range_idx=range_idx, weights=weights, n_free=n_free

  if ~isa(data_model, /array) or ~isa(data_obs, /array) then begin
    message, 'Model and Observational Data must be array variables', /info
    return, !null
  endif

  n = n_elements(data_model)
  if n_elements(data_obs) ne n then begin
    message, 'Model and Observational Data array sizes must match', /info
    return, !null
  endif

  spec_mask = bytarr(n)

  if ~isa(range_idx, /array) then range_idx = lindgen(n)

  if n_elements(range_idx) eq 2 then begin
    if isa(range_idx, /float) then begin
      idx = where(data_model ge min(range_idx, max=max_idx) and data_model le max_idx, count)
      if count gt 0 then spec_mask[idx] = 1
    endif else spec_mask[min(range_idx, max=max_range) > 0:max_range < (n - 1)] = 1
  endif else spec_mask[range_idx > 0 < (n - 1)] = 1

  ; Convert input data to double precision floats
  data_model_d = double(data_model)
  data_obs_d = double(data_obs)

  if isa(data_sdev) then begin
    if (n_elements(data_model) ne n_elements(data_sdev)) then begin
      message, 'Provided SDEV data, not matching model and data array sizes, will be ignored!', /info
    endif else data_sdev_d = double(data_sdev)
  endif

  ;----- optional weights (applied after range_idx) -----
  use_weights = 0b
  if n_elements(weights) gt 0 then begin
    if n_elements(weights) ne n then begin
      message, 'weights size does not match spectrum length; weights ignored (legacy path).', /info
    endif else begin
      weff = double(spec_mask) * double(weights)
      active = where(weff gt 0d, nactive)
      if nactive eq 0 then begin
        message, 'No spectral points with positive weight inside range_idx.', /info
        return, !null
      endif
      use_weights = 1b
      wsum = total(weff[active])
    endelse
  endif

  ;==========================================================================
  ; Legacy unweighted path — bit-compatible with pre-weights callers
  ;==========================================================================
  if ~keyword_set(use_weights) then begin
    mask_idx = where(spec_mask, complement=bad, ncomp=nbad)
    n_mask_idx = total(spec_mask)

    R = correlate(data_model_d * spec_mask, data_obs_d * spec_mask)
    res_spec = data_model_d - data_obs_d
    res = total(res_spec[mask_idx])
    res_spec_norm = res_spec / data_obs_d
    res_norm = total(res_spec_norm[mask_idx]) / n_mask_idx
    res2_spec = res_spec^2
    if nbad gt 0 then res2_spec[bad] = 0
    res2 = total(res2_spec[mask_idx]) / n_mask_idx - res^2 / n_mask_idx
    res2_spec_norm = res_spec_norm^2
    res2_norm = total(res2_spec_norm[mask_idx]) / n_mask_idx
    metrics = {R:R, $
      mask_spec:spec_mask, $
      res_spec:res_spec, $
      res:res, $
      res_spec_norm:res_spec_norm, $
      res_norm:res_norm, $
      res2_spec:res2_spec, $
      res2:res2, $
      res2_spec_norm:res2_spec_norm, $
      res2_norm:res2_norm}
    if isa(data_sdev_d) then begin
      default, n_free, 0
      chi_spec = res_spec / data_sdev_d
      if nbad gt 0 then chi_spec[bad] = 0
      chi = total(chi_spec[mask_idx]) / n_mask_idx
      chi2_spec = chi_spec^2
      if nbad gt 0 then chi2_spec[bad] = 1
      chi2 = total(chi2_spec[mask_idx]) / (n_mask_idx - n_free)
      chi_metrics = {$
        chi_spec:chi_spec, $
        chi:chi, $
        chi2_spec:chi2_spec, $
        chi2:chi2}
      metrics = create_struct(metrics, chi_metrics)
    endif
    return, metrics
  endif

  ;==========================================================================
  ; Weighted path (range_idx ∩ weights)
  ;==========================================================================
  inactive = where(weff le 0d, ninactive)
  ; Correlation on the support of positive weights (soft magnitudes ignored for R)
  R = correlate(data_model_d[active], data_obs_d[active])

  res_spec = data_model_d - data_obs_d
  res = total(weff[active] * res_spec[active])
  ; Relative metrics only over finite, nonzero obs (zero obs must not pad wsum)
  res_spec_norm = make_array(n, /double, value=0d)
  good_obs = where((weff gt 0d) and finite(data_obs_d) and (data_obs_d ne 0d), ngood_obs)
  if ngood_obs gt 0 then begin
    res_spec_norm[good_obs] = res_spec[good_obs] / data_obs_d[good_obs]
    wsum_obs = total(weff[good_obs])
    res_norm = total(weff[good_obs] * res_spec_norm[good_obs]) / wsum_obs
    res2_spec_norm = res_spec_norm^2
    res2_norm = total(weff[good_obs] * res2_spec_norm[good_obs]) / wsum_obs
  endif else begin
    message, 'No finite nonzero data_obs in positive-weight set; RES*_NORM set to NaN.', /info
    res_norm = !values.d_nan
    res2_spec_norm = res_spec_norm^2
    res2_norm = !values.d_nan
  endelse
  res2_spec = res_spec^2
  if ninactive gt 0 then res2_spec[inactive] = 0
  res2 = total(weff[active] * res2_spec[active]) / wsum - (res^2) / wsum

  metrics = {R:R, $
    mask_spec:spec_mask, $
    weights:weff, $
    res_spec:res_spec, $
    res:res, $
    res_spec_norm:res_spec_norm, $
    res_norm:res_norm, $
    res2_spec:res2_spec, $
    res2:res2, $
    res2_spec_norm:res2_spec_norm, $
    res2_norm:res2_norm}

  if isa(data_sdev_d) then begin
    default, n_free, 0
    chi_spec = make_array(n, /double, value=0d)
    good_sd = where((weff gt 0d) and finite(data_sdev_d) and (data_sdev_d ne 0d), ngood_sd)
    if ngood_sd gt 0 then begin
      chi_spec[good_sd] = res_spec[good_sd] / data_sdev_d[good_sd]
      wsum_sd = total(weff[good_sd])
      chi = total(weff[good_sd] * chi_spec[good_sd]) / wsum_sd
      chi2_spec = chi_spec^2
      if ninactive gt 0 then chi2_spec[inactive] = 1
      denom = wsum_sd - double(n_free)
      if denom le 0d then begin
        message, 'CHI2 denominator (sum(weights with valid sdev)-n_free) <= 0; returning NaN chi2.', /info
        chi2 = !values.d_nan
      endif else chi2 = total(weff[good_sd] * chi2_spec[good_sd]) / denom
    endif else begin
      message, 'No finite nonzero data_sdev in positive-weight set; CHI/CHI2 set to NaN.', /info
      chi = !values.d_nan
      chi2_spec = chi_spec^2
      if ninactive gt 0 then chi2_spec[inactive] = 1
      chi2 = !values.d_nan
    endelse
    chi_metrics = {$
      chi_spec:chi_spec, $
      chi:chi, $
      chi2_spec:chi2_spec, $
      chi2:chi2}
    metrics = create_struct(metrics, chi_metrics)
  endif
  return, metrics
end
