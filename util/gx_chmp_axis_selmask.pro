;+
; Byte mask of AXIS_ALL entries that match SPEC_AXIS (the metric subset).
; Procedure (not function) so IDL auto-compiles this file on first call.
;-
pro gx_chmp_axis_selmask, axis_all, spec_axis, sel, tol=tol
  compile_opt idl2
  default, tol, 1d-3
  n = n_elements(axis_all)
  if n eq 0 then begin
    sel = bytarr(1)
    return
  endif
  sel = bytarr(n)
  if n_elements(spec_axis) eq 0 then return
  for k = 0, n_elements(spec_axis) - 1 do begin
    m = min(abs(double(axis_all) - double(spec_axis[k])), i)
    thr = (tol * abs(double(spec_axis[k]))) > 1d-6
    if m le thr then sel[i] = 1b
  endfor
end
