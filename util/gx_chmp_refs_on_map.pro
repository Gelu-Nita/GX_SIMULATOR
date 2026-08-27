;+
; Keep CHMP refs whose FREQ/CHAN matches a layer on MAPOBJ (within tol).
; Used so full-spectrum ROI integrals only request channels the model
; actually synthesized. Sorted by axis.
;-
function gx_chmp_refs_on_map, refs, mapobj, tol=tol, axis=axis, err_msg=err_msg
  compile_opt idl2
  default, tol, 1d-3
  err_msg = ''
  axis = !null
  if size(refs, /tname) ne 'OBJREF' then begin
    err_msg = 'gx_chmp_refs_on_map: refs must be OBJREF'
    return, !null
  endif
  if ~obj_valid(mapobj) then begin
    err_msg = 'gx_chmp_refs_on_map: invalid mapobj'
    return, !null
  endif
  nmap = mapobj->get(/count)
  if nmap lt 1 then begin
    err_msg = 'gx_chmp_refs_on_map: empty mapobj'
    return, !null
  endif

  ; Model axis from first layer (FREQ vs CHAN)
  v0f = mapobj->get(0, /freq)
  v0c = mapobj->get(0, /chan)
  is_chan = (n_elements(v0c) gt 0) && finite(v0c[0]) && $
    ~((n_elements(v0f) gt 0) && finite(v0f[0]))
  mod_axis = dblarr(nmap)
  for j = 0L, nmap - 1 do begin
    if keyword_set(is_chan) then v = mapobj->get(j, /chan) $
    else v = mapobj->get(j, /freq)
    if n_elements(v) eq 0 || ~finite(v[0]) then begin
      err_msg = 'gx_chmp_refs_on_map: model map missing FREQ/CHAN at ' + strtrim(j, 2)
      return, !null
    endif
    mod_axis[j] = double(v[0])
  endfor

  nref = n_elements(refs)
  keep = lonarr(nref)
  nkeep = 0L
  ax_keep = dblarr(nref)
  for k = 0L, nref - 1 do begin
    if ~obj_valid(refs[k]) then continue
    if keyword_set(is_chan) then v = refs[k]->get(0, /chan) $
    else v = refs[k]->get(0, /freq)
    if n_elements(v) eq 0 || ~finite(v[0]) then continue
    ax = double(v[0])
    m = min(abs(mod_axis - ax), ii)
    thr = (tol * abs(ax)) > 1d-6
    if m le thr then begin
      keep[nkeep] = k
      ax_keep[nkeep] = ax
      nkeep++
    endif
  endfor
  if nkeep eq 0 then begin
    err_msg = 'gx_chmp_refs_on_map: no refs match model map axes'
    return, !null
  endif
  keep = keep[0:nkeep-1]
  ax_keep = ax_keep[0:nkeep-1]
  ord = sort(ax_keep)
  out = objarr(nkeep)
  axis = dblarr(nkeep)
  for i = 0L, nkeep - 1 do begin
    out[i] = refs[keep[ord[i]]]
    axis[i] = ax_keep[ord[i]]
  endfor
  if nkeep eq 1 then return, out[0]
  return, out
end
