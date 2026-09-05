;+
; NAME:
;   gx_test_chmp_sdev_ab
; PURPOSE:
;   Synthetic tests for CHMP time-cube Method A/B. Optional HISFM regression
;   if HISFM_ROOT points at the project (all_rotated + all_rotated_refs).
;
; CALLING SEQUENCE:
;   IDL> gx_test_chmp_sdev_ab
;   IDL> gx_test_chmp_sdev_ab, hisfm_root='.../HISFM-Team'
;-
pro gx_test_chmp_sdev_ab, hisfm_root=hisfm_root, failed=nfail
  compile_opt idl2
  nfail = 0L
  npass = 0L

  ;----- sdev_method resolver -----
  m = gx_chmp_sdev_method(sdev_method='auto', search_mode='spectrum', has_cube=1)
  if m ne 'A' then begin
    print, 'FAIL: auto+spectrum+cube should be A, got ', m
    nfail++
  endif else npass++
  m = gx_chmp_sdev_method(sdev_method='auto', search_mode='image', has_cube=1)
  if m ne 'B' then begin
    print, 'FAIL: auto+image+cube should be B, got ', m
    nfail++
  endif else npass++
  m = gx_chmp_sdev_method(sdev_method='auto', search_mode='spectrum', has_cube=0)
  if m ne 'B' then begin
    print, 'FAIL: auto+no cube should be B, got ', m
    nfail++
  endif else npass++
  m = gx_chmp_sdev_method(sdev_method='A', search_mode='image', has_cube=1, err_msg=em)
  if m ne '' or em eq '' then begin
    print, 'FAIL: A+image should error'
    nfail++
  endif else npass++
  m = gx_chmp_sdev_method(sdev_method='A', search_mode='spectrum', has_cube=0, err_msg=em)
  if m ne '' or em eq '' then begin
    print, 'FAIL: A+no cube should error'
    nfail++
  endif else npass++

  ;----- format-3 vs time series -----
  nx = 16
  ny = 16
  nf = 12
  seed = 1L
  base = 10d + randomu(seed, nx, ny)
  cube_unc = dblarr(nx, ny, nf)
  cube_cor = dblarr(nx, ny, nf)
  for t = 0L, nf - 1 do begin
    cube_unc[*, *, t] = base + randomn(seed, nx, ny)
    cube_cor[*, *, t] = base + randomn(seed)
  endfor

  tmpl = make_map(reform(cube_unc[*, *, 0]), dx=1.0, dy=1.0, xc=0.0, yc=0.0, id='AIA 94')
  add_prop, tmpl, chan=94.0, a_beam=1.5, b_beam=1.5, phi_beam=0.0, /replace
  rmaps = replicate(tmpl, nf)
  for t = 0L, nf - 1 do rmaps[t].data = cube_unc[*, *, t]

  if ~gx_ref2chmp_item_is_cube(rmaps) then begin
    print, 'FAIL: RMAPS array should be a cube'
    nfail++
  endif else npass++

  mean_m = tmpl
  sdev_m = tmpl
  gx_ref_cube_stats, cube_unc, md, sd
  mean_m.data = md
  sdev_m.data = sd
  sdev_m.id = 'SDEV ' + sdev_m.id
  snap = {a_beam: 1.5, b_beam: 1.5, phi_beam: 0.0, chan: 94.0, maps: [mean_m, sdev_m]}
  if gx_ref2chmp_item_is_cube(snap) or ~gx_ref2chmp_is_format3(snap) then begin
    print, 'FAIL: format-3 snapshot must not be a cube'
    nfail++
  endif else npass++

  ;----- Method A vs B on synthetic cubes -----
  dOmega = 1d
  img_mask = byte(base * 0) + 1b
  sA_u = gx_ref_cube_sf(cube_unc, img_mask, dOmega)
  gx_ref_cube_stats, cube_unc, mu, su
  sB_u = dOmega * sqrt(total(su^2, /nan))
  ; Uncorrelated: A should be within a small factor of B (not 15–40x)
  if ~(finite(sA_u) and finite(sB_u) and sB_u gt 0) then begin
    print, 'FAIL: uncorrelated A/B not finite'
    nfail++
  endif else if (sA_u / sB_u) gt 4 then begin
    print, 'FAIL: uncorrelated A/B too large: ', sA_u / sB_u
    nfail++
  endif else npass++

  sA_c = gx_ref_cube_sf(cube_cor, img_mask, dOmega)
  gx_ref_cube_stats, cube_cor, mc, sc
  sB_c = dOmega * sqrt(total(sc^2, /nan))
  if ~(finite(sA_c) and finite(sB_c) and sB_c gt 0) then begin
    print, 'FAIL: correlated A/B not finite'
    nfail++
  endif else if (sA_c / sB_c) lt 8 then begin
    print, 'FAIL: correlated A/B too small (expected A>>B): ', sA_c / sB_c
    nfail++
  endif else npass++

  ;----- gx_ref2chmp_one must not treat frame 1 as SDEV -----
  r = gx_ref2chmp_one(rmaps, a_beam=1.5, b_beam=1.5, phi_beam=0, chan=94, err_msg=em, /quiet)
  if obj_valid(r) then begin
    print, 'FAIL: gx_ref2chmp_one should refuse a raw time-series map array'
    nfail++
    obj_destroy, r
  endif else npass++

  cub = gx_ref2chmp_maps2cube(rmaps, err_msg=em)
  if ~isa(cub, 'STRUCT') then begin
    print, 'FAIL: maps2cube: ', em
    nfail++
  endif else begin
    r = gx_ref2chmp_one(cub, a_beam=1.5, b_beam=1.5, phi_beam=0, err_msg=em, /quiet)
    if ~obj_valid(r) then begin
      print, 'FAIL: gx_ref2chmp_one on cube wrapper: ', em
      nfail++
    endif else begin
      gx_ref_cube_bind_ref, r, cub
      if ~gx_ref_has_cube(r) then begin
        print, 'FAIL: bind did not attach cube'
        nfail++
      endif else npass++
      ; Mean map must not equal frame 1
      d0 = r->get(0, /map)
      if max(abs(d0.data - rmaps[1].data)) eq 0 then begin
        print, 'FAIL: Data map is time frame 1 (SDEV-pair bug)'
        nfail++
      endif else npass++
      obj_destroy, r
    endelse
  endelse

  ; Snapshot path still Method B (no cube)
  r = gx_ref2chmp_one(snap, err_msg=em, /quiet)
  if ~obj_valid(r) then begin
    print, 'FAIL: format-3 gx_ref2chmp_one: ', em
    nfail++
  endif else if gx_ref_has_cube(r) then begin
    print, 'FAIL: format-3 ref should not have a cube'
    nfail++
    obj_destroy, r
  endif else begin
    npass++
    obj_destroy, r
  endelse

  ;----- optional HISFM -----
  if n_elements(hisfm_root) eq 1 then begin
    cdir = hisfm_root + path_sep() + 'all_rotated'
    rdir = hisfm_root + path_sep() + 'all_rotated_refs'
    if file_test(cdir, /directory) and file_test(rdir, /directory) then begin
      cf = file_search(cdir, 'AIA94*.sav', count=nc)
      rf = file_search(rdir, 'AIA94*.sav', count=nr)
      if nc gt 0 and nr gt 0 then begin
        cuberef = gx_ref2chmp(cf[0], a_beam=1.5, b_beam=1.5, phi_beam=0, err_msg=em, /quiet)
        snapref = gx_ref2chmp(rf[0], a_beam=1.5, b_beam=1.5, phi_beam=0, err_msg=em2, /quiet)
        if ~obj_valid(cuberef) then begin
          print, 'FAIL: HISFM cube load: ', em
          nfail++
        endif else if ~gx_ref_has_cube(cuberef) then begin
          print, 'FAIL: HISFM all_rotated should attach a cube'
          nfail++
        endif else npass++
        if ~obj_valid(snapref) then begin
          print, 'FAIL: HISFM snapshot load: ', em2
          nfail++
        endif else if gx_ref_has_cube(snapref) then begin
          print, 'FAIL: HISFM all_rotated_refs should not be a cube'
          nfail++
        endif else npass++
        if obj_valid(cuberef) then obj_destroy, cuberef
        if obj_valid(snapref) then obj_destroy, snapref
      endif
    endif
  endif

  print, string(npass, nfail, format="('gx_test_chmp_sdev_ab: ',i0,' passed, ',i0,' failed')")
end
