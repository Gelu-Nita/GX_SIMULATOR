;+
; Shared CHMP cell PostScript: metrics, optional spectrum, then Data|Model|
; residual maps. Used by live search, replot, and Best of Bests winner pages.
;-

function gx_chmp_spectrum_mode, ri
  compile_opt idl2
  if ~isa(ri, 'STRUCT') then return, 0b
  if ~tag_exist(ri, 'search_mode') then return, 0b
  return, strlowcase(strcompress(ri.search_mode, /rem)) eq 'spectrum'
end

function gx_chmp_cell_psname, ri
  compile_opt idl2
  if isa(ri, 'STRUCT') then if tag_exist(ri, 'psfile') then begin
    ps = strtrim(ri.psfile, 2)
    if ps ne '' then return, file_basename(ps)
  endif
  if isa(ri, 'STRUCT') then $
    return, strcompress(string(ri.a, ri.b, $
      format="('set_a',g0,'b',g0,'_final.ps')"), /rem)
  return, 'chmp_cell.ps'
end

; Union of the ~6 lowest RES2 and ~6 lowest CHI2 samples (legacy image
; neighborhood). /extra_only drops the two winning Qs already on the default pages.
function gx_chmp_debug_qidx, ri, extra_only=extra_only
  compile_opt idl2
  if ~isa(ri, 'STRUCT') then return, !null
  if ~ptr_valid(ri.allmetrics) then return, !null
  am = *ri.allmetrics
  n = n_elements(am.q)
  if n eq 0 then return, !null
  kmax = (n - 1L) < 5L
  ir = sort(am.res2)
  ic = sort(am.chi2)
  idx = [ir[0:kmax], ic[0:kmax]]
  idx = idx[uniq(idx, sort(idx))]
  if ~keyword_set(extra_only) then return, idx
  drop = lonarr(n)
  if tag_exist(ri, 'q_res2_best') then begin
    void = min(abs(double(am.q) - double(ri.q_res2_best)), wr)
    drop[wr] = 1
  endif
  if tag_exist(ri, 'q_chi2_best') then begin
    void = min(abs(double(am.q) - double(ri.q_chi2_best)), wc)
    drop[wc] = 1
  endif
  keep = where(drop[idx] eq 0, nk)
  if nk eq 0 then return, !null
  return, idx[keep]
end

; psDir from the argument, else result.psDir. Create it; if that fails
; (sav copied to another machine), use cwd/psDir.
pro gx_chmp_psdir_resolve, result, psDir
  compile_opt idl2
  if n_elements(psDir) eq 1 then begin
    if size(psDir, /tname) eq 'STRING' then if strtrim(psDir, 2) ne '' then goto, mkdir_it
  endif
  if isa(result, 'STRUCT') then if tag_exist(result, 'psDir') then begin
    p = result[0].psDir
    if size(p, /tname) eq 'STRING' then if strtrim(p, 2) ne '' then psDir = p
  endif
  if n_elements(psDir) eq 0 then psDir = curdir() + path_sep() + 'psDir'
mkdir_it:
  if file_test(psDir, /directory) then return
  catch, errn
  if errn ne 0 then begin
    catch, /cancel
    fallback = curdir() + path_sep() + 'psDir'
    message, 'Could not create psDir=' + string(psDir) + '; using ' + fallback, /info
    psDir = fallback
    if ~file_test(psDir, /directory) then file_mkdir, psDir
    return
  endif
  file_mkdir, psDir
  catch, /cancel
  if ~file_test(psDir, /directory) then begin
    fallback = curdir() + path_sep() + 'psDir'
    message, 'Could not create psDir=' + string(psDir) + '; using ' + fallback, /info
    psDir = fallback
    if ~file_test(psDir, /directory) then file_mkdir, psDir
  endif
end

; If any FILES already exist and /overwrite is off, one Yes/No dialog.
; Sets overwrite=1 when there is nothing to confirm or the user agrees.
pro gx_chmp_confirm_overwrite, files, overwrite=overwrite
  compile_opt idl2
  if keyword_set(overwrite) then return
  exist = !null
  for i = 0L, n_elements(files) - 1 do $
    if file_test(files[i]) then exist = [exist, files[i]]
  if n_elements(exist) eq 0 then begin
    overwrite = 1
    return
  endif
  nd = file_dirname(exist[0])
  msg = ['Overwrite existing PostScript in', nd, $
    string(n_elements(exist), format="(i0,' file(s)?')")]
  catch, errn
  if errn ne 0 then begin
    catch, /cancel
    message, 'Overwrite dialog failed; writing files.', /info
    overwrite = 1
    return
  endif
  answ = dialog_message(msg, /question)
  catch, /cancel
  overwrite = strupcase(strtrim(answ, 2)) eq 'YES'
end

; Channel/freq label for map pages (image placeholder spec_axis=0 uses the obs map).
pro gx_chmp_cell_axis, ri, spec_axis, is_chan
  compile_opt idl2
  spec_axis = 0d
  is_chan = 0b
  if ~isa(ri, 'STRUCT') then return
  if gx_chmp_spectrum_mode(ri) then begin
    if tag_exist(ri, 'spec_axis') then spec_axis = ri.spec_axis
    if n_elements(spec_axis) gt 0 then $
      if max(double(spec_axis), /nan) ge 50 then is_chan = 1b
    return
  endif
  objm = !null
  if tag_exist(ri, 'res2_best_metrics') then objm = ri.res2_best_metrics
  if ~obj_valid(objm) then if tag_exist(ri, 'chi2_best_metrics') then $
    objm = ri.chi2_best_metrics
  if ~obj_valid(objm) then return
  obsI = objm->get(1, /map)
  if ~valid_map(obsI) then obsI = objm->get(0, /map)
  if ~valid_map(obsI) then return
  if tag_exist(obsI, 'chan') then if n_elements(obsI.chan) gt 0 then $
    if finite(obsI.chan[0]) and (obsI.chan[0] ge 50) then begin
      spec_axis = double(obsI.chan[0])
      is_chan = 1b
      return
    endif
  if tag_exist(obsI, 'freq') then if n_elements(obsI.freq) gt 0 then $
    if finite(obsI.freq[0]) then spec_axis = double(obsI.freq[0])
end

; Best-effort image map at QTARGET from modDir + Data from the stored winner.
; Does not rebuild Method A S_sdev (not stored on the map object).
pro gx_chmp_debug_restore_image, ri, qtarget, cim, ax
  compile_opt idl2
  cim = !null
  ax = 0d
  if ~isa(ri, 'STRUCT') then return
  if ~tag_exist(ri, 'modDir') then begin
    message, 'debug: no modDir on result; skip Q=' + strtrim(string(qtarget), 2), /info
    return
  endif
  md = ri.modDir
  if ~file_test(md, /directory) then begin
    message, 'debug: modDir not available (' + string(md) + '); skip Q=' + $
      strtrim(string(qtarget), 2), /info
    return
  endif
  files = find_files(string([ri.a, ri.b], format="('*a',f0.2,'b',f0.2,'*.map')"), md)
  if n_elements(files) eq 1 then if files[0] eq '' then begin
    message, 'debug: no model maps in modDir for this (a,b); skip Q=' + $
      strtrim(string(qtarget), 2), /info
    return
  endif
  objm = !null
  if tag_exist(ri, 'res2_best_metrics') then objm = ri.res2_best_metrics
  if ~obj_valid(objm) then if tag_exist(ri, 'chi2_best_metrics') then $
    objm = ri.chi2_best_metrics
  if ~obj_valid(objm) then begin
    message, 'debug: no stored winner metrics; skip Q=' + strtrim(string(qtarget), 2), /info
    return
  endif
  obsI = objm->get(1, /map)
  obsS = objm->get(2, /map)
  msk = !null
  if tag_exist(ri, 'mask') then msk = ri.mask
  catch, errn
  if errn ne 0 then begin
    catch, /cancel
    message, 'debug: restore failed for Q=' + strtrim(string(qtarget), 2) + $
      ': ' + !error_state.msg, /info
    return
  endif
  map = obj_new()
  hit = -1L
  for i = 0L, n_elements(files) - 1 do begin
    obj_destroy, map
    restore, files[i]
    if ~obj_valid(map) then continue
    void = gx_getEBTELparms(map->get(/gx_key), aa, bb, qq)
    thr = (1d-4 * abs(double(qtarget))) > 1d-12
    if abs(double(qq) - double(qtarget)) le thr then begin
      hit = i
      break
    endif
  endfor
  if hit lt 0 then begin
    obj_destroy, map
    catch, /cancel
    message, 'debug: no map file matching Q=' + strtrim(string(qtarget), 2), /info
    return
  endif
  modidx = 0
  nlay = map->get(/count)
  if valid_map(obsI) and (nlay gt 1) then begin
    if tag_exist(obsI, 'chan') then if n_elements(obsI.chan) gt 0 then $
      if finite(obsI.chan[0]) then begin
        chans = dblarr(nlay)
        for k = 0L, nlay - 1 do chans[k] = map->get(k, /chan)
        void = min(abs(chans - obsI.chan[0]), modidx)
      endif
    if tag_exist(obsI, 'freq') then if n_elements(obsI.freq) gt 0 then $
      if finite(obsI.freq[0]) then begin
        freqs = dblarr(nlay)
        for k = 0L, nlay - 1 do freqs[k] = map->get(k, /freq)
        void = min(abs(freqs - obsI.freq[0]), modidx)
      endif
  endif
  modI = map->get(modidx, /map)
  obj_destroy, map
  cim = gx_metrics_map(modI, obsI, obsS, mask=msk, /no_renorm)
  catch, /cancel
  gx_chmp_cell_axis, ri, ax, isc
end

pro gx_plot_chmp_cell_debug, ri, spec_axis, is_chan=is_chan, levels=levels, $
  charsize=charsize, refs_all=refs_all, _extra=_extra
  compile_opt idl2
  idx = gx_chmp_debug_qidx(ri, /extra_only)
  if n_elements(idx) eq 0 then return
  am = *ri.allmetrics
  spec_mode = gx_chmp_spectrum_mode(ri)
  for j = 0L, n_elements(idx) - 1 do begin
    iq = idx[j]
    qv = am.q[iq]
    r2q = am.res2[iq]
    c2q = am.chi2[iq]
    cim = !null
    ax = spec_axis
    if spec_mode and ptr_valid(ri.spec_allmetrics) then begin
      sam = *ri.spec_allmetrics
      void = min(abs(double(sam.q) - double(qv)), is)
      if tag_exist(sam, 'channel_image_metrics') then begin
        cimk = sam[is].channel_image_metrics
        if n_elements(cimk) gt 0 then cim = cimk
      endif
      if tag_exist(sam, 'spec_axis_all') then $
        if n_elements(sam[is].spec_axis_all) gt 0 then ax = sam[is].spec_axis_all
    endif
    if n_elements(cim) eq 0 then gx_chmp_debug_restore_image, ri, qv, cim, ax
    have = 0b
    for ic = 0L, n_elements(cim) - 1 do if obj_valid(cim[ic]) then have = 1b
    if ~have then continue
    gx_plot_chmp_chanmaps, cim, ax, spec_axis, q=qv, res2=r2q, chi2=c2q, $
      levels=levels, charsize=charsize, is_chan=is_chan, _extra=_extra
  endfor
end

; Draw cell pages into an already-open PS device.
pro gx_plot_chmp_cell_pages, ri, charsize=charsize, levels=levels, header=header, $
  debug=debug, refs_all=refs_all, _extra=_extra
  compile_opt idl2
  resolve_routine, 'gx_plot_chmp_chanmaps', /compile_full_file, /either
  resolve_routine, 'gx_plot_chmp_qsearch', /compile_full_file, /either
  resolve_routine, 'gx_plot_chmp_spectrum', /compile_full_file, /either
  if ~isa(ri, 'STRUCT') then return
  default, charsize, !p.charsize
  default, levels, [20, 50, 80]
  default, header, ''
  if n_elements(debug) eq 0 and isa(_extra, 'STRUCT') then $
    if tag_exist(_extra, 'debug') then debug = keyword_set(_extra.debug)
  gx_chmp_cell_axis, ri, spec_axis, is_chan
  spec_mode = gx_chmp_spectrum_mode(ri)
  debug_sam = !null
  if keyword_set(debug) and spec_mode then if ptr_valid(ri.spec_allmetrics) then begin
    sam = *ri.spec_allmetrics
    idx = gx_chmp_debug_qidx(ri, /extra_only)
    if n_elements(idx) gt 0 then begin
      am = *ri.allmetrics
      for j = 0L, n_elements(idx) - 1 do begin
        void = min(abs(double(sam.q) - double(am.q[idx[j]])), is)
        debug_sam = [debug_sam, sam[is]]
      endfor
    endif
  endif
  !p.multi = [0, 1, 2]
  !p.font = -1
  gx_plot_chmp_qsearch, ri, charsize=charsize, header=header
  if spec_mode then begin
    !p.multi = [0, 1, 2]
    gx_plot_chmp_spectrum, cell_res2=ri, charsize=charsize, debug_sam=debug_sam, $
      _extra=_extra
  endif
  gx_chmp_cell_chanmaps, ri, 0, spec_axis, levels=levels, charsize=charsize, $
    is_chan=is_chan, refs_all=refs_all, _extra=_extra
  gx_chmp_cell_chanmaps, ri, 1, spec_axis, levels=levels, charsize=charsize, $
    is_chan=is_chan, refs_all=refs_all, _extra=_extra
  if keyword_set(debug) then $
    gx_plot_chmp_cell_debug, ri, spec_axis, is_chan=is_chan, levels=levels, $
      charsize=charsize, refs_all=refs_all, _extra=_extra
end

;+
; Write one cell's set_a*b*_final.ps (or ri.psfile).
;-
pro gx_plot_chmp_cell, ri, psDir, charsize=charsize, levels=levels, header=header, $
  debug=debug, overwrite=overwrite, refs_all=refs_all, _extra=_extra
  compile_opt idl2
  if ~isa(ri, 'STRUCT') then return
  gx_chmp_psdir_resolve, ri, psDir
  psname = gx_chmp_cell_psname(ri)
  filename = psDir + path_sep() + psname
  if file_test(filename) and ~keyword_set(overwrite) then begin
    message, 'Exists, skipped (pass /overwrite): ' + filename, /info
    return
  endif
  default, charsize, !p.charsize
  default, levels, [20, 50, 80]
  thisDevice = !d.name
  tvlct, rgb, /get
  loadct, 39
  cd, psDir, current=oldcwd
  set_plot, 'ps'
  psObject = obj_new('FSC_PSConfig', /color, /times, filename=psname, $
    directory=psDir, xoffset=0.4, yoffset=0.25, xsize=7.5, ysize=9.5, $
    landscape=0, bits=8)
  psKeys = psObject->GetKeywords()
  obj_destroy, psObject
  device, filename=psname, _extra=psKeys
  gx_plot_chmp_cell_pages, ri, charsize=charsize, levels=levels, header=header, $
    debug=debug, refs_all=refs_all, _extra=_extra
  device, /close
  cd, oldcwd
  tvlct, rgb
  set_plot, thisDevice
  !p.font = 0
  !p.multi = 0
  print, 'Wrote ', filename
end
