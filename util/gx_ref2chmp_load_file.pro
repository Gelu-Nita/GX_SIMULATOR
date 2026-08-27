;+
; Append restored / converted single-ref candidates from one file into ITEMS list.
;-
pro gx_ref2chmp_load_file, file, items, err_msg=err_msg
  err_msg = ''
  ; Use last '.' suffix — break_file treats first '.' as extension start
  bname = file_basename(file)
  dot = strpos(bname, '.', /reverse_search)
  if dot lt 0 then begin
    err_msg = 'Unsupported reference file type: ' + file
    return
  endif
  extu = strupcase(strmid(bname, dot + 1))

  if extu eq 'SAV' then begin
    sObj = OBJ_NEW('IDL_Savefile', file)
    sNames = sObj->Names()
    restored = !null
    restored_refs = !null
    for k = 0, n_elements(sNames) - 1 do begin
      sObj->Restore, sNames[k]
      void = execute('tmp=temporary(' + sNames[k] + ')')
      if strupcase(sNames[k]) eq 'REFS' then restored_refs = temporary(tmp) $
      else if strupcase(sNames[k]) eq 'REF' then restored = temporary(tmp) $
      else if ~isa(restored) then restored = temporary(tmp)
    endfor
    obj_destroy, sObj
    if isa(restored_refs) then begin
      for i = 0, n_elements(restored_refs) - 1 do items.add, restored_refs[i]
    endif else if isa(restored) then begin
      ; One CHMP object, one map object, map struct, or [data,sdev] pair
      if size(restored, /tname) eq 'OBJREF' and n_elements(restored) gt 1 then begin
        for i = 0, n_elements(restored) - 1 do items.add, restored[i]
      endif else begin
        items.add, restored
      endelse
    endif else begin
      err_msg = 'Save file did not contain usable ref data: ' + file
    endelse
    return
  endif

  if (extu eq 'FITS') or (extu eq 'FTS') or (extu eq 'FIT') then begin
    gx_fits2map, file, maps
    if size(maps, /tname) ne 'STRUCT' then begin
      err_msg = 'gx_fits2map failed for: ' + file
      return
    endif
    ; Recover CHAN from AIA###_... filenames when missing on the map
    chan_from_name = !null
    bu = strupcase(bname)
    if strpos(bu, 'AIA') eq 0 then begin
      dig = ''
      for ic = 3, strlen(bname) - 1 do begin
        ch = strmid(bname, ic, 1)
        if (ch ge '0') and (ch le '9') then dig += ch else break
      endfor
      if dig ne '' then chan_from_name = float(dig)
    endif
    for i = 0L, n_elements(maps) - 1 do begin
      mi = maps[i]
      if n_elements(chan_from_name) gt 0 then begin
        if ~tag_exist(mi, 'chan') then add_prop, mi, chan = chan_from_name
      endif
      ; Also try trailing numeric token in ID (e.g. "SDO AIA_4 94")
      if ~tag_exist(mi, 'chan') and ~tag_exist(mi, 'freq') then begin
        toks = strsplit(mi.id, /extract)
        if n_elements(toks) ge 1 then begin
          last = toks[n_elements(toks) - 1]
          if valid_num(last) then add_prop, mi, chan = float(last)
        endif
      endif
      items.add, mi
    endfor
    return
  endif

  err_msg = 'Unsupported reference file type: ' + file
end
