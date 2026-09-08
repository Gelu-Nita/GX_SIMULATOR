;+
; :Description:
;    Group a LIST of gx_ref2chmp load items. Same-axis 2-D intensity maps
;    (M>=2) become one cube wrapper each. Format-3 snapshots and already
;    promoted cubes stay one item. Mixed FREQ/CHAN is left for gx_ref2chmp.
;-
pro gx_ref2chmp_group_timeseries, items, err_msg=err_msg
  compile_opt idl2
  err_msg = ''
  n = items.count()
  if n lt 2 then return

  used = bytarr(n)
  grouped = list()
  for i = 0L, n - 1 do begin
    if used[i] then continue
    it = items[i]
    has_tc = 0b
    if size(it, /tname) eq 'STRUCT' and n_elements(it) eq 1 then $
      if tag_exist(it, 'time_cube') then has_tc = 1b
    if gx_ref2chmp_is_format3(it) or gx_ref2chmp_item_is_cube(it) or has_tc then begin
      grouped.add, it
      used[i] = 1b
      continue
    endif
    if ~valid_map(it) then begin
      grouped.add, it
      used[i] = 1b
      continue
    endif
    if gx_ref_is_sdev_map(it) then begin
      grouped.add, it
      used[i] = 1b
      continue
    endif
    ax = gx_ref_item_axis(it, is_chan=ic)
    if ~finite(ax) then begin
      grouped.add, it
      used[i] = 1b
      continue
    endif
    sz = size(it[0].data)
    if sz[0] ne 2 then begin
      grouped.add, it
      used[i] = 1b
      continue
    endif

    members = list()
    members.add, it[0]
    used[i] = 1b
    for j = i + 1, n - 1 do begin
      if used[j] then continue
      jt = items[j]
      if ~valid_map(jt) or gx_ref2chmp_is_format3(jt) or gx_ref_is_sdev_map(jt) then continue
      if gx_ref2chmp_item_is_cube(jt) then continue
      axj = gx_ref_item_axis(jt, is_chan=icj)
      if ~finite(axj) then continue
      if icj ne ic then continue
      thr = (1d-3 * abs(ax)) > 1d-6
      if abs(axj - ax) gt thr then continue
      jsz = size(jt[0].data)
      if jsz[0] ne 2 then continue
      members.add, jt[0]
      used[j] = 1b
    endfor

    nm = members.count()
    if nm eq 1 then grouped.add, members[0] $
    else begin
      cub = gx_ref2chmp_maps2cube(members, err_msg=em)
      if ~isa(cub, 'STRUCT') then begin
        err_msg = em
        grouped.add, it
      endif else grouped.add, cub
    endelse
  endfor

  items.remove, /all
  foreach g, grouped do items.add, g
end
