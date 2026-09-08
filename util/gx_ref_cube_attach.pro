;+
; :Description:
;    Attach a native time cube to a Data map structure (HAS_CUBE, NFRAME,
;    TIME_CUBE pointer, TIME_TEMPLATE header). Does not copy TIME_CUBE onto
;    the SDEV map.
;-
pro gx_ref_cube_attach, dmap, cube, template
  compile_opt idl2
  if ~valid_map(dmap) then return
  sz = size(cube)
  nframe = (sz[0] ge 3) ? long(sz[3]) : 1L
  tmpl = valid_map(template) ? template[0] : dmap[0]
  add_prop, dmap, has_cube=1b, nframe=nframe, $
    time_cube=ptr_new(double(cube)), time_template=tmpl, /replace
end
