;+
; :Description:
;    This routine performs spatial alignment of a map (1st argument)
;    to maximaize its correlation with a reference map (2nd argument).
;    Both maps can have different spatial resolution and FOV.
;    This procedure only changes xc and yc fields of the map structure
;    and does not touch data.
;
; :Params:
;    map        : in/out, a map to align
;    reference  : in, a reference map
;    
;     
;
;
; :Author: Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
pro gx_align_map, map, reference

; Find out what map has the highest spatial resolution
  if map.dx ge reference.dx then begin
    map_1 = map
    map_2 = reference
    dx_sign = 1
  endif else begin
    map_1 = reference
    map_2= map
    dx_sign = -1
  endelse
  
  ;Interpolate data to the coordinates of the map with the highest resolution
  map_2 = inter_map(map_2, map_1, cubic = -0.5)
  
  ;find shifts betweeen maps
  shifts = gx_align_image(map_2.data, map_1.data)
  shift_x = shifts[0]*map_1.dx/dx_sign
  shift_y = shifts[1]*map_1.dy/dx_sign
  
  ;change map center coordinates
  map.xc = map.xc + shift_x
  map.yc = map.yc + shift_y



end