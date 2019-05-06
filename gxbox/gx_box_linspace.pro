;+
; :Description:
;    Makes equidistant 1D grid in linear space
;
; :Params:
;    x1 - coordinate of the first grid node
;    x2 - coordinate of the last grid node
;    n - number of nodes on the grid
;
;
;
; :Author: Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function gx_box_linspace, x1, x2, n
compile_opt idl2
  return, (dindgen(n)/ (n-1)) * (x2 - x1) + x1
end
