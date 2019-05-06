function spl_int_cumsum,y

  return, total(y,/cum) - 0.5*(y + y[0])

end

function spl_int,y,h, definite = definite,reverse = reverse
compile_opt idl2 
  n = n_elements(y)
  if n_params() eq 1 then begin
    h =1d
  endif
  y0 = y
  if keyword_set(reverse) then y = reverse(y)
  x = findgen(n)*h
  n_i = (n-1)*100+1
  h_i = h*0.01d
  x_i =dindgen(n_i)*h_i
  yp0 = (-25d*y[0]+48d*y[1]-36d*y[2]+16d*y[3]-3d*y[4])/(12d*(h))
  ypn_1 = (25d*y[n-1]-48d*y[n-2]+36d*y[n-3]-16d*y[n-4]+3d*y[n-5])/(12d*(h))
  ;help,yp0,ypn_1
  y2 = spl_init(x, y, yp0 = yp0,ypn_1 = ypn_1)
  y_i=spl_interp(x, y, y2, x_i)
  y = y0
  if keyword_set(definite) then return,(total(y_i,/double)*h_i)
  res = (spl_int_cumsum(y_i)*h_i)[indgen(n)*100l]
  if keyword_set(reverse) then res = reverse(res)
  ;res = spl_int_cumsum(y)*h
  return,res
  
end