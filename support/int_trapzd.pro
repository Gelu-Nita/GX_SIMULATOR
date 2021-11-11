function int_trapzd, x, y
  i=indgen(n_elements(x)-1)
  return, total((y[i]+y[i+1])*abs(x[i+1]-x[i]))/2
end