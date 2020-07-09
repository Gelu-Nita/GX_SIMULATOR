function linspace, x1, x2, n
compile_opt idl2
  return, (dindgen(n)/ (n-1)) * (x2 - x1) + x1
end
