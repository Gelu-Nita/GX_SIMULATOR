function int_trapzdLog, t, u
  i=indgen(n_elements(t)-1)
  return, total((exp(t[i+1]+u[i+1])-exp(t[i]+u[i]))/((t[i+1]+u[i+1])-(t[i]+u[i]))*(t[i+1]-t[i]))
end