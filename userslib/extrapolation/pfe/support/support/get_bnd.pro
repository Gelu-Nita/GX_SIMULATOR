function get_bnd,bx,by,bz
  if n_elements(clean) le 0 then clean=0
  n = size(bx,/dim)
  xb = -reform(bx[0,*,*])
  xt = reform(bx[n[0]-1,*,*])
  yb = -reform(by[*,0,*])
  yt = reform(by[*,n[1]-1,*])
  zb = -reform(bz[*,*,0])
  zt = reform(bz[*,*,n[2]-1])

  
  return,{xb:xb,xt:xt,yb:yb,yt:yt,zb:zb,zt:zt,n:n}
end