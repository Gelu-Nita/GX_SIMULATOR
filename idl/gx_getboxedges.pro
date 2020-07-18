function gx_getboxedges,xrange=xrange,yrange=yrange,zrange=zrange
  p=dblarr(3,8)
  for i=0,7 do p[*,i] = [xrange[(i AND 1)], yrange[((i/2) AND 1)], zrange[((i/4) AND 1)]]
  index=[0,1,3,1,5,7,5,4,6,4,0,2,3,7,6,2]
  p=p[*,index]
  return,p
end  