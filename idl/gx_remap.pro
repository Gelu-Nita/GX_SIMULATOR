function gx_remap,map,xrange,yrange,nxny,time=time
map_xrange=get_map_xrange(map)
map_yrange=get_map_yrange(map)
if map_xrange[0] le xrange[0] and map_xrange[1] ge xrange[1] and map_yrange[0] le yrange[0] and map_yrange[1] ge yrange[1] then begin
 if n_elements(time) ne 0 then smap=drot_map(map,time=time) else smap=map
 sub_map,smap,smap,xrange=xrange,yrange=yrange
 if n_elements(nxny) eq 2 then smap=rebin_map(smap,nxny[1],nxny[1])
endif else begin
  xc=mean(xrange)
  yc=mean(yrange)
  if n_elements(nxny) ne 2 then begin
    nxny=[floor(abs(xrange[1]-xrange[0])/map.dx),floor((xrange[1]-xrange[0])/map.dx)]
  endif
  dx=abs(xrange[1]-xrange[0])/(nxny[0]-1)
  dy=abs(yrange[1]-yrange[0])/(nxny[1]-1)
  time=keyword_set(time)?time:map.time
  refmap=make_map(fltarr(nxny[0],nxny[1])+1,xc=xc,yc=yc,dx=dx,dy=dy,time=time)
  smap=drot_map(map,ref=refmap)
endelse
 return,smap
end