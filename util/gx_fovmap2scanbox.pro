pro gx_fovmap2scanbox,fovmap,xc=xc,yx=yc,xfov=xfov,yfov=yfov,xrange=xrange,yrange=yrange,nx=nx,ny=ny,rsun=rsun,b0=b0,l0=l0
  omap=fovmap
  if ~obj_valid(omap) then begin
    omap=obj_new('map')
    omap->setmap,0,fovmap
    destroy_after=1
  endif
  l0=omap->Get(/l0)
  if n_elements(l0) eq 0 then l0=0.0
  b0=omap->Get(/b0)
  if n_elements(b0) eq 0 then begin
    pbr=pb0r(omap->get(/time),l0=l0)
    b0=pbr[0]
  endif
  rsun=omap->Get(/rsun)
  xc=omap->Get(/xc)
  yc=omap->Get(/yc)
  xfov=delta(get_map_xrange(omap->get(/map),/edge))
  yfov=delta(get_map_yrange(omap->get(/map),/edge))
  xrange=(xc+[-0.5d,0.5d]*xfov)/rsun
  yrange=(yc+[-0.5d,0.5d]*yfov)/rsun
  sz=size(omap->get(/data))
  nx=sz[1]
  ny=sz[2]
  if keyword_set(destroy_after) then obj_destroy,omap
end  