pro gx_fovmap2scanbox,fovmap,xc=xc,yx=yc,xfov=xfov,yfov=yfov,xrange=xrange,yrange=yrange,nx=nx,ny=ny,rsun=rsun
  rsun=fovmap->Get(/rsun)
  xc=fovmap->Get(/xc)
  yc=fovmap->Get(/yc)
  xfov=delta(get_map_xrange(fovmap->get(/map),/edge))
  yfov=delta(get_map_yrange(fovmap->get(/map),/edge))
  xrange=(xc+[-0.5d,0.5d]*xfov)/rsun
  yrange=(yc+[-0.5d,0.5d]*yfov)/rsun
  sz=size(fovmap->get(/data))
  nx=sz[1]
  ny=sz[2]
end  