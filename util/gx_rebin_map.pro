;this is just a wrapper around SSW's rebin_map.pro that corrects the rebinned map's resolution
;in a way that preserves the original edges of the field of view.
function gx_rebin_map,map,gx,gy,err=err,_extra=extra,congrid=congrid
  rmap=rebin_map(map,gx,gy,err=err,_extra=extra,congrid=congrid)
  sz=size(map.data)
  rsz=size(rmap.data)
  rmap.dx=1d*map.dx*sz[1]/rsz[1]
  rmap.dy=1d*map.dy*sz[2]/rsz[2]
  return,rmap
end
