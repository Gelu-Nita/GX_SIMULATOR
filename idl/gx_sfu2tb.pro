function gx_sfu2tb,ds,R=R
  default,R,959.62720658243131
  arcsec2cm=gx_rsun(unit='cm')/R
  ds=(arcsec2cm)^2*ds
  return,1.4568525e026/ds
end  