function gx_sfu2tb,ds,R=R
  ; Flux [sfu] -> brightness temperature [K] scale factor (per pixel area ds [arcsec^2]):
  ;   Tb = gx_sfu2tb(ds) * sfu / freq_GHz^2
  ; Inverse: gx_tb2sfu.pro
  default,R,959.62720658243131
  arcsec2cm=gx_rsun(unit='cm')/R
  ds_cm2=(arcsec2cm)^2*double(ds)
  return,1.4568525d026/ds_cm2
end
