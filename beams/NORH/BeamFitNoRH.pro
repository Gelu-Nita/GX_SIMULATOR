function BeamFitNoRH,file, x, y, beam, gx=gx, quiet=quiet
norh_rd_img,file, index, data
beam=norh_beam(index)
spp=index.norh.sec_per_pix
x=(dindgen(21)-10)*spp
y=(dindgen(21)-10)*spp
FitBeam, x, y, beam, sx, sy, theta
parms=[sx, sy, theta]
obs_freq=double(index.norh.obs_freq)
if ~keyword_set(quiet) then print, 'Gaussian fit parameters @'+index.norh.obs_freq+':', parms
if keyword_set(gx) then begin
  parms=parms*[obs_freq,obs_freq,-1/!dtor]
if ~keyword_set(quiet) then print, 'Gaussian fit parameters for GX_Simulator @1GHz:', parms  
endif
return,parms
end