function BeamFitNoRH,file, x, y, beam,gx=gx, marx=marx,quiet=quiet
norh_rd_img,file, index, data
beam=norh_beam(index,marx=marx)
spp=index.norh.sec_per_pix
x=(dindgen(marx)-long(marx)/2)*spp
y=(dindgen(marx)-long(marx)/2)*spp
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