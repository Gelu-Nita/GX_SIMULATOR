function BeamFitSSRT,time,x,y,beam,gx=gx,quiet=quiet
  
  GetSSRTangles, time, dEW, dNS, pEW, pNS
  if ~keyword_set(quiet) then print, 'SSRT beam parameters:    ', dEW, dNS, pEW, pNS

  MakeSSRTbeam, dEW, dNS, pEW, pNS, 50, 50, 1.0, 1.0, x, y, beam

  FitBeam, x, y, beam, sx, sy, theta
  parms=[sx, sy, theta]
  obs_freq=5.7d
  
  if ~keyword_set(quiet) then print, 'Gaussian fit parameters @5.7GHz', parms
  if keyword_set(gx) then begin
   parms=parms*[obs_freq,obs_freq,-1/!dtor]
   if ~keyword_set(quiet) then print, 'Gaussian fit parameters for GX_Simulator @1GHz:', parms  
  endif
    
  return,parms
  
end  