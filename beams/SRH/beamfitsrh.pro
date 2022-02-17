function BeamFitSRH,filename,x,y,beam,gx=gx,quiet=quiet,obs_freq=obs_freq
  if n_elements(filename) eq 0 then filename=dialog_pickfile(filter=['*.fits','*.fts'])
  header=HEADFITS(filename)
  obs_freq=fxpar(header,'OBS-FREQ')
  rho=fxpar(header,'beam_rho')
  sy=fxpar(header,'beam_sy')
  sx=fxpar(header,'beam_sx')
  dx=fxpar(header,'CDELT1')
  dy=fxpar(header,'CDELT2')
  if ~keyword_set(quiet) then print, 'SRH beam parameters:    ', sx,sy,rho
  
  MakeSRHbeam, sx, sy, rho, 51, 51, dx, dy, x, y, beam
  
  FitBeam, x, y, beam, sx, sy, theta
  parms=[sx, sy, -theta/!dtor]
  beam=beam/total(beam); normalize the beam
  if ~keyword_set(quiet) then print, string(obs_freq,format="('Gaussian fit parameters @',g0,'GHz')"), parms
  if keyword_set(gx) then begin
   parms=parms*[obs_freq,obs_freq,1]
   if ~keyword_set(quiet) then print, 'Gaussian fit parameters for GX_Simulator @1GHz:', parms  
  endif
    
  return,parms
  
end  