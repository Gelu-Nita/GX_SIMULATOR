pro plot_ellipse,parms, xc=xc,yc=yc,deg=deg,overplot=overplot,half_max=half_max,_extra=_extra
  default,xc,0
  default,yc,0
  if n_elements(parms) ne 3 then return
  a=keyword_set(half_max)?parms[0]/sqrt(2*alog(2)):parms[0]
  b=keyword_set(half_max)?parms[1]/sqrt(2*alog(2)):parms[1]
  phi=keyword_set(deg)?parms[2]*!dtor:parms[2]
  
  ; Make an array from 0 to 2pi, in 1 degree increments
  th = findgen(361)*!dtor
  ; Create x and y values for unrotated ellipse at 5.7 GHz
  x = a*cos(th)
  y = b*sin(th)
  ; Rotate ellipse to correct angle
  xp = x*cos(phi) - y*sin(phi)
  yp = x*sin(phi) + y*cos(phi)
  if keyword_set(overplot) then oplot,xc+xp,yc+yp,_extra=_extra else plot,xc+xp,yc+yp,_extra=_extra
end  