pro FitBeam, x, y, beam, $
             sx, sy, theta
;+
; Name:
;   FitBeam
; Purpose:
;   Calculates the rotated 2D Gaussian fit to an instrument beam.
;
; Input parameters:
;   x, y - X- and Y-coordinate arrays of the beam map; 
;          they must have the same size;
;   beam - 2D array repesenting the beam.
; Note: the beam center is assumed to be at (0, 0),
;       its maximum value is assumed to be 1.
;
; Output parameters:
;   sx, sy - semi-axes of the fitted Gaussian ellipse (at 1/e level),
;            in arcsecs;
;   theta - rotation angle of the fitted Gaussian ellipse (in radians, 
;           relative to the X axis, in the counter-clockwise direction); 
;           if theta=0, the semi-axis sx is parallel to the X axis.

 if n_elements(x) ne n_elements(y) $
 then print, 'X and Y arrays must have the same size.' $
 else begin
  N=n_elements(x)

  s=size(beam)
  if (s[0] ne 2) || (s[1] ne N) || (s[2] ne N) $
  then print, 'Beam must be a 2D array with the size matching that of X and Y' $
  else begin
   s_avg=sqrt((max(x)-min(x))^2+(max(y)-min(y))^2)/4
 
   a=[s_avg, s_avg, 0d0]
   q=curvefit([x, y], reform(beam, N^2), w, a, function_name='Gauss2Drot', $
              /noderivative, itmax=100, tol=1d-6)
            
   sx=a[0]
   sy=a[1]
   theta=atan(sin(a[2]), cos(a[2]))
   if theta gt (!dpi/2) then theta-=!dpi
   if theta lt (-!dpi/2) then theta+=!dpi           
  endelse
 endelse
end