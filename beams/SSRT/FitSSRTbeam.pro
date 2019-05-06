pro FitSSRTbeam, dEW, dNS, pEW, pNS, $
                 sx, sy, theta
;+
; Name:
;   FitSSRTbeam
; Purpose:
;   Calculates the rotated 2D Gaussian fit to the SSRT beam.
;
; Input parameters:
;   dEW, dNS, pEW, pNS - SSRT beam parameters 
;                        (output of the GetSSRTangles procedure).
;
; Output parameters:
;   sx, sy - semi-axes of the fitted Gaussian ellipse (at 1/e level),
;            in arcsecs;
;   theta - rotation angle of the fitted Gaussian ellipse (in radians, 
;           relative to the X axis, in the counter-clockwise direction); 
;           if theta=0, the semi-axis sx is parallel to the X axis.

 N=50
                  
 MakeSSRTbeam, dEW, dNS, pEW, pNS, N, N, 2.0, 2.0, x, y, beam
 s_avg=sqrt(dEW^2+dNS^2)/2/sqrt(alog(2d0))
 
 a=[s_avg, s_avg, 0d0]
 q=curvefit([x, y], reform(beam, N^2), w, a, function_name='Gauss2Drot', $
            /noderivative, itmax=100, tol=1d-6)
            
 sx=a[0]
 sy=a[1]
 theta=atan(sin(a[2]), cos(a[2]))
 if theta gt (!dpi/2) then theta-=!dpi
 if theta lt (-!dpi/2) then theta+=!dpi           
end