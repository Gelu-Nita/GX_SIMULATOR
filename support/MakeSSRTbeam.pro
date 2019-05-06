pro MakeSSRTbeam, dEW, dNS, pEW, pNS, nx, ny, dx, dy, $
                  x, y, beam
;+
; Name:
;   MakeSSRTbeam
; Purpose:
;   Calculates the SSRT beam.
;
; Input parameters:
;   dEW, dNS - widths (at 1/2 level) of the SSRT E-W and N-S 1D linear 
;              interferometers, in arcsecs.
;   pEW, pNS - scanning directions of the SSRT E-W and N-S 1D linear
;              interferometers, in degrees, counterclockwise from the Y axis.
;   nx, ny -   size of the resulting map, in pixels.
;   dx, dy -   spatial resolution (pixel size) of the resulting map, 
;              in arcsecs.
;
; Output parameters:
;   x, y - X- and Y-coordinate arrays of the resulting map (1D arrays),
;          in arcsecs. The arrays are centered at (0, 0).
;   beam - the SSRT beam (2D array). The beam is not normalized, i.e.
;          it is assumed that beam=1 at (X, Y)=(0, 0).

 x=(dindgen(nx)-0.5*nx+0.5)*dx
 y=(dindgen(ny)-0.5*ny+0.5)*dy
 beam=dblarr(nx, ny)
 
 cEW=cos(!dpi*pEW/180)
 sEW=sin(!dpi*pEW/180)
 cNS=cos(!dpi*pNS/180)
 sNS=sin(!dpi*pNS/180)
 aEW=dEW^2/alog(2.0)/4
 aNS=dNS^2/alog(2.0)/4
 
 for i=0, nx-1 do for j=0, ny-1 do begin
  rEW=abs(x[i]*cEW+y[j]*sEW)
  rNS=abs(x[i]*cNS+y[j]*sNS)
  beam[i, j]=exp(-rEW^2/aEW-rNS^2/aNS)
 endfor           
end