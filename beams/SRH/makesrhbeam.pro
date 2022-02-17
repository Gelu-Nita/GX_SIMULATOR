pro MakeSRHbeam, sx, sy, rho, nx, ny, dx, dy, $
                 x, y, beam
; Input parameters:
;   sx, sy, rho - widths (at 1/e level, in arcsecs) and the XY correlation
;                 coefficient of the SRH beam.
;                 These parameters can be found in the FITS header as
;                 beam_sx, beam_sy, and beam_rho.
;   nx, ny - size of the resulting map, in pixels.
;   dx, dy - spatial resolution (pixel size) of the resulting map, 
;            in arcsecs.
;
; Output parameters:
;   x, y - X- and Y-coordinate arrays of the resulting map (1D arrays),
;          in arcsecs. The arrays are centered at (0, 0).
;   beam - the SRH beam (2D array). The beam is not normalized, i.e.
;          it is assumed that beam=1 at (X, Y)=(0, 0).

 x=(dindgen(nx)-0.5*nx+0.5)*dx
 y=(dindgen(ny)-0.5*ny+0.5)*dy
 beam=dblarr(nx, ny)

 xx=rebin(x, nx, ny)
 yy=rebin(transpose(y), nx, ny) 
 
 beam=exp(-0.5d0/(1d0-rho^2)*(xx^2/sx^2+yy^2/sy^2-2d0*rho*xx*yy/sx/sy))
end   