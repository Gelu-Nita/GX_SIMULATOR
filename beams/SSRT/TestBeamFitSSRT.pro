pro TestBeamFitSSRT,time, _extra=_extra
if n_elements(time) eq 0 then time=anytim('2016-Jul-24 06:14:05')
 
 sx_sy_theta=BeamFitSSRT(time,x,y,beam,_extra=_extra)
 
 Gauss2Drot, [x, y], sx_sy_theta, fit_beam,/noreform
 
 contour, beam, x, y, /isotropic, xstyle=1, ystyle=1, nlevels=10
 contour, beam, x, y, levels=[exp(-1.0), 0.5], c_linestyle=2, c_thick=3, /overplot
 
 tvlct, [0,     255, 0,     0,    255,     0,    128], $
        [0,     0,   255,   0,    0,       255,  128], $
        [0,     0,   0,     255,  255,     255,  0] 
 
 contour,fit_beam, x, y, levels=[exp(-1.0),0.5], c_color=2, /overplot
 plot_ellipse,sx_sy_theta,color=1,thick=3,/overplot
 plot_ellipse,sx_sy_theta,color=1,thick=3,/overplot,/half_max
end