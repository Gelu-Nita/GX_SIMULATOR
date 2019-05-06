pro TestBeamFitNoRH, file, _extra=_extra
if n_elements(file) eq 0 then begin
 which,'TestBeamFitNoRH',outfile=outfile,/quiet
 file=file_dirname(outfile,/mark)+'ipa160724_061405.fits'
end
 sx_sy_theta=BeamFitNoRH(file, x, y, beam,_extra=_extra)

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