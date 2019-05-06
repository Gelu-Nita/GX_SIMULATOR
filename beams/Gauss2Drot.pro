pro Gauss2Drot, u, a, f,noreform=noreform
;+
; Name:
;   Gauss2Drot
; Purpose:
;   Auxiliary procedure calculating the rotated 2D Gaussian beam.
;
; Input parameters:
;   u=[x, y] - concatenated 1D arrays of X and Y coordinates;
;              x and y arrays must contain equal numbers of elements;
;   a - Gaussian beam parameters:
;       a[0]=sx - 1st semi-axis, in arcsecs;
;       a[1]=sy - 2nd semi-axis, in arcsecs;
;       a[2]=theta - rotation angle of the sx semi-axis (in radians, 
;                    relative to the X axis, in counter-clockwise direction).
;
; Output parameters:
;   f - 1D array containing the computed beam values;
;       before plotting etc., it must be converted (e.g., by reform
;       procedure) to a 2D array.

 N=n_elements(u)/2
 x=u[0 : N-1]
 y=u[N : n_elements(u)-1]
 
 x=rebin(x, N, N)
 y=rebin(transpose(y), N, N)
 
 sx=a[0]
 sy=a[1]
 theta=a[2]
 
 xx= x*cos(theta)+y*sin(theta)
 yy=-x*sin(theta)+y*cos(theta)
 
 f=exp(-xx^2/sx^2-yy^2/sy^2)
 if ~keyword_set(noreform) then f=reform(f, N^2)
end