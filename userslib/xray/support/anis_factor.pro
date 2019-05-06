function anis_factor, Dist_Ang, theta_C, theta_b, dMu, a_4,bz
;  Possible parameter index assigment inside calling xray_tt.pro 
;  Parms[20].Name='Dist_Ang'    & Parms[20].Value=1            & Parms[20].Unit='none'    & Parms[20].Hint='Type of angular distribution'
;  Parms[21].Name='theta_C'     & Parms[21].Value=60           & Parms[21].Unit='degrees' & Parms[21].Hint='Loss-cone boundary'
;  Parms[22].Name='theta_b'     & Parms[22].Value=90           & Parms[22].Unit='degrees' & Parms[22].Hint='Angle of beam direction'
;  Parms[23].Name='dMu'         & Parms[23].Value=0.1          & Parms[23].Unit='none'    & Parms[23].Hint='dMu for gau/exp/SuGau loss-cone'
;  Parms[24].Name='a_4'         & Parms[24].Value=10           & Parms[24].Unit='none'    & Parms[24].Hint='Coeff for a*(Mu-xMu0)^4 for SuGau'
;  Parms[25].Name='Bz'          & Parms[25].Value=100          & Parms[25].Unit='Gauss'   & Parms[25).Hint='Signed vertical magnetic field component'

d_fraction=0.5
;use gaussian analytical solution for supergaussian too, ignores a4
mu_b=cos(theta_b*!dtor)
if dist_ang gt 3.5 then begin 
  if bz gt 1e-4 then d_fraction=(-Erf(mu_b/dmu) + Erf((1 + mu_b)/dmu))/(Erf((1 - mu_b)/dmu) + Erf((1 + mu_b)/dmu))
  if bz lt -1e-4 then d_fraction=(Erf((1 - mu_b)/dmu) + Erf(mu_b/dmu))/(Erf((1 - mu_b)/dmu) + Erf((1 + mu_b)/dmu))
end

return, d_fraction

end