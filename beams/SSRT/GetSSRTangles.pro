pro GetSSRTangles, time0, dEW, dNS, pEW, pNS 
;+
; Name:
;   GetSSRTangles
; Purpose:
;   Calculates the parameters of the SSRT beam at the specified time.
;
; Input parameters:
;   time0 - the time (UT, in anytim format) 
;           for which the parameters are desired.
;
; Output parameters:
;   dEW, dNS - widths (at 1/2 level) of the SSRT E-W and N-S 1D linear 
;              interferometers, in arcsecs.
;   pEW, pNS - scanning directions of the SSRT E-W and N-S 1D linear
;              interferometers, in degrees, counterclockwise from the Y axis.

 u=1d0*anytim(time0, /external)
 year=u[6]
 month=u[5]
 day=u[4]
 hour=u[0]
 minute=u[1]+(u[2]+u[3]/1000)/60

 solar_coords=get_sun(time0)
 d=solar_coords[7]*!dpi/180 ;declination
 P_angle=solar_coords[12] ;polar angle
 
 h=1.0*hour+1.0/60*minute 
 jdcnv, year, month, day, h, jd
 sunpos, jd, ra, dec
 ct2lst, lst, 102.0+13.0/60, a, jd
 noon=h+(ra/15-lst)*0.99726958
 if noon lt 0.0 then noon+=24
 if noon ge 24.0 then noon-=24
 noon*=3600

N = 1000;
_W = 15. * !dtor / 3600.;
t = findgen(N)/(N-1.)*10.*3600.;
h = _W*(t - noon);

phi = 0.903338787600965;
lambda_Nd = 0.052/(127.*4.9);

_c    = 2.99793e8;

gP =  atan(tan(h)*sin(d)) + !pi/2;
gQ =  atan(-(sin(d)/tan(h) + cos(d)/(sin(h)*tan(phi)))) + !pi/2;
gQ[where(h gt 0.)] = !pi + gQ[where(h gt 0.)];

base = [1.,0.,0.];
base_phi = base;
base_phi[0] =-sin(phi)*base[0] + 0.*base[1] + cos(phi)*base[2];
base_phi[1] =       0.*base[0] + 1.*base[1] +       0.*base[2];
base_phi[2] = cos(phi)*base[0] + 0.*base[1] + sin(phi)*base[2];
u =  sin(h)*base_phi[0] + cos(h)*base_phi[1];
v = -sin(d)*cos(h)*base_phi[0] + sin(d)*sin(h)*base_phi[1] + cos(d)*base_phi[2];
pVU = u;
pVV = v;
aV = sqrt(u^2. + v^2.);
gV = !pi - atan(u,v);

base = [0.,1.,0.];
base_phi = base;
base_phi[0] =-sin(phi)*base[0] + 0.*base[1] + cos(phi)*base[2];
base_phi[1] =       0.*base[0] + 1.*base[1] +       0.*base[2];
base_phi[2] = cos(phi)*base[0] + 0.*base[1] + sin(phi)*base[2];
u =  sin(h)*base_phi[0] + cos(h)*base_phi[1];
v = -sin(d)*cos(h)*base_phi[0] + sin(d)*sin(h)*base_phi[1] + cos(d)*base_phi[2];

pUU = u;
pUV = v;
aU = sqrt(u^2. + v^2.);
gU = !pi - atan(u,v);

base = [62.5,63.5,0.];
base_phi = base;
base_phi[0] =-sin(phi)*base[0] + 0.*base[1] + cos(phi)*base[2];
base_phi[1] =       0.*base[0] + 1.*base[1] +       0.*base[2];
base_phi[2] = cos(phi)*base[0] + 0.*base[1] + sin(phi)*base[2];
u =  sin(h)*base_phi[0] + cos(h)*base_phi[1];
v = -sin(d)*cos(h)*base_phi[0] + sin(d)*sin(h)*base_phi[1] + cos(d)*base_phi[2];
pUVU = u;
pUVV = v;
aUV = sqrt(u^2. + v^2.);
gUV = !pi - atan(u,v);

base = [62.5,-63.5,0.];
base_phi = base;
base_phi[0] =-sin(phi)*base[0] + 0.*base[1] + cos(phi)*base[2];
base_phi[1] =       0.*base[0] + 1.*base[1] +       0.*base[2];
base_phi[2] = cos(phi)*base[0] + 0.*base[1] + sin(phi)*base[2];
u =  sin(h)*base_phi[0] + cos(h)*base_phi[1];
v = -sin(d)*cos(h)*base_phi[0] + sin(d)*sin(h)*base_phi[1] + cos(d)*base_phi[2];
pVUU = u;
pVUV = v;
aVU = sqrt(u^2. + v^2.);
gVU = !pi - atan(u,v);

cosP = sin(h) * cos(d);
cosQ = cos(h)*cos(d)*sin(phi) - sin(d)*cos(phi);

given_t = hour*3600. + minute*60.;
if (given_t gt 10.*3600. - 360.) then given_t = noon;

t0_ind = where(t lt given_t - 180.,t0_count);
t1_ind = where(t gt given_t + 180.,t1_count);
if (t0_count eq 0 or t1_count eq 0) then begin
  t0 = n_elements(t)/2;
  t1 = n_elements(t)/2;
end else begin
  t0 = t0_ind[n_elements(t0_ind) - 1];
  t1 = t1_ind[0];
end
t_i = (t0+t1)/2l;

EW_angle = -(gU[t_i]*!radeg - 90.);
NS_angle = -(gQ[t_i]*!radeg - 90.);
if (given_t gt noon) then NS_angle = 180. + NS_angle;
title = string(year) + '/' + string(month,format='(I2.2)') + '/' + string(day,format='(I2.2)') + ', ' + string(hour,format='(I2.2)') + ':' + string(minute,format='(I2.2)');

cosH = cos(_W*(given_t - noon));
sinH = sin(_W*(given_t - noon));

cosP = sinH * cos(d);
cosQ = cosH * cos(d) * sin(phi) - sin(d) * cos(phi);
sinP = sqrt(1. - cosP^2.);
sinQ = sqrt(1. - cosQ^2.);

df_SSRT = 250000.*2; Hz
f0_SSRT = 5.73e9; Hz

dP_SSRT = cosP/sinP * df_SSRT/f0_SSRT * !radeg * 3600.;
dQ_SSRT = cosQ/sinQ * df_SSRT/f0_SSRT * !radeg * 3600.;

beamEW = lambda_Nd/sinP * !radeg * 3600.;
beamNS = lambda_Nd/sinQ * !radeg * 3600.;

dEW=beamEW
dNS=beamNS
pEW=EW_angle - P_angle
pNS=NS_angle - P_angle
end
