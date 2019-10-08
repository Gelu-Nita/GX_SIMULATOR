;+
; :Description: QS_crd
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function QS_crd,a,L0,B0,P0,inversion=inv,sph=sphh,tosph=tosph
if N_elements(inv) le 0 then inv=0
if N_elements(sphh) le 0 then sphh=0
if N_elements(tosph) le 0 then tosph=0
if N_elements(P0) le 0 then p0=0.
if (where(tag_names(a) eq 'T0'))(0) ne -1 then field=1 else field=0
;Def orts
e0=(zx=(zy=fltarr(3)))
	cp=cos(p0*!pi/180)
	sp=-sin(p0*!pi/180)
	cth=cos(!pi/2-B0*!pi/180)
	sph=sin(L0*!pi/180)
	cph=cos(L0*!pi/180)
	sth=sin(!pi/2-B0*!pi/180)
	e0(0)=sth*cph
	e0(1)=sth*sph
	e0(2)=cth
	zx(0)=-e0(2)*e0(0)
	zx(1)=-e0(2)*e0(1)
	zx(2)=1.d0-e0(2)^2
	zx=zx/norm(zx)
	if abs(B0*!pi/180-!pi/2) le 1.e-6 then zx=[0,-1.,0]
	if abs(B0*!pi/180+!pi/2) le 1.e-6 then zx=[0,1.,0]
	zy(0)=zx(1)*e0(2)-zx(2)*e0(1)
	zy(1)=zx(2)*e0(0)-zx(0)*e0(2)
	zy(2)=zx(0)*e0(1)-zx(1)*e0(0)
	e1=zx*sp+zy*cp
	e2=zx*cp-zy*sp
	e0=e0/norm(e0)
	e1=e1/norm(e1)
	e2=e2/norm(e2)
;End Def orts
if sphh then begin
x0=a.x0*sin(a.x1)*cos(a.x2)
x1=a.x0*sin(a.x1)*sin(a.x2)
x2=a.x0*cos(a.x1)
if field then begin
r=sqrt(x0^2+x1^2+x2^2)
er0=x0/r&er1=x1/r&er2=x2/r
ep0=-er1&ep1=er0
et0=ep1*er2&et1=-ep0*er2&et2=ep0*er1-ep1*er0
rp=sqrt(ep0^2+ep1^2)
ep0=ep0/rp&ep1=ep1/rp
rp=sqrt(et0^2+et1^2+et2^2)
et0=et0/rp&et1=et1/rp&et2=et2/rp
t0=a.t0*er0+a.t1*et0+a.t2*ep0
t1=a.t0*er1+a.t1*et1+a.t2*ep1
t2=a.t0*er2+a.t1*et2
endif
endif else begin
x0=a.x0&x1=a.x1&x2=a.x2
if field then begin
t0=a.t0&t1=a.t1&t2=a.t2
endif
endelse
if inv then begin
xi0=x0*e0(0)+x1*e1(0)+x2*e2(0)
xi1=x0*e0(1)+x1*e1(1)+x2*e2(1)
xi2=x0*e0(2)+x1*e1(2)+x2*e2(2)
if field then begin
ti0=t0*e0(0)+t1*e1(0)+t2*e2(0)
ti1=t0*e0(1)+t1*e1(1)+t2*e2(1)
ti2=t0*e0(2)+t1*e1(2)+t2*e2(2)
endif
endif else begin
xi0=x0*e0(0)+x1*e0(1)+x2*e0(2)
xi1=x0*e1(0)+x1*e1(1)+x2*e1(2)
xi2=x0*e2(0)+x1*e2(1)+x2*e2(2)
if field then begin
ti0=t0*e0(0)+t1*e0(1)+t2*e0(2)
ti1=t0*e1(0)+t1*e1(1)+t2*e1(2)
ti2=t0*e2(0)+t1*e2(1)+t2*e2(2)
endif
endelse
if tosph then begin
r=sqrt(xi0^2+xi1^2+xi2^2)
t=acos(xi2/r)
p=atan(xi1,xi0)
if field then begin
er0=xi0/r&er1=xi1/r&er2=xi2/r
ep0=-er1&ep1=er0
et0=ep1*er2&et1=-ep0*er2&et2=ep0*er1-ep1*er0
rp=sqrt(ep0^2+ep1^2)
ep0=ep0/rp&ep1=ep1/rp
rp=sqrt(et0^2+et1^2+et2^2)
et0=et0/rp&et1=et1/rp&et2=et2/rp
tr=er0*ti0+er1*ti1+er2*ti2
tt=et0*ti0+et1*ti1+et2*ti2
tp=ep0*ti0+ep1*ti1
ti0=tr&ti1=tt&ti2=tp
endif
xi0=r&xi1=t&xi2=p
endif
if field then return,{x0:xi0,x1:xi1,x2:xi2,t0:ti0,t1:ti1,t2:ti2}
return,{x0:xi0,x1:xi1,x2:xi2}
end