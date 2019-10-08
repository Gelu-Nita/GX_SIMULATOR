pro stoem, th, ph, p, ex, ey, ez

;+ Creates a triple set of coordinate orts with respect to the center of the
; solar disk. Input parameter - the angles: Latitude of the solar center B0 (th),
; Longitude L0 (ph), and the position angle P0 (p).
;-


ex=fltarr(3) & ey=ex & ez=ex & zx=ex & zy=ex
	cp=cos(p)
	sp=sin(p)
	cth=cos(!pi/2-th)
	sph=sin(ph)
	cph=cos(ph)
	sth=sin(!pi/2-th)
	ex(0)=sth*cph
	ex(1)=sth*sph
	ex(2)=cth
	zx(0)=-ex(2)*ex(0)
	zx(1)=-ex(2)*ex(1)
	zx(2)=1.0-ex(2)^2
	r=sqrt(zx(0)^2+zx(1)^2+zx(2)^2)
	zx=zx/r
	if abs(th-!pi/2) le 1.e-6 then begin
	zx=[0,-1.,0]
	endif
	if abs(th+!pi/2) le 1.e-6 then begin
	zx=[0,1.,0]
	endif
	zy(0)=zx(1)*ex(2)-zx(2)*ex(1)
	zy(1)=zx(2)*ex(0)-zx(0)*ex(2)
	zy(2)=zx(0)*ex(1)-zx(1)*ex(0)
	ey=zx*sp+zy*cp
	ez=zx*cp-zy*sp
	r=sqrt(ex(0)^2+ex(1)^2+ex(2)^2)
	ex=ex/r
	r=sqrt(ey(0)^2+ey(1)^2+ey(2)^2)
	ey=ey/r
	r=sqrt(ez(0)^2+ez(1)^2+ez(2)^2)
	ez=ez/r
	end
