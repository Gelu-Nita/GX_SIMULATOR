; NAME:
;	ee_bremcross
;
; PURPOSE:
;	Computes the relativistic cross section for electron-electron bremsstrahlung,
;	differential in photon energy.
;
; CATEGORY:
;	HESSI, Spectra, Modeling
;
; CALLING SEQUENCE:
;	result=ee_bremcross(eel, eph)
;
; CALLS:
;	FUNCTION part1(x) (this file)
;   FUNCTION part2(x) (this file)
;   FUNCTION partH(x) (this file)
;   FUNCTION QROMO    (standard IDL function)
;
; INPUTS:
;
;	ee		-	Scalar of an electron energy at which to compute
;               the bremsstrahlung cross-section
;
;	eph		-	Scalar of photon energie
;
;
; OPTIONAL INPUTS:
;	none
;
; OUTPUTS: (FUNCTION value of ee_bremcross)
;	cross	-	Scalar of bremsstrahlung cross-section corresponding to the
;				input ee and eph.
;
; OPTIONAL OUTPUTS:
;	none
;
; KEYWORDS:
;	none
;
; COMMON BLOCKS:
;	share1   - general usage variables; used by PART1, PART2, and PARTH
;   share_a  - 'a' coefficients; used by PART1 only
;   share_b  - 'b' coefficients; used by PART2 only
;
; SIDE EFFECTS:
;
;
; RESTRICTIONS:
;   both ee and eph should be scalars
;
; PROCEDURE:
;	The cross section is from Equation (1) of E. Haug (Solar Physics, 178,
;	341, 1998).
;
;   NOTE: There is a misprint in H(e,k,x) for k > 0.5 (see page 347)
;       the term in last line for H(e,k,x) (e*r/x+s) should be (e*r/ww+s)
;
;
; MODIFICATION HISTORY:
;   Version 1, eduard@astro.gla.ac.uk, 23 August 2003
;   25-08-2003: When photon energy approaches maximum photon eneregy the integral
;   of part2 becomes almost equal to the result part1 with a different sign,
;   as spotted by Emslieg@uah.edu
;   28-08-2003: due to round off error in the integral CS can be negative when
;   k is close to maximum k (Emslieg@uah.edu)
;   28-08-2003: Simpson's method of integration is not enough due to long
;   time of convergence (eduard@astro.gla.ac.uk)
;   07-09-2003: common blocks created to speed-up cross-section calculation
;   (eduard@astro.gla.ac.uk)
;-

;*************************************************************************************
FUNCTION part1,x
;first part of the cross section

common share1, e,e2,e3,e4,p,p2,p3,p4,p6,k,k2,k3,W1,W4
common share_a, a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19
;*************************************************************************************

x2=x*x
x3=x*x2
x4=x3*x

W =sqrt(-1.+(e-k-x)^2)

L1=alog(e-k-x+W)
L2=alog((W1*W1+W1*W)/x-e+k)
L3=alog((p2+p*W)/(x+k)-e)
L4=alog(1.+(e-k+1)*(W1*W1-(e-k+1.)*x+W4*W)/(W1*W1+2.*x))

c1=W*(a1+10.*k/(x+k)-a2*x3+a3*x2+a4*x+a5/x-a6/x2+a7/x3)
c2=L1*(a8+4.*(x/k-k/x)-a9/(x+k))+a10*L2/W1
c3=L3*(a11-a12*x4+a13*x3+a14*x2+a15*x+a16/x+a17/x2-a18/x3)/p
c4=a19*W4*L4

result=c1+c2+c3+c4

IF FINITE(result) EQ 0 THEN message,'Error: NaN in part1 !!!'
return, result

END
;*************************************************************************************

;*************************************************************************************
FUNCTION part2,x
;second part of the cross section to be integrated
common share1, e,e2,e3,e4,p,p2,p3,p4,p6,k,k2,k3,W1,W4

common share_b, b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,$
b20,b21,b22,b23,b24,b25,b26,b27
;*************************************************************************************

x2=x*x
x3=x*x2
x4=x3*x

W =sqrt(abs(-1.+(e-k-x)^2))
L1=alog(e-k-x+W)
L2=alog((W1*W1+W1*W)/x-e+k)
L3=alog((p2+p*W)/(x+k)-e)
L4=alog(1.+(e-k+1)*(W1*W1-(e-k+1.)*x+W4*W)/(W1*W1+2.*x))

ro=sqrt(2.*(e+1.-k-x))
Wp=SQRT((e-x)^(2)+2.*k-1.)
W5 =sqrt(k2*W*W+2.*k*x*(x+k))
Wp5=sqrt(x2*W*W+2.*k*x*(x+k))
W6=sqrt((e-1.)*(e-1)*W*W+2.*(e-1.)*k*x)

L =alog(.5*((e-k-1.)*ro+sqrt(abs((W1*W1+2.*x)*(-4.+ro*ro))))/x)
Lp=alog(.5*((e-1.-x)*ro+Wp*sqrt(abs(-4.+ro*ro)))/k)
L5=alog(1.+W*(k*W+W5)/(x*(x+k)))
Lp5=alog(1.+W*(x*W+Wp5)/(k*(x+k)))
L6 =alog(1.+W*((e-1.)*W+W6)/(k*x))

s1=-2.*k*L1/(x+k)+L3*(b1/x+b2/(x+k)+b3/(e-1.-x))/p
s2=L*(b4+b5/x+b6/x2+(b7/x-b8)/(W1*W1+2.*x))/sqrt(W1*W1+2.*x)
s3=Lp*(b9+x+b10/x+b11/(e-1.-x)+(b12+b13*x-b14*x2+b15/x)/(Wp*Wp))/Wp
s4=L5*(b16-b17*x+b18/x+b19/(x+k))/W5
s5=Lp5*(b20-2.*x2+b21*x+4.*e*k/x+b22/(x+k)+(b23+b24/(x+k))/(e-1.-x))/Wp5
s6=L6*(b25+b26*x+x2+b27/x)/W6

result=s1+s2+s3+s4+s5+s6

IF FINITE(result) EQ 0 THEN message,'Error: NaN in part2 !!!'
return,result

END
;*************************************************************************************

;*************************************************************************************
FUNCTION partH,x
;calculates H(e,k,x)
common share1,e,e2,e3,e4,p,p2,p3,p4,p6,k,k2,k3
;*************************************************************************************

x2=x*x
x3=x*x2
x4=x3*x

W =sqrt(-1.+(e-k-x)^2)
ww=sqrt(abs(2.*k-1.))

;*************************************************************************************

IF k LT .5 then begin
W7=sqrt(2.*k-k2-2.*k*ww)
W8=sqrt(2.*k-k2+2.*k*ww)
;Print,'K is less than 0.5'
h1=(3.*e*k-e-k+1.)/ww+e+2.*k-1.-p2*k*(e/ww-1.)/(p2+2.*k)
h2=(e+k-3.*e*k-1.)/ww+e+2.*k-1.+p2*k*(e/ww+1.)/(p2+2.*k)
result=W7*asin(0.99998*(1.+(k+ww)*(e-k-x))/(e+ww-x))*h1/(4.*k)+$
       W8*asin(0.99998*(1.+(k-ww)*(e-k-x))/(e-ww-x))*h2/(4.*k)
endif
;*************************************************************************************

IF k EQ .5 then begin
;Print,'K is equal to 0.5'
result=(e+1.)*W/(2.*e*(e-x))+(1.5*e+.25-0.5/e-3./(4.*e2))*asin(.5+3./(4.*(e-x)))/sqrt(3.)
endif
;*************************************************************************************

IF k GT .5 then begin
;Print,'K is larger than 0.5'
r=sqrt(.5*k*(sqrt(k2+4.*k)+k-2.))
s=sqrt(.5*k*(sqrt(k2+4.*k)-k+2.))
u=k *(e-x)^2+(2.*k-k2-r*W)*(e-x)-2.*k2+k-ww*s*W
v=ww*(e-x)^2-(2.*k*ww+s*W)*(e-x)+ww*(k2-1.+r*W)
L0=alog(sqrt(u*u+v*v)/((e-x)*(e-x)+2.*k-1.))
h1=((e+k-3.*e*k-1.)*s/ww-(e+2.*k-1.)*r+p2*k*(e*s/ww-r)/(p2+2.*k))*L0/(2.*k)
h2=((e+k-3.*e*k-1.)*r/ww+(e+2.*k-1.)*s+p2*k*(e*r/ww+s)/(p2+2.*k))   /(2.*k)
result=h1+h2*atan(v/u)
endif

; H(e,k,x) calculated
IF FINITE(result) EQ 0 THEN message,'Error: NaN in partH !!!'
return, result

end
;*************************************************************************************

function ee_bremcross, ee, eph
;differential e-e cross section in laboratory frame
;Equation (1) Haug (1998) Sol.Phys. 178 pp 341-351
;Last modified 22/08/2003 by eduard@astro.gla.ac.uk

;****************** CONSTANTS **********************************************
mec2 = 510.9989d00                       ; Electron rest mass (keV)
ro2 = 7.941d-26                          ; classical e- radius squared (cm^2)
alpha=1.d0/137.036d0                     ; fine structure constant
;***************************************************************************

IF n_elements(ee+eph) GT 1 THEN message,'Error: Ee and Eph should be scalars !!!'

cross =0.

;****************** General usage values ***********************************
;these are not modified further
common share1, e,e2,e3,e4,p,p2,p3,p4,p6,k,k2,k3,W1,W4

e=1.+(ee/mec2)
e2=e*e
e3=e2*e
e4=e2*e2
p=sqrt(e*e-1.)
p2=p*p
p3=p2*p
p4=p2*p2
p6=p4*p2
k=eph/mec2
k2=k*k
k3=k*k2


;************** MAX photon energy at 0 degrees *****************************
; see Koch & Motz 1959 for details
;***************************************************************************

IF  (k LT (e-1.)/(e+1.-p)) AND (eph LT ee) THEN BEGIN
; for theta =0

W1=sqrt(p2-2.*e*k+k2)
W4=sqrt(-4.+(e-k+1.)^2)

;*************************************************************************************
common share_a, a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19

a1=5.*e/(12.*k3)-(16.*e+13.)/(12.*k2)+(8.*e+35.+(15.*e+14)/p2)/(6.*k)+$
(e2-e*k+19.*e-9.*k)/(6.*p2)+(e-.5)/p4

a2=(2.*e2+1)/(2.*p4*k3)

a3=((e*k+8.*e-1.+.5*e/k)/3.+(4.*e-.5)/p2)/(p2*k2)

a4=(k*(e/6.-.5)-4.*e*(e-1.)/3.-13./3.+(e-3.5)/p2-e*(2.*e+1.)/(3.*k))/(p2*k)+1./(4.*k3)

a5=k*(3.*e2-e*k-4.*e+9.+8./p2+((e-k)*(e3+2.5*p2-5.*e/3.)-1.5*p2-2.*k2/3.+6.*k+$
k*(8.-2.*k-4.*e*k-k*(2.*e2-k2+2.)/(W1*W1))/p2)/(W1*W1) )/p2

a6=k2*(4.*e-1./3.+(6.*e-1.)/p2+(e*k-k2+2)/(3.*W1*W1))/p2

a7=2.*k3*(2.*e2+1.)/(3.*p4)

a8=(p2-e*k-2.*k2)/4.+4.*e+21.*k/4.+7.-k2/(e+1)+k2/(e-k-1)-$
(5.*e*k+2.5*e-5.*k+4.5)/p2-(2.*e*k+4.*e-4.*k+4.)/p4-$
(8.*e2/3.+2.*e+1./6.)/k+(2.*e)*(e+1.)/(3.*k2)-(2.*e2+3.)/(12.*k3)

a9=2.*k*(e+2)

a10=2.5*p2-6.*e*k+5.*k2-7.*e+15.*k-2.*e/(e-k-1.)-(k2*(e*k+6.*e-k-4.)+$
e*(e+35.*k)/3.-10.*k+5.)/p2+W1*W1*((4.-2.*e)/p4+p2/(2.*(p2+2.*k)) )+$
k*((3.5-2.*k)*(e-k)+4.5+(4.*k3-13.*e*k2/3.-2.*e3/3.-2.*e*k+e-8.*k+4.)/p2)/(W1*W1)-$
k2*(e*(e-k)+1.)/(p2*W1*W1*W1*W1)

a11=k*(5.*k+7.)/3.-6.*e*k+k*(e+(k+4.)/3.)/p2+k*(21.*e3/2.+1.)/p4+(2.*e-k3)/(e-k-1.)

a12=e*(e2+2.)/(2.*p4*k3)

a13=((2.*e2-1.)*k+2.*e*(2.*e-1.)*(1.+3./p2)+2.*e2/k)/(3.*p2*k2)

a14=(6.-2.*e-k-2.*e2*(e-k-1.)+(5.-4.*e)/p2-2.*e3/k)/(p2*k)

a15=5.-8.*e+5.*k+(2.*k+12.+6./p2)/p2+(4.*e2-4.*e+2./p2)/k-k2/(e-k-1.)

a16=k*(k*(4.*e-2.*k-4.-(16.+k)/p2+(8.*e3-10.)/p4)-4.*e2+4.*e-2./p2+2.*e/(e-k-1))

a17=e*k2*(2.*e2-k*(2.*e-1.)*(1.+3./p2))/p2

a18=2.*e*k3*(e-k-3.*k/p2)/(3.*p2)

a19=-(e+1)/4.+.5*k2*(e-k)/(e-k-1.)^2
;*************************************************************************************

;*************************************************************************************
common share_b, b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,$
                b20,b21,b22,b23,b24,b25,b26,b27

b1=k*(5.+4.*k-9.*e+(2.*k+12.)/p2+6./p4)+2.*e*(4.*e-3.-2./(e-1.))-8.*e3/k+e*(2.*e-k2)/(e-k-1.)

b2=k*(2.*(e+1.)+4.*(e3-3.*e2+1.)/(-k2+(e-1.)^2))+8.*e3/k

b3=e*k-p2+3.*e-k+1.-2.*e/k+(2.-6.*k-2.*(e-1.)*k2)/(e+k-1.)

b4=2.*k+0.5*(9-e)-3.*k/(e-k-1.)

b5=1.5*p2-k*(4.*e-k+8.)+k2/(e-k-1.)

b6=k*(4.*(e-k)-2.)/(e-k-1.)

b7=.5*(p2-(e-1.)*k)*(p2-k*(3.*e-2.*k+1.))

b8=.5*(e+k-1.)*(p2-k*(3.*e-2.*k+1.))

b9 =2.*k-4.*e-9.+4./k

b10=1.5*p2+.5*(9-e)*k

b11=e-1.-3.*k+2./k

b12=e*(e*k-2.*p2-k)

b13=.5*(5.*e2+e*k-2.*e+3.*k-3.)

b14=e+k-1.

b15=.5*p2*(p2-(e-1.)*k)

b16=6.*e*k+4.*e-2.*k+k*(2.*k2-e*k+2.*e-4.)/(e-k-1.)

b17=k*(2.+k/(e-k-1.))

b18=4.*e3-k*(8.*e2-7.*e*k+2.*k2+4.*e-5.*k+2.*e/(e-k-1.))

b19=k*(2.*e-3.*e*k-k+2.*(e2-e*k2+k)/(e-k-1.))-4.*e3

b20=4.*e*k-8.*e2-k2-4.*e-k

b21=7.*e-2.*k+5

b22=e*(4.*e2+2.*e*k+k2+2.*k)

b23=e*(e*k-k2+k-2)

b24=k*(4.*e2-2.*e3+k2-e*k-3.*k)

b25=4.*e2-6.*e*k+3.*k2-9.*e+4.*k-1.+(k2+4.*k-2.)/(e-1.)+2.*k/(e-1.)^2

b26=4.-3.*(e-k)+(k+2.)/(e-1.)

b27=3.*(e-k)^(2)-(e-k)^(3)-(e-k)*(e2-3.*e+2.*k/(e-1.))+k2+k-2.

;***************************************************************************


k0=(e-1.)/(e+p+1.)
x01=k*(e-p)
x02=k*(e+p)*(k LE k0)+(e-k-1.)*(k GT k0)

cross= (k LT (e-1.)/(e+1.-p))*(part1(x02)-part1(x01)+partH(x02)-partH(x01)$
+QROMO('part2',x01,x02,EPS=1e-5))

IF FINITE(cross) EQ 0 THEN stop
;check the value before return

IF cross LT 0. THEN cross=0.
;due to math error in integral

cross=alpha*ro2*cross/(p2*k*mec2)
; differential cross section in cm^2/keV

ENDIF
;*****************************************************************************

return, cross

END

;*****************************************************************************
