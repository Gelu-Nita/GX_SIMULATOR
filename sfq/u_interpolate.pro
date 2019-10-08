;+
; :Description: U_interpolate
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function U_interpolate,Xx,Yy,Zz,POS=posi,arr=ffi,half=halfi,inside=inside
common com_U_interpolate,pos,ff,half
if n_elements(posi) gt 0 then begin
pos=posi
ff=ffi
if n_elements(halfi) gt 0 then half=halfi
if n_elements(xx) le 0 then return,1
endif
szf=size(ff)
d=1
if n_elements(yy) gt 0 then d=2
if n_elements(zz) gt 0 then d=3
ni=intarr(d)+1
if n_elements(half) le 0 then half=ni*0 else half=half*ni
if d eq 3 then begin
if n_elements(pos) le 0 then pos=[0,0,0,szf(1)-1,szf(2)-1,szf(3)-1]*1d
x=(xx+yy+zz)*0+xx
y=x*0+yy
z=x*0+zz
;f=ff
if total(half) eq 0 then begin
sz=size(ff)
xmin=pos(0)&xmax=pos(3)&ymin=pos(1)&ymax=pos(4)&zmin=pos(2)&zmax=pos(5)
ind =where( (x-xmin)*(x-xmax) ge 0. or  $
         (y-ymin)*(y-ymax) ge 0. or  $
         (z-zmin)*(z-zmax) ge 0.)
inside=fix(x*0)+1
if ind(0) ne -1 then inside(ind)=0
if szf(0) eq 3 then begin
u=interpolate(ff,(x-xmin)/(xmax-xmin)*(sz(1)-1),(y-ymin)/(ymax-ymin)*(sz(2)-1),(z-zmin)/(zmax-zmin)*(sz(3)-1))
return,u
endif

ux=interpolate(ff(*,*,*,0),(x-xmin)/(xmax-xmin)*(sz(1)-1),(y-ymin)/(ymax-ymin)*(sz(2)-1),(z-zmin)/(zmax-zmin)*(sz(3)-1))
uy=interpolate(ff(*,*,*,1),(x-xmin)/(xmax-xmin)*(sz(1)-1),(y-ymin)/(ymax-ymin)*(sz(2)-1),(z-zmin)/(zmax-zmin)*(sz(3)-1))
uz=interpolate(ff(*,*,*,2),(x-xmin)/(xmax-xmin)*(sz(1)-1),(y-ymin)/(ymax-ymin)*(sz(2)-1),(z-zmin)/(zmax-zmin)*(sz(3)-1))
return,{t0:ux,t1:uy,t2:uz}
endif
endif
if d eq 2 then begin
x=(xx+yy)*0+xx
y=x*0+yy
f=ff
if szf(0) lt 3 then f=ff
 if szf(0) eq 3 then begin
if szf(1) eq 1 then f=transpose(ff,[1,2,0])
if szf(2) eq 1 then f=transpose(ff,[0,2,1])
endif
sz=size(f)
if n_elements(pos) le 0 then pos=[0,0,sz(1)-1,sz(2)-1]*1d

if total(half) eq 0 then begin
sz=size(f)
xmin=pos(0)&xmax=pos(2)&ymin=pos(1)&ymax=pos(3)
u=interpolate(f,(x-xmin)/(xmax-xmin)*(sz(1)-1),(y-ymin)/(ymax-ymin)*(sz(2)-1))
ind =where( (x-xmin)*(x-xmax) ge 0. or  $
         (y-ymin)*(y-ymax) ge 0.)
inside=fix(x*0)+1
if ind(0) ne -1 then inside(ind)=0
return,u
endif

endif
if d eq 1 then begin
x=xx
f=ff
if szf(0) lt 2 then f=ff
 if szf(0) eq 3 then f=transpose(ff,[2,0,1])
 if szf(0) eq 2 then  f=transpose(ff,[1,0])

sz=size(f)
if n_elements(pos) le 0 then pos=[0,sz(1)-1]*1d
if total(half) eq 0 then begin
xmin=pos(0)&xmax=pos(1)
u=interpolate(f,(x-xmin)/(xmax-xmin)*(sz(1)-1))
ind =where( (x-xmin)*(x-xmax) ge 0.)
inside=fix(x*0)+1
if ind(0) ne -1 then inside(ind)=0
return,u
endif





endif
f=ffi
sz=size(f)
u=x*0
if d eq 3 then begin
h=(pos(3:5)-pos(0:2))/(sz(1:3)-1-half)
post=pos
post(0:2)=post(0:2)+h*(1-half/2d)
post(3:5)=post(3:5)-h*(1-half/2d)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
fu=f(1:sz(1)-2,1:sz(2)-2,1:sz(3)-2)
ind=where( ((x-xmin)*(x-xmax) le 0.) and  $
         ((y-ymin)*(y-ymax) le 0.) and  $
         ((z-zmin)*(z-zmax))le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3),(z(ind)-zmin)/(zmax-zmin)*(sz(3)-3))

post=[pos(0),pos(1)+h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d),pos(0)+h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d)]
fu=f(0:1,1:sz(2)-2,1:sz(3)-2)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmax) lt 0. and  $
         (y-ymin)*(y-ymax) lt 0. and  $
         (z-zmin)*(z-zmax) lt 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3),(z(ind)-zmin)/(zmax-zmin)*(sz(3)-3))

post=[pos(3)-h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d),pos(3),pos(4)-h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d)]
fu=f(sz(1)-2:sz(1)-1,1:sz(2)-2,1:sz(3)-2)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin) gt 0. and  $
         (y-ymin)*(y-ymax) lt 0. and  $
         (z-zmin)*(z-zmax) lt 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3),(z(ind)-zmin)/(zmax-zmin)*(sz(3)-3))

post=[pos(0)+h(0)*(1-half(0)/2d),pos(1),pos(2)+h(2)*(1-half(2)/2d),pos(3)-h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d)]
fu=f(1:sz(1)-2,0:1,1:sz(3)-2)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin)*(x-xmax) lt 0. and  $
         (y-ymax) lt 0. and  $
         (z-zmin)*(z-zmax) lt 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin)*(sz(3)-3))

post=[pos(0)+h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d),pos(3)-h(0)*(1-half(0)/2d),pos(4),pos(5)-h(2)*(1-half(2)/2d)]
fu=f(1:sz(1)-2,sz(2)-2:sz(2)-1,1:sz(3)-2)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin)*(x-xmax) lt 0. and  $
         (y-ymin) gt 0. and  $
         (z-zmin)*(z-zmax) lt 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin)*(sz(3)-3))


post=[pos(0)+h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(2),pos(3)-h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d)]
fu=f(1:sz(1)-2,1:sz(2)-2,0:1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin)*(x-xmax) lt 0. and  $
         (y-ymin)*(y-ymax) lt 0. and  $
         (z-zmax) lt 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3),(z(ind)-zmin)/(zmax-zmin))

post=[pos(0)+h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d),pos(3)-h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(5)]
fu=f(1:sz(1)-2,1:sz(2)-2,sz(3)-2:sz(3)-1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin)*(x-xmax) lt 0. and  $
         (y-ymin)*(y-ymax) lt 0. and  $
         (z-zmin) gt 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3),(z(ind)-zmin)/(zmax-zmin))

;1x
post=[pos(0)+h(0)*(1-half(0)/2d),pos(1),pos(2),pos(3)-h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d)]
fu=f(1:sz(1)-2,0:1,0:1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin)*(x-xmax) le 0. and  $
         (y-ymax) le 0. and  $
         (z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))
;2x
post=[pos(0)+h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(2),pos(3)-h(0)*(1-half(0)/2d),pos(4),pos(2)+h(2)*(1-half(2)/2d)]
fu=f(1:sz(1)-2,sz(2)-2:sz(2)-1,0:1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin)*(x-xmax) le 0. and  $
         (y-ymin) ge 0. and  $
         (z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))

;3x
post=[pos(0)+h(0)*(1-half(0)/2d),pos(1),pos(5)-h(2)*(1-half(2)/2d),pos(3)-h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(5)]
fu=f(1:sz(1)-2,0:1,sz(3)-2:sz(3)-1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin)*(x-xmax) le 0. and  $
         (y-ymax) le 0. and  $
         (z-zmin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))
;4x
post=[pos(0)+h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d),pos(3)-h(0)*(1-half(0)/2d),pos(4),pos(5)]
fu=f(1:sz(1)-2,sz(2)-2:sz(2)-1,sz(3)-2:sz(3)-1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin)*(x-xmax) le 0. and  $
         (y-ymin) ge 0. and  $
         (z-zmin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))

;1y
post=[pos(0),pos(1)+h(1)*(1-half(1)/2d),pos(2),pos(0)+h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d)]
fu=f(0:1,1:sz(2)-2,0:1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmax) le 0. and  $
         (y-ymin)*(y-ymax) le 0. and  $
         (z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3),(z(ind)-zmin)/(zmax-zmin))

;2y
post=[pos(3)-h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(2),pos(3),pos(4)-h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d)]
fu=f(sz(1)-2:sz(1)-1,1:sz(2)-2,0:1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin) ge 0. and  $
         (y-ymin)*(y-ymax) le 0. and  $
         (z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3),(z(ind)-zmin)/(zmax-zmin))

;3y
post=[pos(0),pos(1)+h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d),pos(0)+h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(5)]
fu=f(0:1,1:sz(2)-2,sz(3)-2:sz(3)-1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmax) le 0. and  $
         (y-ymin)*(y-ymax) le 0. and  $
         (z-zmin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3),(z(ind)-zmin)/(zmax-zmin))


;4y
post=[pos(3)-h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d),pos(3),pos(4)-h(1)*(1-half(1)/2d),pos(5)]
fu=f(sz(1)-2:sz(1)-1,1:sz(2)-2,sz(3)-2:sz(3)-1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin) ge 0. and  $
         (y-ymin)*(y-ymax) le 0. and  $
         (z-zmin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3),(z(ind)-zmin)/(zmax-zmin))

;1z
post=[pos(0),pos(1),pos(2)+h(2)*(1-half(2)/2d),pos(0)+h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d)]
fu=f(0:1,0:1,1:sz(3)-2)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmax) le 0. and  $
         (y-ymax) le 0. and  $
         (z-zmin)*(z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin)*(sz(3)-3))

;2z
post=[pos(3)-h(0)*(1-half(0)/2d),pos(1),pos(2)+h(2)*(1-half(2)/2d),pos(3),pos(1)+h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d)]
fu=f(sz(1)-2:sz(1)-1,0:1,1:sz(3)-2)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin) ge 0. and  $
         (y-ymax) le 0. and  $
         (z-zmin)*(z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin)*(sz(3)-3))

;3z
post=[pos(0),pos(4)-h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d),pos(0)+h(0)*(1-half(0)/2d),pos(4),pos(5)-h(2)*(1-half(2)/2d)]
fu=f(0:1,sz(2)-2:sz(2)-1,1:sz(3)-2)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmax) le 0. and  $
         (y-ymin) ge 0. and  $
         (z-zmin)*(z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin)*(sz(3)-3))

;4z
post=[pos(3)-h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d),pos(3),pos(4),pos(5)-h(2)*(1-half(2)/2d)]
fu=f(sz(1)-2:sz(1)-1,sz(2)-2:sz(2)-1,1:sz(3)-2)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind=where( (x-xmin) ge 0. and  $
         (y-ymin) ge 0. and  $
         (z-zmin)*(z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin)*(sz(3)-3))


;1
post=[pos(0),pos(1),pos(2),pos(0)+h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d)]
fu=f(0:1,0:1,0:1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind =where( (x-xmax) le 0. and  $
         (y-ymax) le 0. and  $
         (z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))

;2
post=[pos(0),pos(4)-h(1)*(1-half(1)/2d),pos(2),pos(0)+h(0)*(1-half(0)/2d),pos(4),pos(2)+h(2)*(1-half(2)/2d)]
fu=f(0:1,sz(2)-2:sz(2)-1,0:1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind =where( (x-xmax) le 0. and  $
         (y-ymin) ge 0. and  $
         (z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))

;3
post=[pos(0),pos(1),pos(5)-h(2)*(1-half(2)/2d),pos(0)+h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(5)]
fu=f(0:1,0:1,sz(3)-2:sz(3)-1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind =where( (x-xmax) le 0. and  $
         (y-ymax) le 0. and  $
         (z-zmin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))

;4
post=[pos(0),pos(4)-h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d),pos(0)+h(0)*(1-half(0)/2d),pos(4),pos(5)]
fu=f(0:1,sz(2)-2:sz(2)-1,sz(3)-2:sz(3)-1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind =where( (x-xmax) le 0. and  $
         (y-ymin) ge 0. and  $
         (z-zmin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))


;1a
post=[pos(3)-h(0)*(1-half(0)/2d),pos(1),pos(2),pos(3),pos(1)+h(1)*(1-half(1)/2d),pos(2)+h(2)*(1-half(2)/2d)]
fu=f(sz(1)-2:sz(1)-1,0:1,0:1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind =where( (x-xmin) ge 0. and  $
         (y-ymax) le 0. and  $
         (z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))

;2a
post=[pos(3)-h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(2),pos(3),pos(4),pos(2)+h(2)*(1-half(2)/2d)]
fu=f(sz(1)-2:sz(1)-1,sz(2)-2:sz(2)-1,0:1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind =where( (x-xmin) ge 0. and  $
         (y-ymin) ge 0. and  $
         (z-zmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))

;3a
post=[pos(3)-h(0)*(1-half(0)/2d),pos(1),pos(5)-h(2)*(1-half(2)/2d),pos(3),pos(1)+h(1)*(1-half(1)/2d),pos(5)]
fu=f(sz(1)-2:sz(1)-1,0:1,sz(3)-2:sz(3)-1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind =where( (x-xmin) ge 0. and  $
         (y-ymax) le 0. and  $
         (z-zmin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))

;4a
post=[pos(3)-h(0)*(1-half(0)/2d),pos(4)-h(1)*(1-half(1)/2d),pos(5)-h(2)*(1-half(2)/2d),pos(3),pos(4),pos(5)]
fu=f(sz(1)-2:sz(1)-1,sz(2)-2:sz(2)-1,sz(3)-2:sz(3)-1)
xmin=post(0)&xmax=post(3)&ymin=post(1)&ymax=post(4)&zmin=post(2)&zmax=post(5)
ind =where( (x-xmin) ge 0. and  $
         (y-ymin) ge 0. and  $
         (z-zmin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin),(z(ind)-zmin)/(zmax-zmin))






xmin=pos(0)&xmax=pos(3)&ymin=pos(1)&ymax=pos(4)&zmin=pos(2)&zmax=pos(5)
ind =where( (x-xmin)*(x-xmax) gt 0. or  $
         (y-ymin)*(y-ymax) gt 0. or  $
         (z-zmin)*(z-zmax) gt 0.)
inside=fix(x*0)+1
if ind(0) ne -1 then inside(ind)=0
return,u
endif

if d eq 2 then begin
h=(pos(2:3)-pos(0:1))/(sz(1:2)-1-half)
post=pos
post(0:1)=post(0:1)+h*(1-half/2d)
post(2:3)=post(2:3)-h*(1-half/2d)
xmin=post(0)&xmax=post(2)&ymin=post(1)&ymax=post(3)
fu=f(1:sz(1)-2,1:sz(2)-2)
ind=where( (x-xmin)*(x-xmax) le 0. and  $
         (y-ymin)*(y-ymax) le 0. )
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3))

post=[pos(0),pos(1)+h(1)*(1-half(1)/2d),pos(0)+h(0)*(1-half(0)/2d),pos(3)-h(1)*(1-half(1)/2d)]
fu=f(0:1,1:sz(2)-2)
xmin=post(0)&xmax=post(2)&ymin=post(1)&ymax=post(3)
ind=where( (x-xmax) lt 0. and  $
         (y-ymin)*(y-ymax) lt 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3))

post=[pos(2)-h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d),pos(2),pos(3)-h(1)*(1-half(1)/2d)]
fu=f(sz(1)-2:sz(1)-1,1:sz(2)-2)
xmin=post(0)&xmax=post(2)&ymin=post(1)&ymax=post(3)
ind=where( (x-xmin) gt 0. and  $
         (y-ymin)*(y-ymax) lt 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin)*(sz(2)-3))

post=[pos(0)+h(0)*(1-half(0)/2d),pos(1),pos(2)-h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d)]
fu=f(1:sz(1)-2,0:1)
xmin=post(0)&xmax=post(2)&ymin=post(1)&ymax=post(3)
ind=where( (x-xmin)*(x-xmax) lt 0. and  $
         (y-ymax) lt 0. )
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin))

post=[pos(0)+h(0)*(1-half(0)/2d),pos(3)-h(1)*(1-half(1)/2d),pos(2)-h(0)*(1-half(0)/2d),pos(3)]
fu=f(1:sz(1)-2,sz(2)-2:sz(2)-1)
xmin=post(0)&xmax=post(2)&ymin=post(1)&ymax=post(3)
ind=where( (x-xmin)*(x-xmax) lt 0. and  $
         (y-ymin) gt 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3),(y(ind)-ymin)/(ymax-ymin))
;1
post=[pos(0),pos(1),pos(0)+h(0)*(1-half(0)/2d),pos(1)+h(1)*(1-half(1)/2d)]
fu=f(0:1,0:1)
xmin=post(0)&xmax=post(2)&ymin=post(1)&ymax=post(3)
ind =where( (x-xmax) le 0. and  $
         (y-ymax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin))
;2
post=[pos(0),pos(3)-h(1)*(1-half(1)/2d),pos(0)+h(0)*(1-half(0)/2d),pos(3)]
fu=f(0:1,sz(2)-2:sz(2)-1)
xmin=post(0)&xmax=post(2)&ymin=post(1)&ymax=post(3)
ind =where( (x-xmax) le 0. and  $
         (y-ymin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin))

;3
post=[pos(2)-h(0)*(1-half(0)/2d),pos(1),pos(2),pos(1)+h(1)*(1-half(1)/2d)]
fu=f(sz(1)-2:sz(1)-1,0:1)
xmin=post(0)&xmax=post(2)&ymin=post(1)&ymax=post(3)
ind =where( (x-xmin) ge 0. and  $
         (y-ymax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin))

;4
post=[pos(2)-h(0)*(1-half(0)/2d),pos(3)-h(1)*(1-half(1)/2d),pos(2),pos(3)]
fu=f(sz(1)-2:sz(1)-1,sz(2)-2:sz(2)-1)
xmin=post(0)&xmax=post(2)&ymin=post(1)&ymax=post(3)
ind =where( (x-xmin) ge 0. and  $
         (y-ymin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin),(y(ind)-ymin)/(ymax-ymin))

xmin=pos(0)&xmax=pos(2)&ymin=pos(1)&ymax=pos(3)
ind =where( (x-xmin)*(x-xmax) gt 0. or  $
         (y-ymin)*(y-ymax) gt 0. )
inside=fix(x*0)+1
if ind(0) ne -1 then inside(ind)=0
return,u

endif
if d eq 1 then begin
h=(pos(1)-pos(0))/(sz(1)-1-half)
post=pos
post(0)=post(0)+h*(1-half/2d)
post(1)=post(1)-h*(1-half/2d)
xmin=post(0)&xmax=post(1)
fu=f(1:sz(1)-2)
ind=where( (x-xmin)*(x-xmax) le 0. )
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin)*(sz(1)-3))
;1
post=[pos(0),pos(0)+h*(1-half/2d)]
fu=f(0:1)
xmin=post(0)&xmax=post(1)
ind =where( (x-xmax) le 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin))
;2
post=[pos(1)-h*(1-half/2d),pos(1)]
fu=f(sz(1)-2:sz(1)-1)
xmin=post(0)&xmax=post(1)
ind =where( (x-xmin) ge 0.)
if ind(0) ne -1 then u(ind)=interpolate(fu,(x(ind)-xmin)/(xmax-xmin))

xmin=pos(0)&xmax=pos(1)
ind =where( (x-xmin)*(x-xmax) gt 0.)
inside=fix(x*0)+1
if ind(0) ne -1 then inside(ind)=0
return,u
endif
end
;________
;goto,lb
n=[100,90,70]
half=[1,1,1]
pos=[0,0,0d,!dpi/2,!dpi/2*1.2,!dpi/2*0.9]
g=u_grid(pos(0:2)-.2,pos(3:5)-pos(0:2)+.5,n-1)
gh=u_grid(pos(0:2),pos(3:5)-pos(0:2),n,half=half)
fh=sin(gh.x)*cos(gh.y)*sin(2*gh.z)+2
f=sin(g.x)*cos(g.y)*sin(2*g.z)+2
uf=U_interpolate(g.x,g.y,g.z,POS=pos,arr=fh,half=half)
print,max(abs(uf-f)/abs(f))
goto,lb
n=[100,90]
half=[1,1]
pos=[0,0d,!dpi/2,!dpi/2*1.2]
g=u_grid(pos(0:1),pos(2:3)-pos(0:1),n-1)
gh=u_grid(pos(0:1),pos(2:3)-pos(0:1),n,half=half)
fh=sin(gh.x)*cos(gh.y)+2
f=sin(g.x)*cos(g.y)+2
uf=U_interpolate(g.x,g.y,POS=pos,arr=fh,half=half)
print,max(abs(uf-f)/abs(f))
n=100
half=1
pos=[0.01,!dpi/2]
g=u_grid(pos(0),pos(1)-pos(0),n-1)
gh=u_grid(pos(0),pos(1)-pos(0),n,half=half)
fh=sin(gh)+2
f=sin(g)+2
uf=U_interpolate(g,POS=pos,arr=fh,half=half)
print,max(abs(uf-f)/abs(f))
lb:

end