;+
; :Description: u_grid
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function u_grid,r,ll,nn,half=halfi,pos=pos,n=nu
if n_elements(pos) gt 0 then begin
np=n_elements(pos)
r=pos(0:np/2-1)
ll=pos(np/2:*)-r
endif
if n_elements(nu) gt 0 then nn=nu
if n_elements(halfi) gt 0 then if (size(halfi))(0) eq 0 then half=halfi(0) else half=halfi
d=n_elements(r)
ni=intarr(d)+1

if n_elements(halfi) le 0 then half=ni*0 else half=half*ni
n=lonarr(d)+nn
e=r(0)*0+1.
l=r*0+e*ll
case d of
1: if half(0) eq 0 then $
	return,r(0)+lindgen(n(0))*e/(n(0)-1)*l(0) else begin
	rr=r(0)-l(0)/(n(0)-2)/2+lindgen(n(0))*e/(n(0)-1)*(l(0)+l(0)/(n(0)-2))
	rr(0)=r(0)&rr(n_elements(rr)-1)=r(0)+l
	return,rr
	endelse
2: begin
if half(0) eq 0 then x=r(0)+lindgen(n(0))*e/(n(0)-1)*l(0) else begin
x=r(0)-l(0)/(n(0)-2)/2+lindgen(n(0))*e/(n(0)-1)*(l(0)+l(0)/(n(0)-2))
x(0)=r(0)&x(n(0)-1)=r(0)+l(0)
endelse
if half(1) eq 0 then y=r(1)+lindgen(n(1))*e/(n(1)-1)*l(1) else begin
y=r(1)-l(1)/(n(1)-2)/2+lindgen(n(1))*e/(n(1)-1)*(l(1)+l(1)/(n(1)-2))
y(0)=r(1)&y(n(1)-1)=r(1)+l(1)
endelse
return,{x:x#replicate(1,n(1)),y:replicate(1,n(0))#y}
end
3: begin
if half(0) eq 0 then x=r(0)+lindgen(n(0))*e/(n(0)-1)*l(0) else begin
x=r(0)-l(0)/(n(0)-2)/2+lindgen(n(0))*e/(n(0)-1)*(l(0)+l(0)/(n(0)-2))
x(0)=r(0)&x(n(0)-1)=r(0)+l(0)
endelse
if half(1) eq 0 then y=r(1)+lindgen(n(1))*e/(n(1)-1)*l(1) else begin
y=r(1)-l(1)/(n(1)-2)/2+lindgen(n(1))*e/(n(1)-1)*(l(1)+l(1)/(n(1)-2))
y(0)=r(1)&y(n(1)-1)=r(1)+l(1)
endelse
if half(2) eq 0 then z=r(2)+lindgen(n(2))*e/(n(2)-1)*l(2) else begin
z=r(2)-l(2)/(n(2)-2)/2+lindgen(n(2))*e/(n(2)-1)*(l(2)+l(2)/(n(2)-2))
z(0)=r(2)&z(n(2)-1)=r(2)+l(2)
endelse
x=x#replicate(1,n(1))
y=replicate(1,n(0))#y
zz=(xx=(yy=lonarr(n(0),n(1),n(2))*e))
for i=0l,n(2)-1 do begin
xx(*,*,i)=x
yy(*,*,i)=y
zz(*,*,i)=z(i)
endfor
return,{x:xx,y:yy,z:zz}
end
else: return,-1
endcase
end
