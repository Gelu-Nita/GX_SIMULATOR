;+
; :Description: m0_to_arc
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function m0_to_arc,yi,zi,xi,radius=radius,inv=inv
if n_elements(radius) le 0 then radius=959.63
alf=radius*!pi/180/3600
dsun = 1./sin(alf)
if n_elements(inv) le 0 then inv=0
if inv then begin
xarcs=yi+zi*0
yarcs=zi+yi*0
if n_elements(xi) gt 0 then x=xi+yi*0+zi*0 ;!!!
tx=tan(xarcs*!pi/180/3600)
ty=tan(yarcs*!pi/180/3600)
if n_elements(x) le 0 then begin
ro0=cos(alf)
a=dsun*(tx^2+ty^2)/(tx^2+ty^2+1)
b=(dsun^2*(tx^2+ty^2)-1)/(tx^2+ty^2+1)
;b=(tx^2+ty^2-1)/(tx^2+ty^2+1)
det=a^2-b
indin=where(det ge 0)
indout=where(det lt 0)
x=xarcs*0
if indin(0) ne -1 then begin
x(indin)=a(indin)+sqrt(det(indin))
endif
if indout(0) ne -1 then begin
t=sqrt(tx(indout)^2+ty(indout)^2)
x(indout)=tan(alf)*dsun*t/(tan(alf)*t+1)
endif
y=tx*(dsun-x)
z=ty*(dsun-x)
endif else begin
y=tx*(dsun-x)
z=ty*(dsun-x)
endelse
t=(tan(xarcs*!pi/180/3600))^2+(tan(yarcs*!pi/180/3600))^2
betta=acos(x/sqrt(x^2+y^2+z^2))
indin=where((t le (1/(dsun^2-1)))  and (betta le (!dpi/2-alf) ) )
indout=where((t gt (1/(dsun^2-1))) or (betta gt (!dpi/2-alf) ) )
visible=where((t gt (1/(dsun^2-1))) or (betta le (!dpi/2-alf) ))
unvisible=where((t le (1/(dsun^2-1))) and (betta gt (!dpi/2-alf)) )
return,{x0:x,x1:y,x2:z,indin:indin,indout:indout,visible:visible,unvisible:unvisible}
endif
if n_elements(xi) le 0 then begin
y=yi+zi*0
z=zi+yi*0
ro0=cos(radius*!pi/180/3600)
ro1=sqrt(y^2+z^2)
indin=where((y^2+z^2) le ro0^2)
indout=where((y^2+z^2) gt ro0^2)
x=y*0
if indin(0) ne -1l then x(indin)=sqrt(1-ro1(indin)^2)
if indout(0) ne -1l then x(indout)=ro1(indout)*tan(radius*!pi/180/3600)
endif else begin
x=xi+yi*0+zi*0
y=yi+xi*0+zi*0
z=zi+yi*0+xi*0
endelse
xarcs=atan(y,dsun-x)*180/!pi*3600
yarcs=atan(z,dsun-x)*180/!pi*3600
t=(tan(xarcs*!pi/180/3600))^2+(tan(yarcs*!pi/180/3600))^2
betta=acos(x/sqrt(x^2+y^2+z^2))
indin=where((t le (1/(dsun^2-1)))  and (betta le (!pi/2-alf) ) )
indout=where((t gt (1/(dsun^2-1))) or (betta gt (!pi/2-alf) ) )
visible=where((t gt (1/(dsun^2-1))) or (betta le (!pi/2-alf) ))
unvisible=where((t le (1/(dsun^2-1))) and (betta gt (!pi/2-alf)) )
return,{x0:x,x1:xarcs,x2:yarcs,indin:indin,indout:indout,visible:visible,unvisible:unvisible}
end
