;+
; :Description: get_str_mag
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function get_str_mag,bx,by,bz,apos,Rsun
L_zero=0.
b_zero=0.
p_zero=0.
sz=size(bx)
n=sz(1:2)
gr=u_grid(pos=float(apos),n=n)
alf=Rsun*!pi/180/3600
dsun = 1./sin(alf)
tx=tan(gr.x*!pi/180/3600)
ty=tan(gr.y*!pi/180/3600)
ro0=cos(alf)
a=dsun*(tx^2+ty^2)/(tx^2+ty^2+1)
b=(dsun^2*(tx^2+ty^2)-1)/(tx^2+ty^2+1)
det=a^2-b
indin=where(det ge 0)
indout=where(det lt 0)
x=gr.x*0
if indin(0) ne -1 then begin
x(indin)=a(indin)+sqrt(det(indin))
endif
if indout(0) ne -1 then begin
t=sqrt(tx(indout)^2+ty(indout)^2)
x(indout)=tan(alf)*dsun*t/(tan(alf)*t+1)
endif
tmp=x*0+1
visible=where(tmp)
qs={l:float(L_zero),b:float(B_zero),p:float(P_zero),r:float(Rsun),type:'arc'}
return,{x0:x,x1:gr.x,x2:gr.y,t0:float(bz),t1:float(bx),t2:float(by),pos:float(apos),qs:qs,ts:qs,indin:indin,indout:indout,visible:visible,unvisible:-1l}
end
