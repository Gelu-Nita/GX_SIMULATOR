;+
; :Description: b_spl
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function b_spl,x,y,zu,pos=pos,hap=h,arr=a,boundary=b
if n_elements(h) le 0 then h=B_spl_k(a,boundary=b)
sz=size(h)&if sz(0) lt 3 then sz(1:2)=sz(1:2)-2 else sz(1:3)=sz(1:3)-2
if sz(0) eq 1 then begin
if n_elements(pos) le 0 then pos=[0.,sz(1)-1]
n=sz(1)-1
xx=x
if b then begin
ind=where((xx-pos(0)) ge 0)
if ind(0) ne -1 then xx(ind)=pos(0)+((xx(ind)-pos(0)) mod (pos(1)-pos(0)))
ind=where((xx-pos(0)) lt 0)
if ind(0) ne -1 then xx(ind)=pos(0)+(pos(1)-pos(0)-( abs(xx(ind)-pos(0)) mod (pos(1)-pos(0)) )  )
endif
xi=(xx-pos(0))/(pos(1)-pos(0))*n
k=long(xi+.5)
if (size(k))(0) eq 0 then k=k*[1]
z=xi-k
f=z^2*(h(k)-2*h(k+1)+h(k+2))+z*(h(k+2)-h(k))+(h(k)+6*h(k+1)+h(k+2))/4
if (size(xi))(0) eq 0 then f=f(0)
return,f
endif
if sz(0) eq 2 then begin
if n_elements(pos) le 0 then pos=[0.,0,sz(1)-1,sz(2)-1]
n=sz(1:2)-1

xx=x
yy=y
if b(0) then begin
ind=where((xx-pos(0)) ge 0)
if ind(0) ne -1 then xx(ind)=pos(0)+((xx(ind)-pos(0)) mod (pos(2)-pos(0)))
ind=where((xx-pos(0)) lt 0)
if ind(0) ne -1 then xx(ind)=pos(0)+(pos(2)-pos(0)-( abs(xx(ind)-pos(0)) mod (pos(2)-pos(0)) )  )
endif
if b(1) then begin
ind=where((yy-pos(1)) ge 0)
if ind(0) ne -1 then yy(ind)=pos(1)+((yy(ind)-pos(1)) mod (pos(3)-pos(1)))
ind=where((yy-pos(1)) lt 0)
if ind(0) ne -1 then yy(ind)=pos(1)+(pos(3)-pos(1)-( abs(yy(ind)-pos(1)) mod (pos(3)-pos(1)) )  )
endif
xi=(xx-pos(0))/(pos(2)-pos(0))*n(0)&yi=(yy-pos(1))/(pos(3)-pos(1))*n(1)
k=fix(xi+.5)&nu=fix(yi+.5)
if (size(k))(0) eq 0 then begin
k=k*[1]
nu=nu*[1]
endif
z=xi-k&v=yi-nu

zsqm=(z-.5)^2&zsqp=(z+.5)^2&vsqm=(v-.5)^2&vsqp=(v+.5)^2&zsq0=-2*z^2+1.5&vsq0=-2*v^2+1.5

f=h(k,nu)*zsqm*vsqm+h(k,nu+1)*zsqm*vsq0+h(k,nu+2)*zsqm*vsqp+$
h(k+1,nu)*zsq0*vsqm+h(k+1,nu+1)*zsq0*vsq0+h(k+1,nu+2)*zsq0*vsqp+$
h(k+2,nu)*zsqp*vsqm+h(k+2,nu+1)*zsqp*vsq0+h(k+2,nu+2)*zsqp*vsqp
if (size(xi))(0) eq 0 then f=f(0)
return,f
endif
;3D
if n_elements(pos) le 0 then pos=[0.,0,0,sz(1)-1,sz(2)-1,sz(3)-1]
n=sz(1:3)-1

xx=x
yy=y
zz=zu
if b(0) then begin
ind=where((xx-pos(0)) ge 0)
if ind(0) ne -1 then xx(ind)=pos(0)+((xx(ind)-pos(0)) mod (pos(3)-pos(0)))
ind=where((xx-pos(0)) lt 0)
if ind(0) ne -1 then xx(ind)=pos(0)+(pos(3)-pos(0)-( abs(xx(ind)-pos(0)) mod (pos(3)-pos(0)) )  )
endif
if b(1) then begin
ind=where((yy-pos(1)) ge 0)
if ind(0) ne -1 then yy(ind)=pos(1)+((yy(ind)-pos(1)) mod (pos(4)-pos(1)))
ind=where((yy-pos(1)) lt 0)
if ind(0) ne -1 then yy(ind)=pos(1)+(pos(4)-pos(1)-( abs(yy(ind)-pos(1)) mod (pos(4)-pos(1)) )  )
endif
if b(2) then begin
ind=where((zz-pos(2)) ge 0)
if ind(0) ne -1 then zz(ind)=pos(2)+((zz(ind)-pos(2)) mod (pos(5)-pos(2)))
ind=where((zz-pos(2)) lt 0)
if ind(0) ne -1 then zz(ind)=pos(2)+(pos(5)-pos(2)-( abs(zz(ind)-pos(2)) mod (pos(5)-pos(2)) )  )
endif
xi=(xx-pos(0))/(pos(3)-pos(0))*n(0)&yi=(yy-pos(1))/(pos(4)-pos(1))*n(1)&zi=(zz-pos(2))/(pos(5)-pos(2))*n(2)
k=long(xi+.5)&nu=long(yi+.5)&mu=long(zi+.5)
if (size(k))(0) eq 0 then begin
k=k*[1]
nu=nu*[1]
mu=mu*[1]
endif
z=xi-k&v=yi-nu&w=zi-mu

zsqm=(z-.5)^2&zsqp=(z+.5)^2&vsqm=(v-.5)^2&vsqp=(v+.5)^2&zsq0=-2*z^2+1.5&vsq0=-2*v^2+1.5
wsqm=(w-.5)^2&wsqp=(w+.5)^2&wsq0=-2*w^2+1.5
f=$
h(k,nu,mu)*zsqm*vsqm*wsqm+h(k,nu,mu+1)*zsqm*vsqm*wsq0+h(k,nu,mu+2)*zsqm*vsqm*wsqp+$
h(k,nu+1,mu)*zsqm*vsq0*wsqm+h(k,nu+1,mu+1)*zsqm*vsq0*wsq0+h(k,nu+1,mu+2)*zsqm*vsq0*wsqp+$
h(k,nu+2,mu)*zsqm*vsqp*wsqm+h(k,nu+2,mu+1)*zsqm*vsqp*wsq0+h(k,nu+2,mu+2)*zsqm*vsqp*wsqp+$

h(k+1,nu,mu)*zsq0*vsqm*wsqm+h(k+1,nu,mu+1)*zsq0*vsqm*wsq0+h(k+1,nu,mu+2)*zsq0*vsqm*wsqp+$
h(k+1,nu+1,mu)*zsq0*vsq0*wsqm+h(k+1,nu+1,mu+1)*zsq0*vsq0*wsq0+h(k+1,nu+1,mu+2)*zsq0*vsq0*wsqp+$
h(k+1,nu+2,mu)*zsq0*vsqp*wsqm+h(k+1,nu+2,mu+1)*zsq0*vsqp*wsq0+h(k+1,nu+2,mu+2)*zsq0*vsqp*wsqp+$

h(k+2,nu,mu)*zsqp*vsqm*wsqm+h(k+2,nu,mu+1)*zsqp*vsqm*wsq0+h(k+2,nu,mu+2)*zsqp*vsqm*wsqp+$
h(k+2,nu+1,mu)*zsqp*vsq0*wsqm+h(k+2,nu+1,mu+1)*zsqp*vsq0*wsq0+h(k+2,nu+1,mu+2)*zsqp*vsq0*wsqp+$
h(k+2,nu+2,mu)*zsqp*vsqp*wsqm+h(k+2,nu+2,mu+1)*zsqp*vsqp*wsq0+h(k+2,nu+2,mu+2)*zsqp*vsqp*wsqp


if (size(xi))(0) eq 0 then f=f(0)
return,f

end
n=[90,70,50]
pos=[.3,-.2,.4]&pos=[pos,pos+2*!dpi*[1.1,0.7,.9]]
test='3D'
case test of
'1D':begin
x=u_grid(pos(0),pos(3)-pos(0),n(0))
f=sin(x)
q=b_spl(x,pos=[pos(0),pos(3)],arr=f,boundary=0)
window,0&plot,x,f
window,1&plot,x,q
print,sqrt(total((f-q)^2)),sqrt(total((f)^2))
end
'2D':begin
gr=u_grid(pos([0,1]),pos([3,4])-pos([0,1]),n(0:1))&x=gr.x&y=gr.y
f=sin(x)*cos(y)
q=b_spl(x,y,pos=pos([0,1,3,4]),arr=f,boundary=[0,0])
print,sqrt(total((f-q)^2)),sqrt(total((f)^2))
end
'3D':begin
gr=u_grid(pos(0:2),pos(3:5)-pos(0:2),n)&x=gr.x&y=gr.y&z=gr.z
f=sin(x)*cos(y)*sin(z)^2
q=b_spl(x,y,z,pos=pos,arr=f,boundary=[0,0,0])
print,sqrt(total((f-q)^2)),sqrt(total((f)^2))
end

else:
endcase
end