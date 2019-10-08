;+
; :Description: _Lfff_fft_n
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
;-
function _Lfff_fft_n,z,set=set,derivs=derivs
common com__Lfff_fft,pq,hx,hy,hz,alfa,pos
if n_elements(derivs) le 0 then derivs=0
if n_elements(set) gt 0 then begin
pq=set.data
hx=set.vx
hy=set.vy
hz=set.vz
alfa=set.alfa
pos=set.pos
return,1
endif

if n_elements(pq) le 0 then return,0
if size(pq,/type) eq 5 then begin
pi_=!dpi
e_=1d
ci=dcomplex(0,1)
ce=dcomplex(1,0)
db_=1
endif else begin
pi_=!pi
e_=1.
ci=complex(0,1)
ce=complex(1,0)
db_=0
endelse
if n_elements(z) le 0 then z=0*e_
hs=(pos(2:3)-pos(0:1))/2
e0=pq(0)*0
e=pq(0)*0+1
sz=size(pq)
if sz(1)/2*2 eq sz(1) then begin
km=e+lindgen((sz(1)-2)/2)
km=[0,km,sz(1)/2.,-reverse(km)]
endif else begin
km=e+lindgen((sz(1)-1)/2)
km=[0,km,-reverse(km)]
endelse
if sz(2)/2*2 eq sz(2) then begin
kn=e+lindgen((sz(2)-2)/2)
kn=[0,kn,sz(2)/2.,-reverse(kn)]
endif else begin
kn=e+lindgen((sz(2)-1)/2)
kn=[0,kn,-reverse(kn)]
endelse
km=km#replicate(1,sz(2))
kn=replicate(1,sz(1))#kn
;t=hs*2
;kx=2*!dpi*km/sz(1)/t(0)
;ky=2*!dpi*kn/sz(2)/t(1)
kx=pi_*km/hs(0)
ky=pi_*kn/hs(1)
if alfa eq 0 then begin
q=sqrt(kx^2+ky^2)
bmnx=-ci*kx
bmny=-ci*ky
endif else begin
ind1=where((kx^2+ky^2-alfa^2) le 0)
q=sqrt((kx^2+ky^2-alfa^2)*ce)
bmnx=-ci*kx+ci*alfa*(alfa*q+kx*ky)/(q*kx+alfa*ky)
bmny=-ci*ky+ci*alfa*(alfa*q-kx*ky)/(q*ky-alfa*kx)
if ind1(0) ne -1 then begin
bmnx(ind1)=0
bmny(ind1)=0
endif
endelse
ind=where((kx^2+ky^2-alfa^2) gt 0)
bmnh=bmnx*hx+bmny*hy+q*hz
fq=fft(pq)
if db_ then cmn=dcomplexarr(sz(1),sz(2)) else cmn=complexarr(sz(1),sz(2))
cmn(ind)=fq(ind)/bmnh(ind)
bmnh=0
nz=n_elements(z)
if db_ then ez=dblarr(sz(1),sz(2)) else ez=fltarr(sz(1),sz(2))
if nz eq 1 then begin
ez(ind)=exp(-q(ind)*z)
tmp=cmn*bmnx*ez
if derivs ne 1 then bmnx=0
if db_ then bx=double((fft(tmp,/inv))) else bx=float((fft(tmp,/inv)))
tmp=0
tmp=cmn*bmny*ez
if derivs ne 1 then bmny=0
if db_ then by=double((fft(tmp,/inv))) else by=float((fft(tmp,/inv)))
tmp=0
tmp=cmn*q*ez
if derivs ne 1 then q=0
if db_ then bz=double((fft(tmp,/inv))) else bz=float((fft(tmp,/inv)))
bl=bx*hx+by*hy+bz*hz
if derivs then begin
tmp=cmn*bmnx*ez*ci*kx
if db_ then bx_x=double((fft(tmp,/inv))) else bx_x=float((fft(tmp,/inv)))
tmp=cmn*bmny*ez*ci*kx
if db_ then by_x=double((fft(tmp,/inv))) else by_x=float((fft(tmp,/inv)))
tmp=cmn*q*ez*ci*kx
if db_ then bz_x=double((fft(tmp,/inv))) else bz_x=float((fft(tmp,/inv)))

tmp=cmn*bmnx*ez*ci*ky
if db_ then bx_y=double((fft(tmp,/inv))) else bx_y=float((fft(tmp,/inv)))
tmp=cmn*bmny*ez*ci*ky
if db_ then by_y=double((fft(tmp,/inv))) else by_y=float((fft(tmp,/inv)))
tmp=cmn*q*ez*ci*ky
if db_ then bz_y=double((fft(tmp,/inv))) else bz_y=float((fft(tmp,/inv)))

tmp=-cmn*bmnx*ez*q
if db_ then bx_z=double((fft(tmp,/inv))) else bx_z=float((fft(tmp,/inv)))
tmp=-cmn*bmny*ez*q
if db_ then by_z=double((fft(tmp,/inv))) else by_z=float((fft(tmp,/inv)))
tmp=-cmn*q*ez*q
if db_ then bz_z=double((fft(tmp,/inv))) else bz_z=float((fft(tmp,/inv)))
endif
endif else begin
if db_ then  bx=(by=(bz=(bl=dblarr(sz(1),sz(2),nz)))) else bx=(by=(bz=(bl=fltarr(sz(1),sz(2),nz))))
if derivs then bx_x=(by_x=(bz_x=(bx_y=(by_y=(bz_y=(bx_z=(by_z=(bz_z=bx))))))))
tmpx=cmn*bmnx
tmpy=cmn*bmny
tmpz=cmn*q
for i=0l,nz-1 do begin
ez(ind)=exp(-q(ind)*z(i))
tmp=tmpx*ez
if db_ then  bx(*,*,i)=double((fft(tmp,/inv))) else bx(*,*,i)=double((fft(tmp,/inv)))
tmp=tmpy*ez
if db_ then  by(*,*,i)=double((fft(tmp,/inv))) else by(*,*,i)=double((fft(tmp,/inv)))
tmp=tmpz*ez
if db_ then  bz(*,*,i)=double((fft(tmp,/inv))) else bz(*,*,i)=double((fft(tmp,/inv)))
bl(*,*,i)=bx(*,*,i)*hx+by(*,*,i)*hy+bz(*,*,i)*hz
if derivs then begin
tmp=tmpx*ez*ci*kx
if db_ then  bx_x(*,*,i)=double((fft(tmp,/inv))) else bx_x(*,*,i)=double((fft(tmp,/inv)))
tmp=tmpy*ez*ci*kx
if db_ then  by_x(*,*,i)=double((fft(tmp,/inv))) else by_x(*,*,i)=double((fft(tmp,/inv)))
tmp=tmpz*ez*ci*kx
if db_ then  bz_x(*,*,i)=double((fft(tmp,/inv))) else bz_x(*,*,i)=double((fft(tmp,/inv)))
tmp=tmpx*ez*ci*ky
if db_ then  bx_y(*,*,i)=double((fft(tmp,/inv))) else bx_y(*,*,i)=double((fft(tmp,/inv)))
tmp=tmpy*ez*ci*ky
if db_ then  by_y(*,*,i)=double((fft(tmp,/inv))) else by_y(*,*,i)=double((fft(tmp,/inv)))
tmp=tmpz*ez*ci*ky
if db_ then  bz_y(*,*,i)=double((fft(tmp,/inv))) else bz_y(*,*,i)=double((fft(tmp,/inv)))
tmp=-tmpx*ez*q
if db_ then  bx_z(*,*,i)=double((fft(tmp,/inv))) else bx_z(*,*,i)=double((fft(tmp,/inv)))
tmp=-tmpy*ez*q
if db_ then  by_z(*,*,i)=double((fft(tmp,/inv))) else by_z(*,*,i)=double((fft(tmp,/inv)))
tmp=-tmpz*ez*q
if db_ then  bz_z(*,*,i)=double((fft(tmp,/inv))) else bz_z(*,*,i)=double((fft(tmp,/inv)))

endif
endfor
endelse
if derivs then $
return,{t0:bx,t1:by,t2:bz,bl:bl,alf:alfa,pos:pos,z:z,data:pq,t00:bx_x,t10:by_x,t20:bz_x,t01:bx_y,t11:by_y,t21:bz_y,t02:bx_z,t12:by_z,t22:bz_z} $
else  return,{t0:bx,t1:by,t2:bz,bl:bl,alf:alfa,pos:pos,z:z,data:pq}
end
