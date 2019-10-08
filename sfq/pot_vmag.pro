;+
; :Description: pot_vmag
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function pot_vmag,s,outbpos=outbpos,fcenter=fcenter,simple=simple
normal=1
if n_elements(alpha) le 0 then alpha=0
if n_elements(normal) le 0 then normal=1
fcenter=1;0-GUI,1-square,2-wheigt
if n_elements(fcenter) le 0 then fcenter=1
if (n_elements(bpos) le 0) and fcenter eq 0 then fcenter=1
if normal then begin
  l0=0.
  b0=0.
  p0=0.
endif else begin
  l0=s.qs.l
  b0=s.qs.b
  p0=s.qs.p
endelse
rad=s.qs.r
if fcenter ne 0 then begin
  ua={x0:s.x0(s.indin),x1:s.x1(s.indin),x2:s.x2(s.indin),qs:{l:l0,b:b0,p:p0,r:rad,type:'arc'}}
  ub={qs:ua.qs}
  ub.qs.type='dec'
  u=SOL_crd(ua,ub,/crd)
  if fcenter then begin
    xc=mean(u.x0)
    yc=mean(u.x1)
    zc=mean(u.x2)
    endif
  if fcenter eq 2 then begin
    if (where(tag_names(s) eq 'T2'))(0) ne -1 then b_mod=(sqrt(s.t0(s.indin)^2+s.t1(s.indin)^2+s.t2(s.indin)^2)) else b_mod=abs(s.t0(s.indin))
    yc=total(b_mod*u.x1)/total(b_mod)
    zc=total(b_mod*u.x2)/total(b_mod)
    xc=sqrt(1-yc^2-zc^2)
  endif
  ad=QS_crd({x0:xc,x1:yc,x2:zc},L0,B0,P0,/inv,/tosph)
  lc=ad.x2*180/!pi
  bc=90-ad.x1*180/!pi
  ad=QS_crd({x0:u.x0,x1:u.x1,x2:u.x2},L0,B0,P0,/inv)
  as=QS_crd(ad,lc,bc,0.,/tosph)
  l=as.x2*180/!pi
  b=90-as.x1*180/!pi
  hs=[max(abs(l)),max(abs(b))]
  rm=sqrt(u.x1^2+u.x2^2)
  srt=(sort(rm))(0:1)
  ROB0=NORM([double(u.x0(srt(0))),double(u.x1(srt(0))),double(u.x2(srt(0)))])
  ROB1=NORM([double(u.x0(srt(1))),double(u.x1(srt(1))),double(u.x2(srt(1)))])
  pixs=double(u.x0(srt(0)))*double(u.x0(srt(1)))+double(u.x1(srt(0)))*double(u.x1(srt(1)))+double(u.x2(srt(0)))*double(u.x2(srt(1)))
  pixs=pixs/rob0/rob1
  pixs=float(acos(pixs))
  n=fix(2*hs*!pi/180/pixs)>5
  hs=(n-1)*pixs*180/!pi/2
  bpos={l:lc,b:bc,hs:hs,n:[n,min(n)/2]}
endif
outbpos=bpos
n=bpos.n(0:1)
hs=bpos.hs*[1.,1]
grb=u_grid([-hs(0),-hs(1)],2*[hs(0),hs(1)],N*[1,1])
ph=grb.x*!pi/180
th=grb.y*!pi/180
grb=0
r=0*ph
sa={a:{x0:s.x0,x1:s.x1,x2:s.x2,t0:s.t0,qs:{l:l0,b:b0,p:p0,r:rad,type:'arc'},pos:s.pos},b:{qs:{l:bpos.l,b:bpos.b,p:0.,type:'sbox'}},proc:'b_spl',vec:0}
q=a_field(ph,th,r,set=sa)
if q.err then return,0
set={bl:q.t0,spos:{L:l0,b:b0,p:p0},bpos:{l:bpos.l,b:bpos.b,hs:hs},alfa:alpha}
res=get_fftplane(set=set,simple=simple)
pos=[-hs(0),-hs(1),hs(0),hs(1)]*!pi/180
z=0.
uu=get_fftplane(pos,n,z,simple=simple)

sa={a:{x0:uu.x0,x1:uu.x1,x2:uu.x0*0,t0:uu.t0,t1:uu.t1,t2:uu.t2,qs:sa.b.qs,ts:sa.b.qs,pos:pos},b:{qs:sa.a.qs,ts:sa.a.qs},proc:'b_spl',vec:1}
q1=a_field(s.x0,s.x1,s.x2,set=sa)
s1=s
s1.t0=q1.t0
if (where(tag_names(s) eq 'T1'))(0) eq -1 then s1=u_str_add(s1,'t1',q1.t1) else s1.t1=q1.t1
if (where(tag_names(s) eq 'T2'))(0) eq -1 then s1=u_str_add(s1,'t2',temporary(q1.t2)) else s1.t2=temporary(q1.t2)
if (where(tag_names(s) eq 'TYPE'))(0) ne -1 then s1.type='vecp '+s1.type else s1=u_str_add(s1,'type','vecp')
return,s1
end
