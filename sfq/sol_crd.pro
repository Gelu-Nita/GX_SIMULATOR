function SOL_crd,a,b,crd=crd,fld=fld
;a={x0:,x1:,x2:,t0:,t1:,t2,qs:{L:,B:,P:,R:,Type:('Dec','Arc','Sph','Sbox','Box')},ts:{L:,B:,P:,Type:('Dec','Sph','Sbox','Box')}}
;b={qs:{L:,B:,P:,R:,hs:hs},Type:('Dec','Arc','Sph','Sbox','Box'),ts:{L:,B:,P:},Type:('Dec','Sph','Sbox','Box')}
if N_elements(fld) le 0 then fld=0
if N_elements(crd) le 0 then crd=0
if (fld eq 0) and (crd eq 0) then begin
fld=1
crd=1
endif
if fld and crd then begin
if (where(tag_names(a) eq 'QS'))(0) eq -1 then aqs={l:0.,b:0.,p:0.,type:'Cdec'} else aqs=a.qs
if (where(tag_names(a) eq 'TS'))(0) eq -1 then ats={l:0.,b:0.,p:0.,type:'Cdec'} else ats=a.ts
if n_elements(b) le 0 then b={qs:{l:0.,b:0.,p:0.,type:'Cdec'},ts:{l:0.,b:0.,p:0.,type:'Cdec'}}
if (where(tag_names(b) eq 'QS'))(0) eq -1 then bqs={l:0.,b:0.,p:0.,type:'Cdec'} else bqs=b.qs
if (where(tag_names(b) eq 'TS'))(0) eq -1 then bts={l:0.,b:0.,p:0.,type:'Cdec'} else bts=b.ts
endif else begin
if crd then begin
if (where(tag_names(a) eq 'QS'))(0) eq -1 then aqs={l:0.,b:0.,p:0.,type:'Cdec'} else aqs=a.qs
if n_elements(b) le 0 then b={qs:{l:0.,b:0.,p:0.,type:'Cdec'},ts:{l:0.,b:0.,p:0.,type:'Cdec'}}
if (where(tag_names(b) eq 'QS'))(0) eq -1 then bqs={l:0.,b:0.,p:0.,type:'Cdec'} else bqs=b.qs
endif else begin
if (where(tag_names(a) eq 'QS'))(0) eq -1 then aqs={l:0.,b:0.,p:0.,type:'Cdec'} else aqs=a.qs
if (where(tag_names(a) eq 'TS'))(0) eq -1 then ats={l:0.,b:0.,p:0.,type:'Cdec'} else ats=a.ts
if n_elements(b) le 0 then b={qs:{l:0.,b:0.,p:0.,type:'Cdec'},ts:{l:0.,b:0.,p:0.,type:'Cdec'}}
if (where(tag_names(b) eq 'QS'))(0) eq -1 then bqs={l:0.,b:0.,p:0.,type:'Cdec'} else bqs=aqs
if (where(tag_names(b) eq 'TS'))(0) eq -1 then bts={l:0.,b:0.,p:0.,type:'Cdec'} else bts=b.ts
endelse
endelse
if strlowcase(aqs.type) eq 'cdec' then begin
xi0=a.x0
xi1=a.x1
xi2=a.x2
endif else begin
if (where(tag_names(aqs) eq 'P'))(0) eq -1 then p0=0. else p0=a.qs.p
stoem, aqs.b*!pi/180, aqs.l*!pi/180, -p0*!pi/180, e0, e1, e2
endelse
if strlowcase(aqs.type) eq 'dec' then begin
xi0=a.x0*e0(0)+a.x1*e1(0)+a.x2*e2(0)
xi1=a.x0*e0(1)+a.x1*e1(1)+a.x2*e2(1)
xi2=a.x0*e0(2)+a.x1*e1(2)+a.x2*e2(2)
endif
if strlowcase(aqs.type) eq 'box' then begin
ph=(a.x0*2*aqs.hs(0)-aqs.hs(0))*!pi/180
th=(-a.x1*2*aqs.hs(1)+90+aqs.hs(1))*!pi/180
r=a.x2*2*aqs.hs(0)*(!pi/180d)+1
x0=r*sin(th)*cos(ph)
x1=r*sin(th)*sin(ph)
x2=r*cos(th)
xi0=x0*e0(0)+x1*e1(0)+x2*e2(0)
xi1=x0*e0(1)+x1*e1(1)+x2*e2(1)
xi2=x0*e0(2)+x1*e1(2)+x2*e2(2)
endif

if strlowcase(aqs.type) eq 'sbox' then begin
ph=a.x0
th=!pi/2-a.x1
r=a.x2+1.
x0=r*sin(th)*cos(ph)
x1=r*sin(th)*sin(ph)
x2=r*cos(th)
xi0=x0*e0(0)+x1*e1(0)+x2*e2(0)
xi1=x0*e0(1)+x1*e1(1)+x2*e2(1)
xi2=x0*e0(2)+x1*e1(2)+x2*e2(2)
endif
if strlowcase(aqs.type) eq 'sph' then begin
ph=a.x2
th=a.x1
r=a.x0
x0=r*sin(th)*cos(ph)
x1=r*sin(th)*sin(ph)
x2=r*cos(th)
xi0=x0*e0(0)+x1*e1(0)+x2*e2(0)
xi1=x0*e0(1)+x1*e1(1)+x2*e2(1)
xi2=x0*e0(2)+x1*e1(2)+x2*e2(2)
endif
if strlowcase(aqs.type) eq 'arc' then begin
if (where(tag_names(a) eq 'X0'))(0) eq -1 then s=m0_to_arc(a.x1,a.x2,radius=aqs.r,/inv) else $
s=m0_to_arc(a.x1,a.x2,a.x0,radius=aqs.r,/inv)
xi0=s.x0*e0(0)+s.x1*e1(0)+s.x2*e2(0)
xi1=s.x0*e0(1)+s.x1*e1(1)+s.x2*e2(1)
xi2=s.x0*e0(2)+s.x1*e1(2)+s.x2*e2(2)
s=0
endif
if fld then begin
if strlowcase(ats.type) eq 'cdec' then begin
ti0=a.t0
ti1=a.t1
ti2=a.t2
endif else begin
if (where(tag_names(ats) eq 'P'))(0) eq -1 then p0=0. else p0=ats.p
stoem, ats.b*!pi/180, ats.l*!pi/180, -p0*!pi/180, e0, e1, e2
endelse
if (strlowcase(ats.type) eq 'dec') or (strlowcase(ats.type) eq 'arc') then begin
ti0=a.t0*e0(0)+a.t1*e1(0)+a.t2*e2(0)
ti1=a.t0*e0(1)+a.t1*e1(1)+a.t2*e2(1)
ti2=a.t0*e0(2)+a.t1*e1(2)+a.t2*e2(2)
endif
if (strlowcase(ats.type) eq 'box') or (strlowcase(ats.type) eq 'sbox') or (strlowcase(ats.type) eq 'sph') then begin
x0=xi0*e0(0)+xi1*e0(1)+xi2*e0(2)
x1=xi0*e1(0)+xi1*e1(1)+xi2*e1(2)
x2=xi0*e2(0)+xi1*e2(1)+xi2*e2(2)
r=sqrt(x0^2+x1^2+x2^2)
er0=x0/r&er1=x1/r&er2=x2/r
ep0=-er1&ep1=er0
et0=ep1*er2&et1=-ep0*er2&et2=ep0*er1-ep1*er0
rp=sqrt(ep0^2+ep1^2)
ep0=ep0/rp&ep1=ep1/rp
rp=sqrt(et0^2+et1^2+et2^2)
et0=et0/rp&et1=et1/rp&et2=et2/rp

if (strlowcase(ats.type) eq 'box') or (strlowcase(ats.type) eq 'sbox') then begin
ti0_=a.t2*er0-a.t1*et0+a.t0*ep0
ti1_=a.t2*er1-a.t1*et1+a.t0*ep1
ti2_=a.t2*er2-a.t1*et2
ti0=ti0_*e0(0)+ti1_*e1(0)+ti2_*e2(0)
ti1=ti0_*e0(1)+ti1_*e1(1)+ti2_*e2(1)
ti2=ti0_*e0(2)+ti1_*e1(2)+ti2_*e2(2)
endif else begin
ti0_=a.t0*er0+a.t1*et0+a.t2*ep0
ti1_=a.t0*er1+a.t1*et1+a.t2*ep1
ti2_=a.t0*er2+a.t1*et2
ti0=ti0_*e0(0)+ti1_*e1(0)+ti2_*e2(0)
ti1=ti0_*e0(1)+ti1_*e1(1)+ti2_*e2(1)
ti2=ti0_*e0(2)+ti1_*e1(2)+ti2_*e2(2)
endelse
endif
endif

if crd then begin
if strlowcase(bqs.type) eq 'cdec' then begin
x0=xi0
x1=xi1
x2=xi2
endif
if (where(tag_names(bqs) eq 'P'))(0) eq -1 then p0=0. else p0=bqs.p
stoem, bqs.b*!pi/180, bqs.l*!pi/180, -p0*!pi/180, e0, e1, e2
if strlowcase(bqs.type) eq 'box' then begin
x0=xi0*e0(0)+xi1*e0(1)+xi2*e0(2)
x1=xi0*e1(0)+xi1*e1(1)+xi2*e1(2)
x2=xi0*e2(0)+xi1*e2(1)+xi2*e2(2)
r=sqrt(x0^2+x1^2+x2^2)
t=acos(x2/r)
p=atan(x1,x0)
x2=(r-1)/2/b.qs.hs(0)*180/!pi
x1=(90+b.qs.hs(1)-t*180/!pi)/hs(1)/2
x0=(atan(sin(p),cos(p))*180d/!pi+hs(0))/hs(0)/2
endif
if strlowcase(bqs.type) eq 'sbox' then begin
x0=xi0*e0(0)+xi1*e0(1)+xi2*e0(2)
x1=xi0*e1(0)+xi1*e1(1)+xi2*e1(2)
x2=xi0*e2(0)+xi1*e2(1)+xi2*e2(2)
r=sqrt(x0^2+x1^2+x2^2)
t=acos(x2/r)
x0=atan(x1,x0)
x2=r-1
x1=!pi/2-t
endif
if strlowcase(bqs.type) eq 'sph' then begin
x0=xi0*e0(0)+xi1*e0(1)+xi2*e0(2)
x1=xi0*e1(0)+xi1*e1(1)+xi2*e1(2)
x2=xi0*e2(0)+xi1*e2(1)+xi2*e2(2)
r=sqrt(x0^2+x1^2+x2^2)
t=acos(x2/r)
x2=atan(x1,x0)
x1=temporary(t)
x0=temporary(r)
endif
if strlowcase(bqs.type) eq 'dec' then begin
x0=xi0*e0(0)+xi1*e0(1)+xi2*e0(2)
x1=xi0*e1(0)+xi1*e1(1)+xi2*e1(2)
x2=xi0*e2(0)+xi1*e2(1)+xi2*e2(2)
endif
if strlowcase(bqs.type) eq 'arc' then begin
x0=xi0*e0(0)+xi1*e0(1)+xi2*e0(2)
x1=xi0*e1(0)+xi1*e1(1)+xi2*e1(2)
x2=xi0*e2(0)+xi1*e2(1)+xi2*e2(2)
s=m0_to_arc(x1,x2,x0,radius=bqs.r)
indin=s.indin
indout=s.indout
visible=s.visible
unvisible=s.unvisible

x0=s.x0
x1=s.x1
x2=s.x2
s=0
endif
endif

if fld then begin
if strlowcase(bts.type) eq 'cdec' then begin
t0=ti0
t1=ti1
t2=ti2

endif
if (where(tag_names(bts) eq 'P'))(0) eq -1 then p0=0. else p0=bts.p
stoem, bts.b*!pi/180, bts.l*!pi/180, -p0*!pi/180, e0, e1, e2
if (strlowcase(bts.type) eq 'box') or (strlowcase(bts.type) eq 'sbox') or (strlowcase(bts.type) eq 'sph') then begin
xa0=xi0*e0(0)+xi1*e0(1)+xi2*e0(2)
xa1=xi0*e1(0)+xi1*e1(1)+xi2*e1(2)
xa2=xi0*e2(0)+xi1*e2(1)+xi2*e2(2)
r=sqrt(xa0^2+xa1^2+xa2^2)
er0=xa0/r&er1=xa1/r&er2=xa2/r
ep0=-er1&ep1=er0
et0=ep1*er2&et1=-ep0*er2&et2=ep0*er1-ep1*er0
rp=sqrt(ep0^2+ep1^2)
ep0=ep0/rp&ep1=ep1/rp
rp=sqrt(et0^2+et1^2+et2^2)
et0=et0/rp&et1=et1/rp&et2=et2/rp

if (strlowcase(bts.type) eq 'box') or (strlowcase(bts.type) eq 'sbox') then begin
ti0_=ti0*e0(0)+ti1*e0(1)+ti2*e0(2)
ti1_=ti0*e1(0)+ti1*e1(1)+ti2*e1(2)
ti2_=ti0*e2(0)+ti1*e2(1)+ti2*e2(2)

t2=er0*ti0_+er1*ti1_+er2*ti2_
t1=-(et0*ti0_+et1*ti1_+et2*ti2_)
t0=ep0*ti0_+ep1*ti1_
endif else begin
ti0_=ti0*e0(0)+ti1*e0(1)+ti2*e0(2)
ti1_=ti0*e1(0)+ti1*e1(1)+ti2*e1(2)
ti2_=ti0*e2(0)+ti1*e2(1)+ti2*e2(2)

t0=er0*ti0_+er1*ti1_+er2*ti2_
t1=et0*ti0_+et1*ti1_+et2*ti2_
t2=ep0*ti0_+ep1*ti1_
endelse
endif
if (strlowcase(bts.type) eq 'dec') or (strlowcase(bts.type) eq 'arc') then begin
t0=ti0*e0(0)+ti1*e0(1)+ti2*e0(2)
t1=ti0*e1(0)+ti1*e1(1)+ti2*e1(2)
t2=ti0*e2(0)+ti1*e2(1)+ti2*e2(2)

endif
endif
c=a
if n_elements(indin) gt 0 then begin
c=u_str_add(c,'indin',indin)
c=u_str_add(c,'indout',indout)
c=u_str_add(c,'visible',visible)
c=u_str_add(c,'unvisible',unvisible)

endif
if crd and fld then begin
xi0=0&xi1=0&xi2=0
ti0=0&ti1=0&ti2=0
if (where(tag_names(c) eq 'X0'))(0) eq -1 then c=u_str_add(c,'x0',x1)
c.x0=temporary(x0)
c.x1=temporary(x1)
c.x2=temporary(x2)
c.t0=temporary(t0)
c.t1=temporary(t1)
c.t2=temporary(t2)
c=u_str_add(c,'qs',bqs)
c=u_str_add(c,'ts',bts)
return,c
endif else begin
if crd then begin
xi0=0&xi1=0&xi2=0
ti0=0&ti1=0&ti2=0
if (where(tag_names(c) eq 'X0'))(0) eq -1 then c=u_str_add(c,'x0',x1)
c.x0=temporary(x0)
c.x1=temporary(x1)
c.x2=temporary(x2)
c=u_str_add(c,'qs',bqs)
return,c
endif else begin
xi0=0&xi1=0&xi2=0
ti0=0&ti1=0&ti2=0
c.t0=temporary(t0)
c.t1=temporary(t1)
c.t2=temporary(t2)
c=u_str_add(c,'ts',b.ts)
return,c
endelse
endelse
end
file='20061212_203005.fits'
file='20061217_002528.fits'
file='VMAG_SOLIS.fits'
s=RD_ASUN(file);,/ex)
;p=pos_extract(s.t0,s.pos)
;s.pos=p.pos
;s=u_str_add(s,'x0',s.x0(p.ipos(0):p.ipos(2),p.ipos(1):p.ipos(3)))
;s=u_str_add(s,'x1',s.x1(p.ipos(0):p.ipos(2),p.ipos(1):p.ipos(3)))
;s=u_str_add(s,'x2',s.x2(p.ipos(0):p.ipos(2),p.ipos(1):p.ipos(3)))
;s=u_str_add(s,'t0',s.t0(p.ipos(0):p.ipos(2),p.ipos(1):p.ipos(3)))
;s=u_str_add(s,'t1',s.t1(p.ipos(0):p.ipos(2),p.ipos(1):p.ipos(3)))
;s=u_str_add(s,'t2',s.t2(p.ipos(0):p.ipos(2),p.ipos(1):p.ipos(3)))
;s=sfq_frame(s)
s=sfq_full(s)
end