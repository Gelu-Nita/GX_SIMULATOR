;+
; :Description: Lfff_fft
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function Lfff_fft,z,set=set,derivs=derivs,simple=simple,nl=nl
common com_Lfff_fft,spos,bpos
;set={bl:,spos:,bpos:alfa}
if n_elements(simple) le 0 then simple=0
if n_elements(set) gt 0 then begin
if size(set.bl,/type) eq 5 then begin
pi_=!dpi
e_=1d
endif else begin
pi_=!pi
e_=1.
endelse
spos=set.spos
bpos={l:set.bpos.l,b:set.bpos.b,hs:set.bpos.hs*[1,1]}
alfa=set.alfa
rl=spos.l*pi_/180
rb=spos.b*pi_/180
rp=spos.p*pi_/180
rbl=bpos.l*pi_/180
rbb=bpos.b*pi_/180
rbp=0*e_
stoem, rb, rl, -rp, ex, ey, ez
stoem, rbb, rbl, -rbp, exb, eyb, ezb
exbx=ex(0)*exb(0)+ex(1)*exb(1)+ex(2)*exb(2)
exby=ex(0)*eyb(0)+ex(1)*eyb(1)+ex(2)*eyb(2)
exbz=ex(0)*ezb(0)+ex(1)*ezb(1)+ex(2)*ezb(2)
hx=exby
hy=exbz
hz=exbx
pos=[-bpos.hs(0),-bpos.hs(1),bpos.hs(0),bpos.hs(1)]*pi_/180
nl=[hx,hy,hz]
sett={data:set.bl,vx:hx,vy:hy,vz:hz,alfa:alfa,pos:pos}
if simple then a=_Lfff_fft_n(set=sett) else a=_Lfff_fft(set=sett)
return,1
endif
if n_elements(spos) le 0 then return,0
if simple then a=_Lfff_fft_n(z,derivs=derivs) else a=_Lfff_fft(z,derivs=derivs)
a=u_str_add(a,'spos',spos)
a=u_str_add(a,'bpos',bpos)
return,a
end