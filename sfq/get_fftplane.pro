;+
; :Description: get_fftplane
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function get_fftplane,pos,n,z,half=half,set=set,derivs=derivs,simple=simple,nl=nl
if n_elements(set) gt 0 then begin
a=Lfff_fft(set=set,simple=simple,nl=nl)
return,1
endif
if n_elements(half) gt 0 then hlf=half else hlf=0
gr=u_grid(pos(0:1),pos(2:3)-pos(0:1),n,half=half)
a=Lfff_fft(z,derivs=derivs,simple=simple)
if n_elements(derivs) le 0 then derivs=0
bx=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t0)
by=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t1)
bz=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t2)
bl=b_spl( gr.x,gr.y,pos=a.pos, arr=a.bl)
if derivs then begin
bx_x=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t00)
by_x=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t10)
bz_x=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t20)
bx_y=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t01)
by_y=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t11)
bz_y=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t21)
bx_z=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t02)
by_z=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t12)
bz_z=b_spl( gr.x,gr.y,pos=a.pos, arr=a.t22)
endif
bpos=a.bpos
bpos.hs=(pos(2:3)-pos(0:1))/2*180/!pi
if derivs then $
return,{x0:gr.x,x1:gr.y,t0:bx,t1:by,t2:bz,bl:bl,pos:pos,z:a.z,spos:a.spos,bpos:a.bpos,half:hlf,t00:bx_x,t10:by_x,t20:bz_x,t01:bx_y,t11:by_y,t21:bz_y,t02:bx_z,t12:by_z,t22:bz_z} $
  else return,{x0:gr.x,x1:gr.y,t0:bx,t1:by,t2:bz,bl:bl,pos:pos,z:a.z,spos:a.spos,bpos:a.bpos,half:hlf}
end