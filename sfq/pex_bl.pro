;+
; :Description: pex_bl_
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function pex_bl_,si,sh=sh,silent=silent
  s=si
  if n_elements(sh) le 0 then sh=0
  if sh ne 0 then sh=1
  if n_elements(bin) le 0 then bin=0
  sz=size(s.t0)
  nx=sz(1)
  ny=sz(2)
  sz=size(s.t0)
    nx=sz(1)
    ny=sz(2)
    if bin eq 0 then sx=256. else sx=256./bin
    if bin eq 0 then sy=256. else sy=256./bin
  
    npx=ceil(nx/sx)>10
    npy=ceil(ny/sy)>10
    sx= nx / npx
    sy= ny / npy
  
  if s.indout(0) ne -1 then begin
  s.t0(s.indout)=0.
  endif
  pot=s
  pot.t0=0.
  if (where(tag_names(pot) eq 'T1'))(0) ne -1 then pot.t1=0. else pot=u_str_add(pot,'t1',pot.t0)
  if (where(tag_names(pot) eq 'T2'))(0) ne -1 then pot.t2=0. else pot=u_str_add(pot,'t2',pot.t0)
  ss=s
  ss=u_str_add(ss,'t1')
  ss=u_str_add(ss,'t2')
  wgt=s.t0*0    
  nparts=2*(npx+2)*(npy+2)
  oldcomplete=0
  for i=-1, npx do for j=-1,npy do begin
    ;-----------------------------------------------
    if not keyword_set(silent) then begin ;prints progress in percents
      complete=round(100.*float(j+1+(i+1)*(npx+2)+sh*nparts/2.)/nparts)
      if (complete gt oldcomplete) and((complete mod 10) eq 0) then message,'potential field calculating: '+strcompress(complete,/remove_all)+ ' % complete',/info
      oldcomplete=complete
    endif
    ;-----------------------------------------------
    xst=((i*sx+sx/2*sh)> 0)<(nx-1)
    xen=(((i+1)*sx-1+sx/2*sh)>0)<(nx-1)
    yst=((j*sy+sy/2*sh)>0)<(ny-1)
    yen=(((j+1)*sy-1+sy/2*sh)>0)<(ny-1)
    ss=u_str_add(ss,['t0','x0','x1','x2'],s.t0[xst:xen,yst:yen],s.x0[xst:xen,yst:yen],s.x1[xst:xen,yst:yen],s.x2[xst:xen,yst:yen])
    ss.pos=[s.x1(xst,yst),s.x2(xst,yst),s.x1(xen,yen),s.x2(xen,yen)]
    dx=(ss.pos(2)-ss.pos(0))/2
    xc=(ss.pos(2)+ss.pos(0))/2
    dy=(ss.pos(3)-ss.pos(1))/2
    yc=(ss.pos(3)+ss.pos(1))/2
    wgts=exp(-6*((ss.x1-xc)/dx)^2-6*((ss.x2-yc)/dy)^2)
    ;ss.type=s.type
    radius=s.qs.r
    alf=radius*!pi/180/3600
    dsun = 1./sin(alf)
    tx=tan(ss.x1*!pi/180/3600)
    ty=tan(ss.x2*!pi/180/3600)
    ro0=cos(alf)
    tx2ty2=temporary(tx)^2+temporary(ty)^2;Optimize memory
    a=dsun*(tx2ty2)/(tx2ty2+1)
    b=(dsun^2*(tx2ty2)-1)/(tx2ty2+1)
    det=temporary(a)^2-temporary(b)
    indin=where(det ge 0)
    indout=where(temporary(det) lt 0)
    ss=u_str_add(ss,['indin','indout'],indin,indout)
    if indout(0) ne -1 then ss.t0(indout)=0
    if (total(abs(ss.t0) gt 0) ne 0) and (n_elements(ss.indin) gt 1) then begin
      ss1=pot_vmag(ss,/simple)
      if (indout(0) ne -1) and (size(ss1,/type) eq 8 ) then begin
        ss1.t0(indout)=0
        ss1.t1(indout)=0
        ss1.t2(indout)=0
        wgts(indout)=0
      endif
      if size(ss1,/type) eq 8 then begin
            pot.t0[xst:xen,yst:yen]=ss1.t0
            pot.t1[xst:xen,yst:yen]=ss1.t1
            pot.t2[xst:xen,yst:yen]=ss1.t2
            wgt[xst:xen,yst:yen]=wgts
      endif
    endif
  endfor
  s.t0=pot.t0
  if (where(tag_names(s) eq 'T1'))(0) ne -1 then s.t1=pot.t1 else s=u_str_add(s,'t1',pot.t1)
  if (where(tag_names(s) eq 'T2'))(0) ne -1 then s.t2=pot.t2 else s=u_str_add(s,'t2',pot.t2)
  s=u_str_add(s,'wgt',wgt)
  return,s
 end
 function pex_bl,s0,silent=silent
  time=systime(/sec)
  if not keyword_set(silent) then message,'starting precise potential field calculating in parts',/info
  s1=pex_bl_(s0,silent=silent)
  s2=pex_bl_(s0,/sh,silent=silent)
  wgt=s1.wgt+s2.wgt
  t0=(s1.t0*s1.wgt+s2.t0*s2.wgt)
  t1=(s1.t1*s1.wgt+s2.t1*s2.wgt)
  t2=(s1.t2*s1.wgt+s2.t2*s2.wgt)
  s1=0
  s2=0
  ind=where(wgt ne 0)
  if ind(0) ne -1 then begin
    t0(ind)=t0(ind)/wgt(ind)
    t1(ind)=t1(ind)/wgt(ind)
    t2(ind)=t2(ind)/wgt(ind)
  endif
  s={t1:t1,t2:t2}
  if not keyword_set(silent) then message,'Potential field calculation complete in '+strcompress(systime(/sec)-time,/remove)+' seconds',/info
  return,s
end
