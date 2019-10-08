;+
; :Description: sfq_step1
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function sfq_step1,mag,pot,silent=silent, acute = acute
  time=systime(/sec)
  if not keyword_set(silent) then message,'starting preliminary SFQ  disambiguation',/info
  ind=where(mag.t2 lt 0)
  if ind(0) ne -1 then begin
  mag.t1(ind)=-mag.t1(ind)
  mag.t2(ind)=-mag.t2(ind)
  endif
  
  if keyword_set(acute) then begin
    au = mag.t1*pot.t1 +mag.t2*pot.t2   
    ind = where( au lt 0)
    mag.t1(ind)=-mag.t1(ind)
    mag.t2(ind)=-mag.t2(ind)
    if not keyword_set(silent) then message,'preliminary SFQ  disambiguation complete in '+strcompress(systime(/sec)-time,/remove)+' seconds',/info
    return,mag 
  endif
  
  
  
  n=(size(mag.t0))(1:2)
  ;h=(mag.pos(2:3)-mag.pos(0:1))/(n-1)
  Bpy_y=(shift(pot.t1,-1,0)-pot.t1);/h(0)
  By_y=(shift(mag.t1,-1,0)-mag.t1);/h(0)
  au=(Bpy_y-By_y)^2
  au_=(Bpy_y+By_y)^2
  Bpy_y=(shift(pot.t1,0,-1)-pot.t1);/h(1)
  By_y=(shift(mag.t1,0,-1)-mag.t1);/h(1)
  au=au+(Bpy_y-By_y)^2
  au_=au_+(Bpy_y+By_y)^2
  Bpy_y=(shift(pot.t2,-1,0)-pot.t2);/h(0)
  By_y=(shift(mag.t2,-1,0)-mag.t2);/h(0)
  au=au+(Bpy_y-By_y)^2
  au_=au_+(Bpy_y+By_y)^2
  Bpy_y=(shift(pot.t2,0,-1)-pot.t2);/h(1)
  By_y=(shift(mag.t2,0,-1)-mag.t2);/h(1)
  au=sqrt(au+(Bpy_y-By_y)^2)
  au_=sqrt(au_+(Bpy_y+By_y)^2)
  ind=where(au gt au_)
  Bpy_y=(By_y=(au=(au_=0)))
  mag.t1(ind)=-mag.t1(ind)
  mag.t2(ind)=-mag.t2(ind)
  if not keyword_set(silent) then message,'preliminary SFQ  disambiguation complete in '+strcompress(systime(/sec)-time,/remove)+' seconds',/info
  return,mag
end
