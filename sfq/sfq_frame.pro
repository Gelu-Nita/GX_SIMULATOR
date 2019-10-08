
;+
; :Description: sfq_frame
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function sfq_frame,s,solis=solis,hmi=hmi,silent=silent, acute = acute
  time=systime(/sec)
  if not keyword_set(silent) then message,'starting precise potential field calculating',/info
  pot=pot_vmag(s,/simple)
  if not keyword_set(silent) then message,'Potential field calculation complete in '+strcompress(systime(/sec)-time,/remove)+' seconds',/info
  s=sfq_step1(s,pot,silent=silent, acute = acute)
  by=s.t1
  bz=s.t2
  
  sz = size(by)
  bo = 10
  by_ = dblarr(sz[1]+bo*2,sz[2]+bo*2)
  bz_ = dblarr(sz[1]+bo*2,sz[2]+bo*2)
   by_[bo:bo+sz[1] -1,bo:bo+sz[2] -1] =by
    bz_[bo:bo+sz[1] -1,bo:bo+sz[2] -1] =bz
  
  
  ;sfq_clean,by,bz
  if n_elements(solis) le 0 then solis=0
  if keyword_set(hmi) then solis=1
  if solis ne 0 then solis=1
  if solis eq 1 then sfq_clean,by_,bz_,/mode else sfq_clean,by_,bz_
  by = by_[bo:bo+sz[1] -1,bo:bo+sz[2] -1]
  bz = bz_[bo:bo+sz[1] -1,bo:bo+sz[2] -1]
  s.t1=by
  s.t2=bz
  return,s
end
