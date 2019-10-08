;+
; :Description: gaussf
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function gaussf,n,sigm
  if n_elements(sigm) ne 1 then sigm=3.
  x=findgen(n)-0.5*(float(n)-1.)
  return,1/(sqrt(2*!pi)*sigm)*exp(-x*x/(2*sigm*sigm))
end