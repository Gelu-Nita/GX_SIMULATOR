;+
; :Description: sfq_clean
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
pro sfq_clean,bx,by,s,gauss=gauss,median=median,show=show,mode=mode,silent=silent
  if not keyword_set(s) then begin
    if not keyword_set(silent) then begin
      time=systime(/sec)
      message,'Sarting SFQ cleaning',/info
    endif
    sfq_clean,bx,by,3,/median,show=show
    if n_elements(mode) le 0 then mode=0
    if mode eq 0 then begin
    if min((size(bx))[1:2]) gt 150 then sfq_clean,bx,by,19,gauss=gauss,median=median,show=show
   if min((size(bx))[1:2]) gt 100 then sfq_clean,bx,by,9,gauss=gauss,median=median,show=show
   endif
    sfq_clean,bx,by,5,gauss=gauss,median=median,show=show
    sfq_clean,bx,by,3,/median,show=show
    if not keyword_set(silent) then begin
      time=systime(/sec)-time
      message,'SFQ cleaning complete in '+strcompress(time,/remove_all)+' seconds',/info
    endif
   return
  endif
  n=ceil(s*3)*2+1
  ker=gaussf(n,s)
  gaussk=(ker[n/2]^2)
  for i=0 ,300 do begin
  if keyword_set(gauss) then begin
      mbx=pwf_smooth(bx,s)-bx*gaussk
     mby=pwf_smooth(by,s)-by*gaussk
    endif else begin
    if not keyword_set(median) then begin
      mbx=smooth(bx,s,/edge_tr)-bx/float(s^2)
    mby=smooth(by,s,/edge_tr)-by/float(s^2)
    endif
    endelse
     if keyword_set(median) then begin
      mbx=median(bx,s,/even)
     mby=median(by,s,/even)
    endif
    ind= where((mbx*bx+by*mby) lt 0)
    if n_elements(ind) lt ((n_elements(bx)*0.0001) >5) then goto,eee
    bx(ind)=-bx(ind)
    by(ind)=-by(ind)
    if keyword_set(show) then tvscl,bx
  endfor
  eee:
end