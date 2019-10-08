;+
; :Description: u_str_add
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function u_str_add,s,an,a0,a1,a2,a3,a4,a5,a6,a7
if n_elements(s) le 0  then begin
  nt=n_elements(an)
  tags=an;
  tags=tags+':a'+strcompress(indgen(nt),/remove_all) 
  l='u={'+strjoin(tags,',')+'}
  Result = EXECUTE(l)
  return,u
endif
tags=tag_names(s)
for i=0,n_elements(an)-1 do begin; Delete new tags from tag list
  ind= where(strlowcase(tags) ne strlowcase(an[i]))
  tags=tags[ind]
endfor
if (n_params() gt 2)  then begin 
  nt=n_elements(an)
  if n_params() ne (nt+2) then message,'wrong parameter count'
  newtags=an;
  newtags=newtags+':a'+strcompress(indgen(nt),/remove_all)    
endif
tags=tags+':s.'+tags
if n_elements(newtags) gt 0 then tags=[tags,newtags]
tags=strjoin(tags,',')
cmd="u={"+tags+"}"
Result = EXECUTE(cmd)
return,u
end