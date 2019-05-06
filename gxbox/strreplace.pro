function strreplace,str,find,replace
  compile_opt idl2
  if n_elements(str) gt 1 then begin
    n=n_elements(str)
    result=strarr(n)
    for i=0,n-1 do begin
      result[i]=strreplace(str[i],find,replace)
    endfor
    return,result
  endif
  pos=0
  n=strlen(find)
  found=0
  res=''
  while 1 do begin
    index=strpos(str,find,pos)
    if index eq -1 then begin
      return,res+strmid(str,pos)
    endif
    res=res+strmid(str,pos,index-pos)+replace
    pos=index+n
  endwhile
end