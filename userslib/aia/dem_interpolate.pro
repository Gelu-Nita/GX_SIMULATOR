pro dem_interpolate,n,t,dem,path=path,logtdem=logtdem,dem_run=dem_run,qrun=qrun,lrun=lrun,qarr=qarr,larr=larr,tr=tr,ss=ss,verbose=verbose
  if n_elements(logtdem) eq 0 or n_elements(dem_run) eq 0 or n_elements(qrun) eq 0 or n_elements(lrun) eq 0 then begin
    if n_elements(path) eq 0 then begin
      dirpath=file_dirname((ROUTINE_INFO('dem_interpolate',/source)).path,/mark)
      path=dirpath+ (keyword_set(ss)?'ebtel_ss.sav':'ebtel.sav')
    end
    restore,path,/verb
    if keyword_set(tr) then dem_run=dem_tr_run else dem_run=dem_cor_run
  end  
 
  if n_elements(larr) eq 0 then larr=0
  if n_elements(qarr) eq 0 then qarr=0
  t0=systime(/s)
  x=lrun[0,*]
  dlogt=logtdem[1]-logtdem[0]
  n=fltarr(n_elements(larr))
  T=fltarr(n_elements(larr))
  if arg_present(dem) then dem=fltarr(n_elements(logtdem), n_elements(larr)) 
  xloc=VALUE_LOCATE( x, larr )
  xmm=[-1,n_elements(x)-1]
  ymm=[-1,n_elements(qrun[*,0])-1]
for k=0l, n_elements(larr)-1 do begin
   i1=xloc[k]
   if i1 eq xmm[0] or i1 eq xmm[1] then goto,skip
   l=larr[k]
   q=qarr[k]
   j1=VALUE_LOCATE(qrun[*,i1], q)
   if j1 eq ymm[0] or j1 eq ymm[1] then goto,skip
   j3=VALUE_LOCATE(qrun[*,i1+1], q)
   if j3 eq ymm[0] or j3 eq ymm[1] then goto,skip
   a1=dem_run[*,j1,i1]
   a2=dem_run[*,j1+1,i1]
   a3=dem_run[*,j3,i1+1]
   a4=dem_run[*,j3+1,i1+1]
   dx1=abs(x[i1]-l)
   dx2=abs(x[i1]-l)
   dx3=abs(x[i1+1]-l)
   dx4=abs(x[i1+1]-l)
   dy1=abs(qrun[j1,i1]-q)
   dy2=abs(qrun[j1+1,i1]-q)
   dy3=abs(qrun[j3,i1+1]-q)
   dy4=abs(qrun[j3+1,i1+1]-q)
   a12=(a1*dy2+a2*dy1)/(dy1+dy2)
   a34=(a3*dy4+a4*dy3)/(dy3+dy4)
  
   if arg_present(dem) then begin
      dem[*,k]=(a12*dx3+a34*dx1)/(dx1+dx3)
      N[k]=sqrt(alog(10.)*dlogt*total(dem[*,k]*(10.^logtdem),/double))
      T[k]=total(dem[*,k]*(10^logtdem)^2,/double)/total(dem[*,k]*(10.^logtdem),/double)   
   endif else begin
      dem=(a12*dx3+a34*dx1)/(dx1+dx3)
      N[k]=sqrt(alog(10.)*dlogt*total(dem*(10.^logtdem),/double))
      T[k]=total(dem*((10^logtdem)^2),/double)/total(dem*(10.^logtdem),/double)
   endelse

skip:
end
if keyword_set(verbose) then print,systime(/s)-t0
end