pro MWTransfer_user,parms,rowdata,path,parmin,datain,info=info
 if n_elements(path) eq 0 then begin
  dirpath=getenv('HOME')+'/MWTransfer/GS'
  path=dirpath+'/MWTransfer.so'
  if ~file_test(path) then begin
   file_mkdir, dirpath
   src_path=file_dirname((ROUTINE_INFO('mwtransfer_user',/source)).path)
   spawn, 'cp '+src_path+'/*.h '+dirpath
   spawn, 'cp '+src_path+'/*.cpp '+dirpath
   spawn, 'cp '+src_path+'/makefile '+dirpath
   cd, dirpath, current=cdr
   spawn, 'rm *.o'
   spawn, 'make'
   cd, cdr
  endif
 endif
 if arg_present(info) then begin
    if n_elements(parms) gt 0 then dummy=temporary(parms)
    if n_elements(info) eq 0 then begin
      result=call_external(path,'GET_PARMS',/F_VALUE,/unload )
      openr,lun,'Parms.txt',/get,error=error
      if error eq 1 then return
      line=''
      WHILE ~ EOF(lun) DO BEGIN 
         READF, lun, line 
         info=strsplit(line,';',/extract)
         if n_elements(info) eq 3 then info=[info,'']
         if n_elements(parms) eq 0 then begin
         parms={name:strcompress(info[0],/rem),value:float(info[1]),unit:strcompress(info[2],/rem),hint:info[3]}
         endif else begin
         parms=[parms,{name:strcompress(info[0],/rem),value:float(info[1]),unit:strcompress(info[2],/rem),hint:info[3]}]
         end
      ENDWHILE
      free_lun,lun
    endif else parms=info.parms  
    Nvox=1L
    dummy_parmin=parms.value
    dummy_datain=fltarr(7,parms[18].value)
    test_call=call_external(path,'GET_MW',Nvox,dummy_parmin,dummy_datain,/F_VALUE,/unload )
    info={parms:parms,$
          pixdim:[parms[18].value,2,3],$
          spectrum:{x:{axis:reform(dummy_datain[0,*]),label:'Frequency',unit:'GHz'},$
                    y:{label:['LCP','RCP','[RCP+LCP]','[RCP-LCP]','[R-L]/[R+L]','T_LCP','T_RCP','T_I','T_V'],unit:['sfu','sfu','sfu','sfu','%','K','K','K','K']}},$
          channels:['Exact Coupling', 'Weak Coupling', 'Strong Coupling']}                  
    return
 end
   sz=size(rowdata,/dim)
   Npix=sz[0]
   Nfreq=sz[1]
   Npol=sz[2]
   sz=size(parms,/dim)
   Npix=sz[0] 
   Nvox=sz[1]  
   Nparms=sz[2]
   rowdata[*]=0
   if n_elements(datain) eq 0 then datain=fltarr(7,Nfreq)
   if n_elements(parmin) eq 0 then parmin=fltarr(Nparms,Nvox)
   for pix=0, Npix-1 do begin
         parmin[*,*]=transpose(parms[pix,*,*])
         RESULT=call_external(path,'GET_MW',Nvox,parmin,datain,/F_VALUE,/unload )
         rowdata[pix,*,0,0]=datain[5,*];eL
         rowdata[pix,*,1,0]=datain[6,*];eR
         rowdata[pix,*,0,1]=datain[1,*];wL
         rowdata[pix,*,1,1]=datain[2,*];wR
         rowdata[pix,*,0,2]=datain[3,*];sL
         rowdata[pix,*,1,2]=datain[4,*];sR
   endfor
end