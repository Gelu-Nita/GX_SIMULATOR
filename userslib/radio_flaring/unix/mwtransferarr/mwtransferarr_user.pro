pro MWTransferArr_user,parms,rowdata,nparms,rparms,path,datain,freqlist,info=info

 if n_elements(path) eq 0 then begin
   dirpath=getenv('HOME')+'/mwtransfer/gs_arr'
   path=dirpath+'/MWTransferArr.so'
   if ~file_test(path) then begin
     file_mkdir, dirpath
     src_path=file_dirname((ROUTINE_INFO('mwtransferarr_user',/source)).path)
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
      result=call_external(path,'GET_PARMS_SLICE',/F_VALUE,/unload )
      openr,lun,'parms_input.txt',/get,error=error
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
      file_delete,'Parms_input.txt',/q
      
      openr,lun,'Long_input.txt',/get,error=err
      line=''
      count=0
      WHILE ~ EOF(lun) DO BEGIN
        READF, lun, line
        info=strsplit(line,';',/extract)
        unit=strcompress(info[2],/rem)
        unit=unit eq ''?'':' ('+unit+')'
        if count eq 0 then begin
          nparms={name:strcompress(info[0]),value:fix(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER'?1:0,hint:info[4]}
        endif else begin
          nparms=[nparms,{name:strcompress(info[0]),value:fix(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER' ?1:0,hint:info[4]}]
        end
        count+=1
      ENDWHILE
      free_lun,lun
      file_delete,'Long_input.txt',/q
      
      openr,lun,'Real_input.txt',/get,error=error
      line=''
      count=0
      WHILE ~ EOF(lun) DO BEGIN
        READF, lun, line
        info=strsplit(line,';',/extract)
        unit=strcompress(info[2],/rem)
        unit=unit eq ''?'':'('+unit+')'
        if count eq 0 then begin
          rparms={name:strcompress(info[0]),value:double(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER'?1:0,hint:info[4]}
        endif else begin
          rparms=[rparms,{name:strcompress(info[0]),value:double(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER'?1:0,hint:info[4]}]
        end
        count+=1
      ENDWHILE
      free_lun,lun
      file_delete,'Real_input.txt',/q
      
    endif else begin
      parms=info.parms
      nparms=info.nparms
      rparms=info.rparms
      freqlist=info.spectrum.x.axis
    endelse
   
    E_arr=0d
    mu_arr=0d
    f_arr_M=0d
    
    Lparms_M=long(nparms.value)
    Npix=Lparms_M[0]
    Nvox=Lparms_M[1]
    Nfreq=Lparms_M[2]
    dummy_datain=dblarr(7, Nfreq, Npix)
    if n_elements(freqlist) eq Nfreq then begin
     dummy_datain[0,*,*]=freqlist#replicate(1d,Npix)
    endif
    Rparms_M=double(rparms.value)#replicate(1d,Npix)
    Parms_M=replicate(1d,n_elements(parms),Nvox,Npix)
    for i=0,Nvox-1 do for j=0,Npix-1 do Parms_M[*,i,j]=parms.value

    test_call=call_external(path, 'GET_MW_SLICE', Lparms_M, Rparms_M,Parms_M, E_arr, mu_arr, f_arr_M,dummy_datain,/unload)
    freqlist=reform(dummy_datain[0,*])
    
    info={parms:parms,$
          nparms:nparms,$
          rparms:rparms,$
          pixdim:[nfreq,2,3],$
          spectrum:{x:{axis:freqlist,label:'Frequency',unit:'GHz'},$
                    y:{label:['LCP','RCP','[RCP+LCP]','[RCP-LCP]','[R-L]/[R+L]','T_LCP','T_RCP','T_I','T_V'],unit:['sfu','sfu','sfu','sfu','%','K','K','K','K']}},$
          channels:['Exact Coupling', 'Weak Coupling', 'Strong Coupling']}                  
    return
 end
   sz=size(rowdata,/dim)
   Npix=sz[0]
   Nfreq=sz[1]
   Npol=sz[2]
   Ncouplings=sz[3]
   sz=size(parms,/dim)
   Npix=sz[0] 
   Nvox=sz[1]  
   N_parms=sz[2]
   if n_elements(datain) eq 0 then datain=dblarr(7,Nfreq,Npix)
   datain[*]=0
   if n_elements(freqlist) eq Nfreq then begin
     datain[0,*,*]=freqlist#replicate(1d,Npix)
   endif

   parmin=transpose(parms,[2,1,0])

   ;calculating the emission for analytical distribution (array -> off)
   datain=dblarr(7, Nfreq, Npix)
   nparms=long(nparms)
   rparms=rparms#replicate(1d,Npix)
   result=call_external(path, 'GET_MW_SLICE', nparms, rparms, parmin, 0, 0, 0, datain, /unload)
   ;--------------------------------------------
   
   
   rowdata[*,*,0,0]=transpose(datain[5,*,*]);eL
   rowdata[*,*,1,0]=transpose(datain[6,*,*]);eR
   rowdata[*,*,0,1]=transpose(datain[1,*,*]);wL
   rowdata[*,*,1,1]=transpose(datain[2,*,*]);wR
   rowdata[*,*,0,2]=transpose(datain[3,*,*]);sL
   rowdata[*,*,1,2]=transpose(datain[4,*,*]);sR
end