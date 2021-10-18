pro MW_TRANSFER_ARR, parms,rowdata,nparms,rparms,path,datain,freqlist,E_arr,mu_arr,f_arr,info=info

 if n_elements(path) eq 0 then begin
  dirpath=file_dirname((ROUTINE_INFO('mw_transfer_arr',/source)).path,/mark)+'binaries\'
  path=dirpath+(!version.memory_bits eq 64?'MWTransferArr64.dll':'MWTransferArr32.dll')
 end
 
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
      
      ;Start adding LOS parameters needed by the wrapper
      parms=[parms,{Name:'SpineS',Value:0.0,unit:'',Hint:'Fluxtube spine longitudinal coordinate '}]
      parms=[parms,{Name:'SpineR',Value:0.0,unit:'',Hint:'Fluxtube spine radial distance '}]
      parms=[parms,{Name:'HasArr',Value:0.0,unit:'',Hint:'Fluxtube has array defined distributions'}]
      ;End adding LOS parameters need by the wrapper
      openr,lun,'Long_input.txt',/get,error=err
      line=''
      count=0
      WHILE ~ EOF(lun) DO BEGIN
        READF, lun, line
        info=strsplit(line,';',/extract)
        unit=strcompress(info[2],/rem)
        unit=unit eq ''?'':' ('+unit+')'
        if count eq 0 then begin
          nparms={name:strcompress(info[0]),value:long(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER'?1:0,hint:info[4]}
        endif else begin
          nparms=[nparms,{name:strcompress(info[0]),value:long(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER' ?1:0,hint:info[4]}]
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
      freqlist=double(info.spectrum.x.axis)
      E_arr=double(info.aparms.E_arr)
      mu_arr=double(info.aparms.mu_arr)
      f_arr=double(info.aparms.f_arr)
    endelse   
    parms_idx=n_elements(parms)-4
    Lparms_M=long(nparms.value)
    Npix=Lparms_M[0]
    Nvox=Lparms_M[1]
    Nfreq=Lparms_M[2]
    if (size(f_arr))[0] eq 3 then begin
      f_arr_M=0d
      Lparms_M[3]=n_elements(E_arr)
      Lparms_M[4]=n_elements(mu_arr)
    endif else begin
      Lparms_M[3]=0
      Lparms_M[4]=0
      E_arr=0d
      mu_arr=0d
      f_arr=0d
      f_arr_M=0d
    endelse
    nparms.value=Lparms_M
    datain=dblarr(7, Nfreq, Npix)
    if n_elements(freqlist) eq Nfreq then begin
     datain[0,*,*]=array_replicate(freqlist,Npix)
    endif
    Rparms_M=array_replicate(double(rparms.value),Npix)
    Parms_M=array_replicate(double((parms.value)[0:parms_idx]),Nvox,Npix)
    
    ;______________
    ;Test Call
    ;to speed up test calculation, use Npix=1 & Nvox=1 
    dummy_Lparms_M=Lparms_M
    dummy_Lparms_M[0:1]=[1,1]
    dummy_Lparms_M[3:4]=[0,0]
    dummy_E_arr=0d
    dummy_Mu_arr=0d
    dummy_Parms_M=Parms_M[*,0,0]
    dummy_datain=datain[*,*,0]
    test_call=call_external(path, 'GET_MW_SLICE', dummy_Lparms_M, Rparms_M,dummy_Parms_M, E_arr, dummy_mu_arr, f_arr_M,dummy_datain,/unload)
    freqlist=reform(dummy_datain[0,*])
    ;______________
    
    info={parms:parms,$
          nparms:nparms,$
          rparms:rparms,$
          aparms:{e_arr:e_arr,mu_arr:mu_arr,f_arr:f_arr},$
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
   parms_idx=N_parms-4
   
   if n_elements(datain) eq 0 then datain=dblarr(7, Nfreq, Npix) else datain[*]=0
   
   if n_elements(freqlist) eq Nfreq and rparms[1] eq 0 then begin
     datain[0,*,*]=freqlist#replicate(1d,Npix)
   endif

   ;calculating the emission for analytical distribution
   Lparms_M=long(nparms)
   Lparms_M[0:2]=[Npix,Nvox,Nfreq]
   Lparms_M[3]=0; changed bellow if defined
   Lparms_M[4]=0; changed bellow if defined
   Rparms_M=rparms#replicate(1d,Npix)
   Parms_M=double(transpose(parms[*,*,0:parms_idx],[2,1,0]))
   if n_elements(f_arr) ne 0 then begin
     f_size=size(f_arr)
     if f_size[0] eq 3 then begin
      if f_size[1] eq n_elements(E_arr) and f_size[2] eq n_elements(mu_arr)then begin
        Lparms_M[3]=n_elements(E_arr)
        Lparms_M[4]=n_elements(mu_arr)
        E_arr=double(E_arr)
        mu_arr=double(mu_arr)
        SpineS=transpose(parms[*,*,parms_idx+1])
        HasArr=transpose(parms[*,*,parms_idx+3])
        aparms_idx=where(HasArr eq 1, aparms_count,comp=no_aparms_idx, ncomp=no_aparms_count)
        if no_aparms_count gt 0 then begin
          arr_key=reform(Parms_M[21,*,*])
          arr_key[no_aparms_idx]=1
          Parms_M[21,*,*]=arr_key
        end
        f_arr_M=double(reform(f_arr[*,*,SpineS],Lparms_M[3],Lparms_M[4],Nvox,Npix))*$
        (transpose(array_replicate(parms[*,*,parms_idx+2],Lparms_M[3],Lparms_M[4]),[2,3,1,0]))>1d-100  
      endif else no_aparms=1
     endif else no_aparms=1
   endif else no_aparms=1
   
   if keyword_set(no_aparms) then begin
    E_arr=0d
    mu_arr=0d
    f_arr=0d
    f_arr_M=0d
   endif
   
   if LPARMS_M[8] eq -1 then begin
    ;This is a GX-implemented custom use of arr_key global switch
    ;arr_key=-1 sets analytical nonthermal density to 0 in the entire volume
    ;thus, it allows contributions only from the array defined distributions, if any 
    Parms_M[7,*,*]=0
    LPARMS_M[8]=0; arr_key is switched on 
   endif
   result=call_external(path, 'GET_MW_SLICE', Lparms_M, Rparms_M, Parms_M, E_arr, mu_arr, f_arr_M, datain, /unload)
   ;--------------------------------------------
   
   
   rowdata[*,*,0,0]=transpose(datain[5,*,*]);eL
   rowdata[*,*,1,0]=transpose(datain[6,*,*]);eR
   rowdata[*,*,0,1]=transpose(datain[1,*,*]);wL
   rowdata[*,*,1,1]=transpose(datain[2,*,*]);wR
   rowdata[*,*,0,2]=transpose(datain[3,*,*]);sL
   rowdata[*,*,1,2]=transpose(datain[4,*,*]);sR
end