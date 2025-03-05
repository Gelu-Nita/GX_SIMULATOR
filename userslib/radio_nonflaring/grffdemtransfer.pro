pro grffdemtransfer,parms,rowdata,nparms,rparms,path,parmin,datain,freqlist,ebtel_path=ebtel_path,libpath=libpath,$
logtdem=logtdem,dem_run=dem_run,ddm_run=ddm_run,qrun=qrun,lrun=lrun,info=info
if n_elements(path) eq 0 then path=gx_libpath('grffdem')
 if arg_present(info) then begin
    if n_elements(parms) gt 0 then dummy=temporary(parms)
    if n_elements(info) eq 0 then begin
      result=call_external(path,'GET_GX_PARMS_SLICE',/F_VALUE,/unload )
      openr,lun,'Parms_input.txt',/get,error=error
      line=''
      WHILE ~ EOF(lun) DO BEGIN 
         READF, lun, line 
         info=strsplit(line,';',/extract)
         if n_elements(info) eq 3 then info=[info,'']
         if n_elements(parms) eq 0 then begin
         parms={name:strcompress(info[0],/rem),value:float(info[1]),unit:strcompress(info[2],/rem),hint:info[4]}
         endif else begin
         parms=[parms,{name:strcompress(info[0],/rem),value:float(info[1]),unit:strcompress(info[2],/rem),hint:info[4]}]
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
      ;update EBTEL Fields
      restore,gx_ebtel_path()
      idx=gx_name2idx(nparms,'N_temp')
      if idx ge 0 then nparms[idx].value=n_elements(logtdem)
      idx=gx_name2idx(nparms,'DDM_key')
      hasDDM=(n_elements(DDM_cor_run) gt 0)?1:0
      if idx ge 0 then begin
        nparms[idx].value=1-hasDDM
        nparms[idx].user=hasDDM
      endif
    endif else begin
      parms=info.parms
      nparms=info.nparms
      rparms=info.rparms
      freqlist=info.spectrum.x.axis
    endelse

    ndat=long((nparms.value))
    Npix=ndat[0]
    Nvox=ndat[1]
    Nfreq=ndat[2]
    dummy_datain=dblarr(7, Nfreq, Npix)
    if n_elements(freqlist) eq Nfreq then begin
     dummy_datain[0,*,*]=freqlist#replicate(1d,Npix)
    endif

    rdat=double(rparms.value)#replicate(1,Npix)
    
    dummy_parmin=replicate(1d,n_elements(parms),Nvox,Npix)
    for i=0,Nvox-1 do for j=0,Npix-1 do dummy_parmin[*,i,j]=(parms.value)
    ;update EBTEL Fields
    restore,gx_ebtel_path()
    sz=size(dem_cor_run)
    ndat[3]=sz[2]
    ndat[4]=sz[3]
    ndat[5]=sz[1]
    nparms.value=ndat
    test_call=call_external(path, 'GET_GX_MW_SLICE', ndat, rdat,dummy_parmin, qrun,lrun,logtdem, dem_cor_run, ddm_cor_run, dummy_datain,/unload)
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
   sz=size(parms,/dim)
   Npix=sz[0] 
   Nvox=sz[1]  
   N_parms=sz[2]
   rowdata[*]=0
   ndat=long(nparms)
   rdat=rparms#replicate(1,Npix)
   if n_elements(datain) eq 0 then datain=dblarr(7,Nfreq,Npix)
   if n_elements(parmin) eq 0 then parmin=dblarr(N_parms,Nvox,Npix)
   parmin[*]=transpose(parms,[2,1,0])
   if n_elements(dem_run) eq 0 then begin
     ;update EBTEL Fields
     restore,gx_ebtel_path()
     dem_run=dem_cor_run
     ddm_run=ddm_cor_run
   end
   sz=size(dem_run)
   ndat[3]=sz[2]
   ndat[4]=sz[3]
   ndat[5]=sz[1]
   datain[*]=0
   if n_elements(freqlist) eq Nfreq then begin
     datain[0,*,*]=freqlist#replicate(1d,Npix)
   endif
   result=call_external(path, 'GET_GX_MW_SLICE', ndat, rdat,parmin, qrun,lrun,logtdem, dem_run, ddm_run, datain,/unload)
   rowdata[*,*,0,0]=transpose(datain[5,*,*]);eL
   rowdata[*,*,1,0]=transpose(datain[6,*,*]);eR
   rowdata[*,*,0,1]=transpose(datain[1,*,*]);wL
   rowdata[*,*,1,1]=transpose(datain[2,*,*]);wR
   rowdata[*,*,0,2]=transpose(datain[3,*,*]);sL
   rowdata[*,*,1,2]=transpose(datain[4,*,*]);sR
end