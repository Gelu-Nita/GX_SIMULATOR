pro grffdemtransfer_deprecated,parms,rowdata,nparms,rparms,path,parmin,datain,freqlist,ebtel_path=ebtel_path,libpath=libpath,$
logtdem=logtdem,dem_run=dem_run,ddm_run=ddm_run,qrun=qrun,lrun=lrun,use_dem=use_dem,has_ddm=has_ddm,info=info
if n_elements(path) eq 0 then path=gx_libpath('grffdem')
 if arg_present(info) then begin
    if n_elements(parms) gt 0 then dummy=temporary(parms)
    ;update EBTEL Fields
    restore,gx_ebtel_path()
    if n_elements(info) eq 0 then begin
      result=call_external(path,'GET_PARMS_SLICE',/F_VALUE,/unload )
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
     
      ;Start adding LOS parameters needed by the wrapper
      parms=[parms,{Name:'Q',Value:0.0,unit:'',Hint:'Heating rate'}]
      parms=[parms,{Name:'Length',Value:0.0,unit:'cm',Hint:'Half length of the associated fieldline'}]
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
          nparms={name:strcompress(info[0]),value:fix(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER'?1:0,hint:info[4]}
        endif else begin
          nparms=[nparms,{name:strcompress(info[0]),value:fix(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER' ?1:0,hint:info[4]}]
        end
        count+=1
      ENDWHILE
      free_lun,lun
      file_delete,'Long_input.txt',/q
      
      ;Start adding integer input parameters need by the wrapper
      nparms=[nparms, {name:' DEMavg',value:0,unit:'',user:0,hint:'DEM Interpolation Method'}]
      nparms=[nparms, {name:' Recompute n&T',value:0,unit:'',user:1,hint:'Recompute DEM moments'}]
      ;End adding integer input parameters need by the wrapper
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

    ndat=long((nparms.value)[0:n_elements(nparms.value)-3])
    Npix=ndat[0]
    Nvox=ndat[1]
    Nfreq=ndat[2]
    Ntemp=ndat[3]
    dummy_datain=dblarr(7, Nfreq, Npix)
    if n_elements(freqlist) eq Nfreq then begin
     dummy_datain[0,*,*]=freqlist#replicate(1d,Npix)
    endif

    rdat=double(rparms.value)#replicate(1,Npix)
    
    dummy_parmin=replicate(1d,n_elements(parms)-2,Nvox,Npix)
    for i=0,Nvox-1 do for j=0,Npix-1 do dummy_parmin[*,i,j]=(parms.value)[0:n_elements(parms.value)-3]
    tdem=dblarr(Ntemp)
    dem=(ddm=dblarr(Ntemp,Nvox,Npix))

    test_call=call_external(path, 'GET_MW_SLICE', ndat, rdat,dummy_parmin, tdem, dem, ddm, dummy_datain,/unload)
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
   parms_idx=N_parms-3
   rowdata[*]=0
   ndat=long(nparms[0:n_elements(nparms)-3])
   rdat=rparms#replicate(1,Npix)
   if n_elements(logtdem) gt 0 then tdem=10d0^logtdem
   if n_elements(datain) eq 0 then datain=dblarr(7,Nfreq,Npix)
   if n_elements(rowparmin) eq 0 then rowparmin=dblarr(N_parms,Nvox,Npix)
   if n_elements(parmin) eq 0 then parmin=dblarr(N_parms,Nvox)
  for pix=0, Npix-1 do begin
    parmin[*,*]=transpose(parms[pix,*,*])
    gx_dem_interpolate,n,t,los_dem,los_ddm,ebtel_path=ebtel_path,libpath=libpath,logtdem=logtdem,dem_run=dem_run,ddm_run=ddm_run,qrun=qrun,lrun=lrun,$
      qarr=parmin[parms_idx+1,*],larr=parmin[parms_idx+2,*],avgdem=nparms[5],use_dem=use_dem,has_ddm=has_ddm
    DEMvox=where((n gt 0 and t gt 0),nDemvox,comp=noDEMvox,ncomp=nNoDEMvox)
    if ~keyword_set(has_ddm) then los_ddm=los_dem*0
    if n_elements(dem) eq 0 then begin
      sz=size(los_dem)
      dem=(ddm=dblarr(sz[1],sz[2],Npix))
    endif
    dem[*,*,pix]=los_dem
    ddm[*,*,pix]=los_ddm
    if nDemVox gt 0 then begin
      if nparms[6] gt 0 then begin
        ;Replace n&T computed from volume interpolated DEM/DDM with LOS-interpolated DEM/DDM moments
        parmin[1,DEMvox]=t[DEMvox]
        parmin[2,DEMvox]=n[DEMvox]
      end
      parmin[11,DEMvox]=0
      parmin[12,DEMvox]=0
    end
    if nNoDemVox gt 0 then begin
      parmin[11,noDEMvox]=1
      parmin[12,noDEMvox]=1
    endif
    rowparmin[*,*,pix]=parmin
  end 
   if n_elements(tdem) eq 0 then tdem=10d0^logtdem
   rowparmin=double(rowparmin[0:parms_idx,*,*])
   datain[*]=0
   if n_elements(freqlist) eq Nfreq then begin
     datain[0,*,*]=freqlist#replicate(1d,Npix)
   endif
   result=call_external(path, 'GET_MW_SLICE', ndat, rdat,rowparmin, tdem, dem, ddm, datain)
   rowdata[*,*,0,0]=transpose(datain[5,*,*]);eL
   rowdata[*,*,1,0]=transpose(datain[6,*,*]);eR
   rowdata[*,*,0,1]=transpose(datain[1,*,*]);wL
   rowdata[*,*,1,1]=transpose(datain[2,*,*]);wR
   rowdata[*,*,0,2]=transpose(datain[3,*,*]);sL
   rowdata[*,*,1,2]=transpose(datain[4,*,*]);sR
end