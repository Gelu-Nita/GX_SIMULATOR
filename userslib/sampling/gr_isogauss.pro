pro gr_isogauss,parms,rowdata,nparms,rparms,path,parmin,datain,freqlist,rdat,ebtel_path=ebtel_path,libpath=libpath,grparms=grparms,logtdem=logtdem,$
dem_run=dem_run,ddm_run=ddm_run,qrun=qrun,lrun=lrun,use_dem=use_dem,has_ddm=has_ddm,info=info
 if n_elements(path) eq 0 then path=gx_libpath('grffdem')
 if arg_present(info) then begin
    if n_elements(parms) gt 0 then dummy=temporary(parms)
    ;update EBTEL Fields
    if ~file_exist(ebtel_path) then ebtel_path=gx_ebtel_path()
    restore,ebtel_path
    if n_elements(info) eq 0 then begin
      result=call_external(path,'GET_PARMS1_SLICE',/F_VALUE,/unload )
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
      parms=[parms,{Name:'VoxelX',Value:0.0,unit:'',Hint:'LOS node X fractional index'}]
      parms=[parms,{Name:'VoxelY',Value:0.0,unit:'',Hint:'LOS node Y fractional index'}]
      parms=[parms,{Name:'VoxelZ',Value:0.0,unit:'',Hint:'LOS node Z fractional index'}]
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
    
    dummy_grparms=dblarr(7,nfreq,nparms[7].value-nparms[6].value+1,Npix)

    rdat=double(rparms.value)#replicate(1,Npix)
    
    dummy_parmin=replicate(1d,n_elements(parms)-5,Nvox,Npix)
    for i=0,Nvox-1 do for j=0,Npix-1 do dummy_parmin[*,i,j]=(parms.value)[0:n_elements(parms.value)-6]
    tdem=dblarr(Ntemp)
    dem=(ddm=dblarr(Ntemp,Nvox,Npix))

    test_call=call_external(path, 'GET_MW1_SLICE', ndat, rdat,dummy_parmin, tdem, dem, ddm, dummy_datain,dummy_grparms,/unload)
    freqlist=reform(dummy_datain[0,*])
    nchan=nparms[7].value-nparms[6].value+1
    ngrparms=7
    info={parms:parms,$
          nparms:nparms,$
          rparms:rparms,$
          pixdim:[nfreq,ngrparms,nchan],$
          spectrum:{x:{axis:freqlist,label:'Frequency',unit:'GHz'},$
                    y:{label:['LCP_Teff','LCP_tau','RCP_Teff','RCP_tau','X','Y','Z'],unit:['K','K','','','index','index','index']}},$
          channels:string(nparms[6].value+lindgen(nchan),format="('s',i0)")}                  
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
   parms_idx=N_parms-6
   rowdata[*]=0
   avgdem=nparms[6]
   recomputeNT=nparms[7]
   ndat=long(nparms[0:n_elements(nparms)-3])
   if n_elements(rdat) eq 0 then rdat=rparms#replicate(1,Npix)
   if n_elements(logtdem) gt 0 then tdem=10d0^logtdem
   if n_elements(datain) eq 0 then datain=dblarr(7,Nfreq,Npix)
   if n_elements(grparms) eq 0 then grparms=dblarr(7,Nfreq,nparms[7]-nparms[6]+1,Npix)
   if n_elements(rowparmin) eq 0 then rowparmin=dblarr(N_parms,Nvox,Npix)
   if n_elements(parmin) eq 0 then parmin=dblarr(N_parms,Nvox)

   skip_DEMDDM=(ndat[4] and ndat[5]) 
    for pix=0, Npix-1 do begin
      parmin[*,*]=transpose(parms[pix,*,*])
      
      for k=3,5 do begin
      idx=parmin[parms_idx+k,*]
      good=where(idx ne -1,count)
      if count gt 0 then begin
        idx=minmax(idx[good])
        rdat[k,pix]=idx[0]
        rdat[k+3,pix]=idx[1]
      end
      end

      if ~keyword_set(skip_DEMDDM) then begin
        ;if both DEM_Key and DDM_Key are turned off then skip the gx_dem_interpolate block
      gx_dem_interpolate,n,t,los_dem,los_ddm,ebtel_path=ebtel_path,libpath=libpath,logtdem=logtdem,dem_run=dem_run,ddm_run=ddm_run,qrun=qrun,lrun=lrun,$
        qarr=parmin[parms_idx+1,*],larr=parmin[parms_idx+2,*],avgdem=avgdem,use_dem=use_dem,has_ddm=has_ddm
      
      DEMvox=where((n gt 0 and t gt 0),nDemvox,comp=noDEMvox,ncomp=nNoDEMvox)
      if ~keyword_set(has_ddm) then los_ddm=los_dem*0
      if n_elements(dem) eq 0 then begin
        sz=size(los_dem)
        dem=(ddm=dblarr(sz[1],sz[2],Npix))
      endif
      dem[*,*,pix]=los_dem
      ddm[*,*,pix]=los_ddm
      if nDemVox gt 0 then begin
        if recomputeNT gt 0 then begin
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
     end 
      rowparmin[*,*,pix]=parmin
    end 
    if keyword_set(skip_DEMDDM) then begin  
      Ntemp=ndat[3]
      tdem=dblarr(Ntemp)
      dem=(ddm=dblarr(Ntemp,Nvox,Npix))
    end
   if n_elements(tdem) eq 0 then tdem=10d0^logtdem
   rowparmin=double(rowparmin[0:parms_idx,*,*])
   datain[*]=0
   if n_elements(freqlist) eq Nfreq then begin
     datain[0,*,*]=freqlist#replicate(1d,Npix)
   endif
   grparms[*]=0
   result=call_external(path, 'GET_MW1_SLICE', ndat, rdat,rowparmin, tdem, dem, ddm, datain,grparms)
   rowdata=transpose(grparms,[3,1,0,2])
end