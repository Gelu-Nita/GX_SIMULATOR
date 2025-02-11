pro gx_euv_lib,parms,rowdata,nparms,rparms,sparms,ebtel_path, libpath, $
          logtdem=logtdem,dem_cor_run=dem_cor_run,qrun=qrun,lrun=lrun,$
          response=response,dem_tr_run=dem_tr_run,q0=q0,l0=l0,info=info,flux_m=flux_m,instrument=instrument
 default,instrument,'aia'
 if arg_present(info) then begin
  gx_euv_setup_lib, instrument=instrument,info=info 
  return
 endif else gx_euv_setup_lib, idx=idx 
   if n_elements(libpath) eq 0 then libpath=gx_libpath('rendereuv')
   if n_elements(dem_cor_run) eq 0 then begin
     if n_elements(ebtel_path) eq 0 then ebtel_path=gx_findfile(sparms[idx.sparms.ebtel])
     restore,ebtel_path
   end
   s=size(DEM_cor_run, /dimensions)
   NT_DEM=s[0]
   NQ=s[1]
   NL=s[2]

   instrument=sparms[idx.sparms.instr]
   AddTR=nparms[idx.nparms.addtr]
   ApplyTRfactor=nparms[idx.nparms.applytrf]
   Npix=nparms[idx.nparms.npix]
   Nvox=nparms[idx.nparms.nvox]
   Nchan=nparms[idx.nparms.nchan]
   rowdata[*]=0
   update_response=(n_elements(response) eq 0)?1:(response.date ne sparms[idx.sparms.responsedate])
   if (update_response eq 1) then begin
    response_date=atime(gx_utcstr2time(sparms[idx.sparms.responsedate]))
    response=strupcase(instrument) eq 'AIA'?gx_euv_response(response_date,instrument,$
                                            evenorm=nparms[idx.nparms.evenorm],$
                                            chiantifix=nparms[idx.nparms.chiantifix]): $
                                            gx_euv_response(response_date,instrument)    
   end  
   logTe_rsp=float(response.logte)
   NT_rsp=n_elements(response.logte) 
   arcsec_cm=wcs_rsun(unit='cm')/response.rsun_arcsec
   ds_arcsec2=rparms[idx.rparms.ds]/arcsec_cm^2
   ds_rsp=double(response.pix_arcsec^2)
   ds_pix=ds_arcsec2/ds_rsp
   r=double(response.all)
   
   norm_pix=rparms[idx.rparms.relabund]*ds_pix
   
   Lparms_M=[Npix, Nvox, Nchan, NT_rsp, NQ, NL, NT_DEM]
   Rparms_M=array_replicate([dS_arcsec2, dS_rsp],Npix)
   if n_elements(flux_M) eq 0 then flux_M=dblarr(3, Nchan, Npix)
   res=call_external(libpath, 'GET_GX_EUV_SLICE', $
     Lparms_M, Rparms_M, transpose(parms,[2,1,0]), logTe_rsp, r, $
     Qrun, Lrun, logtDEM, DEM_cor_run, DEM_tr_run, flux_M) 
     for k=0,2 do for l=0,Nchan-1 do flux_M[k,l,*]=flux_M[k,l,*]<max(flux_M[k,l,*],/nan);removes Nans, if any
     rowdata[*,*,1]=transpose(flux_m[0,*,*]); TR, no TR Mask
     rowdata[*,*,3]=transpose(flux_m[1,*,*]); Corona 
     rowdata[*,*,4]=transpose(flux_m[2,*,*]); TR Mask
     rowdata[*,*,2]=rowdata[*,*,3]*rowdata[*,*,4]; Masked TR 
     rowdata[*,*,0]=rowdata[*,*,1]+AddTR*rowdata[*,*,2]
end