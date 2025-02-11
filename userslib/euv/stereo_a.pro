pro stereo_a,parms,rowdata,nparms,rparms,sparms,ebtel_path,logtdem=logtdem,dem_cor_run=dem_cor_run,qrun=qrun,lrun=lrun,response=response,dem_tr_run=dem_tr_run,q0=q0,l0=l0,info=info
  if arg_present(info) then begin
    gx_euv_setup, instrument='stereo-a',info=info
    return
  endif else gx_euv,parms,rowdata,nparms,rparms,sparms,ebtel_path,$
    logtdem=logtdem,dem_cor_run=dem_cor_run,qrun=qrun,lrun=lrun,$
    response=response,dem_tr_run=dem_tr_run,q0=q0,l0=l0
end