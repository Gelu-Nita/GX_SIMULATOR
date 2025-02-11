pro aia,parms,rowdata,nparms,rparms,sparms,ebtel_path, libpath=libpath,logtdem=logtdem,dem_cor_run=dem_cor_run,qrun=qrun,lrun=lrun,response=response,dem_tr_run=dem_tr_run,q0=q0,l0=l0,info=info
  instrument='aia'
  if arg_present(info) then begin
    gx_euv_setup, instrument=instrument,info=info
    return
  endif else gx_euv,parms,rowdata,nparms,rparms,sparms,ebtel_path, libpath,$
    logtdem=logtdem,dem_cor_run=dem_cor_run,qrun=qrun,lrun=lrun,$
    response=response,dem_tr_run=dem_tr_run,q0=q0,l0=l0
end