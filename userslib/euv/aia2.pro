pro aia2,parms,rowdata,nparms,rparms,sparms,ebtel_path,logtdem=logtdem,dem_run=dem_run,qrun=qrun,lrun=lrun,logte=logte,response=response,dem_tr_run=dem_tr_run,q0=q0,l0=l0,info=info
  instrument='aia2'
  if arg_present(info) then begin
    gx_euv_setup, instrument=instrument,info=info
    return
  endif else gx_euv,parms,rowdata,nparms,rparms,sparms,ebtel_path,$
    logtdem=logtdem,dem_run=dem_run,qrun=qrun,lrun=lrun,logte=logte,$
    response=response,dem_tr_run=dem_tr_run,q0=q0,l0=l0
end