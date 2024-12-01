pro gx_euv,parms,rowdata,nparms,rparms,sparms,ebtel_path,$
          logtdem=logtdem,dem_run=dem_run,qrun=qrun,lrun=lrun,logte=logte,$
          response=response,dem_tr_run=dem_tr_run,q0=q0,l0=l0,info=info,instrument=instrument
 default,instrument,'aia'
 if arg_present(info) then begin
  gx_euv_setup, instrument=instrument,info=info 
  return
 endif else gx_euv_setup, idx=idx 
   if n_elements(ebtel_path) eq 0 then ebtel_path=gx_findfile(sparms[idx.sparms.ebtel])
   instrument=sparms[idx.sparms.instr]
   useDEM=nparms[idx.nparms.usedem]
   AddTR=nparms[idx.nparms.addtr]
   ApplyTRfactor=nparms[idx.nparms.applytrf]
   avgdem=nparms[idx.nparms.demavg]
   norm_tr=rparms[idx.rparms.relabund]*rparms[idx.rparms.ds]/((4.5e7)^2)
   n_hi0=rparms[idx.rparms.nhi0]
   Npix=nparms[idx.nparms.npix]
   Nchan=nparms[idx.nparms.nchan]
   rowdata[*]=0
   update_response=(n_elements(response) eq 0)?1:(response.date ne sparms[idx.sparms.responsedate])
   if (n_elements(logte) eq 0) or (update_response eq 1) then begin
    response_date=atime(gx_utcstr2time(sparms[idx.sparms.responsedate]))
    rdate=AIA_BP_UTC2DATE_STRING(ANYTIM2UTC(response_date, /ccsds))
    case strupcase(instrument) of 
      'TRACE':begin
        restore,gx_findfile('trace_response.sav')
        response=rep_tag_value(response,rdate,'date')
      end
      'SXT':begin
        restore,gx_findfile('sxt_response.sav')
        response=rep_tag_value(response,rdate,'date')
      end
      'SOLO-FSI':begin
        response=rep_tag_value(gx_get_eui_response(response_date,/fsi),rdate,'date')
      end
      'SOLO-HRI':begin
        response=rep_tag_value(gx_get_eui_response(response_date,/hri),rdate,'date')
      end
      'STEREO-A':begin
        response=rep_tag_value(gx_get_euvi_response(response_date,/a),rdate,'date')
      end
      'STEREO-B':begin
        response=rep_tag_value(gx_get_euvi_response(response_date,/b),rdate,'date')
      end
      'AIA2':begin
        response=rep_tag_value(gx_get_aia_response(response_date),rdate,'date')
      end
      else: response=aia_get_response(/temp,/dn,timedepend_date=response_date,evenorm=nparms[idx.nparms.evenorm],chiantifix=nparms[idx.nparms.chiantifix])
    endcase
      logte=response.logte
   end  
   maxLogT=max(logte,min=minLogT)
   
   for pix=0, Npix-1 do begin
     rowparms=transpose(parms[pix,*,*])
     cutoff=max(where(rowparms[idx.parms.nhi,*] ge n_hi0))
     if cutoff ge 0 then rowparms[idx.parms.nhi,0:cutoff]=n_hi0
     tr_idx=max(where((ulong(rowparms[idx.parms.voxid,*]) and gx_voxelid(/euv)) ne 0))
     point_in=where((rowparms[idx.parms.t0,*] gt 0 and rowparms[idx.parms.voxid,*] gt 1 and rowparms[idx.parms.nhi,*] lt n_hi0), Nvox)
     if Nvox gt 0 then begin
        parmin=rowparms[*,point_in]
        norm=parmin[idx.parms.dr,*]*norm_tr
       if useDEM eq 1 then begin
         gx_dem_interpolate,n,t,dem,path=ebtel_path,logtdem=logtdem,dem_run=dem_run,qrun=qrun,lrun=lrun,qarr=parmin[idx.parms.q,*],larr=parmin[idx.parms.l,*],avgdem=avgdem
         tr_factor=1
         if AddTR eq 1 then begin
           tr_idx=max(where((ulong(parmin[idx.parms.voxid,*]) and gx_voxelid(/euv)) ne 0))
           if tr_idx ge 0 then begin
           point_in=where((parmin[idx.parms.t0,*] gt 0 and parmin[idx.parms.nhi,*] lt n_hi0))
           gx_dem_interpolate,n_tr,t_tr,dem_tr,path=ebtel_path,logtdem=logtdem,dem_run=dem_tr_run,lrun=lrun,qrun=qrun,$
             larr=parmin[idx.parms.l,tr_idx],qarr=parmin[idx.parms.q,tr_idx],/tr,avgdem=avgdem
             tr_factor=ApplyTRfactor gt 0?parmin[idx.parms.trf,tr_idx]:1
             tr_add=(n_tr[0] gt 0 and t_tr[0] gt 0)
           endif else tr_add=0
         endif else tr_add=0
         dlogt = logtdem(1) - logtdem(0)
         for chan=0, nchan-1 do begin
           noDEMvox=where((n eq 0 or t eq 0),nnoDemvox, comp=DEMvox,ncomp=nDemvox)
           if nnoDEMvox gt 0 then begin
             g = dspline(logte, response.all[*,chan], alog10(reform(parmin[idx.parms.t0,noDEMvox]))<maxLogT>minLogT)
             rowdata[pix,chan]= rowdata[pix,chan]+total(norm*reform(parmin[idx.parms.n0,noDEMvox])^2*g,/double)
           end
           DEMvox=where((n gt 0 and t gt 0),nDemvox)
           if nDEMvox gt 0 then begin
             g = dspline(logte, response.all[*,chan], logtdem<maxLogT>minLogT)
             rowdata[pix,chan]= rowdata[pix,chan]+ alog(10.)*dlogt*total(norm*((g*(10.^logtdem))#dem))
             if tr_add eq 1 then begin
               rowdata[pix,chan]= rowdata[pix,chan]+ norm_tr*tr_factor*alog(10.)*dlogt*total((g*(10.^logtdem))*dem_tr)
             end 
           end
         end
       endif else begin
        for chan=0, nchan-1 do begin
            g = dspline(logte, response.all[*,chan], alog10(reform(parmin[idx.parms.t0,*]))<maxLogT>minLogT)
            rowdata[pix,chan] = total(norm*reform(parmin[idx.parms.n0,*])^2*g,/double)
        end
       end
     end
   endfor 
end