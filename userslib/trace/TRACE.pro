pro trace, parms, rowdata, path=path, logtdem=logtdem, dem_run=dem_run, qrun=qrun, $
		lrun=lrun, logte=logte, response=response, dem_tr_run=dem_tr_run, q0=q0, l0=l0, $
		info=info

     if n_elements(response_path) eq 0 then begin
      dirpath=file_dirname((ROUTINE_INFO('trace',/source)).path,/mark)
      response_path=dirpath+'TRACE_Response.sav'
     end

 if arg_present(info) then begin
     if n_elements(info) eq 0 then begin
        Parms=Replicate({Name:'unused',Value:0d,Unit:'',Hint:''},12)
       Parms[0].Name='dS'          & Parms[0].Value=0.180E+19     & Parms[0].Unit='cm^2'     & Parms[0].Hint='Source/pixel Area
       Parms[1].Name='dR'          & Parms[1].Value=0.600E+09     & Parms[1].Unit='cm'       & Parms[1].Hint='Source/voxel Depth
       Parms[2].Name='T_0'         & Parms[2].Value=0.200E+08     & Parms[2].Unit='K'        & Parms[2].Hint='Plasma Temperature
       Parms[3].Name='n_0'         & Parms[3].Value=0.500E+10    & Parms[3].Unit='cm^{-3}' & Parms[3].Hint='Thermal e density
       Parms[4].Name='Q'           & Parms[4].Value=0            & Parms[4].Unit=''        & Parms[4].Hint='Heating rate'
       Parms[5].Name='Length'      & Parms[5].Value=0            & Parms[5].Unit='cm'      & Parms[5].Hint='Half length of the associated fieldline'
       Parms[6].Name='UseDEM'      & Parms[6].Value=0            & Parms[6].Unit='0/1'        & Parms[6].Hint='Use DEM'
       Parms[7].Name='VoxelID'      & Parms[7].Value=0            & Parms[7].Unit='0/1/2'        & Parms[7].Hint='chromo/TR/corona'
       Parms[8].Name='AddTR'       & Parms[8].Value=0            & Parms[8].Unit='0/1'        & Parms[8].Hint='Add Transition Region Contribution'
       Parms[9].Name='n_hi'       & Parms[9].Value=0            & Parms[9].Unit='cm^{-3}'        & Parms[9].Hint='Neutral Hydrogen density'
       Parms[10].Name='n_hi0'    & Parms[10].Value=1          & Parms[10].Unit='cm^{-3}'        & Parms[10].Hint='Neutral Hydrogen density coronal cutoff'
       Parms[11].Name='SS'       & Parms[11].Value=0            & Parms[11].Unit=''        & Parms[11].Hint='Use steady state DEM table'
    endif else parms=info.parms
     restore,response_path
     nchan=n_elements(response.channels)
     w=fltarr(nchan)
     for i=0,nchan-1 do w[i]=fix(strmid(response.channels[i],0))
     rgb=ptr_new()
     catch, error_stat
     if error_stat ne 0 then begin
       catch, /cancel
       MESSAGE, /INFO, !ERROR_STATE.MSG
       goto,skip_rgb
     end
     restore,dirpath+'TRACE_RGB.sav'

		; Note: No change has been made for TRACE.

     skip_rgb:
     info={parms:parms,$
           pixdim:[nchan],$
           spectrum:{x:{axis:w,label:'Wavelength',unit:'A'},$
                    y:{label:'I',unit:'counts/s/pix'}},rgb:rgb} 
     return
 end
   sz=size(rowdata,/dim)
   Npix=sz[0]
   Nchan=sz[1]
   rowdata[*]=0
   if (n_elements(logte) eq 0) or (n_elements(response) eq 0) then begin
    restore,response_path
    logte=response.logte
    response=response.all
   end  
   maxLogT=max(logte,min=minLogT)
   for pix=0, Npix-1 do begin
     rparms=transpose(parms[pix,*,*])
     n_hi0=rparms[10,0]
     cutoff=max(where(rparms[9,*] ge n_hi0))
    if cutoff ge 0 then rparms[9,0:cutoff]=n_hi0
     tr_idx=max(where(rparms[7,*] eq 1))
     rparms=rparms[*,(tr_idx>0):*]
     point_in=where((rparms[2,*] gt 0 and rparms[7,*] gt 0 and rparms[9,*] lt n_hi0), Nvox)
     if Nvox gt 0 then begin
        parmin=rparms[*,point_in]
        norm=parmin[0,*]*parmin[1,*]/((4.5e7)^2)
        norm_tr=parmin[0,0]/((4.5e7)^2)
        tr_idx=max(where(rparms[7,*] eq 1))
       if parmin[6,0] eq 1 then begin
         dem_interpolate,n,t,dem,path=path,logtdem=logtdem,dem_run=dem_run,qrun=qrun,lrun=lrun,qarr=parmin[4,*],larr=parmin[5,*],ss=parmin[11,0]
         if tr_idx ge 0 then begin
         dem_interpolate,n_tr,t_tr,dem_tr,path=path,logtdem=logtdem,dem_run=dem_tr_run,lrun=lrun,qrun=qrun,$
                          larr=parmin[5,tr_idx],qarr=parmin[4,tr_idx],/tr,ss=parmin[11,0]
         if n_tr gt 0 then if keyword_set(verbose) then print,n_tr
         endif
         dlogt = logtdem(1) - logtdem(0)
         for chan=0, nchan-1 do begin
           noDEMvox=where((n eq 0 or t eq 0) and (parmin[9,*] eq 0),nnoDemvox, comp=DEMvox,ncomp=nDemvox)
           if nnoDEMvox gt 0 then begin
             g = dspline(logte, response[*,chan], alog10(reform(parmin[2,noDEMvox]))<maxLogT>minLogT)
             rowdata[pix,chan]= rowdata[pix,chan]+total(norm*reform(parmin[3,noDEMvox])^2*g,/double)
           end
           DEMvox=where((n gt 0 and t gt 0) and (parmin[9,*] eq 0),nDemvox)
           if nDEMvox gt 0 then begin
             g = dspline(logte, response[*,chan], logtdem<maxLogT>minLogT)
             rowdata[pix,chan]= rowdata[pix,chan]+ alog(10.)*dlogt*total(norm*((g*(10.^logtdem))#dem))
             if parmin[8,0] eq 1 then begin
             if n_elements(n_tr ) gt 0 and n_elements(t_tr ) gt 0 then $
              noDEMvox=where(n_tr eq 0 or t_tr eq 0,nnoDemvox, comp=DEMvox,ncomp=nDemvox) else nnoDemvox=-1
              if nnoDemvox eq 0 then begin
               rowdata[pix,chan]= rowdata[pix,chan]+ norm_tr*alog(10.)*dlogt*total((g*(10.^logtdem))*dem_tr)
              end
             end 
           end
         end
       endif else begin
        for chan=0, nchan-1 do begin
            g = dspline(logte, response[*,chan], alog10(reform(parmin[2,*]))<maxLogT>minLogT)
            rowdata[pix,chan] = total(norm*reform(parmin[3,*])^2*g,/double)
        end
       end
     end
   endfor  
end