pro aia2,parms,rowdata,nparms,rparms,path=path,logtdem=logtdem,dem_run=dem_run,qrun=qrun,lrun=lrun,logte=logte,response=response,dem_tr_run=dem_tr_run,q0=q0,l0=l0,info=info
     if n_elements(response_path) eq 0 then begin
      dirpath=file_dirname((ROUTINE_INFO('aia2',/source)).path,/mark)
      response_path=dirpath+'AIA_Response.sav'
     end
 if arg_present(info) then begin
     if n_elements(info) eq 0 then begin
       Parms=Replicate({Name:'unused',Value:0d,Unit:'',Hint:''},8)
       k=0
       Parms[0].Name='dR'          & Parms[k].Value=0.600E+09       & Parms[k].Unit='cm'        & Parms[k++].Hint='Source/voxel Depth
       Parms[1].Name='T_0'         & Parms[k].Value=0.200E+08       & Parms[k].Unit='K'         & Parms[k++].Hint='Plasma Temperature
       Parms[2].Name='n_0'         & Parms[k].Value=0.500E+10       & Parms[k].Unit='cm^{-3}'   & Parms[k++].Hint='Thermal e density
       Parms[3].Name='Q'           & Parms[k].Value=0               & Parms[k].Unit=''          & Parms[k++].Hint='Heating rate'
       Parms[4].Name='Length'      & Parms[k].Value=0               & Parms[k].Unit='cm'        & Parms[k++].Hint='Half length of the associated fieldline'
       Parms[5].Name='VoxelID'     & Parms[k].Value=0               & Parms[k].Unit='0/1/2'     & Parms[k++].Hint='chromo/TR/corona'
       Parms[6].Name='n_hi'        & Parms[k].Value=0               & Parms[k].Unit='cm^{-3}'   & Parms[k++].Hint='Neutral Hydrogen density'
       Parms[7].Name='TRfactor'    & Parms[k].Value=0               & Parms[k].Unit=''          & Parms[k++].Hint='TR factor'
      
       NParms=Replicate({Name:'unused',Value:0l,Unit:'',user:0,Hint:''},4)
       k=0
       NParms[0].Name='ApplyTRfactor'& NParms[k].Value=1    & NParms[k].Unit='0/1'        & NParms[k++].Hint='Apply TR Factor'
       NParms[1].Name='AddTR'        & NParms[k].Value=1    & NParms[k].Unit='0/1'        & NParms[k++].Hint='Add Transition Region Contribution'
       NParms[2].Name='DEMavg'       & NParms[k].Value=0    & NParms[k].Unit='0/1'        & NParms[k++].Hint='DEM Interpolation Method'
       NParms[3].Name='UseDEM'       & NParms[k].Value=1    & NParms[k].Unit='0/1'        & NParms[k].user=1     & NParms[k++].Hint='Use DEM'

       RParms=[{Name:'dS',Value:0.180d+19,Unit:'cm^2',user:0,Hint:'Source/pixel Area'},$
               {Name:'n_HI0',Value:1d,Unit:'cm^{-3}',user:1,Hint:'Neutral Hydrogen density coronal cutoff'}]


    endif else begin
      parms=info.parms
      nparms=info.nparms
      rparms=info.rparms
    endelse
     restore,response_path
     nchan=n_elements(response.channels)
     w=fltarr(nchan)
     for i=0,nchan-1 do w[i]=fix(strmid(response.channels[i],1))
     rgb=ptr_new()
     catch, error_stat
     if error_stat ne 0 then begin
       catch, /cancel
       MESSAGE, /INFO, !ERROR_STATE.MSG
       goto,skip_rgb
     end
     restore,dirpath+'AIA_RGB.sav'
     skip_rgb:
     info={parms:parms,nparms:nparms,rparms:rparms,$
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
   ApplyTRfactor=nparms[0]
   AddTRinput=nparms[1]
   avgdem=nparms[2]
   useDEM=nparms[3]
   dS=rparms[0]
   n_hi0=rparms[1]
   norm_tr=dS/((4.5e7)^2)
   for pix=0, Npix-1 do begin
     row_parms=transpose(parms[pix,*,*])
     cutoff=max(where(row_parms[6,*] ge n_hi0))
     if cutoff ge 0 then row_parms[6,0:cutoff]=n_hi0
     tr_idx=max(where((ulong(row_parms[5,*]) and gx_voxelid(/euv)) ne 0))
     point_in=where((row_parms[1,*] gt 0 and row_parms[5,*] gt 1 and row_parms[6,*] lt n_hi0), Nvox)
     if Nvox gt 0 then begin
        parmin=row_parms[*,point_in]
        norm=parmin[0,*]*norm_tr
       if useDEM eq 1 then begin
         dem_interpolate,n,t,dem,path=path,logtdem=logtdem,dem_run=dem_run,qrun=qrun,lrun=lrun,qarr=parmin[3,*],larr=parmin[4,*],avgdem=avgdem
         tr_factor=1
         if AddTRinput eq 1 then begin
           tr_idx=max(where((ulong(parmin[5,*]) and gx_voxelid(/euv)) ne 0))
           if tr_idx ge 0 then begin
           point_in=where((parmin[1,*] gt 0 and parmin[6,*] lt n_hi0))
           dem_interpolate,n_tr,t_tr,dem_tr,path=path,logtdem=logtdem,dem_run=dem_tr_run,lrun=lrun,qrun=qrun,$
             larr=parmin[4,tr_idx],qarr=parmin[3,tr_idx],/tr,avgdem=avgdem
             tr_factor=ApplyTRfactor gt 0?parmin[7,tr_idx]:1
             addTR=(n_tr[0] gt 0 and t_tr[0] gt 0)
           endif else addTR=0
         endif else addTR=0
         dlogt = logtdem(1) - logtdem(0)
         for chan=0, nchan-1 do begin
           noDEMvox=where((n eq 0 or t eq 0),nnoDemvox, comp=DEMvox,ncomp=nDemvox)
           if nnoDEMvox gt 0 then begin
             g = dspline(logte, response[*,chan], alog10(reform(parmin[1,noDEMvox]))<maxLogT>minLogT)
             rowdata[pix,chan]= rowdata[pix,chan]+total(norm*reform(parmin[2,noDEMvox])^2*g,/double)
           end
           DEMvox=where((n gt 0 and t gt 0),nDemvox)
           if nDEMvox gt 0 then begin
             g = dspline(logte, response[*,chan], logtdem<maxLogT>minLogT)
             rowdata[pix,chan]= rowdata[pix,chan]+ alog(10.)*dlogt*total(norm*((g*(10.^logtdem))#dem))
             if addTR eq 1 then begin
               rowdata[pix,chan]= rowdata[pix,chan]+ norm_tr*tr_factor*alog(10.)*dlogt*total((g*(10.^logtdem))*dem_tr)
             end 
           end
         end
       endif else begin
        for chan=0, nchan-1 do begin
            g = dspline(logte, response[*,chan], alog10(reform(parmin[1,*]))<maxLogT>minLogT)
            rowdata[pix,chan] = total(norm*reform(parmin[2,*])^2*g,/double)
        end
       end
     end
   endfor  
end