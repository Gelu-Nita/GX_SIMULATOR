pro aia,parms,rowdata,nparms,rparms,path=path,logtdem=logtdem,dem_run=dem_run,qrun=qrun,lrun=lrun,logte=logte,response=response,dem_tr_run=dem_tr_run,q0=q0,l0=l0,info=info
     if n_elements(response_path) eq 0 then begin
      dirpath=file_dirname((ROUTINE_INFO('aia',/source)).path,/mark)
      response_path=dirpath+'aia_response.sav'
     end
     dr_idx=0
     t0_idx=1
     n0_idx=2
     q_idx=3
     l_idx=4
     v_idx=5
     nhi_idx=6
     trf_idx=7
 if arg_present(info) then begin
     if n_elements(info) eq 0 then begin
       Parms=Replicate({Name:'unused',Value:0d,Unit:'',Hint:''},8)
       Parms[dr_idx].Name='dR'          & Parms[dr_idx].Value=0.600E+09     & Parms[dr_idx].Unit='cm'       & Parms[dr_idx].Hint='Source/voxel Depth'
       Parms[t0_idx].Name='T_0'         & Parms[t0_idx].Value=0.200E+08     & Parms[t0_idx].Unit='K'        & Parms[t0_idx].Hint='Plasma Temperature'
       Parms[n0_idx].Name='n_0'         & Parms[n0_idx].Value=0.500E+10    & Parms[n0_idx].Unit='cm^{-3}' & Parms[n0_idx].Hint='Thermal e density'
       Parms[q_idx].Name='Q'           & Parms[q_idx].Value=0            & Parms[q_idx].Unit=''        & Parms[q_idx].Hint='Heating rate'
       Parms[l_idx].Name='Length'      & Parms[l_idx].Value=0            & Parms[l_idx].Unit='cm'      & Parms[l_idx].Hint='Half length of the associated fieldline'
       Parms[v_idx].Name='VoxelID'      & Parms[v_idx].Value=0            & Parms[v_idx].Unit='0/1/2'        & Parms[v_idx].Hint='chromo/TR/corona'
       Parms[nhi_idx].Name='n_hi'       & Parms[nhi_idx].Value=0            & Parms[nhi_idx].Unit='cm^{-3}'        & Parms[nhi_idx].Hint='Neutral Hydrogen density'
       Parms[trf_idx].Name='TRfactor'     & Parms[trf_idx].Value=0            & Parms[trf_idx].Unit=''        & Parms[trf_idx].Hint='TR factor'
       nparms=[{name:'N_pix',value:0l,unit:'(int)',user:0,hint:'Number of pixels'},$
               {name:'N_vox',value:0l,unit:'(int)',user:0,hint:'Number of voxels'},$
               {name:'N_chan',value:0l,unit:'(int)',user:0,hint:'Number of channels'},$
               {name:'UseDEM',value:1l,unit:'(int)',user:0,hint:'Use DEM'},$
               {name:'AddTR',value:1l,unit:'(int)',user:0,hint:'Add TR Contribution'},$
               {name:'ApplyTRfactor',value:1l,unit:'(int)',user:0,hint:'Apply TR Factor'},$
               {name:'DEMavg',value:0l,unit:'(int)',user:0,hint:'DEM Interpolation Method'},$
               {name:'EVEnorm',value:1l,unit:'(int)',user:1,hint:'Perform EVE normalization'},$
               {name:'CHIANTIfix',value:0l,unit:'(int)',user:1,hint:'Apply CHIANTI correction'}]
       restore,response_path
       rparms=[{name:'dS',value:0d,unit:'(cm^2)',user:0,hint:'Source pixel/area'},$
               {name:'AIA_response_date',value:gx_utcstr2time(response.date,/seconds),unit:'(UTsec)',user:0,hint:gx_utcstr2time(response.date)},$
               {name:'n_hi0',value:1d,unit:'cm^{-3}',user:1,Hint:'Neutral H density coronal cutoff'},$
               {name:'relative_abundance',value:1d,unit:'',user:1,Hint:'Relative to coronal abundance for Chianti'},$
               {name:'rsun',value:960d,unit:'arcseconds',user:1,Hint:"Observer's solar radius"}]

    endif else begin
      parms=info.parms
      nparms=info.nparms
      rparms=info.rparms
    endelse
     update_response=(n_elements(response) eq 0)?1:(gx_utcstr2time(response.date,/seconds) ne rparms[1].value)
     if update_response ne 0 then response=aia_get_response(/temp,/dn,/evenorm,timedepend_date=atime(rparms[1].value),/silent)
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
     restore,dirpath+'aia_rgb.sav'
     skip_rgb:
     info={parms:parms,$
           nparms:nparms,$
           rparms:rparms,$
           pixdim:[nchan],$
           spectrum:{x:{axis:w,label:'Wavelength',unit:'A'},$
                     y:{label:'I',unit:'counts/s/pix'}},rgb:rgb} 
     return
 end
   useDEM=nparms[3]
   AddTR=nparms[4]
   ApplyTRfactor=nparms[5]
   avgdem=nparms[6]
   evenorm=nparms[7]
   chiantifix=nparms[8]
   norm_tr=rparms[3]*rparms[0]/((4.5e7)^2)
   n_hi0=rparms[2]
   r_sun=rparms[4]
   sz=size(rowdata,/dim)
   Npix=sz[0]
   Nchan=sz[1]
   rowdata[*]=0
   update_response=(n_elements(response) eq 0)?1:(gx_utcstr2time(response.date,/seconds) ne rparms[1])
   if (n_elements(logte) eq 0) or (update_response eq 1) then begin
    response=aia_get_response(/temp,/dn,timedepend_date=atime(rparms[1]),evenorm=evenorm,chiantifix=chiantifix)
    logte=response.logte
   end  
   maxLogT=max(logte,min=minLogT)
   
   for pix=0, Npix-1 do begin
     rowparms=transpose(parms[pix,*,*])
     cutoff=max(where(rowparms[nhi_idx,*] ge n_hi0))
     if cutoff ge 0 then rowparms[nhi_idx,0:cutoff]=n_hi0
     tr_idx=max(where((ulong(rowparms[v_idx,*]) and gx_voxelid(/euv)) ne 0))
     point_in=where((rowparms[t0_idx,*] gt 0 and rowparms[v_idx,*] gt 1 and rowparms[nhi_idx,*] lt n_hi0), Nvox)
     if Nvox gt 0 then begin
        parmin=rowparms[*,point_in]
        norm=parmin[dr_idx,*]*norm_tr
       if useDEM eq 1 then begin
         gx_dem_interpolate,n,t,dem,path=path,logtdem=logtdem,dem_run=dem_run,qrun=qrun,lrun=lrun,qarr=parmin[q_idx,*],larr=parmin[l_idx,*],avgdem=avgdem
         tr_factor=1
         if AddTR eq 1 then begin
           tr_idx=max(where((ulong(parmin[v_idx,*]) and gx_voxelid(/euv)) ne 0))
           if tr_idx ge 0 then begin
           point_in=where((parmin[t0_idx,*] gt 0 and parmin[nhi_idx,*] lt n_hi0))
           gx_dem_interpolate,n_tr,t_tr,dem_tr,path=path,logtdem=logtdem,dem_run=dem_tr_run,lrun=lrun,qrun=qrun,$
             larr=parmin[l_idx,tr_idx],qarr=parmin[q_idx,tr_idx],/tr,avgdem=avgdem
             tr_factor=ApplyTRfactor gt 0?parmin[trf_idx,tr_idx]:1
             tr_add=(n_tr[0] gt 0 and t_tr[0] gt 0)
           endif else tr_add=0
         endif else tr_add=0
         dlogt = logtdem(1) - logtdem(0)
         for chan=0, nchan-1 do begin
           noDEMvox=where((n eq 0 or t eq 0),nnoDemvox, comp=DEMvox,ncomp=nDemvox)
           if nnoDEMvox gt 0 then begin
             g = dspline(logte, response.all[*,chan], alog10(reform(parmin[t0_idx,noDEMvox]))<maxLogT>minLogT)
             rowdata[pix,chan]= rowdata[pix,chan]+total(norm*reform(parmin[n0_idx,noDEMvox])^2*g,/double)
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
            g = dspline(logte, response.all[*,chan], alog10(reform(parmin[t0_idx,*]))<maxLogT>minLogT)
            rowdata[pix,chan] = total(norm*reform(parmin[n0_idx,*])^2*g,/double)
        end
       end
     end
   endfor 
   rowdata*=(R_sun/960.)^2 
end