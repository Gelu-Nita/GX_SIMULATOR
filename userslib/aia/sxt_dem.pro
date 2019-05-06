pro sxt_dem,parms,rowdata,path=path,logtdem=logtdem,dem_run=dem_run,brun=brun,lrun=lrun,logte=logte,response=response,dem_tr_run=dem_tr_run,info=info
     if n_elements(response_path) eq 0 then begin
      dirpath=file_dirname((ROUTINE_INFO('sxt_dem',/source)).path,/mark)
      response_path=dirpath+'sxt_response.sav'
     end
 if arg_present(info) then begin
     if n_elements(info) eq 0 then begin
       Parms=Replicate({Name:'unused',Value:0d,Unit:'',Hint:''},29)
       Parms[0].Name='dS'           & Parms[0].Value=0.180E+19     & Parms[0].Unit='cm^2'     & Parms[0].Hint='Source/pixel Area
       Parms[1].Name='dR'           & Parms[1].Value=0.600E+09     & Parms[1].Unit='cm'       & Parms[1].Hint='Source/voxel Depth
       Parms[2].Name='T_0'          & Parms[2].Value=0.200E+08     & Parms[2].Unit='K'        & Parms[2].Hint='Plasma Temperature
       Parms[3].Name='eps'          & Parms[3].Value=0.500E-01     & Parms[3].Unit='none'     & Parms[3].Hint='Matching parm. for TNT distr-ns
       Parms[4].Name='kappa'        & Parms[4].Value=4.00          & Parms[4].Unit='none'     & Parms[4].Hint='Index of Kappa-distr-n
       Parms[5].Name='N'            & Parms[5].Value=16            & Parms[5].Unit='none'     & Parms[5].Hint='Number of integration nodes
       Parms[6].Name='Emin'         & Parms[6].Value=0.100         & Parms[6].Unit='MeV'      & Parms[6].Hint='Low energy cutoff
       Parms[7].Name='Emax'         & Parms[7].Value=10.0          & Parms[7].Unit='MeV'      & Parms[7].Hint='High energy cutoff
       Parms[8].Name='E_break'      & Parms[8].Value=1.00          & Parms[8].Unit='MeV'      & Parms[8].Hint='Break E for DPL
       Parms[9].Name='delta1'       & Parms[9].Value=4.00          & Parms[9].Unit='none'     & Parms[9].Hint='(LE) Power-Law index
       Parms[10].Name='delta2'      & Parms[10].Value=6.00         & Parms[10].Unit='none'    & Parms[10].Hint='(HE) Power-Law index for DPL
       Parms[11].Name='n_0'         & Parms[11].Value=0.500E+10    & Parms[11].Unit='cm^{-3}' & Parms[11].Hint='Thermal e density
       Parms[12].Name='n_b'         & Parms[12].Value=0.300E+08    & Parms[12].Unit='cm^{-3}' & Parms[12].Hint='Nonthermal e density
       Parms[13].Name='B'           & Parms[13].Value=200.         & Parms[13].Unit='G'       & Parms[13].Hint='Magnetic field
       Parms[14].Name='theta'       & Parms[14].Value=35.0         & Parms[14].Unit='degrees' & Parms[14].Hint='Viewing angle
       Parms[15].Name='Eph_min'     & Parms[15].Value=1            & Parms[15].Unit='keV'     & Parms[15].Hint='Starting photon energy
       Parms[16].Name='dEph'        & Parms[16].Value=0.02         & Parms[16].Unit='Log(keV)'& Parms[16].Hint='Logarithmic step in photon energy
       Parms[17].Name='Dist_E'      & Parms[17].Value=3            & Parms[17].Unit='none'    & Parms[17].Hint='Type of distribution over energy
       Parms[18].Name='N_E'         & Parms[18].Value=101          & Parms[18].Unit='none'    & Parms[18].Hint='Number of energies'
       Parms[19].Name='Dist_Ang'    & Parms[19].Value=1            & Parms[19].Unit='none'    & Parms[19].Hint='Type of angular distribution
       Parms[20].Name='theta_C'     & Parms[20].Value=60.0         & Parms[20].Unit='degrees' & Parms[20].Hint='Loss-cone boundary
       Parms[21].Name='theta_b'     & Parms[21].Value=90.0         & Parms[21].Unit='degrees' & Parms[21].Hint='Angle of beam direction
       Parms[22].Name='dMu'         & Parms[22].Value=0.100        & Parms[22].Unit='none'    & Parms[22].Hint='dMu for gau/exp/SuGau loss-cone
       Parms[23].Name='a_4'         & Parms[23].Value=10.0         & Parms[23].Unit='none'    & Parms[23].Hint='Coeff for a*(Mu-xMu0)^4 for SuGau
       Parms[24].Name='Bmed'        & Parms[24].Value=0            & Parms[24].Unit='G'       & Parms[24].Hint='Averaged magnetic field along the associated fieldline'                        
       Parms[25].Name='Length'      & Parms[25].Value=0            & Parms[25].Unit='cm'      & Parms[25].Hint='Half length of the associated fieldline'
       Parms[26].Name='UseDEM'      & Parms[26].Value=0            & Parms[26].Unit='0/1'        & Parms[26].Hint='Use DEM'
       Parms[27].Name='TR    '      & Parms[27].Value=0            & Parms[27].Unit='0/1'        & Parms[27].Hint='Voxel above transition region?'
       Parms[28].Name='UseTR'       & Parms[28].Value=0            & Parms[28].Unit='0/1'        & Parms[28].Hint='Add Transition Region Contribution'
     endif else parms=info.parms
     restore,response_path
     nchan=n_elements(response.channels)
     w=fltarr(nchan)
     for i=0,nchan-1 do w[i]=fix(strmid(response.channels[i],3))
     rgb=ptr_new()
     catch, error_stat
     if error_stat ne 0 then begin
       catch, /cancel
       MESSAGE, /INFO, !ERROR_STATE.MSG
       goto,skip_rgb
     end
     ;restore,dirpath+'AIA_RGB.sav'
     skip_rgb:
     info={parms:parms,$
           pixdim:[nchan],$
           spectrum:{x:{axis:w,label:'Wavelength',unit:'A'},$
                    y:{label:'I',unit:'counts/s/pix'}}};,rgb:rgb} 
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
     point_in=where((rparms[2,*] gt 0) and (rparms[27,*] gt 0), Nvox)
     if Nvox gt 0 then begin
        parmin=rparms[*,point_in]
        norm=parmin[0,0]*parmin[1,0]/((4.5e7)^2)
        tr_idx=(min(where(parmin[27,*] eq 1))>0)
        norm_tr=parmin[0,0]/((4.5e7)^2)
        ;Parms[27].Name='Above TR voxels'
        ;Parms[28].Name='Add Transition Region Contribution'
        ;Parms[24].Name='Bmed'                         
        ;Parms[25].Name='Length'
        ;Parms[26].Name='useDEM'
       if parmin[26,0] eq 1 then begin
         dem_interpolate,n,t,dem,path=path,logtdem=logtdem,dem_run=dem_run,brun=brun,lrun=lrun,barr=parmin[24,*],larr=parmin[25,*]
         if parmin[27,0] eq 1 then dem_interpolate,n_tr,t_tr,dem_tr,path=path,logtdem=logtdem,dem_run=dem_tr_run,brun=brun,lrun=lrun,barr=parmin[24,tr_idx],larr=parmin[25,tr_idx],/tr
         dlogt = logtdem(1) - logtdem(0)
         for chan=0, nchan-1 do begin
           noDEMvox=where(n eq 0 or t eq 0,nnoDemvox, comp=DEMvox,ncomp=nDemvox)
           if nnoDEMvox gt 0 then begin
             g = dspline(logte, response[*,chan], alog10(reform(parmin[2,noDEMvox]))<maxLogT>minLogT)
             rowdata[pix,chan]= rowdata[pix,chan]+norm*total(reform(parmin[11,noDEMvox])^2*g,/double)
           end
           if nDEMvox gt 0 then begin
             g = dspline(logte, response[*,chan], logtdem<maxLogT>minLogT)
             rowdata[pix,chan]= rowdata[pix,chan]+ norm*alog(10.)*dlogt*total((g*(10.^logtdem))#dem)
             if parmin[28,0] eq 1 then begin
              if n_tr ne 0 and t_tr ne 0 then begin
               rowdata[pix,chan]= rowdata[pix,chan]+ norm_tr*alog(10.)*dlogt*total((g*(10.^logtdem))*dem_tr)
              end
             end 
           end
         end
       endif else begin
        for chan=0, nchan-1 do begin
          g = dspline(logte, response[*,chan], alog10(reform(parmin[2,*]))<maxLogT>minLogT)
          rowdata[pix,chan] = norm*total(reform(parmin[11,*])^2*g,/double)
        end
       end
     end
   endfor  
   wait,0.01
end