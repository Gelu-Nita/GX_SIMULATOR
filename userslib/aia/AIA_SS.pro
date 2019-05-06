pro aia_ss,parms,rowdata,path,logte=logte,response=response,info=info
;Steady State Version
 if n_elements(path) eq 0 then begin
  dirpath=file_dirname((ROUTINE_INFO('aia_ss',/source)).path,/mark)
  path=dirpath+'AIA_Response.sav'
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
       Parms[24].Name='Unused'      & Parms[24].Value=0            & Parms[24].Unit='none'    & Parms[24].Hint=''                        
       Parms[25].Name='Unused'      & Parms[25].Value=12           & Parms[25].Unit='f_ce'    & Parms[25].Hint=''
       Parms[26].Name='Unused'      & Parms[26].Value=12           & Parms[26].Unit='f_ce'    & Parms[26].Hint=''
       Parms[27].Name='Unused'      & Parms[27].Value=1            & Parms[27].Unit='1/0'     & Parms[27].Hint=''
       Parms[28].Name='Unused'      & Parms[28].Value=2            & Parms[28].Unit='2/1/0'   & Parms[28].Hint=''
     endif else parms=info.parms
     restore,path
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
    restore,path
    logte=response.logte
    response=response.all
   end  
   maxLogT=max(logte,min=minLogT)
   
   for pix=0, Npix-1 do begin
     rparms=transpose(parms[pix,*,*])
     point_in=where(rparms[2,*] gt 0, Nvox)
     if Nvox gt 0 then begin
        parmin=rparms[*,point_in]
        norm=parmin[0,0]*parmin[1,0]/((4.5e7)^2)
       for chan=0, nchan-1 do begin
        g = dspline(logte, response[*,chan], alog10(reform(parmin[2,*]))<maxLogT>minLogT)
        rowdata[pix,chan] = norm*total(reform(parmin[11,*])^2*g,/double)
       end
     end
   endfor  
end