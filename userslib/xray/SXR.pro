pro SXR,parms,rowdata,info=info
 if arg_present(info) then begin
     if n_elements(info) eq 0 then begin
       Parms=Replicate({Name:'unused',Value:0d,Unit:'',Hint:''},19)
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
     endif else parms=info.parms
     E1=Parms[15].value  
     dE=Parms[16].value
     E=10^(Alog10(E1)+findgen(Parms[18].value)*dE)
     dataout=fltarr(parms[18].value)
     dirpath=file_dirname((ROUTINE_INFO('SXR',/source)).path,/mark)
     info={parms:parms,$
           pixdim:[parms[18].value],$
           spectrum:{x:{axis:E,label:'Energy',unit:'keV'},$
                    y:{label:'Flux',unit:'keV/(s cm^2 keV)'}}} 
     return
 end
   sz=size(rowdata,/dim)
   Npix=sz[0]
   rowdata[*]=0
   for pix=0, Npix-1 do begin
     rparms=transpose(parms[pix,*,*])
     point_in=where(rparms[2,*] gt 0, Nvox)
     if Nvox gt 0 then begin
     parmin=rparms[*,point_in]
     kb=8.617343e-8 ;(keV K-1)
     norm=(8.1e-39)*parmin[0,0]*parmin[1,0]
     E1=parmin[15,0] 
     dE=parmin[16,0]
     E=10^(Alog10(E1)+findgen(parmin[18,0])*dE)
     for i=0, n_elements(E)-1 do rowdata[pix,i]=total(exp(-E[i]/kb/reform(parmin[2,*]))*reform(parmin[11,*]^2)/reform(sqrt(parmin[2,*])))*norm
     print,'***************'
     print,minmax(e),minmax(rowdata[pix,*]),minmax(parmin[2,*]),minmax(parmin[11,*])
     end
   endfor  
end