; first written in 2017
; Eduard@Glasgow 27 July 2017 - accounts for Thick-Target emission in the chromosphere.
; The routine calculates TT HXR spectrum if the particles are in the chromosphere
; Calculations follow the TT formula from 
; http://adsabs.harvard.edu/abs/2003ApJ...595L.115B
; TT calculations are only for specific voxels
;Parms[19].Name='VoxelID' & Parms[19].Value=0 & Parms[19].Unit='0/1/2' & Parms[19].Hint='chromo/TR/corona'
;
;gnita@njit 07-Dec-2017 change explicit TR index from 2L to gx_voxelid(/tr) to allow future redefinition if needed
;modified by Gelu@NJIT 24 Dec 2022  - added relative_abundances user input

pro xray_tt,parms,rowdata,rparms,xray_cs=xray_cs,info=info
 if arg_present(info) then begin
     if n_elements(info) eq 0 then begin
       Parms=Replicate({Name:'unused',Value:0d,Unit:'',Hint:''},20)
       Parms[0].Name='dS'           & Parms[0].Value=0.180E+19    & Parms[0].Unit='cm^2'    & Parms[0].Hint='Source/pixel Area'
       Parms[1].Name='dR'           & Parms[1].Value=0.600E+09    & Parms[1].Unit='cm'      & Parms[1].Hint='Source/voxel Depth'
       Parms[2].Name='T_0'          & Parms[2].Value=0.200E+08    & Parms[2].Unit='K'       & Parms[2].Hint='Plasma Temperature'
       Parms[3].Name='eps'          & Parms[3].Value=0.500E-01    & Parms[3].Unit='none'    & Parms[3].Hint='Matching parm. for TNT distr-ns'
       Parms[4].Name='kappa'        & Parms[4].Value=4.00         & Parms[4].Unit='none'    & Parms[4].Hint='Index of Kappa-distr-n'
       Parms[5].Name='N'            & Parms[5].Value=16           & Parms[5].Unit='none'    & Parms[5].Hint='Number of integration nodes'
       Parms[6].Name='Emin'         & Parms[6].Value=0.100        & Parms[6].Unit='MeV'     & Parms[6].Hint='Low energy cutoff'
       Parms[7].Name='Emax'         & Parms[7].Value=10.0         & Parms[7].Unit='MeV'     & Parms[7].Hint='High energy cutoff'
       Parms[8].Name='E_break'      & Parms[8].Value=1.00         & Parms[8].Unit='MeV'     & Parms[8].Hint='Break E for DPL'
       Parms[9].Name='delta1'       & Parms[9].Value=4.00         & Parms[9].Unit='none'    & Parms[9].Hint='(LE) Power-Law index'
       Parms[10].Name='delta2'      & Parms[10].Value=6.00         & Parms[10].Unit='none'    & Parms[10].Hint='(HE) Power-Law index for DPL'
       Parms[11].Name='n_0'         & Parms[11].Value=0.500E+10    & Parms[11].Unit='cm^{-3}' & Parms[11].Hint='Thermal e density'
       Parms[12].Name='n_b'         & Parms[12].Value=0.300E+08    & Parms[12].Unit='cm^{-3}' & Parms[12].Hint='Nonthermal e density'
       Parms[13].Name='B'           & Parms[13].Value=200.         & Parms[13].Unit='G'       & Parms[13].Hint='Magnetic field'
       Parms[14].Name='theta'       & Parms[14].Value=35.0         & Parms[14].Unit='degrees' & Parms[14].Hint='Viewing angle'
       Parms[15].Name='Eph_min'     & Parms[15].Value=3            & Parms[15].Unit='keV'     & Parms[15].Hint='Starting photon energy'
       Parms[16].Name='dEph'        & Parms[16].Value=0.02         & Parms[16].Unit='Log(keV)' & Parms[16].Hint='Logarithmic step in photon energy'
       Parms[17].Name='Dist_E'      & Parms[17].Value=3            & Parms[17].Unit='none'    & Parms[17].Hint='Type of distribution over energy'
       Parms[18].Name='N_E'         & Parms[18].Value=101          & Parms[18].Unit='none'    & Parms[18].Hint='Number of energy channels'
       Parms[19].Name='VoxelID'    & Parms[19].Value=0            & Parms[19].Unit='1/2/4' & Parms[19].Hint='chromo/TR/corona'
       ; corrected by Eduard@glasgow after converstation with Gelu Nita about Parms.unit 
        rparms=[{name:'relative_abundance',value:1d,unit:'',user:1.0,hint:'Relative to coronal abundance for Chianti'}]
     endif else begin
      parms=info.parms
      rparms=info.rparms
     endelse
     E1=Parms[15].value  
     dEph=Parms[16].value
     Eph=10^(Alog10(E1)+findgen(Parms[18].value)*dEph)
     ; output photon energy array
     logdE=Parms[16].value
     EE =10^(Alog10(E1)+findgen(Parms[18].value+round(1./logDe))*logdE)
     DE =10^(Alog10(E1)+(findgen(Parms[18].value+round(1./logDe))+1)*logdE)-EE
     EE=EE + min(dEph)*0.5
     ;electron energies so that max(ee) =10 x max(eph)
     info={parms:parms,$
           rparms:rparms,$
           pixdim:[parms[18].value],$
           spectrum:{x:{axis:Eph,label:'Energy',unit:'keV'},$
                    y:{label:'Flux',unit:'1/(s cm^2 keV)'}}}                          
    return
 end
   sz=size(rowdata,/dim)
   nrows=sz[0]
   rowdata[*]=0
   for r=0, nrows-1 do begin
   tparms=transpose(parms[r,*,*])
   point_in=where(tparms[2,*] gt 0, Nvox);added
   if Nvox gt 0 then begin
   parmin=tparms[*,point_in];added
 
   E1=parmin[15,0] 
   logdE=parmin[16,0]
   Eph =10^(Alog10(E1)+findgen(parmin[18,0])*logdE)
   DEph= 10^(Alog10(E1)+(findgen(parmin[18,0])+1)*logdE)-eph
   ; output photon energy array
   eph_2n=transpose([[Eph],[Eph+deph]])
   ; 2xn photon array
   
   EE =10^(Alog10(E1)+findgen(parmin[18,0]+round(1./logDe))*logdE)
   DE =10^(Alog10(E1)+(findgen(parmin[18,0]+round(1./logDe))+1)*logdE)-EE
   Nee=N_elements(EE)
   EE=EE + min(dEph)*0.5
   ;electron energies so that max(ee) =10 x max(eph)
   
    
   if n_elements(xray_cs) eq 0 then xray_cross_section, ee, Eph, Xray_CS
  
   
   Ve= sqrt(2.*EE*1.6e-9/9.8d-28) 
   e_dataout  =fltarr(N_elements(ee))
   e_data_tt  =fltarr(N_elements(ee))
   eph_dataout=fltarr(N_elements(eph))
   
   Te_thr=0.09 ; keV
   ; lowest temperature that can be calculated for thermal plasma SXR emission
   abun =rparms[0]
   ; fractional element abunadences with respect to coronal
   
   ; constant KK is defined for Coloumb log =20, 
   ; kk= 2\pi e^4*20 keV^2cm^4
   KK =2.6d-18
   ; see for details:
   ; http://adsabs.harvard.edu/abs/2011SSRv..159..301K
 
   
   ; normalisation 
   ;****************************************************************
   A_vox=parmin[0,*]*1d0
   ;voxel area
   V_vox=parmin[0,*]*parmin[1,*]*1d0
   ; Voxel volume in cm^3
   Np_vox=parmin[11,*]*1d0
   ; plasma number density in the voxel
   AU2_4pi=(4.*!PI*1.496d13^2)
   ;4pi*1AU^2 
   norm=V_vox*Np_vox/AU2_4pi
   ; normalisation factor
   EM49=reform(V_vox*Np_vox^2*1d-49)
   npV =V_vox*Np_vox
   ;
  ;main loop over all voxels  
   FOR i=0,Nvox-1 DO BEGIN
  
  ;**********************************************************
   ; If electron distribution is a power-law
   IF (ROUND(Parmin[17,i]) EQ 3 OR ROUND(Parmin[17,i]) EQ 11 ) THEN BEGIN
   ; Electron distribution is a power-law   
   E_dist =((ee GE Parmin[6,i]*1000.) AND (ee LT Parmin[7,i]*1000.))*EE ^(-Parmin[9,i])
   E_dist_norm=total(E_dist*de)
   
   IF (E_dist_norm GT 0) THEN BEGIN
    
    e_dataout=Parmin[12, i]*ve*E_dist*norm[i]/e_dist_norm
    
    ; the block below calculates TT emission
    IF ((ulong(Parmin[19,i]) and 2l) NE 2) THEN     eph_dataout+=xray_cs#(e_dataout*DE)
      ; calculates coronal X-ray emission using thin target
      
    ;IF (((ulong(Parmin[19,i]) and 2l) EQ 2) and (Parmin[19,i] gt 8e2)) THEN BEGIN
    IF ((ulong(Parmin[19,i]) and 2l) EQ 2) THEN BEGIN
      ;IF ((Parms[19,i] and 2) EQ 2) THEN BEGIN
      ; checks if it is the transition region
    ; calculates chromospheric X-ray emission using thick target  
    FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox[i]/KK/npV[i]
    eph_dataout+=xray_cs#(e_data_tt*DE)
    ENDIF
    ; ******************* TT emission ************************************
    
   ENDIF
   
   ENDIF    
   ; end of power-law case     
   ;**********************************************************
   
   ;**********************************************************
   ; Electron  distribution is double power-law
   IF (ROUND(Parmin[17,i]) EQ 4 OR ROUND(Parmin[17,i]) EQ 12) THEN BEGIN
   E_dist=((ee GE Parmin[6,i]*1e3) AND (ee LT Parmin[8,i]*1e3))*(EE/(Parmin[8,i]*1e3))^(-Parmin[9,i])+$
   ((ee GE Parmin[8,i]*1e3) AND (ee LT Parmin[7,i]*1e3))*(EE/(Parmin[8,i]*1e3))^(-Parmin[10,i])
   E_dist_norm=total(E_dist*de)
   IF (E_dist_norm GT 0) THEN BEGIN

    e_dataout=Parmin[12, i]*ve*E_dist*norm[i]/E_dist_norm
    
    ; the block below calculates TT emission
    IF ((ulong(Parmin[19,i]) and 2l) NE 2) THEN     eph_dataout+=xray_cs#(e_dataout*DE)
    ; calculates coronal X-ray emission using thin target

    ;IF (((ulong(Parmin[19,i]) and 2l) EQ 2) and (Parmin[19,i] gt 8e2)) THEN BEGIN
    IF ((ulong(Parmin[19,i]) and 2l) EQ 2) THEN BEGIN
      ;IF ((Parms[19,i] and 2) EQ 2) THEN BEGIN
      ; checks if it is the transition region
      ; calculates chromospheric X-ray emission using thick target
      FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox[i]/KK/npV[i]
      eph_dataout+=xray_cs#(e_data_tt*DE)
    ENDIF
    ; ******************* TT emission ************************************

   ENDIF
   
   ENDIF
   ;**********************************************************
   
   
   ;**********************************************************
   ; Electron  distribution is thermal/non-thermal over energy
   IF (ROUND(Parmin[17,i]) EQ 5) THEN BEGIN
   kb=8.617343e-8 ;(keV K-1)
   Te=parmin[2,i]*kb
   E_cr=Te/parmin[3,i]
   ; critial energy in keV
   E_dist_cr=sqrt(E_cr/(!PI*Te))*exp(-E_cr/Te)
   
    dens_e=Parmin[11, i]*(parmin[2,i] GT 2e5)+0.* (parmin[2,i] LE 2e5)
   ; density if ionised Te > 2e5K
   
   E_dist =((ee GE Parmin[6,i]*1000.) AND (ee LT E_cr))*sqrt(EE/(!PI*Te))*exp(-EE/Te)
   E_dist =((ee GE E_cr) AND (ee LT Parmin[7,i]*1000.))*E_dist_cr*(EE/E_cr) ^(-Parmin[9,i])
   E_dist_norm=total(E_dist*de)
   IF (E_dist_norm GT 0) THEN BEGIN
    e_dataout=dens_e*ve*E_dist*norm[i]/E_dist_norm
    ;eph_dataout+=xray_cs#(e_dataout*DE)
    ;EM49=E_dist_norm*1d-49
    ;f_vth Valid range is 1.01 - 998.0 MegaKelvin or 0.0870317 - 85.9977 keV
    IF (Te GT Te_thr) THEN eph_dataout+=f_vth(eph_2n, [EM49[i],Te,abun])
    IF (Te LE Te_thr) THEN eph_dataout+=xray_cs#(e_dataout*DE)
 
    ;f_vth(e, [EM49,Te,1.])
    ; EM49 is Emission measure
    ; Te temperature
    ; 1 is the element abundances (enhancement factor)
    ; 
   ENDIF
   
   
   ENDIF
   ;**********************************************************
   
   
   ;**********************************************************
   ; Electron  distribution is kappa distribution
   IF (ROUND(Parmin[17,i]) EQ 6) THEN BEGIN
   kb=8.617343e-8 ;(keV K-1)
   Te=parmin[2,i]*kb
   Thet=Te/511.
   ; critial energy in keV
   gam=1.d0+EE/511.
   
   dens_e=Parmin[11, i]*(parmin[2,i] GT 2e5)+0.* (parmin[2,i] LE 2e5)
   ; density if ionised Te > 2e5K
   
   E_dist =((ee GE Parmin[6,i]*1000.) AND (ee LT Parmin[7,i]*1000.))*$
   gam*sqrt(gam*gam-1.)/(1.+(gam-1.)/(Parmin[4,i]-1.5)/Thet)^(Parmin[4,i]+1)/thet^(1.5)
   E_dist_norm=total(E_dist*de)
   IF (E_dist_norm GT 0) THEN BEGIN
    e_dataout=dens_e*ve*E_dist*norm[i]/E_dist_norm

    ; the block below calculates TT emission
    IF ((ulong(Parmin[19,i]) and gx_voxelid(/tr)) NE gx_voxelid(/tr)) THEN     eph_dataout+=xray_cs#(e_dataout*DE)
    ; calculates coronal X-ray emission using thin target

    ;IF (((ulong(Parmin[19,i]) and 2l) EQ 2) and (Parmin[19,i] gt 8e2)) THEN BEGIN
    IF ((ulong(Parmin[19,i]) and gx_voxelid(/tr)) EQ gx_voxelid(/tr)) THEN BEGIN
      ;IF ((Parms[19,i] and 2) EQ 2) THEN BEGIN
      ; checks if it is the transition region
      ; calculates chromospheric X-ray emission using thick target
      FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox[i]/KK/npV[i]
      eph_dataout+=xray_cs#(e_data_tt*DE)
    ENDIF
    ; ******************* TT emission ************************************


   ENDIF
  
   ENDIF
   ;**********************************************************
   
   ;**********************************************************
   ; If electron distribution is a power-law in momentum p*c
   IF (ROUND(Parmin[17,i]) EQ 7 ) THEN BEGIN
   ; Electron distribution is a power-law 
   
   pc=sqrt((ee+511.)^2-511.^2)
   d_pc=(ee+511.)*de/pc
   pc_min=sqrt((Parmin[6,i]*1000.+511.)^2-511.^2)
   pc_max=sqrt((Parmin[7,i]*1000.+511.)^2-511.^2)
     
   E_dist =((pc GE pc_min) AND (pc LT pc_max))*pc^(-Parmin[9,i])
   E_dist_norm=total(E_dist*d_pc)
   IF (E_dist_norm GT 0) THEN BEGIN
    e_dataout=Parmin[12, i]*ve*E_dist*norm[i]/e_dist_norm

    ; the block below calculates TT emission
    IF ((ulong(Parmin[19,i]) and gx_voxelid(/tr)) NE gx_voxelid(/tr)) THEN     eph_dataout+=xray_cs#(e_dataout*DE)
    ; calculates coronal X-ray emission using thin target

    ;IF (((ulong(Parmin[19,i]) and 2l) EQ 2) and (Parmin[19,i] gt 8e2)) THEN BEGIN
    IF ((ulong(Parmin[19,i]) and gx_voxelid(/tr)) EQ gx_voxelid(/tr)) THEN BEGIN
      ;IF ((Parms[19,i] and 2) EQ 2) THEN BEGIN
      ; checks if it is the transition region
      ; calculates chromospheric X-ray emission using thick target
      FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox[i]/KK/npV[i]
      eph_dataout+=xray_cs#(e_data_tt*DE)
    ENDIF
    ; ******************* TT emission ************************************

   ENDIF
   ENDIF    
   ; end of power-law case     
   ;**********************************************************
   ; If electron distribution is a power-law in momentum p*c
   IF (ROUND(Parmin[17,i]) EQ 7 ) THEN BEGIN
   ; Electron distribution is a power-law 
   
   pc=sqrt((ee+511.)^2-511.^2)
   d_pc=(ee+511.)*de/pc
   pc_min=sqrt((Parmin[6,i]*1000.+511.)^2-511.^2)
   pc_max=sqrt((Parmin[7,i]*1000.+511.)^2-511.^2)
     
   E_dist =((pc GE pc_min) AND (pc LT pc_max))*pc^(-Parmin[9,i])
   E_dist_norm=total(E_dist*d_pc)
   IF (E_dist_norm GT 0) THEN BEGIN
    e_dataout=Parmin[12, i]*ve*E_dist*norm[i]/e_dist_norm

    ; the block below calculates TT emission
    IF ((ulong(Parmin[19,i]) and gx_voxelid(/tr)) NE gx_voxelid(/tr)) THEN     eph_dataout+=xray_cs#(e_dataout*DE)
    ; calculates coronal X-ray emission using thin target

    ;IF (((ulong(Parmin[19,i]) and 2l) EQ 2) and (Parmin[19,i] gt 8e2)) THEN BEGIN
    IF ((ulong(Parmin[19,i]) and gx_voxelid(/tr)) EQ gx_voxelid(/tr)) THEN BEGIN
      ;IF ((Parms[19,i] and 2) EQ 2) THEN BEGIN
      ; checks if it is the transition region
      ; calculates chromospheric X-ray emission using thick target
      FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox[i]/KK/npV[i]
      eph_dataout+=xray_cs#(e_data_tt*DE)
    ENDIF
    ; ******************* TT emission ************************************

   ENDIF
   ENDIF    
   ; end of power-law case     
   ;**********************************************************
   
   
   ;**********************************************************
   ; Electron background distribution is thermal - should be always added
  ; IF (ROUND(Parmin[17,i]) EQ 1) THEN BEGIN
   ; Electron distribution is thermal
   kb=8.617343e-8 ;(keV K-1)
   Te=parmin[2,i]*kb
   ;local temperature in keV   
   ;e_dataout+=norm[i]*ve*parmin[11,i]*sqrt(EE/(!PI*Te))*exp(-EE/Te)
   E_dist=norm[i]*ve*parmin[11,i]*sqrt(EE/(!PI*Te))*exp(-EE/Te)
   ; changed to photon calculation
   E_dist_norm=total(E_dist*dE)
   ;EM49=E_dist_norm*1d-49
   
   ;f_vth Valid range is 1.01 - 998.0 MegaKelvin or 0.0870317 - 85.9977 keV
   
   IF (Te GT Te_thr) THEN eph_dataout+=f_vth(eph_2n, [EM49[i],Te,abun])
   IF (Te LE Te_thr) THEN eph_dataout+=xray_cs#(e_dist*DE)

   ; electron flux for 3D maxwellian
  ; ENDIF
   ;**********************************************************
   ; background thermal electron distribution
ENDFOR
   ;ends FOR loop over voxels   
   ;rowdata[r,*]=(xray_cs#(e_dataout*DE))
   rowdata[r,*]=eph_dataout
   ;turning mean LOS electron flux into photon flux
      
   end
end
;ends FOR over row   
END

