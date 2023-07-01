; first written in 2017
; Eduard@Glasgow 27 July 2017 - accounts for Thick-Target emission in the chromosphere.
; The routine calculates TT HXR spectrum if the particles are in the chromosphere
; Calculations follow the TT formula from
; http://adsabs.harvard.edu/abs/2003ApJ...595L.115B
; TT calculations are only for specific voxels
;Parms[voxelid_idx].Name='VoxelID' & Parms[voxelid_idx].Value=0 & Parms[voxelid_idx].Unit='0/1/2' & Parms[voxelid_idx].Hint='chromo/TR/corona'
; Modification history:
; 
; gnita@njit 07-Dec-2017 change explicit TR index from 2L to gx_voxelid(/tr) to allow future redefinition if needed
; Eduard@Glasgow & Gelu@njit 28-Feb-2018 added albedo component
; Eduard@Glasgow & Gelu@njit 19-June-2018 changed albedo matrix interpolation
; Gelu@njit 15-March-2023 added the option of using array defined parameters. 
; NOTE: THiS IS JUST A BETA VERSION, AWAITING VALIDATION FROM Eduard@Glasgow!
; Eduard@Glasgow 13-April-2023: corrected the errors in array units
; Eduard@Glasgow 26-may-2023 added scaling for non-AU observations; requires R_sun in arcseconds
; Gelu@njit 26-may-2023 added interface input for R_sun in arcseconds

pro xray_tt_albedo_arr,parms,rowdata,nparms,rparms,xray_cs=xray_cs,albedo=albedo,E_arr,mu_arr,f_arr,info=info
  if arg_present(info) then begin
    if n_elements(info) eq 0 then begin
      parms=[{Name:'dR',Value:0.60E+09,Unit:'cm',Hint:'Source/voxel Depth'},$
             {Name:'T_0',Value:0.20E+08,Unit:'K',Hint:'Plasma Temperature'},$
             {Name:'eps',Value:0.50E-01,Unit:'none',Hint:'Matching parm. for TNT distr-ns'},$
             {Name:'kappa',Value:4.00,Unit:'none',Hint:'Index of Kappa-distr-n'},$
             {Name:'Dist_E',Value:3.0,Unit:'none',Hint:'Type of distribution over energy'},$
             {Name:'Emin',Value:0.10,Unit:'MeV',Hint:'Low energy cutoff'},$
             {Name:'Emax',Value:10.00,Unit:'MeV',Hint:'High energy cutoff'},$
             {Name:'Ebreak',Value:1.00,Unit:'MeV',Hint:'Break E for DPL'},$
             {Name:'delta1',Value:4.00,Unit:'none',Hint:'(LE) Power-Law index'},$
             {Name:'delta2',Value:6.00,Unit:'none',Hint:'(HE) Power-Law index for DPL'},$
             {Name:'Dist_Ang',Value:1.0,Unit:'none',Hint:'Type of angular distribution'},$
             {Name:'theta_C',Value:60.0,Unit:'degrees',Hint:'Loss-cone boundary'},$
             {Name:'theta_b',Value:90.0,Unit:'degrees',Hint:'Angle of beam direction'},$
             {Name:'dMu',Value:0.1,Unit:'none',Hint:'dMu for gau/exp/SuGau loss-cone'},$
             {Name:'a_4',Value:10.0,Unit:'none',Hint:'Coeff for a*(Mu-xMu0)^4 for SuGau'},$
             {Name:'n_0',Value:0.500E+10,Unit:'cm^{-3}',Hint:'Thermal e density'},$
             {Name:'n_b',Value:0.300E+08,Unit:'cm^{-3}',Hint:'Nonthermal e density'},$
             {Name:'B',Value:200.0,Unit:'G',Hint:'Magnetic field'},$
             {Name:'theta',Value:35.0,Unit:'degrees',Hint:'Viewing angle'},$
             {Name:'Bz',Value:100.0,Unit:'G',Hint:'Signed vertical magnetic field component'},$
             {Name:'hc_angle',Value:45.0,Unit:'degrees',Hint:'Heliocentric angle (0-90 deg)'},$
             {Name:'VoxelID',Value:0.0,Unit:'1/2/4',Hint:'chromo/TR/corona'},$
             {Name:'arr_key_loc',Value:0.0,Unit:'1/2/4',Hint:'chromo/TR/corona'},$
             {Name:'SpineS',Value:0.0,unit:'',Hint:'Fluxtube spine longitudinal coordinate '},$
             {Name:'SpineR',Value:0.0,unit:'',Hint:'Fluxtube spine radial distance '},$
             {Name:'HasArr',Value:0.0,unit:'0/1',Hint:'Fluxtube has array defined distributions'},$
             {Name:'rsun',Value:960.0, unit:'arcseconds', hint:"Observer's solar radius"}]
      nparms=[{name:'N_pix',value:0l,unit:'(int)',user:0,hint:'Number of pixels'},$
              {name:'N_vox',value:0l,unit:'(int)',user:0,hint:'Number of voxels'},$ 
              {name:'N_Eph',value:101l,unit:'(int)',user:1,hint:'Number of energy channels'},$
              {name:'arr_key',value:0l,unit:'(-2/-1/0/1)',user:1,hint:'test/ana_off/arr_on/arr_off'},$
              {name:'N_E',value:0l,unit:'(int)',user:0,hint:'Number of array energies'},$
              {name:'N_mu',value:0l,unit:'(int)',user:0,hint:'Number of array pitch angles'}]
              
      rparms=[{name:'dS',value:1d,unit:'(cm^2)',user:0,hint:'Source/pixel Area'},$
              {name:'Eph_min',value:3d,unit:'keV',user:1,hint:'Starting photon energy'},$
              {name:'dEph',value:0.02d,unit:'Log(keV)',user:1,hint:'Logarithmic step in photon energy'},$
              {name:'a',value:1d,unit:'none',user:1,hint:'chromo anisotropy ratio'},$
              {name:'relative_abundance',value:1d,unit:'',user:1,hint:'relative to Chianti'}]
     endif else begin
      parms=info.parms
      nparms=info.nparms
      rparms=info.rparms
      E_arr=double(info.aparms.E_arr)
      mu_arr=double(info.aparms.mu_arr)
      f_arr=double(info.aparms.f_arr)
     endelse
     E1=rparms[1].value
     logdE=rparms[2].value
     N_Eph=nparms[2].value
     Eph=10^(Alog10(E1)+findgen(N_Eph)*logdE)
     default,E_arr,0d
     default,mu_arr,0d
     default,f_arr,0d
    info={parms:parms,$
      nparms:nparms,$
      rparms:rparms,$
      aparms:{e_arr:e_arr,mu_arr:mu_arr,f_arr:f_arr},$
      pixdim:[N_Eph,2],$
      spectrum:{x:{axis:Eph,label:'Energy',unit:'keV'},$
      y:{label:['Direct Flux','Albedo Flux','Flux'],unit:replicate('1/(s cm^2 keV)',3)}}}
    return
  end
  
  ;name parms indices
  dR_idx=0
  T_0_idx=1
  eps_idx=2
  kappa_idx=3
  Dist_E_idx=4
  Emin_idx=5
  Emax_idx=6
  Ebreak_idx=7
  delta1_idx=8
  delta2_idx=9
  Dist_Ang_idx=10
  theta_C_idx=11
  theta_b_idx=12
  dMu_idx=13
  a_4_idx=14
  n_0_idx=15
  n_b_idx=16
  B_idx=17
  theta_idx=18
  Bz_idx=19
  hc_angle_idx=20
  VoxelID_idx=21
  arr_key_loc_idx=22
  SpineS_idx=23
  SpineR_idx=24
  HasArr_idx=25
  rsun_idx=26
  ;define rparms indices
  dS_idx= 0
  Eph_min_idx= 1
  dEph_idx= 2
  a_idx= 3
  relative_abundance_idx= 4

  ;define nparms indices
  N_pix_idx= 0
  N_vox_idx= 1
  N_Eph_idx= 2
  arr_key_idx= 3
  N_E_idx= 4
  N_mu_idx= 5
  ;
  sz=size(parms,/dim)
  N_parms=sz[2]
  
  sz=size(rowdata,/dim)
  nLOS=sz[0]
  rowdata[*]=0

  nEph=nparms[N_Eph_idx]
  E1=rparms[Eph_min_idx]
  logdE=rparms[dEph_idx]
  Eph =10^(Alog10(E1)+findgen(nEph)*logdE)
  DEph= 10^(Alog10(E1)+(findgen(nEph)+1)*logdE)-eph
  ; output photon energy array
  eph_2n=transpose([[Eph],[Eph+deph]])
  ; 2xn photon array

  EE =10^(Alog10(E1)+findgen(nEph+round(1./logDe))*logdE)
  DE =10^(Alog10(E1)+(findgen(nEph+round(1./logDe))+1)*logdE)-EE
  Nee=N_elements(EE)
  EE=EE + min(dEph)*0.5
  ;electron energies so that max(ee) =10 x max(eph)


  if n_elements(xray_cs) eq 0 then xray_cross_section, ee, Eph, Xray_CS

  e_dataout  =fltarr(N_elements(ee))
  e_data_tt  =fltarr(N_elements(ee))
  eph_dataout=fltarr(N_elements(eph))
  ;chromo_anis defined below is not used yet anywhere
  chromo_anis=rparms[a_idx]
  anisotropy=chromo_anis
  angle=parms[0,0,hc_angle_idx]
  mu=cos(angle*!PI/180.)
  Print,'Correction for an source at ',acos(mu)*180./!PI,' degrees', '  cos(theta) =',mu
  r_sun=parms[0,0,rsun_idx]
  ; the following lines upload albedo correction files
  ; The description of the method is given in
  ;http://adsabs.harvard.edu/abs/2006A%26A...446.1157K
  ; and
  ; http://www.astro.gla.ac.uk/users/eduard/rhessi/albedo/
  if n_elements(albedo) eq 0 then begin
    IF ((mu GT 0.05) AND (mu LT 0.95)) THEN BEGIN
      print,'reading data from files ..............................................'
      file1=getenv('SSW')+'/packages/xray/dbase/albedo/'+'green_compton_mu'+string(format='(I3.3)',5*FLOOR(mu*20.),/print)+'.dat'
      file2=getenv('SSW')+'/packages/xray/dbase/albedo/'+'green_compton_mu'+string(format='(I3.3)',5*CEIL(mu*20.),/print)+'.dat'
      restore,file1
      p1=p
      restore,file2
      p2=p
      a=p1.albedo+(p2.albedo-p1.albedo)*(mu - float(floor(mu*20))/20.)
    END ELSE BEGIN

      IF (mu LT 0.05) THEN BEGIN
        print,'Warning! Assuming heliocentric angle  =',acos(0.05)*180./!PI
        file=getenv('SSW')+'/packages/xray/dbase/albedo/'+'green_compton_mu'+string(format='(I3.3)',5*FLOOR(0.05*20.),/print)+'.dat'
        restore,file
        a=p.albedo
      ENDIF

      IF (mu GT 0.95) THEN BEGIN
        print,'Warning! Assuming heliocentric angle  =',acos(0.95)*180./!PI
        file=getenv('SSW')+'/packages/xray/dbase/albedo/'+'green_compton_mu'+string(format='(I3.3)',5*FLOOR(0.95*20.),/print)+'.dat'
        restore,file
        a=p.albedo
      ENDIF

    ENDELSE
    e_given=total(p.edges,1)/2
    eph_points=(eph-min(eph))>0
    albedo=interpolate(a,eph_points,eph_points,/grid)
  end
;  End albedo computation

;Prepare array defined distributions
;calculating the emission for analytical distribution
Lparms_M=long(nparms)
Npix=Lparms_M[N_pix_idx]
Nvox=Lparms_M[N_vox_idx]
Lparms_M[N_E_idx]=0; changed bellow if defined
Lparms_M[N_mu_idx]=0; changed bellow if defined
if n_elements(f_arr) ne 0 then begin
  f_size=size(f_arr)
  if f_size[0] eq 3 then begin
    if f_size[1] eq n_elements(E_arr) and f_size[2] eq n_elements(mu_arr)then begin
      Lparms_M[N_E_idx]=n_elements(E_arr)
      Lparms_M[N_mu_idx]=n_elements(mu_arr)
      E_arr=double(E_arr)
      mu_arr=double(mu_arr)
      SpineS=transpose(parms[*,*,SpineS_idx])
      SpineR=transpose(parms[*,*,SpineR_idx])
      HasArr=transpose(parms[*,*,HasArr_idx])
      aparms_idx=where(HasArr eq 1, aparms_count,comp=no_aparms_idx, ncomp=no_aparms_count)
      if aparms_count ne 0 then begin
        if no_aparms_count gt 0 then begin
          arr_key_loc=reform(parms[*,*,arr_key_loc_idx])
          arr_key_loc[no_aparms_idx]=1
          parms[*,*,arr_key_loc_idx]=arr_key_loc
        end
        f_arr_M=double(reform(f_arr[*,*,SpineS],Lparms_M[N_E_idx],Lparms_M[N_mu_idx],Nvox,Npix))*$
          (transpose(array_replicate(SpineR,Lparms_M[N_E_idx],Lparms_M[N_mu_idx]),[2,3,0,1]))
      endif else no_aparms=1    
    endif else no_aparms=1
  endif else no_aparms=1
endif else no_aparms=1
if keyword_set(no_aparms) then begin
  E_arr=0d
  mu_arr=0d
  f_arr=0d
  f_arr_M=0d
endif
case LPARMS_M[arr_key_idx] of
  ;This is a GX-implemented custom use of arr_key global switch
  ;arr_key=-1 sets analytical nonthermal density to 0 in the entire volume
  ;thus, it allows contributions only from the array defined distributions, if any
  ;arr_key=-2 replace analytical nonthermal density in the entire volume
  ;with numerical values derived from the array defined distributions, if any
  -1: begin
    parms[*,*,n_b_idx]=0
    LPARMS_M[arr_key_idx]=0; arr_key is switched on
  end
  -2:begin
    if keyword_set(aparms_count) then begin
      nb_arr=reform(parms[*,*,n_b_idx])
      loge=double(alog(e_arr))
      sz=size(f_arr)
      n_e=sz[1]
      n_mu=sz[2]
      n_s=sz[3]
      ;Nvox,Npix
      nb_arr2ana=dblarr(n_s)
      int_E_local=dblarr(n_E)
      for j=0,n_s-1 do begin
        for i=0, n_e-1 do begin
          int_E_local[i]=2d0*!dpi*int_trapzd(double(mu_arr), double(f_arr[i, *,j]))
        endfor
        nb_arr2ana[j]=int_trapzdlog(loge, double(alog(int_E_local)))
      end
      nb_arr2ana=nb_arr2ana[SpineS]*SpineR
      bad=where(finite(nb_arr2ana) eq 0,count)
      if count gt 0 then begin
        nb_arr2ana[bad]=0
      endif
      nb_arr[aparms_idx]=nb_arr2ana[aparms_idx]
      parms[*,*,n_b_idx]=nb_arr
      LPARMS_M[arr_key_idx]=1; arr_key is switched off
    endif
  end
  else:
endcase
; End of Array defined distributions
  for los_idx=0, nLOS-1 do begin
    tparms=transpose(parms[los_idx,*,*])
    point_in=where(tparms[T_0_idx,*] gt 0, Nvox);added
    if Nvox gt 0 then begin
      parmin=tparms[*,point_in];added
      if isa(f_arr_m,/array) then f_arr_in=reform(f_arr_m[*,*,point_in,los_idx])
      e_dataout[*]  =0
      e_data_tt[*]  =0
      eph_dataout[*]=0

      Ve= sqrt(2.*EE*1.6e-9/9.8d-28)
      Te_thr=0.09 ; keV
      ; lowest temperature that can be calculated for thermal plasma SXR emission
      abun =rparms[relative_abundance_idx]
      ; fractional element abunadences with respect to coronal

      ; constant KK is defined for Coloumb log =20,
      ; kk= 2\pi e^4*20 keV^2cm^4
      KK =2.6d-18
      ; see for details:
      ; http://adsabs.harvard.edu/abs/2011SSRv..159..301K

      ; normalisation
      ;****************************************************************
      A_vox=rparms[dS_idx]*1d0
      ;voxel area
      V_vox=A_vox*parmin[dR_idx,*]*1d0
      ; Voxel volume in cm^3
      Np_vox=parmin[n_0_idx,*]*1d0
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
        ;compute TR anisotropy
        IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) EQ gx_voxelid(/tr))  THEN BEGIN
          ;anis defined below is used to multiply below e_data_tt when needed
          anis=anis_factor(Parmin[Dist_Ang_idx,i], Parmin[theta_C_idx,i], Parmin[theta_b_idx,i], Parmin[dMu_idx,i], Parmin[a_4_idx,i],Parmin[Bz_idx,i])  
          ; the value that mimics anisotropy of the source
          if fix(Parmin[HasArr_idx,i]) eq 1 then begin 
            anis_arr=anis_factor_arr(f_arr_in[*,*,i],e_arr,mu_arr,Parmin[Bz_idx,i])
            print,'anis_ana, anis_arr:',anis, anis_arr
          end   
        ENDIF
        ;****************************************************
        ;Here we compute array defined contribution, if any
        e_data_tt[*]  =0
        if fix(Parmin[HasArr_idx,i]) eq 1 then begin
          if LPARMS_M[arr_key_idx] eq 0 then begin
            dist_e_arr=dblarr(n_elements(e_arr))
            for k=0, n_elements(e_arr)-1 do dist_e_arr[k]=2d0*!dpi*int_trapzd(mu_arr, f_arr_in[k,*,i])
            dist_e_int=interpol(dist_e_arr,e_arr*1e3,ee)/1e3 ; 1MeV = 1e3keV;  dist_e_int is the lectron distribution per keV (dist_e_arr per MeV)
            e_dataout=ve*dist_e_int*norm[i]
            ; the block below calculates TT emission
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) NE gx_voxelid(/tr)) THEN  eph_dataout+=xray_cs#(e_dataout*DE)
            ; calculates coronal X-ray emission using thin target
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) EQ gx_voxelid(/tr)) THEN BEGIN
              ; checks if it is the transition region
              ; calculates chromospheric X-ray emission using thick target
              FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox/KK/npV[i]
              eph_dataout+=xray_cs#(e_data_tt*DE*anis_arr)
            ENDIF
            ; ******************* TT emission ************************************
          end  
        endif 
        ; End of array defined contribution
        ;**********************************************************
        e_data_tt[*]  =0
        ; If electron distribution is a power-law
        IF (ROUND(Parmin[Dist_E_idx,i]) EQ 3 OR ROUND(Parmin[Dist_E_idx,i]) EQ 11 ) THEN BEGIN
          ; Electron distribution is a power-law
          E_dist =((ee GE Parmin[Emin_idx,i]*1000.) AND (ee LT Parmin[Emax_idx,i]*1000.))*EE ^(-Parmin[delta1_idx,i])
          E_dist_norm=total(E_dist*de)

          IF (E_dist_norm GT 0) THEN BEGIN

            e_dataout=Parmin[n_b_idx, i]*ve*E_dist*norm[i]/e_dist_norm

            ; the block below calculates TT emission
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) NE gx_voxelid(/tr)) THEN     eph_dataout+=xray_cs#(e_dataout*DE)
            ; calculates coronal X-ray emission using thin target

            ;IF (((ulong(Parmin[VoxelID_idx,i]) and 2l) EQ 2) and (Parmin[VoxelID_idx,i] gt 8e2)) THEN BEGIN
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) EQ gx_voxelid(/tr)) THEN BEGIN
              ; checks if it is the transition region
              ; calculates chromospheric X-ray emission using thick target
              FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox/KK/npV[i]
              eph_dataout+=xray_cs#(e_data_tt*DE*anis)
            ENDIF
            ; ******************* TT emission ************************************

          ENDIF

        ENDIF
        ; end of power-law case
        ;**********************************************************

        ;**********************************************************
        ; Electron  distribution is double power-law
        IF (ROUND(Parmin[Dist_E_idx,i]) EQ 4 OR ROUND(Parmin[Dist_E_idx,i]) EQ 12) THEN BEGIN
          E_dist=((ee GE Parmin[Emin_idx,i]*1e3) AND (ee LT Parmin[E_break_idx,i]*1e3))*(EE/(Parmin[E_break_idx,i]*1e3))^(-Parmin[delta1_idx,i])+$
            ((ee GE Parmin[E_break_idx,i]*1e3) AND (ee LT Parmin[Emax_idx,i]*1e3))*(EE/(Parmin[E_break_idx,i]*1e3))^(-Parmin[delta2_idx,i])
          E_dist_norm=total(E_dist*de)
          IF (E_dist_norm GT 0) THEN BEGIN

            e_dataout=Parmin[n_b_idx, i]*ve*E_dist*norm[i]/E_dist_norm

            ; the block below calculates TT emission
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) NE gx_voxelid(/tr)) THEN     eph_dataout+=xray_cs#(e_dataout*DE)
            ; calculates coronal X-ray emission using thin target

            ;IF (((ulong(Parmin[VoxelID_idx,i]) and 2l) EQ 2) and (Parmin[VoxelID_idx,i] gt 8e2)) THEN BEGIN
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) EQ gx_voxelid(/tr)) THEN BEGIN
              ;IF ((Parms[VoxelID_idx,i] and 2) EQ 2) THEN BEGIN
              ; checks if it is the transition region
              ; calculates chromospheric X-ray emission using thick target
              FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox/KK/npV[i]
              eph_dataout+=xray_cs#(e_data_tt*DE*anis)
            ENDIF
            ; ******************* TT emission ************************************

          ENDIF

        ENDIF
        ;**********************************************************


        ;**********************************************************
        ; Electron  distribution is thermal/non-thermal over energy
        IF (ROUND(Parmin[Dist_E_idx,i]) EQ 5) THEN BEGIN
          kb=8.617343e-8 ;(keV K-1)
          Te=parmin[T_0_idx,i]*kb
          E_cr=Te/parmin[eps_idx,i]
          ; critial energy in keV
          E_dist_cr=sqrt(E_cr/(!PI*Te))*exp(-E_cr/Te)

          dens_e=Parmin[n_0_idx, i]*(parmin[T_0_idx,i] GT 2e5)+0.* (parmin[T_0_idx,i] LE 2e5)
          ; density if ionised Te > 2e5K

          E_dist =((ee GE Parmin[Emin_idx,i]*1000.) AND (ee LT E_cr))*sqrt(EE/(!PI*Te))*exp(-EE/Te)
          E_dist =((ee GE E_cr) AND (ee LT Parmin[Emax_idx,i]*1000.))*E_dist_cr*(EE/E_cr) ^(-Parmin[delta1_idx,i])
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
        IF (ROUND(Parmin[Dist_E_idx,i]) EQ 6) THEN BEGIN
          kb=8.617343e-8 ;(keV K-1)
          Te=parmin[T_0_idx,i]*kb
          Thet=Te/511.
          ; critial energy in keV
          gam=1.d0+EE/511.

          dens_e=Parmin[n_0_idx, i]*(parmin[T_0_idx,i] GT 2e5)+0.* (parmin[T_0_idx,i] LE 2e5)
          ; density if ionised Te > 2e5K

          E_dist =((ee GE Parmin[Emin_idx,i]*1000.) AND (ee LT Parmin[Emax_idx,i]*1000.))*$
            gam*sqrt(gam*gam-1.)/(1.+(gam-1.)/(Parmin[kappa_idx,i]-1.5)/Thet)^(Parmin[kappa_idx,i]+1)/thet^(1.5)
          E_dist_norm=total(E_dist*de)
          IF (E_dist_norm GT 0) THEN BEGIN
            e_dataout=dens_e*ve*E_dist*norm[i]/E_dist_norm

            ; the block below calculates TT emission
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) NE gx_voxelid(/tr)) THEN     eph_dataout+=xray_cs#(e_dataout*DE)
            ; calculates coronal X-ray emission using thin target

            ;IF (((ulong(Parmin[VoxelID_idx,i]) and 2l) EQ 2) and (Parmin[VoxelID_idx,i] gt 8e2)) THEN BEGIN
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) EQ gx_voxelid(/tr)) THEN BEGIN
              ;IF ((Parms[VoxelID_idx,i] and 2) EQ 2) THEN BEGIN
              ; checks if it is the transition region
              ; calculates chromospheric X-ray emission using thick target
              FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox/KK/npV[i]
              eph_dataout+=xray_cs#(e_data_tt*DE*anis)
            ENDIF
            ; ******************* TT emission ************************************


          ENDIF

        ENDIF
        ;**********************************************************

        ;**********************************************************
        ; If electron distribution is a power-law in momentum p*c
        IF (ROUND(Parmin[Dist_E_idx,i]) EQ 7 ) THEN BEGIN
          ; Electron distribution is a power-law

          pc=sqrt((ee+511.)^2-511.^2)
          d_pc=(ee+511.)*de/pc
          pc_min=sqrt((Parmin[Emin_idx,i]*1000.+511.)^2-511.^2)
          pc_max=sqrt((Parmin[Emax_idx,i]*1000.+511.)^2-511.^2)

          E_dist =((pc GE pc_min) AND (pc LT pc_max))*pc^(-Parmin[delta1_idx,i])
          E_dist_norm=total(E_dist*d_pc)
          IF (E_dist_norm GT 0) THEN BEGIN
            e_dataout=Parmin[n_b_idx, i]*ve*E_dist*norm[i]/e_dist_norm

            ; the block below calculates TT emission
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) NE gx_voxelid(/tr)) THEN     eph_dataout+=xray_cs#(e_dataout*DE)
            ; calculates coronal X-ray emission using thin target

            ;IF (((ulong(Parmin[VoxelID_idx,i]) and 2l) EQ 2) and (Parmin[VoxelID_idx,i] gt 8e2)) THEN BEGIN
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) EQ gx_voxelid(/tr)) THEN BEGIN
              ;IF ((Parms[VoxelID_idx,i] and 2) EQ 2) THEN BEGIN
              ; checks if it is the transition region
              ; calculates chromospheric X-ray emission using thick target
              FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox/KK/npV[i]
              eph_dataout+=xray_cs#(e_data_tt*DE*anis)
            ENDIF
            ; ******************* TT emission ************************************

          ENDIF
        ENDIF
        ; end of power-law case
        ;**********************************************************
        ; If electron distribution is a power-law in momentum p*c
        IF (ROUND(Parmin[Dist_E_idx,i]) EQ 7 ) THEN BEGIN
          ; Electron distribution is a power-law

          pc=sqrt((ee+511.)^2-511.^2)
          d_pc=(ee+511.)*de/pc
          pc_min=sqrt((Parmin[Emin_idx,i]*1000.+511.)^2-511.^2)
          pc_max=sqrt((Parmin[Emax_idx,i]*1000.+511.)^2-511.^2)

          E_dist =((pc GE pc_min) AND (pc LT pc_max))*pc^(-Parmin[delta1_idx,i])
          E_dist_norm=total(E_dist*d_pc)
          IF (E_dist_norm GT 0) THEN BEGIN
            e_dataout=Parmin[n_b_idx, i]*ve*E_dist*norm[i]/e_dist_norm

            ; the block below calculates TT emission
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) NE gx_voxelid(/tr)) THEN     eph_dataout+=xray_cs#(e_dataout*DE)
            ; calculates coronal X-ray emission using thin target

            ;IF (((ulong(Parmin[VoxelID_idx,i]) and 2l) EQ 2) and (Parmin[VoxelID_idx,i] gt 8e2)) THEN BEGIN
            IF ((ulong(Parmin[VoxelID_idx,i]) and gx_voxelid(/tr)) EQ gx_voxelid(/tr)) THEN BEGIN
              ;IF ((Parms[VoxelID_idx,i] and 2) EQ 2) THEN BEGIN
              ; checks if it is the transition region
              ; calculates chromospheric X-ray emission using thick target
              FOR j=0, N_elements(e_data_tt)-2 DO e_data_tt[j]=total(E_dataout[j:Nee-1]*DE[j:Nee-1])*EE[j]*A_vox/KK/npV[i]
              eph_dataout+=xray_cs#(e_data_tt*DE*anis)
            ENDIF
            ; ******************* TT emission ************************************

          ENDIF
        ENDIF
        ; end of power-law case
        ;**********************************************************
        
        ;**********************************************************
        ; Electron background distribution is thermal - should be always added
        ; IF (ROUND(Parmin[Dist_E_idx,i]) EQ 1) THEN BEGIN
        ; Electron distribution is thermal
        kb=8.617343e-8 ;(keV K-1)
        Te=parmin[T_0_idx,i]*kb
        ;local temperature in keV
        ;e_dataout+=norm[i]*ve*parmin[n_0_idx,i]*sqrt(EE/(!PI*Te))*exp(-EE/Te)
        E_dist=norm[i]*ve*parmin[n_0_idx,i]*sqrt(EE/(!PI*Te))*exp(-EE/Te)
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
      eph_dataout=eph_dataout*(R_sun/960.)^2 ; R_sun in arcseconds
      rowdata[los_idx,*,0]=eph_dataout
      rowdata[los_idx,*,1]=(n_elements(albedo) gt 0?anisotropy*(transpose(albedo)#(eph_dataout*deph)):0)
    end
  end
  ;ends FOR over row
END
