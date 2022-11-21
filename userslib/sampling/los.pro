;this is an utility routine that may be used to sample the magnetic field in the model at certain locations along LOS marked by a particular voxel ID flag 

pro los,parms,rowdata,info=info
 if arg_present(info) then begin
     if n_elements(info) eq 0 then begin
       Parms=Replicate({Name:'unused',Value:0d,Unit:'',Hint:''},9)
       Parms[0].Name='dS'           & Parms[0].Value=0.180E+19    & Parms[0].Unit='cm^2'    & Parms[0].Hint='Source/pixel Area'
       Parms[1].Name='dR'           & Parms[1].Value=0.600E+09    & Parms[1].Unit='cm'      & Parms[1].Hint='Source/voxel Depth'
       Parms[2].Name='T_0'          & Parms[2].Value=0.200E+08    & Parms[2].Unit='K'       & Parms[2].Hint='Plasma Temperature'
       Parms[3].Name='n_0'          & Parms[3].Value=0.500E+10    & Parms[3].Unit='cm^{-3}' & Parms[3].Hint='Thermal e density'
       Parms[4].Name='n_b'          & Parms[4].Value=0.300E+08    & Parms[4].Unit='cm^{-3}' & Parms[4].Hint='Nonthermal e density'
       Parms[5].Name='B'            & Parms[5].Value=200.         & Parms[5].Unit='Gauss'   & Parms[5].Hint='Magnetic field'
       Parms[6].Name='theta'        & Parms[6].Value=0            & Parms[6].Unit='degrees' & Parms[6].Hint='Inclination'
       Parms[7].Name='phi'          & Parms[7].Value=0            & Parms[7].Unit='degrees' & Parms[7].Hint='Azimuth'
       Parms[8].Name='VoxelID'      & Parms[8].Value=0            & Parms[8].Unit='unsigned integer' & Parms[8].Hint='chromo/TR/corona'
     endif else parms=info.parms      
      info={parms:parms,$
      pixdim:[1,4,3],$,2
      spectrum:{x:{axis:[0],label:'',unit:'' },$
      y:{label:['n0_L', 'EM', 'Wth','nb_L'],unit:['cm^-2','cm^-5','erg/cm^2','cm^-2']}},$
      channels:['Corona', 'Transition Region', 'Chromosphere']}                          
    return
 end
   sz=size(rowdata,/dim)
   Npix=sz[0]
   Nchan=sz[1]
   voxel_id=ulong(parms[*,*,8])
   
   n0dL=parms[*,*,1]*parms[*,*,3]
   n02dL=parms[*,*,1]*parms[*,*,3]^2
   n0TdL=parms[*,*,1]*parms[*,*,2]*parms[*,*,3]
   nbdL=parms[*,*,1]*parms[*,*,4]
   voxel_id=ulong(parms[*,*,8])
   
   chromo_mask=(tr_mask=(corona_mask=parms[*,*,1]*0))

   corona_id=where((voxel_id and gx_voxelid(/corona)) ne 0,corona_count)
   tr_id=where((voxel_id and gx_voxelid(/euv)) ne 0,tr_count)
   chromo_id=where((voxel_id and gx_voxelid(/corona,/chromo,/tr,/euv)) eq gx_voxelid(/chromo) ,chromo_count)
   if chromo_count gt 0 then begin
     chromo_mask[chromo_id]=1
   endif
   if tr_count gt 0 then begin
     tr_mask[tr_id]=1
   endif
   if corona_count gt 0 then begin
    corona_mask[corona_id]=1
   endif
   rowdata[*]=0
   ;the factor of 3 takes into account contribution from protons to the thermal energy
   rowdata[*,0,0,0]=total(corona_mask*n0dL,2,/double)
   rowdata[*,0,1,0]=total(corona_mask*n02dL,2,/double)
   rowdata[*,0,2,0]=3*(1.38064852e-16)*total(corona_mask*n0TdL,2,/double)
   rowdata[*,0,3,0]=total(corona_mask*nbdL,2,/double)
   
   rowdata[*,0,0,1]=total(tr_mask*n0dL,2,/double)
   rowdata[*,0,1,1]=total(tr_mask*n02dL,2,/double)
   rowdata[*,0,2,1]=3*(1.38064852e-16)*total(tr_mask*n0TdL,2,/double)
   rowdata[*,0,3,1]=total(tr_mask*nbdL,2,/double)

   rowdata[*,0,0,2]=total(chromo_mask*n0dL,2,/double)
   rowdata[*,0,1,2]=total(chromo_mask*n02dL,2,/double)
   rowdata[*,0,2,2]=3*(1.38064852e-16)*total(chromo_mask*n0TdL,2,/double)
   rowdata[*,0,3,2]=total(chromo_mask*nbdL,2,/double)
END

