;this is an utility routine that may be used to sample the magnetic field in the model at TR level

pro blos_tr,parms,rowdata,info=info
 if arg_present(info) then begin
     if n_elements(info) eq 0 then begin
       Parms=Replicate({Name:'unused',Value:0d,Unit:'',Hint:''},14)
       Parms[0].Name='dS'           & Parms[0].Value=0.180E+19    & Parms[0].Unit='cm^2'    & Parms[0].Hint='Source/pixel Area'
       Parms[1].Name='dR'           & Parms[1].Value=0.600E+09    & Parms[1].Unit='cm'      & Parms[1].Hint='Source/voxel Depth'
       Parms[2].Name='B'            & Parms[2].Value=200.         & Parms[2].Unit='G'       & Parms[2].Hint='Magnetic field'
       Parms[3].Name='theta'        & Parms[3].Value=0            & Parms[3].Unit='degrees' & Parms[3].Hint='Inclination'
       Parms[4].Name='phi'          & Parms[4].Value=0            & Parms[4].Unit='degrees' & Parms[4].Hint='Azimuth'
       Parms[5].Name='VoxelID'      & Parms[5].Value=0            & Parms[5].Unit='unsigned integer' & Parms[5].Hint='chromo/TR/corona'
       Parms[6].Name='VoxelX'       & Parms[6].Value=0            & Parms[6].Unit='double'  & Parms[6].Hint='Voxel fractional X index'
       Parms[7].Name='VoxelY'       & Parms[7].Value=0            & Parms[7].Unit='double'  & Parms[7].Hint='Voxel fractional Y index'
       Parms[8].Name='VoxelZ'       & Parms[8].Value=0            & Parms[8].Unit='double'  & Parms[8].Hint='Voxel fractional Z index'
       Parms[9].Name='Bx'           & Parms[9].Value=0            & Parms[9].Unit='G'       & Parms[9].Hint='Bx'
       Parms[10].Name='By'          & Parms[10].Value=0           & Parms[10].Unit='G'      & Parms[10].Hint='By'
       Parms[11].Name='Bz'          & Parms[11].Value=0           & Parms[11].Unit='G'      & Parms[11].Hint='Bz'
       Parms[12].Name='T_0'         & Parms[12].Value=0           & Parms[12].Unit='K'      & Parms[12].Hint='Plasma Temperature'
       Parms[13].Name='h'           & Parms[13].Value=0           & Parms[13].Unit='km'     & Parms[13].Hint='TR height'
   endif else parms=info.parms      
      info={parms:parms,$
      pixdim:[1,15],$
      spectrum:{x:{axis:[0],label:'Dummy Index',unit:'' },$
      y:{label:['Absolute B', 'LOS B', 'Transverse B','Inclination','Azimuth','VoxelID', 'VoxelX', 'VoxelY', 'VoxelZ','Bx','By','Bz','TR T','h','Mask'],unit:['G','G','G','deg','deg','','','','','G','G','G','K','km','']}}}                          
    return
 end
   sz=size(rowdata,/dim)
   Npix=sz[0]
   Nchan=sz[1]
   rowdata[*]=0
   for pix=0, Npix-1 do begin
     rparms=transpose(parms[pix,*,*])
     z=float(reform(rparms[8,*]))
     idx=max(where(((ulong(rparms[5,*]) and 2l) eq 2l) and (z ge 0),count))
     if count gt 0 then begin
      rowdata[pix,0,0]=rparms[2,idx]
      rowdata[pix,0,1]=rparms[2,idx]*cos(rparms[3,idx]*!dtor)
      rowdata[pix,0,2]=rparms[2,idx]*sin(rparms[3,idx]*!dtor)
      rowdata[pix,0,3]=rparms[3,idx]
      rowdata[pix,0,4]=rparms[4,idx]
      rowdata[pix,0,5]=rparms[5,idx]
      rowdata[pix,0,6]=rparms[6,idx]
      rowdata[pix,0,7]=rparms[7,idx]
      rowdata[pix,0,8]=rparms[8,idx]
      rowdata[pix,0,9]=rparms[9,idx]
      rowdata[pix,0,10]=rparms[10,idx]
      rowdata[pix,0,11]=rparms[11,idx]
      rowdata[pix,0,12]=rparms[12,idx]
      rowdata[pix,0,13]=rparms[13,idx]
      rowdata[pix,0,14]=(ulong(rparms[5,idx]) and gx_voxelid(/umb))
     endif else begin
      dummy=0; here is just a debug stop point
     endelse
   endfor  
END

