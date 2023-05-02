;this is an utility routine that may be used to sample the magnetic field in the model at certain locations along LOS marked by a particular voxel ID flag 

pro blos_layers,parms,rowdata,info=info
 if arg_present(info) then begin
     if n_elements(info) eq 0 then begin
       Parms=Replicate({Name:'unused',Value:0d,Unit:'',Hint:''},15)
       Parms[0].Name='dS'           & Parms[0].Value=0.180E+19    & Parms[0].Unit='cm^2'    & Parms[0].Hint='Source/pixel Area'
       Parms[1].Name='dR'           & Parms[1].Value=0.600E+09    & Parms[1].Unit='cm'      & Parms[1].Hint='Source/voxel Depth'
       Parms[2].Name='B'            & Parms[2].Value=200.         & Parms[2].Unit='Gauss'       & Parms[2].Hint='Magnetic field'
       Parms[3].Name='theta'        & Parms[3].Value=0            & Parms[3].Unit='degrees' & Parms[3].Hint='Inclination'
       Parms[4].Name='phi'          & Parms[4].Value=0            & Parms[4].Unit='degrees' & Parms[4].Hint='Azimuth'
       Parms[5].Name='VoxelID'      & Parms[5].Value=0              & Parms[5].Unit='unsigned integer' & Parms[5].Hint='chromo/TR/corona'  
       Parms[6].Name='Bx'            & Parms[6].Value=200.         & Parms[6].Unit='Gauss'       & Parms[6].Hint='Bx'
       Parms[7].Name='By'            & Parms[7].Value=200.         & Parms[7].Unit='Gauss'       & Parms[7].Hint='By'
       Parms[8].Name='Bz'            & Parms[8].Value=200.         & Parms[8].Unit='Gauss'       & Parms[8].Hint='Bz'
       Parms[9].Name='N'             & Parms[9].Value=2            & Parms[9].Unit='unsigned integer' & Parms[9].Hint='Number of layers'
       Parms[10].Name='VoxelX'       & Parms[10].Value=0            & Parms[10].Unit='double'  & Parms[10].Hint='Voxel fractional X index'
       Parms[11].Name='VoxelY'       & Parms[11].Value=0            & Parms[11].Unit='double'  & Parms[11].Hint='Voxel fractional Y index'
       Parms[12].Name='VoxelZ'       & Parms[12].Value=0            & Parms[12].Unit='double'  & Parms[12].Hint='Voxel fractional Z index'
       Parms[13].Name='T_0'         & Parms[13].Value=0           & Parms[13].Unit='K'      & Parms[13].Hint='Plasma Temperature'
       Parms[14].Name='h'           & Parms[14].Value=0           & Parms[14].Unit='km'     & Parms[14].Hint='Voxel Height'
      endif else parms=info.parms      
      info={parms:parms,$
      pixdim:[parms[9].value,15],$
      spectrum:{x:{axis:lindgen(parms[9].value),label:'Layer Index',unit:''},$
      y:{label:['Absolute B', 'LOS B', 'Transverse B','Inclination','Azimuth','Mask','Bx','By','Bz','VoxelID', 'VoxelX', 'VoxelY', 'VoxelZ','T','h'],unit:['G','G','G','deg','deg','idx','G','G','G','','','','','K','km']}}}                          
    return
 end
   sz=size(rowdata,/dim)
   Npix=sz[0]
   Nchan=sz[1]
   rowdata[*]=0
   for pix=0, Npix-1 do begin
     rparms=transpose(parms[pix,*,*])
     z=float(reform(rparms[12,*]))
     for chanid =0, nchan-1 do begin
       idx=max(where(fix(z) eq chanid,count))
       if count gt 0 then begin
        rowdata[pix,chanid,0]=rparms[2,idx]
        rowdata[pix,chanid,1]=rparms[2,idx]*cos(rparms[3,idx]*!dtor)
        rowdata[pix,chanid,2]=rparms[2,idx]*sin(rparms[3,idx]*!dtor)
        rowdata[pix,chanid,3]=rparms[3,idx]
        rowdata[pix,chanid,4]=rparms[4,idx]
        rowdata[pix,chanid,5]=(ulong(rparms[5,idx]) and gx_voxelid(/umb))
        rowdata[pix,chanid,6]=rparms[6,idx]
        rowdata[pix,chanid,7]=rparms[7,idx]
        rowdata[pix,chanid,8]=rparms[8,idx]
        rowdata[pix,chanid,9]=rparms[5,idx]
        rowdata[pix,chanid,10]=rparms[10,idx]
        rowdata[pix,chanid,11]=rparms[11,idx]
        rowdata[pix,chanid,12]=rparms[12,idx]
        rowdata[pix,chanid,13]=rparms[13,idx]
        rowdata[pix,chanid,14]=rparms[14,idx]
       endif
     end  
   endfor  
END

