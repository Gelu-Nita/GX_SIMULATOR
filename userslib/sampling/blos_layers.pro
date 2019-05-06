;this is an utility routine that may be used to sample the magnetic field in the model at certain locations along LOS marked by a particular voxel ID flag 

pro blos_layers,parms,rowdata,info=info
 if arg_present(info) then begin
     if n_elements(info) eq 0 then begin
       Parms=Replicate({Name:'unused',Value:0d,Unit:'',Hint:''},7)
       Parms[0].Name='dS'           & Parms[0].Value=0.180E+19    & Parms[0].Unit='cm^2'    & Parms[0].Hint='Source/pixel Area'
       Parms[1].Name='dR'           & Parms[1].Value=0.600E+09    & Parms[1].Unit='cm'      & Parms[1].Hint='Source/voxel Depth'
       Parms[2].Name='B'            & Parms[2].Value=200.         & Parms[2].Unit='G'       & Parms[2].Hint='Magnetic field'
       Parms[3].Name='theta'        & Parms[3].Value=0            & Parms[3].Unit='degrees' & Parms[3].Hint='Inclination'
       Parms[4].Name='phi'          & Parms[4].Value=0            & Parms[4].Unit='degrees' & Parms[4].Hint='Azimuth'
       Parms[5].Name='VoxelID'    & Parms[5].Value=0              & Parms[5].Unit='unsigned integer' & Parms[5].Hint='chromo/TR/corona'
       Parms[6].Name='N'    & Parms[6].Value=2                    & Parms[6].Unit='unsigned integer' & Parms[6].Hint='Number of layers'
     endif else parms=info.parms      
      info={parms:parms,$
      pixdim:[parms[6].value,6],$
      spectrum:{x:{axis:lindgen(parms[6].value),label:'Layer Index',unit:''},$
      y:{label:['Absolute B', 'LOS B', 'Transverse B','Inclination','Azimuth','Mask'],unit:['G','G','G','deg','deg','idx']}}}                          
    return
 end
   sz=size(rowdata,/dim)
   Npix=sz[0]
   Nchan=sz[1]
   rowdata[*]=0
   for pix=0, Npix-1 do begin
     rparms=transpose(parms[pix,*,*])
     for chanid =0, nchan-1 do begin
       idx=max(where(ishft(ulong(rparms[5,*]) and not gx_voxelid(/umb),-16) eq chanid+1,count))
       if count gt 0 then begin
        rowdata[pix,chanid,0]=rparms[2,idx]
        rowdata[pix,chanid,1]=rparms[2,idx]*cos(rparms[3,idx]*!dtor)
        rowdata[pix,chanid,2]=rparms[2,idx]*sin(rparms[3,idx]*!dtor)
        rowdata[pix,chanid,3]=rparms[3,idx]
        rowdata[pix,chanid,4]=rparms[4,idx]
        rowdata[pix,chanid,5]=(ulong(rparms[5,idx]) and gx_voxelid(/umb))
       end
     end  
   endfor  
END

