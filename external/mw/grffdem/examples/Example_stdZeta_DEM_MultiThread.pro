pro Example_stdZeta_DEM_MultiThread
 ;Test code: computing the gyroresonance and free-free emission using pre-defined zeta function and DEM/DDM, 
 ;for a number of lines-of-sight
 
 libname='GRFF_DEM_Transfer_64.dll' ;library name (change if necessary)
 
 Npix=4L ;number of pixels (number of lines-of-sight)
 Nz=100L ;number of voxels
 Nf=240L ;number of frequencies
 
 restore, 'DEMexample.sav', /verbose ;reading the DEM, DDM and temperature arrays - for a single voxel
 T_arr=double(T_arr) ;all input/output arrays must be of double precision
 NT=n_elements(T_arr) ;number of temperatures where DEM and DDM are defined
 ;preparing the 3D NT*Nz*Npix arrays (with the same DEM and DDM in all voxels of all lines-of-sight):
 DEM_arr_M=double(rebin(DEM_arr, NT, Nz, Npix))
 DDM_arr_M=double(rebin(DDM_arr, NT, Nz, Npix))
 
 ;array of dimensions etc.:
 Lparms_M=[Npix,  $ ;number of pixels
           Nz,    $ ;number of voxels
           Nf,    $ ;number of frequencies
           NT,    $ ;number of temperatures
           0,     $ ;global DEM on/off key (on)
           0      ] ;global DDM on/off key (on)
 
 ;global floating-point parameters for a single line-of-sight: 
 Rparms_single=[1d18,  $ ;source area, cm^2
                3d9,   $ ;starting frequency, Hz 
                0.005  ] ;logarithmic step in frequency 

 ;single-voxel parameters: 
 ParmLocal=dblarr(15)
 ParmLocal[0]=2d9    ;source depth, cm (total depth - the depths for individual voxels will be computed later)
 ParmLocal[1]=1d6    ;plasma temperature, K (not used in this example)
 ParmLocal[2]=1d9    ;electron/atomic concentration, cm^{-3} (not used in this example)
 ParmLocal[3]=100d0  ;magnetic field, G (will be changed later)
 ParmLocal[4]=120d0  ;viewing angle, degrees
 ParmLocal[5]=0d0    ;azimuthal angle, degrees
 ParmLocal[6]=0      ;emission mechanism flag (all on)
 ParmLocal[7]=30     ;maximum harmonic number
 ParmLocal[8]=0      ;proton concentration, cm^{-3} (not used in this example)
 ParmLocal[9]=0      ;neutral hydrogen concentration, cm^{-3}
 ParmLocal[10]=0     ;neutral helium concentration, cm^{-3}
 ParmLocal[11]=0     ;local DEM on/off key (on)
 ParmLocal[12]=0     ;local DDM on/off key (on)
 ParmLocal[13]=0     ;element abundance code (coronal, following Feldman 1992)
 ParmLocal[14]=0     ;reserved
 
 Rparms_M=dblarr(3, Npix) ;global floating-point parameters for the lines-of-sight
 Parms_M=dblarr(15, Nz, Npix) ;multi-voxel parameters along the lines-of-sight
 for pix=0, Npix-1 do begin
  Rparms_M[*, pix]=Rparms_single ;the same parameters for all lines-of-sight are used in this example
 
  for i=0, Nz-1 do begin
   Parms_M[*, i, pix]=ParmLocal
  
   Parms_M[0, i, pix]=ParmLocal[0]/Nz ;depths of individual voxels
   Parms_M[3, i, pix]=(1000d0-700d0*i/(Nz-1))*(1d0-0.1*pix) 
    ;magnetic field decreases along a line-of-sight linearly:
    ;from 1000 to 300 G in the first line of sight,
    ;and is 10%, 20%, 30% less in the other lines-of-sight
  
   ;all other parameters are the same in all voxels
  endfor
 endfor
 
 RL_M=dblarr(7, Nf, Npix) ;output array
 
 ;main call:
 res=call_external(libname, 'GET_MW_SLICE', Lparms_M, Rparms_M, Parms_M, T_arr, DEM_arr_M, DDM_arr_M, RL_M)

 ;extracting the results:
 freq=reform(RL_M[0, *, *]) ;frequencies
 I_L=reform(RL_M[5, *, *]) ;left-hand-polarized flux, exact mode coupling
 I_R=reform(RL_M[6, *, *]) ;right-hand-polarized flux, exact mode coupling
 
 ;--------------------------------------------
 ;Now plotting the results
 ;(spectra for different lines-of-sight are shown by different linestyles)
 
 window, 1, title='Intensity'
 wset, 1
 plot, freq[*, 0], I_L[*, 0]+I_R[*, 0], /xlog, /ylog, xstyle=1, /nodata, $
       xtitle='Frequency, GHz', ytitle='Intensity, sfu'
 for pix=0, Npix-1 do oplot, freq[*, pix], I_L[*, pix]+I_R[*, pix], linestyle=pix 
 
 window, 2, title='Polarization degree'
 wset, 2
 plot, freq[*, 0], (I_L[*, 0]-I_R[*, 0])/(I_L[*, 0]+I_R[*, 0]), /xlog, yrange=[-1, 1], xstyle=1, ystyle=1, /nodata, $
       xtitle='Frequency, GHz', ytitle='Polarization degree'
 for pix=0, Npix-1 do oplot, freq[*, pix], (I_L[*, pix]-I_R[*, pix])/(I_L[*, pix]+I_R[*, pix]), linestyle=pix
end
