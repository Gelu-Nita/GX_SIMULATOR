pro gx_bz2pfe,Bin,nz,dr,Bcube
  
  ;Bin must be a two dimensional array containing the base Bz data
  sz=size(Bin)
  Nx=sz[1]
  Ny=sz[2]
  N=[Nx,Ny]
  if sz[0] ge 4 then Bin=reform(Bin[*,*,*,2])
  if sz[0] ge 3 then Bin=reform(Bin[*,*,sz[3]-1])
  
  ;Nz specifies the height of the extrapolation
  if n_elements(Nz) eq 0 then Nz= min([Nx,Ny])
  nz=long(nz[0])
  ;dr represents voxel size in each direction (arcsec)
  if n_elements(dr) eq 0 then dr=[2.,2.,2.]
  
  
  b_struct=fft_pot_open(Bin,nz)
  ;Bcube is the four dimensional datacube output
  Bcube=fltarr(Nx,Ny,Nz,3)
  Bcube[*,*,*,0]=b_struct.bx
  Bcube[*,*,*,1]=b_struct.by
  Bcube[*,*,*,2]=b_struct.bz
  
end