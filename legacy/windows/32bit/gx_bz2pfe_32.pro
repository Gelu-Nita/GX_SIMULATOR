pro gx_bz2pfe_32,Bz,nz,dr,Bcube
  
  ;bz must be a two dimensional array containing the base Bz data
  sz=size(bz)
  Nx=sz[1]
  Ny=sz[2]
  N=[Nx,Ny]
  if sz[0] ge 4 then bz=reform(bz[*,*,*,2])
  if sz[0] ge 3 then bz=reform(bz[*,*,sz[3]-1])
  
  ;Nz specifies the height of the extrapolation
  if n_elements(Nz) eq 0 then Nz= min([Nx,Ny])
  
  N=long([N,Nz])
  
  ;dr represents voxel size in each direction (arcsec)
  if n_elements(dr) eq 0 then dr=[2.,2.,2.]
  
  ;Bcube is the four dimensinal datacube output
  Bcube=fltarr(Nx,Ny,Nz,3)
   
  ;if not in the current directory, pathPFE must indicate the absolute location of the PFE_Dll2.dll file
  pathPFE=gx_findfile('PFE_Dll_32.dll',folder='userslib\Extrapolation\Windows\32bit')
  
  RESULT=call_external(pathPFE,'GET_PFE_DATACUBE',N,dr,bz,Bcube,/F_VALUE,/unload )
end