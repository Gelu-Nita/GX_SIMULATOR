pro gx_bz2lff,Bz,nz,dr,Bcube,alpha1=alpha1,seehafer=seehafer, sub_b00=sub_b00, sub_plan=sub_plan
  
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
  z=findgen(Nz)*dr[2]/dr[0]
  ;use J.R.Costa j_b_lff extrapolation routine
  j_b_lff,  bz, z, bxout, byout, bzout,alpha1=alpha1,seehafer=seehafer, sub_b00=sub_b00, sub_plan=sub_plan
  ;Bcube is the four dimensional datacube output
 ;Bcube is the four dimensional datacube output
  Bcube=fltarr(Nx,Ny,Nz,3)
  Bcube[*,*,*,0]=temporary(bxout)
  Bcube[*,*,*,1]=temporary(byout)
  Bcube[*,*,*,2]=temporary(bzout)
end

