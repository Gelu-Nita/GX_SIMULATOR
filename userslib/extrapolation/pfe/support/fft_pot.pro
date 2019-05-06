

function fft_pot_flux,bn_
  sz = size(bn_)

  tmp = spl_int3d(reform(bn_, sz[1],sz[2],1),/x,/def)
  return, (spl_int3d(reform(tmp, 1,sz[2],1),/y,/def))[0]


  sz = size(bn_)
  bn = double(bn_)
  bn[0,*]*=0.5
  bn[sz[1]-1,*]*=0.5d
  bn[*,0]*=0.5d
  bn[*,sz[2]-1]*=0.5d
  return,total(bn)
end


function fft_pot_decompose,Bz_,Nz,open = open;,kx,ky,kz
  bz = bz_
  sz = size(Bz)
  Nx = sz[1]*2-2
  ny =  sz[2]*2-2
  hax=(FINDGEN((Nx - 1)/2) + 1)
  hay=(FINDGEN((Ny - 1)/2) + 1)
  kx = (2.*!pi*[0.0, haX, Nx/2, -Nx/2 + haX]/nx)#replicate(1d,ny)
  ky = (2.*!pi*[0.0, hay, Ny/2, -Ny/2 + hay]/ny)##replicate(1d,nx)
  kz = sqrt(kx^2+ky^2)
  B = -2.*kz*(Nz-1d)
  Bz_pad = dblarr(nx,ny)
  bz_pad[sz[1]:*,sz[2]:*] = reverse(reverse(bz[1:sz[1]-2,1:sz[2]-2],1),2)
  bz_pad[sz[1]:*,0:sz[2]-1] = reverse(bz[1:sz[1]-2,*],1)
  bz_pad[0:sz[1]-1,sz[2]:*] = reverse(bz[*,1:sz[2]-2],2)
  bz_pad[0:sz[1]-1,0:sz[2]-1] = bz

  Bz_f = fft(Bz_pad)
  temp = kz*(exp(b)-1.)
  temp[0,0] = 1.
  A = Bz_f/temp
  a[0,0] =0;
  return,{A:a,nz:float(nz)}
end




function fft_pot_field_plane,coeff,z
  phi=-1
  ax=-1
  ay=-1
  A =  coeff.a
  nz = coeff.nz
  sz = size(a)
  nx = sz[1]
  ny = sz[2]


  hax=(FINDGEN((Nx - 1)/2) + 1)
  hay=(FINDGEN((Ny - 1)/2) + 1)
  kx = (2.*!pi*[0.0, haX, Nx/2, -Nx/2 + haX]/nx)#replicate(1d,ny)
  ky = (2.*!pi*[0.0, hay, Ny/2, -Ny/2 + hay]/ny)##replicate(1d,nx)
  kz=sqrt(kx^2+ky^2)
  B = -2.*kz*Nz
  kzz = kz*z
  xsz = (nx+2)/2 -1
  ysz = (ny+2)/2 -1
  i = complex(0.,1.)
  kz_=1/kz
  ind=where(kz eq 0)
  kz_(ind)=0
  bx = (fft(i*kx*a*(exp(B+kzz)+exp(-kzz)),/inv))[0:xsz,0:ysz];-ay_z
  by = (fft(i*ky*a*(exp(B+kzz)+exp(-kzz)),/inv))[0:xsz,0:ysz];ax_z
  bz = (fft(a*kz*(exp(B+kzz)-exp(-kzz)),/inv))[0:xsz,0:ysz];+coeff.bz0;ay_x-ax_y
  return,{bx:float(bx),by:float(by),bz:float(bz),phi:float(phi),ax:float(ax),ay:float(ay)}
end

function fft_pot_face,bz0,height
  phi=-1
  ax=-1
  ay=-1
  coeff = fft_pot_decompose(Bz0,height)
  sz = size(bz0)
  nx = sz[1]
  ny = sz[2]
  nz = round(height)+1
  type = size(bz0,/type)
  bx = make_array(nx,ny,nz,type = type)
  by = bx
  bz = bx
  for i = 0,height do begin
    field = fft_pot_field_plane(coeff,i)
    bx[*,*,i] = field.bx
    by[*,*,i] = field.by
    bz[*,*,i] = field.bz
  endfor
  return,{bx:bx,by:by,bz:bz,phi:phi,ax:ax,ay:ay}
end

function fft_pot_compensation,bn
  phi=-1
  ax=-1
  ay=-1
  az=-1
  nx = double(bn.n[0])
  ny = double(bn.n[1])
  nz = double(bn.n[2])
  fxb = fft_pot_flux(bn.xb)
  fxt = fft_pot_flux(bn.xt)
  fyb = fft_pot_flux(bn.yb)
  fyt = fft_pot_flux(bn.yt)
  fzb = fft_pot_flux(bn.zb)
  fzt = fft_pot_flux(bn.zt)

  ;

  ; m = dblarr(5,5)
  m = dblarr(5,6)
  m[0,0] = -(nz-1d)*(ny-1d)
  m[0,1] = (nz-1d)*(ny-1d) & m[3,1] = 2d*(nx-1d)*(ny-1d)*(nz-1d)
  m[1,2] = -(nz-1d)*(nx-1d)
  m[1,3] = (nz-1d)*(nx-1d) & m[4,3] = 2d*(nx-1d)*(ny-1d)*(nz-1d)
  m[2,4] = -(ny-1d)*(nx-1d)

  m[2,5] = (ny-1d)*(nx-1d) & m[3,5] = -2d*(nx-1d)*(ny-1d)*(nz-1d) & m[4,5] = -2d*(nx-1d)*(ny-1d)*(nz-1d)
  b =double([fxb,fxt,fyb,fyt,fzb,fzt])
  ;b =double([fxb,fxt,fyb,fyt,fzb])

  ;c = float(cramer(m,b))
  SVDC, m, W, U, V
  c = SVSOL(U, W, V, B)

  x = rebin(reform(findgen(nx),[nx,1,1]),nx,ny,nz)
  y = rebin(reform(findgen(ny),[1,ny,1]),nx,ny,nz)
  z = rebin(reform(findgen(nz),[1,1,nz]),nx,ny,nz)
  bx = c[0]+c[3]*2.*x
  by = c[1]+c[4]*2.*y
  bz = c[2] - 2.*z*(c[3]+c[4])
  bndc = get_bnd(bx,by,bz)
  bnd = bn
  bnd.xb -= bndc.xb
  bnd.xt -= bndc.xt
  bnd.yb -= bndc.yb
  bnd.yt -= bndc.yt
  bnd.zb -= bndc.zb
  bnd.zt -= bndc.zt
  return, {bx: bx,by: by,bz: bz,bnd: bnd, phi:phi,ax:ax,ay:ay,az:az,bndc:bndc}
end



;+
  ; :Description:
  ;    Calculates potential field matching normal compponents at the boundaries of a given field
  ;
  ; :Params:
  ;    bx_in - dblarr(nx, ny, nz) X - compnent of the magntic field given in a 3D box
  ;    by_in - dblarr(nx, ny, nz) Y - compnent of the magntic field given in a 3D box
  ;    bz_in - dblarr(nx, ny, nz) z - compnent of the magntic field given in a 3D box
  ; 
  ; :Returns:
  ;   Returns  strucure containig calculated potential field components {bx:bx_pot, bx:by_pot, bz:bz_pot}
  ; 
  ; :Usage:
  ;   pot_field = fft_pot(Bx, By, Bz)
  ;
  ; :Author: sergey
  ;-
function fft_pot,bx_in,by_in,bz_in
  bn=get_bnd(bx_in,by_in,bz_in)
  comp = fft_pot_compensation(bn)
  bx = fltarr(bn.n[0],bn.n[1],bn.n[2])
  n=(size(bx))(1:3)
  message,'calculating face 1 of 6...',/info
  field = fft_pot_face(-comp.bnd.zb,bn.n[2]-1d)
  bx = field.bx
  by = field.by
  bz = field.bz
  message,'calculating face 2 of 6...',/info
  field = fft_pot_face(-comp.bnd.zt,bn.n[2]-1d)
  bx += reverse(field.bx,3)
  by += reverse(field.by,3)
  bz += -reverse(field.bz,3)
  message,'calculating face 3 of 6...',/info
  field = fft_pot_face(-comp.bnd.xb,bn.n[0]-1d)
  bx += transpose(field.bz,[2,0,1])
  by += transpose(field.bx,[2,0,1])
  bz += transpose(field.by,[2,0,1])
  message,'calculating face 4 of 6...',/info
  ;
  field = fft_pot_face(-comp.bnd.xt,bn.n[0]-1.)
  bx += -reverse(transpose(field.bz,[2,0,1]),1)
  by += reverse(transpose(field.bx,[2,0,1]),1)
  bz += reverse(transpose(field.by,[2,0,1]),1)
  ;;
  message,'calculating face 5 of 6...',/info
  field = fft_pot_face(-comp.bnd.yb,bn.n[1]-1.)
  bx += transpose(field.bx,[0,2,1])
  by += transpose(field.bz,[0,2,1])
  bz += transpose(field.by,[0,2,1])
  message,'calculating face 6 of 6...',/info
  ;;
  field = fft_pot_face(-comp.bnd.yt,bn.n[1]-1.)
  bx += reverse(transpose(field.bx,[0,2,1]),2)
  by += -reverse(transpose(field.bz,[0,2,1]),2)
  bz += reverse(transpose(field.by,[0,2,1]),2)
  bx +=comp.bx
  by +=comp.by
  bz +=comp.bz


  return,{bx:bx,by:by,bz:bz}



end

