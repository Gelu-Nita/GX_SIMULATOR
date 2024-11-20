function fft_pot_decompose_open,Bz_
  bz = bz_
  sz = size(Bz)
  Nx = sz[1]*2
  ny =  sz[2]*2
  kx = (2.*!pi*[dindgen(nx/2),-reverse(dindgen(nx/2)+1.0)]/nx)#replicate(1d,ny)
  ky = (2.*!pi*[dindgen(ny/2),-reverse(dindgen(ny/2)+1.0)]/ny)##replicate(1d,nx)
  kz = sqrt(kx^2+ky^2)
  Bz_0 = mean(bz)
  
  Bz_pad = dblarr(nx,ny)-Bz_0/3d
  bz_pad[0:sz[1]-1,0:sz[2]-1] = bz
  
  Bz_f = fft(Bz_pad)
  kz[0,0] =1.
  
  A = Bz_f/(-kz)
  a[0,0] =0;
  return,{A:a}
end

function fft_pot_field_plane_open, coeff,z
  A =  coeff.a
  sz = size(a)
  nx = sz[1]
  ny = sz[2]
  kx = (2.*!pi*[findgen(nx/2),-reverse(findgen(nx/2)+1.0)]/nx)#replicate(1.,ny)
  ky = (2.*!pi*[findgen(ny/2),-reverse(findgen(ny/2)+1.0)]/ny)##replicate(1.,nx)
  kz=sqrt(kx^2+ky^2)
  kzz = kz*z
  xsz = (nx)/2-1
  ysz = (ny)/2-1
  i = dcomplex(0.,1.)
  bx = (fft(i*kx*a*exp(-kzz),/inv))[0:xsz,0:ysz]
  by = (fft(i*ky*a*exp(-kzz),/inv))[0:xsz,0:ysz]
  bz =-(fft(a*kz*exp(-kzz),/inv))[0:xsz,0:ysz];+coeff.bz0
  
  sz = size(bz)
  return,{bx:float(bx),by:float(by),bz:float(bz)}
end

function fft_pot_open,bn,nz
  sz = size(bn)
  bx = dblarr(sz[1],sz[2],nz)
  by = bx
  bz = bx
  
  coeff = fft_pot_decompose_open(Bn)
  for i = 0,nz-1 do begin
    field = fft_pot_field_plane_open(coeff,i)
    bx[*,*,i] = field.bx
    by[*,*,i] = field.by
    bz[*,*,i] = field.bz
  endfor
  return,{bx:bx,by:by,bz:bz}
end

;pro test_pot_open
;  bz = dblarr(64,64)
;  bz[30:35,10:15] = 100d
;  bz[30:38,30:38] = -100d
;  bz = ssrt_smooth(bz,5)
;  field = fft_pot_open(bz,64)
;  tvplot,field.bz[*,*,0]
;end
