function fastccorrelate2,im1,im2
  si=size(im1)
  nx=si[1]
  ny=si[2]
  m=total(im1)/(nx*ny)
  imm1=im1-m
  m=total(im2)/(nx*ny)
  imm2=im2-m
  res=double(fft(fft(imm1,-1)*conj(fft(imm2,-1)),+1))
  norm=sqrt(total(imm1^2))*sqrt(total(imm2^2))
  ;norm=norm/(1.-findgen(nt)/float(nt))
  res=(nx*ny)*res;/norm
  res/=norm
  return,res
end

;+
; :Description:
;    This function finds shifts between two images of the same size by maximazing correlation coefficient
;
; :Params:
;    image :  in/out, image to align to a reference onr
;    ref_image : in, reference image
;
; :Keywords:
;    replace : set this keyword to replace the IMAGE parameter by its aligned version
; 
; :Return value: , dblarr(2)
;   The function returns shifts between two images as a 2-element array
; 
; :Author: Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
; 23-Nov-2022 Gelu Nita (gnita@njit.edu): changed floatto double in fascorrelate2
;-
function gx_align_image, image, ref_image, replace = replace

  accuracy=0.1

  k=1.0/accuracy
  nx=(size(image))(1)
  ny=(size(image))(2)
  cor=fastccorrelate2(double(image), double(ref_image))
  zero_x = (nx-1)/2
  zero_y = (ny-1)/2
  cor=shift(cor,zero_x,zero_y)
  correlation=max(cor,mai)
  ym=floor(mai /nx)
  xm=(mai mod nx)
  x_k = linspace(0.,nx-1, nx*k)
  t = interpolate(reform(cor[*,ym]),x_k,cubic = -0.5)
  foo=max(t,mai)
  xc=x_k[mai]

  y_k = linspace(0.,ny-1, ny*k)
  t = interpolate(reform(cor[xm,*]),y_k,cubic = -0.5)
  foo=max(t,mai)
  yc=y_k[mai]

  dx = xc - zero_x
  dy = yc - zero_y

  if keyword_set(replace) then begin
    x = findgen(nx) + dx
    y = findgen(ny) + dy
    image = interpolate(image,x,y,cubic = -0.5,/grid)
  endif
  return,[dx,dy]


end