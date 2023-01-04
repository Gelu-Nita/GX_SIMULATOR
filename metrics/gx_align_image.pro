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
;    Finds maximum position with the resolution of 0.1 pixel
;    in assumption that values are given at the pixel centres
; 
;
;
; :Params:
;    var  : in, 1d or 2d array
;    coord: out, coordinate
;
; :Return value: [x_max, y_max] or x_max
;   The function return 1d (for 1d input) or 2d (for 2d input)
;   coordinates of the maximum in the input array
;
;
; :Author: Sergey Anfinogentov (email: anfinogentov@iszf.irk.ru)
; 28-Dec-2022 Sergey Anfinogentov (anfinogentov@iszf.irk.ru): Added as
;             axilary function  for gx_align_image
;-
Function gx_align_image_maxi,var,coord
  si=size(var)
  if si(0) eq 1 then begin
    n=si(1)
    foo=max(var,xm)
    xst=xm-10
    xen=xm+10
    if xst lt 0 then xst=0
    if xen ge n then xen=n-1
    ivar= Congrid(var(xst:xen),(xen-xst)*10+1,cubic=-0.5,/minus_one)
    res=max(ivar,xmi)
    coord=float(xst)+xmi/10. + 0.5
    return,res
  endif
  if si(0) eq 2 then begin
    nx=si(1)
    ny=si(2)
    foo=max(var,ind)
    xm=ind mod nx
    ym=floor(ind/float(nx))
    xst=xm-10
    xen=xm+10
    yst=ym-10
    yen=ym+10
    if xst lt 0 then xst=0
    if xen ge nx then xen=nx-1
    if yst lt 0 then yst=0
    if yen ge ny then yen=ny-1
    snx=(xen-xst)*10 + 1
    sny=(yen-yst)*10 + 1
    ivar= Congrid(var(xst:xen,yst:yen),snx,sny,cubic=-0.5,/minus_one)
    res=max(ivar,ind)
    xmi=ind mod snx
    ymi=floor(ind/float(snx))
    coord=[float(xst)+xmi/10.+ 0.5,float(yst)+ymi/10.+ 0.5]
    return,res
  endif
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
; 28-Dec-2022 Sergey Anfinogentov (anfinogentov@iszf.irk.ru): fixing incorrect determination
;             of the cross-correlation maximum
;-
function gx_align_image, image, ref_image, replace = replace

  nx=(size(image))(1)
  ny=(size(image))(2)
  cor=fastccorrelate2(double(image), double(ref_image))
  zero_x = (nx-1)/2
  zero_y = (ny-1)/2
  cor=shift(cor,zero_x,zero_y)
  correlation=max(cor,mai)
  
  maxval = gx_align_image_maxi(cor, maxpos)
  
  ; If there is zero shift between images, maximum of crosscorrelation function
  ;  will appear at the center of pixel [0,0]
  ; that corresponds to (x=0.5px, y=0.5px). Therefore we subtract 0.5px from the shifts. 
  dx = maxpos[0] - zero_x - 0.5
  dy = maxpos[1] - zero_y - 0.5

  if keyword_set(replace) then begin
    x = findgen(nx) + dx
    y = findgen(ny) + dy
    image = interpolate(image,x,y,cubic = -0.5,/grid)
  endif
  return,[dx,dy]


end