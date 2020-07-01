;;;;;;;;;;;;;;;;;;;;;;;;
;;;   linspace.pro   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;  Author: wd (Wolfgang.Dobler@ncl.ac.uk)
;;;  Date:   21-Jun-2001
;;;  Version: 0.35 (CVS: $Revision: 1.3 $)
;;;  Description:
;;;     Return a real vector of length N, containing equidistant values
;;;   between x1 and x2 inclusively.
;;;   If N is omitted, a value of 100 is adopted.
;;;     Mimics the octave function `linspace'.
;;;     Allows for the first 2 arguments to be written as a vector,
;;;   so `linspace(minmax(v),10)' works.
;;;  Keywords:
;;;   PERIODIC  -- flag for periodic grid (i.e. x[n-1]=x2-dx)
;;;   GHOST     -- set this to the number of ghost cells before x1 or
;;;                after x2; GHOST can be a 2-element array
;;;                [left,right] or a scalar (applied to both sides)
;;;   UNIQUE    -- flag for returning a list of unique elements.
;;;                This implies that you may get less than N elements
;;;                (in many cases just one).
;;;                Useful if you call
;;;                  contour,..,LEVELS=linspace(minmax(var),N)
;;;                where no repeated levels are allowed

function linspace, x1, x2, n, $
  PERIODIC=peri, $
  GHOST=ghost, $
  UNIQUE=unique
  on_error,2

  if (n_elements(ghost) eq 0) then begin
    nghost = [0,0]
  endif else begin
    nghost = rebin([ghost], 2)  ; replicate if necessary
  endelse
  if (keyword_set(peri)) then nghost=[0,-1]
  default, unique, 0

  if (n_elements(x1) ge 2) then begin ; Reshuffle arguments
    xx1 = x1[0]
    xx2 = x1[1]
    if (n_elements(x2) ne 0) then nn = x2
  endif else begin
    xx1 = x1
    xx2 = x2
    if (n_elements(n) ne 0) then nn = n
  endelse
  default, nn, 100
  n_real = nn - nghost[0] - nghost[1]
  list = xx1 + (findgen(nn)-nghost[0])*(xx2-xx1)/(n_real-1)

  if (unique) then list = list(uniq(list))

  return, list
end
; End of file linspace.pro
function fastccorrelate2,im1,im2
  si=size(im1)
  nx=si[1]
  ny=si[2]
  m=total(im1)/(nx*ny)
  imm1=im1-m
  m=total(im2)/(nx*ny)
  imm2=im2-m
  res=float(fft(fft(imm1,-1)*conj(fft(imm2,-1)),+1))
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
;-
function gx_align_image, image, ref_image, replace = replace

  accuracy=0.1

  k=1.0/accuracy
  nx=(size(image))(1)
  ny=(size(image))(2)
  cor=fastccorrelate2(image, ref_image)
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
  ;dxdy=[-dx,-dy]
  ;print,xc,yc
  if keyword_set(replace) then begin
    x = findgen(nx) + dx
    y = findgen(ny) + dy
    image = interpolate(image,x,y,cubic = -0.5,/grid)
  endif
  return,[dx,dy]


end