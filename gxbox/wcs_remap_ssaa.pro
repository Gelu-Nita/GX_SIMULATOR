;+
  ; :Description:
  ;    Calculate Lanczos function.
  ;
  ; :Params:
  ;    x - independend variable
  ;
  ; :Keywords:
  ;    a - parameter of the Lanczos function
  ;
  ; :Author: Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
  ;-
function lanczos,x, a = a
  if not keyword_set(a) then  a = 2d
  ind1 = where(x eq 0)
  result = x*0d
  if ind1[0] ne -1 then  result[ind1] = 1d
  ind = where((x ge -a) and (x lt a) and (x ne 0))
  if ind[0] ne -1 then result[ind] = a*sin(!dpi*x[ind])*sin(!dpi*x[ind]/a)/(!dpi^2*x[ind]^2)
  return, result
end
;+
  ; :Description:
  ;    Remaps data from one World Coordinate System (WCS) to another
  ;    using super sampling techique to avoid aliasing artefacts
  ;
  ; :Params:
  ;    data_from - data to be remapped
  ;    wcs_from - WCS structure describing coordinate system of the given
  ;    wcs_to - WCS structure describing coordinate system where the data will be remapped to
  ;    
  ; :Keywords:
  ;    sample - set to use nearest neighbour interpolation instead of the cubic one
  ;
  ;
  ; :Author: Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
  ;-
function wcs_remap_ssaa, data_from, wcs_from, wcs_to, sample = sample, missing = missing
  nx = wcs_to.naxis[0]
  ny = wcs_to.naxis[1]
  a = 2d
  ns = 8
  n = ns^2
  dx = gx_box_linspace(-1d,1d,ns)#replicate(1d,ns)
  dy = gx_box_linspace(-1d,1d,ns)##replicate(1d,ns)
  dx*=a
  dy*=a
  w = lanczos(dx,a=a)*lanczos(dy,a=a)
  w /= total(w)
  
  result = dblarr(nx,ny)
  
  for i = 0, n-1 do begin
    pix_to = dblarr(2,nx,ny)
    pix_to[0,*,*] = gx_box_linspace(0d,nx-1,nx) #  replicate(1d,ny) + dx[i]
    pix_to[1,*,*] = gx_box_linspace(0d,ny-1,ny) ## replicate(1d,nx) + dy[i]
    cc_to = wcs_get_coord(wcs_to,pix_to)
    wcs_convert_from_coord,wcs_to,cc_to,'HG', lon, lat, /carrington
    wcs_convert_to_coord,wcs_from,cc_from,'HG', lon, lat, /carrington
    pix = wcs_get_pixel(wcs_from, cc_from)
    result += w[i] * reform(interpolate(data_from,pix[0,*,*], pix[1,*,*], missing = missing)) 
  endfor

  return, result
end