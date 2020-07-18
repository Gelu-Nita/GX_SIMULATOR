function implot_linspace, x1, x2, n
compile_opt idl2
  return, (dindgen(n)/ n) * (x2 - x1) + x1
end

function implot_logspace, x1, x2, n
compile_opt idl2
  if x1 le 0 or x2 le 0 then message,'Both limits must be positive'
  dx = (double(x2)/double(x1))^(1d/(n-1d))
  return, product([x1,replicate(dx,n-1)], /cumulative)
end

;+
  ; :Description:
  ;    Displays an image with the coordinate axes. Works similar to the CONTOUR routine
  ;
  ; :Params:
  ;    image  - image to display
  ;    x      - A vector representing the X-coordinate values to be plotted. If X is not specified,
  ;             it is calculated as a point number (starting at zero for linear scale and at one for log scale)
  ;    y      - A vector representing the Y-coordinate values to be plotted. If Y is not specified,
  ;             it is calculated as a point number (starting at zero for linear scale and at one for log scale)
  ;
  ; :Keywords:
  ;    xlog       - plot X axis in log scale. The image will ve interpolated to much the axis.
  ;    ylog       - plot Y axis in log scale. The image will ve interpolated to much the axis.
  ;    xrange     - The desired data range of the X axis, a 2-element vector
  ;    yrange     - The desired data range of the Y axis, a 2-element vector
  ;    sample     - Set this keyword to use nearest neighbour interpolation
  ;    resolution - required resolution (minimal image size) for the Post Script output, default 500
  ;    missing    - value to fill missing pixels during interpolation
  ;    _extra     - Most of the Direct Graphics keywords are supported
  ;
  ; :Author: Sergey Anfinogentov (segey.istp@gmail.com)
  ;-
function imploto, image, x, y, xlog = xlog, ylog = ylog, xrange = xrange, yrange = yrange,$
        sample = sample, _extra =_extra, missing = missing, resolution = resolution,$
        max_value = max_value, min_value =min_value, keep_aspect = keep_aspect
compile_opt idl2

  sz = size(image)
  nx = sz[1]
  ny = sz[2]
  

  if not keyword_Set(x) then begin
    x = dindgen(nx)
    if keyword_set(xlog) then x += 1d
  endif
  if not keyword_Set(y) then begin
    y = dindgen(ny)
    if keyword_set(ylog) then y += 1d
  endif
  
  if not keyword_set(xrange)      then xrange = minmax(x)
  if not keyword_set(yrange)      then yrange = minmax(y)
  if not keyword_set(resolution)  then resolution = 500l
  
    
  p = plot(y,  /nodata, _extra = _extra, $
    xrange = xrange, yrange = yrange, xlog = xlog, ylog = ylog,axis_Style =2)
  
  if keyword_Set(xlog) then xr = alog10(xrange) else xr = xrange  
  if keyword_Set(ylog) then yr = alog10(yrange) else yr = yrange 
  
  ;xr = xrange
  ;yr = yrange
  
  
  crd = p.ConvertCoord(xrange,yrange,/data,/to_device)
  
  Nx_pix = round(crd[0,1] -crd[0,0])
  Ny_pix = round(crd[1,1] -crd[1,0])
  
  nx_pix *=2 ;double the resolution
  ny_pix *=2
  
  if keyword_set(xlog) then begin
    x_out = implot_logspace(xrange[0], xrange[1], nx_pix)
  endif else begin
    x_out = implot_linspace(xrange[0], xrange[1], nx_pix)
  endelse
  
  if keyword_set(ylog) then begin
    y_out = implot_logspace(yrange[0], yrange[1], ny_pix)
  endif else begin
    y_out = implot_linspace(yrange[0], yrange[1], ny_pix)
  endelse
  
  x_out_pix = interpol(dindgen(nx),x, x_out);, /spline)
  y_out_pix = interpol(dindgen(ny),y, y_out);, /spline)
  
  if keyword_Set(sample) then begin
    x_out_pix = round(x_out_pix)
    y_out_pix = round(y_out_pix)
  endif
  
   
  img = interpolate(image, x_out_pix, y_out_pix, cubic = -0.5, /grid, missing = missing)
  ;p.delete
;  im = image(img, image_location =[x_out[0],y_out[0]], image_dimensions = [xrange[1]-xrange[0],yrange[1]-yrange[0]],$
;  /overplot, _extra = _extra)

   if keyword_set(keep_aspect) then begin
      aspect_ratio = (xr[1]-xr[0])/(xrange[1]-xrange[0])/((yr[1]-yr[0])/(yrange[1]-yrange[0]))
   endif else begin
    aspect_ratio = (xr[1]-xr[0])/(yr[1]-yr[0])*ny_pix/nx_pix
   endelse

   
   im = image(img, image_location =[xr[0],yr[0]], image_dimensions = [xr[1] - xr[0],yr[1]-yr[0]],$
  /overplot, _extra = _extra,xlog = xlog, ylog = ylog,aspect_ratio = aspect_ratio,$
  max_value = max_value, min_value =min_value)
  im.order, /SEND_TO_BACK
  
  
  return,im

  
  ;tvscl, img, !d.x_size*!x.window[0],  !d.y_size*!y.window[0], xsize = xsize, ysize = ysize
  
  ;!P.multi = Pmsave
  ;contour, img, x_out, y_out, xlog = xlog, ylog = ylog, xrange = xrange, yrange = yrange,$
 ;   /nodata, /noerase, _extra =_extra, xst = 1, yst =1
  
  


end
