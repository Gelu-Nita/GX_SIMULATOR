;+
; :Description:
;    Finds maximum position with subpixel resolution
;
;
; :Params:
;    var: 1d or 2d array
;    coord: coordinate
;
;
;
; :Author: Sergey Anfinogentov (email: anfinogentov@iszf.irk.ru)
;-
Function AR_maxi,var,coord
  si=size(var)
  if si(0) eq 1 then begin
    n=si(1)
    foo=max(var,xm)
    xst=xm-10
    xen=xm+10
    if xst lt 0 then xst=0
    if xen ge n then xen=n-1
    ivar= Congrid(var(xst:xen),(xen-xst+1)*10,cubic=-0.5)
    res=max(ivar,xmi)
    coord=float(xst)+xmi/10.
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
    snx=(xen-xst+1)*10
    sny=(yen-yst+1)*10
    ivar= Congrid(var(xst:xen,yst:yen),snx,sny,cubic=-0.5)
    res=max(ivar,ind)
    xmi=ind mod snx
    ymi=floor(ind/float(snx))
    coord=[float(xst)+xmi/10.,float(yst)+ymi/10.]
    return,res
  endif
end

;+
  ; :Description:
  ;    Extract a subimage from in_map matching  the reference map FOV with the use of X and Y shifts
  ;
  ; :Params:
  ;    in_map - a map structure where the subimage will be extracted from
  ;    ref_map - a reference map
  ;    shift_x - X shift
  ;    shift_y - Y shift
  ;
  ;
  ;
  ; :Author: Sergey Anfinogentov (email: anfinogentov@iszf.irk.ru)
  ;-
function AR_image2ref,in_map,ref_map,shift_x,shift_y
  compile_opt idl2
  if not keyword_set(shift_x) then shift_x =0d
  if not keyword_set(shift_y) then shift_y =0d
  x_model = get_map_xp(ref_map)
  y_model = get_map_yp(ref_map)

  x_obs = get_map_xp(in_map,/oned)
  y_obs = get_map_yp(in_map,/oned)
  sz = size(in_map.data)
  c_x = linfit(x_obs + shift_x,dindgen(sz[1]))
  c_y = linfit(y_obs+ shift_y,dindgen(sz[2]))

  x_sub = c_x[0] + c_x[1]*x_model
  y_sub = c_y[0] + c_y[1]*y_model
  return,interpolate(in_map.data,x_sub,y_sub,cubic = -0.5)
end


;+
  ; :Description:
  ;    Alignes input map to match a reference map with sub-pixel accuracy.
  ;    Maps can be fo differnet FOV and resolution. The input map is assumed to have larger FOV
  ;    
  ;
  ; :Params:
  ;    in_map - map structer to be aligned to the reference map. The map position (xc and yc) will be overwritten
  ;    ref_map - reference map structer
  ;    width - integer number, representing maximal offset
  ;
  ;
  ;
  ; :Author: Sergey Anfinogentov (email: anfinogentov@iszf.irk.ru)
  ;-
pro  ar_align_map, in_map, ref_map, width
  compile_opt idl2
  if keyword_set(width) then w = width else w = 20
  cor = dblarr(2*w+1,2*w+1)
  img_ref = ref_map.data
  message,'aligning...',/info
  for x = -w, w do begin
    for y = -w,w do begin
      img_in= AR_image2ref(in_map, ref_map,double(x), double(y))
      cor[x+w, y+w] = correlate(img_in,img_ref)
    endfor
    ;message,'calculating cross-correlation: '+strcompress(x)+ ' of ['+ strcompress(-w,/remove_all)+","+strcompress(w,/remove_all)+']',/info
  endfor
  cor_coeff = ar_maxi(cor,xy)
  xy -= w
  in_map.xc += xy[0]
  in_map.yc += xy[1]

  message, 'x offset:'+strcompress(xy[0]), /info
  message, 'y offset:'+strcompress(xy[1]),/info
  message, 'correlation coefficient:'+strcompress(cor_coeff),/info
end