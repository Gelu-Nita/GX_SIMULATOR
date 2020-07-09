;+
; :Description:
;    This procedure quantitatively compared modeled image with the observed one
;
; :Params:
;    map - map structure, in,  synthetic MW image to benchmark
;    reference - map structure, in,  reference (observed) map to compare with
;
  ; :Params:
  ;    data_model - dblarr(nx, ny), in,  synthetic MW image to benchmark
  ;    data_obs - dblarr(nx, ny), in,  reference observed image to compare with
  ;    data_sdev - dblarr(nx, ny), in,  reference observed image standard deviation
  ;
  ; :Keywords:
  ;    mask - double, default:0d, in, set this keyword to analize
  ;       only pixels with the brightness above some threshold.
  ;       Setting "mask=0.1d" will result in processing only pixels
  ;       with the brightness above 10% of the maximum brigntness values in the reference image
  ;   OR
  ;   mask - bytarr(nx,ny) with ones indicating the area of interest pixels
  ;          to be used for computing the metrics
  ;   
  ;   OR
  ;   mask - lonarr(n_pix) array indices indicating the area of interest pixels 
  ;          to be used for computing the metrics
  ;          
  ;   n_free - number of degrees of freedom, default 0, to be used to compute CHI2 metrics

; :Return value:
  ;     The routine returns a structure with the following fields:
  ;       cor - Pearson correlation coefficient
  ;       res_img= data_model - data_obs
  ;       res= total(res_img[mask_pix])
  ;       res_img_norm=res_img/data_obs
  ;       res_norm=total(res_img_norm[mask_pix])/n_mask_pix
  ;       res2_img=res_img^2
  ;       res2=total(res2_img[mask_pix])-res^2/n_mask_pix 
  ;       res2_img_norm=res_img_norm^2
  ;       res2_norm=total(res2_img_norm[mask_pix])-res_norm^2
  ;       
  ;       
  ;       chi_img=res_img/data_sdev
  ;       chi=total(chi_img[mask_pix])/n_mask_pix       
  ;       chi2_img=chi_img^2
  ;       chi2=total(chi2_img[mask_pix])/(n_mask_pix-n_free)        
  ;
;
; :Author: Sergey Anfinopgentov (anfinogentov@iszf.irk.ru)
  ;  Modification history:
  ;  07/08/20-Gelu Nita (gnita@njit.edu) Redefined metrics and addded the option of using SDEV maps

function gx_metrics_map, map, reference, sdev,_extra=_extra
  ;align map
  gx_align_map, map, reference
  
  ;Interpolate reference map (which is assumed to have broader FOV than the synthetic image)
  map_ref = inter_map(reference, map)
  if valid_map(sdev) then begin
  ;If provided, interpolate sdev map (which is assumed to have broader FOV than the synthetic image)
    map_sdev = inter_map(sdev, map)
    data_sdev=map_sdev.data
  endif
  
  metrics = gx_metrics_image(map.data, map_ref.data,data_sdev, _extra=_extra)
  omap=obj_new('map')
  amap=map
  k=0
  
  amap.id=string(map.xc-map.orig_xc,map.yc-map.orig_yc,format="('ALLIGNED [',f5.2,',',f5.2,'] ')")+map.id
  amap.data=map.data
  omap->setmap,k++,amap
  
  amap.id='REMAPPED '+map_ref.id
  amap.data=map_ref.data
  omap->setmap,k++,amap
  
  if valid_map(map_sdev) then begin
    amap.id='REMAPPED '+map_sdev.id
    amap.data=map_sdev.data
    omap->setmap,k++,amap 
  endif
  
  if tag_exist(metrics,'mask_img') then begin
    amap.id=string(total(metrics.mask_img),n_elements(metrics.mask_img),format="('ROI MASK (',g0,'/',g0,' pixels)')")
    amap.data=metrics.mask_img
    omap->setmap,k++,amap
  endif
  
  if tag_exist(metrics,'res_img') then begin
    amap.data=metrics.res_img
    add_prop,amap,roi_metrics=metrics.res,/replace
    amap.id=string(total(amap.roi_metrics),format="('RESIDUAL (',g0,')')")
    omap->setmap,k++,amap
  endif
  
  if tag_exist(metrics,'res_img_norm') then begin
    amap.data=metrics.res_img_norm
    add_prop,amap,roi_metrics=metrics.res_norm,/replace
    amap.id=string(total(amap.roi_metrics),format="('NORMALIZED RESIDUAL (',g0,')')")
    omap->setmap,k++,amap
  endif
  if tag_exist(metrics,'res2_img') then begin
    amap.data=metrics.res2_img
    add_prop,amap,roi_metrics=metrics.res2,/replace
    amap.id=string(total(amap.roi_metrics),format="('SQUARED RESIDUAL (',g0,')')")
    omap->setmap,k++,amap
  endif
  if tag_exist(metrics,'res2_img_norm') then begin
    amap.data=metrics.res2_img_norm
    add_prop,amap,roi_metrics=metrics.res2_norm,/replace
    amap.id=string(total(amap.roi_metrics),format="('NORMALIZED SQUARED RESIDUAL (',g0,')')")
    omap->setmap,k++,amap
  endif
  if tag_exist(metrics,'chi_img') then begin
    amap.data=metrics.chi_img
    add_prop,amap,roi_metrics=metrics.chi,/replace
    amap.id=string(total(amap.roi_metrics),format="('CHI (',g0,')')")
    omap->setmap,k++,amap
  endif
  if tag_exist(metrics,'chi2_img') then begin
    amap.data=metrics.chi2_img
    add_prop,amap,roi_metrics=metrics.chi2,/replace
    amap.id=string(total(amap.roi_metrics),format="('CHI2 (',g0,')')")
    omap->setmap,k++,amap
  endif
  return, omap
end