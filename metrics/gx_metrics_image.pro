

;+
  ; :Description:
  ;    This procedure quantitatively compared modeled image with the observed one
  ;
  ; :Params:
  ;    data_model - dblarr(nx, ny), in,  synthetic MW image to benchmark
  ;    data_obs - dblarr(nx, ny), in,  reference observed image to compare with
  ;    data_sdev - dblarr(nx, ny), in,  reference observed image standard deviation
  ;
  ; :Keywords:
  ;   mask - double, default:0d, in, set this keyword to analize
  ;       only pixels with the brightness above some threshold.
  ;       Setting "mask=10d" will result in processing only pixels
  ;       with the brightness above 10% of the maximum brigntness values in the reference image
  ;   apply2- byte indicating how the mask should be applied
  ;         0 no mask
  ;         1 only pixels where data_obs is about the mask threshold 
  ;         2 only pixels where data_model is about the mask threshold  
  ;         3 only pixels where either data_obs or data_model are above the mask threshold (default)
  ;         4 only pixels where both data_obs and data_model are above the mask threshold      
  ;   OR
  ;   mask - bytarr(nx,ny) with ones indicating the area of interest pixels
  ;          to be used for computing the metrics
  ;   
  ;   OR
  ;   mask - lonarr(n_pix) array indices indicating the area of interest pixels 
  ;          to be used for computing the metrics
  ;   
  ;   n_free - number of degrees of freedom, default 0, to be used to compute CHI2 metrics
  ;       
  ; :Return value:
  ;     The routine returns a structure with the following fields:
  ;       R - Pearson correlation coefficient
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
  ;       chi2=total(chi2_img[mask_pix])/(n_mask_pix-n_free)-chi^2        
  ;
  ; :Author: Sergey Anfinopgentov (anfinogentov@iszf.irk.ru)
  ;  Modification history:
  ;  07/08/20-Gelu Nita (gnita@njit.edu) Redefined metrics and addded the option of using SDEV images

function gx_metrics_image, data_model, data_obs, data_sdev,mask=mask,apply2=apply2,n_free=n_free
  if ~isa(data_model) or ~isa(data_obs) then begin
    message, 'Model Data, Observational Data, or both, not provided',/cont
    return,!null
  endif
  
  if ~isa(data_model,/array) or ~isa(data_obs,/array) then begin
    message, 'Model and Observational Data must be array variables',/cont
    return,!null
  endif
  
  if ~array_equal((size(data_model))[0:2],(size(data_obs))[0:2]) then begin
    message, 'Model and Observational Data must be arrays of equal size!',/cont
    return,!null
  endif
  
  if isa(mask) then begin
    if n_elements(mask) eq 1 then begin
      ;this is assumed to be a brightness threshold provided as a pecentage, so the image mask must be computed
      default,apply2,3
      img_mask=byte(data_obs*0)
      data_mask=data_obs gt (mask * max(data_obs)/100d)
      model_mask=data_model gt (mask * max(data_model)/100d)
      case apply2 of
        1: img_mask=data_mask
        2: img_mask=model_mask
        3: img_mask=data_mask or model_mask
        4: img_mask=data_mask and model_mask
        else: img_mask[*]=1;nomask
      endcase
      
    endif else begin
      if array_equal(size(img_mask),size(data_obs)) then begin
        ;this is assumed to be an already precompute image mask
        img_mask=mask
      endif else begin
        if size(mask,/tname) eq 'LONG' then begin
          ;this is assumed to be an array of image mask pixel indices
          if min(mask,max=max_index) ge 0 and max_index lt n_elements(data_obs) then begin
            img_mask=byte(data_obs*0)
            img_mask[mask]=1b
          endif else begin
            message,'Provided mask indices are incompatible with modell and data array sizes, no mask will be applied!',/cont
          endelse
        endif
      endelse
    endelse
  endif else begin
    img_mask=byte(data_obs*0)+1b
  endelse
  
  ;Convert input data to double precision floats
  data_model_d = double(data_model)
  data_obs_d = double(data_obs)
  
  if isa(data_sdev) then begin
    if ~array_equal(size(data_model),size(data_sdev)) then begin
     message,'Provided SDEV data, not matching model and data array sizes, will be ignored!',/cont 
    endif else data_sdev_d=double(data_sdev)
  endif
  
  mask_pix = where(img_mask,complement=bad,ncomp=nbad)
  n_mask_pix = total(img_mask)
   
  R = correlate(data_model_d*img_mask, data_obs_d*img_mask)     
         res_img= data_model_d - data_obs_d
         if nbad gt 0 then res_img[bad]=0
         res= total(res_img[mask_pix])
         res_img_norm=res_img/data_obs_d
         if nbad gt 0 then res_img_norm[bad]=1
         res_norm=total(res_img_norm[mask_pix])/n_mask_pix
         res2_img=res_img^2
         if nbad gt 0 then res2_img[bad]=0
         res2=total(res2_img[mask_pix])/n_mask_pix-res^2/n_mask_pix
         res2_img_norm=res_img_norm^2
         if nbad gt 0 then res2_img_norm[bad]=1
         res2_norm=total(res2_img_norm[mask_pix])/n_mask_pix-res_norm^2
 metrics={R:R,$
          mask_img:img_mask,$
          res_img:res_img,$
          res:res,$
          res_img_norm:res_img_norm,$
          res_norm:res_norm,$
          res2_img:res2_img,$
          res2:res2,$
          res2_img_norm:res2_img_norm,$
          res2_norm:res2_norm}
 if isa(data_sdev_d) then begin
         default,n_free,0
         chi_img=res_img/data_sdev_d
         if nbad gt 0 then chi_img[bad]=0
         chi=total(chi_img[mask_pix])/n_mask_pix
         chi2_img=chi_img^2
         if nbad gt 0 then chi2_img[bad]=1
         chi2=total(chi2_img[mask_pix])/(n_mask_pix-n_free)-chi^2
 chi_metrics={$
         chi_img:chi_img,$
         chi:chi,$
         chi2_img:chi2_img,$
         chi2:chi2}
 metrics=create_struct(metrics,chi_metrics)        
 endif         
 return,metrics
end