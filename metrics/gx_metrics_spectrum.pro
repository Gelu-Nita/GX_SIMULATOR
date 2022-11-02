

;+
  ; :Description:
  ;    This procedure quantitatively compared modeled spectrum with the observed one
  ;
  ; :Params:
  ;    data_model - dblarr(n), in,  synthetic MW spectrum to benchmark
  ;    data_obs - dblarr(n), in,  reference observed spectrum to compare with
  ;    data_sdev - dblarr(n), in,  reference observed spectrum standard deviation
  ;
  ; :Keywords:
  ;   range_idx - lonnar, default:lindgen(n_elements(data_mod)), in, set this keyword to compare
  ;       only with selected model points.   
  ;   OR
  ;   range_idx - dblarr(2), set to the desired data range for which the comparison should be performed
  ;   
  ;   n_free - number of degrees of freedom, default 0, to be used to compute CHI2 metrics
  ;       
  ; :Return value:
  ;     The routine returns a structure with the following fields:
  ;       R - Pearson correlation coefficient
  ;       res_spec= data_model - data_obs
  ;       res= total(res_spec[mask_idx])
  ;       res_spec_norm=res_spec/data_obs
  ;       res_norm=total(res_spec_norm[mask_idx])/n_mask_idx
  ;       res2_spec=res_spec^2
  ;       res2=total(res2_spec[mask_idx])-res^2/n_mask_idx 
  ;       res2_spec_norm=res_spec_norm^2
  ;       res2_norm=total(res2_spec_norm[mask_idx])-res_norm^2
  ;       
  ;       
  ;       chi_spec=res_spec/data_sdev
  ;       chi=total(chi_spec[mask_idx])/n_mask_idx       
  ;       chi2_spec=chi_spec^2
  ;       chi2=total(chi2_spec[mask_idx])/(n_mask_idx-n_free)-chi^2        
  ;
  ; :Author: Gelu Nita (gnita@njit.edu) 7/26/20
  ;  Modification history:
  ;  

function gx_metrics_spectrum, data_model, data_obs, data_sdev,range_idx=range_idx,n_free=n_free
  
  if ~isa(data_model,/array) or ~isa(data_obs,/array) then begin
    message, 'Model and Observational Data must be array variables',/info
    return,!null
  endif
 
 spec_mask=bytarr(n_elements(data_model))
 
 if ~isa(range_idx,/array) then range_idx = lindgen(n_elements(data_model))
 
 if n_elements(range_idx) eq 2 then begin
  if isa(range_idx,/float) then  begin
    idx=where(data_model ge min(range_idx,max=max_idx) and data_model le max_idx,count)
    if count gt 0 then spec_mask[idx]=1
  endif else spec_mask[min(range_idx,max=max_range)>0:max_range<(n_elements(data_model)-1)]=1
 endif else spec_mask[range_idx>0<(n_elements(data_model)-1)]=1

  ;Convert input data to double precision floats
  data_model_d = double(data_model)
  data_obs_d = double(data_obs)
  
  if isa(data_sdev) then begin
    if (n_elements(data_model) ne n_elements(data_sdev)) then begin
     message,'Provided SDEV data, not matching model and data array sizes, will be ignored!',/info 
    endif else data_sdev_d=double(data_sdev)
  endif
  
  mask_idx = where(spec_mask,complement=bad,ncomp=nbad)
  n_mask_idx = total(spec_mask)
   
  R = correlate(data_model_d*spec_mask, data_obs_d*spec_mask)     
         res_spec= data_model_d - data_obs_d
         res= total(res_spec[mask_idx])
         res_spec_norm=res_spec/data_obs_d
         res_norm=total(res_spec_norm[mask_idx])/n_mask_idx
         res2_spec=res_spec^2
         if nbad gt 0 then res2_spec[bad]=0
         res2=total(res2_spec[mask_idx])/n_mask_idx-res^2/n_mask_idx
         res2_spec_norm=res_spec_norm^2
         res2_norm=total(res2_spec_norm[mask_idx])/n_mask_idx
 metrics={R:R,$
          mask_spec:spec_mask,$
          res_spec:res_spec,$
          res:res,$
          res_spec_norm:res_spec_norm,$
          res_norm:res_norm,$
          res2_spec:res2_spec,$
          res2:res2,$
          res2_spec_norm:res2_spec_norm,$
          res2_norm:res2_norm}
 if isa(data_sdev_d) then begin
         default,n_free,0
         chi_spec=res_spec/data_sdev_d
         if nbad gt 0 then chi_spec[bad]=0
         chi=total(chi_spec[mask_idx])/n_mask_idx
         chi2_spec=chi_spec^2
         if nbad gt 0 then chi2_spec[bad]=1
         chi2=total(chi2_spec[mask_idx])/(n_mask_idx-n_free)
 chi_metrics={$
         chi_spec:chi_spec,$
         chi:chi,$
         chi2_spec:chi2_spec,$
         chi2:chi2}
 metrics=create_struct(metrics,chi_metrics)        
 endif         
 return,metrics
end