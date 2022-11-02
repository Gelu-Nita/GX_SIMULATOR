;+
; Description:
;    Replicates ascalar or add up to 8 dimensions to an array by replicating the values of the input array or scalar
;    Dimensions to be added may be provided as a list, an array, or a combination of list and array
; Use:
;   new_array=arry_replicate(old_array,dim1,[.., dim8])
;   new_array=arry_replicate(old_array,[dim1, ...,dim8])
; Note: If the requested number of dimensions is lager than 8, 
;        or a non vald input is provided, an undifined variable is returned  
;
; Author: Gelu M Nita, May 2021 (email: gnita@njit.edu)
;-
function array_replicate,arr,dim1,dim2,dim3,dim4,dim5,dim6,dim7,dim8
  catch, error_status
  if error_status ne 0 then begin
      message, !error_state.msg,/info
      return,[]
  endif
  dim=[]
  for k=1,n_params()-1 do void=execute(string(k,"('dim=[dim,dim',i0,'[*]]')"))
  arrdim=size(arr,/dim)
  return,(n_elements(arr) eq 1)?reform(replicate(arr,dim),dim):reform(arr[*]#replicate(1,product(dim)),[arrdim,dim])
end
