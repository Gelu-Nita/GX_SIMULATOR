function anis_factor_arr, f_arr,e_arr,mu_arr,bz
d_fraction=0.5
if isa(f_arr,/array) and isa(e_arr,/array) and isa(mu_arr,/array) then begin
  sz=size(f_arr)
  n_e=sz[1]
  n_mu=sz[2]
  if n_e eq n_elements(e_arr) and n_mu eq n_elements(mu_arr) then begin
    int_e=dblarr(n_mu)
    loge=alog(e_arr)
    logf_arr=alog(f_arr>min(f_arr[where(f_arr)]))
    for i=0,n_mu-1 do begin
      int_e[i]=int_trapzdLog(loge, logf_arr[*,i])
    endfor
    if bz gt  1e-4  then begin
      down_idx=where(mu_arr le 0)
      up_idx=where(mu_arr ge 0)
      f_up=2d0*!dpi*int_trapzd(mu_arr[up_idx], int_e[up_idx])
      f_down=2d0*!dpi*int_trapzd(mu_arr[down_idx], int_e[down_idx])
      d_fraction=f_down/(f_up+f_down)
    endif
    if bz lt  -1e-4  then begin
      down_idx=where(mu_arr ge 0) 
      up_idx=where(mu_arr le 0)
      f_up=2d0*!dpi*int_trapzd(mu_arr[up_idx], int_e[up_idx])
      f_down=2d0*!dpi*int_trapzd(mu_arr[down_idx], int_e[down_idx])
      d_fraction=f_down/(f_up+f_down)
    endif
  endif
endif

return, d_fraction

end