


;+
; :Description:
;    Downloads from JSOC all SDO/HMI files required to produce GX-simulator box
;
; :Params:
;    t - time of the magnetogram (any format recognised by ANYTIM)
;    tmp_dir - directory to save downloaded files
; :Keywords:
;   time_window - time window in seconds to search HMI data (default: 720)
;
; :Author: Sergey Anfinogentov (sergey.istp@gmail.com)
;-
function gx_box_download_hmi_data, t, tmp_dir, time_window = time_window
compile_opt idl2

  if not keyword_set(time_window) then time_window =720d
  
  t_ = anytim(t)
  t1 = t_ - time_window / 2d
  t2 = t_ + time_window / 2d

  ds = ['hmi.B_720s','hmi.B_720s','hmi.B_720s','hmi.B_720s','hmi.M_720s','hmi.Ic_noLimbDark_720s']
  segment = ['field','inclination','azimuth','disambig','magnetogram','continuum']
  files = {field:'',inclination:'',azimuth:'',disambig:'',magnetogram:'',continuum:''}
  
  for i = 0, n_elements(ds)-1 do begin
    files.(i) =gx_box_jsoc_get_fits(t1, t2, ds[i], segment[i], tmp_dir)
  endfor
  
 return, files
end