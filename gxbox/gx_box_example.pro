
;+
  ; :Description:
  ;    Example of preparing GX-Simulator compatible box file 
  ;
  ;
  ;
  ;
  ;
  ; :Author: Sergey Anfinigentov (sergey.istp@gmail.com)
  ;-
pro gx_box_example
   out_dir = '~/temp/gx_box_test'
  tmp_dir = '~/temp/jsoc_cache'
  
  
  ;time to search for SDO data
  time='2016-02-20 17:00:00'
  ;center pixel in arcseconds
  centre=[-15,185]
  ;box size in voxels
  size_pix=[130,130,130]
  ;resolution in kilometers
  dx_km=700
  
  
;  time='2017-08-05 18:05:00'
;  centre=[-422.,-192.0] ; arcseconds
;  dx_km=1500.
;  size_pix=[128,128,128]
;  
  gx_box_prepare_box, time, centre, size_pix, dx_km, out_dir = out_dir,/cea,$
    AIA_time_window = 60., tmp_dir = tmp_dir,/aia_uv,/aia_euv;,/make_pbox;,/sfq;,/aia_euv;, /aia_uv
;stop
end