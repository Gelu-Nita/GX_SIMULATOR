pro create_box_20160220,_extra=_extra
 
 ;time to search for SDO data
 time='2016-02-20 17:00:00'
 ;center pixel in arcseconds
 center_arcsec=[-15,185]
 ;box size in voxels
 size_pix=[64,64,64]
 ;resolution in kilometers
 dx_km=1400
 if (!version.os_family eq 'Windows') then begin
  ;these are the defaults on Windows platforms:
  out_dir='C:\gx_models'
  tmp_dir='C:\jsoc_cache'
 endif else begin
  ;these are the defaults on Linux/Mac platforms:
  out_dir = filepath('gx_models',root = curdir())
  tmp_dir=filepath('jsoc_cache',root = curdir())
 endelse
 gx_fov2box,time,center_arcsec=center_arcsec,size_pix=size_pix,dx_km=dx_km,out_dir=out_dir,tmp_dir=tmp_dir,/cea,$
                     /save_empty_box,/save_potential,/save_bounds,/uv,/euv,/center_vox,_extra=_extra
end