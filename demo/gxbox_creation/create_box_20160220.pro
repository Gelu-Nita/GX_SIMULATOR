pro create_box_20160220 
 
 ;time to search for SDO data
 time='2016-02-20 17:00:00'
 ;center pixel in arcseconds
 center_arcsec=[-15,185]
 ;box size in voxels
 size_pix=[130,130,130]
 ;resolution in kilometers
 dx_km=1400
 out_dir='C:\gx_models';
 tmp_dir='C:\jsoc_cache'
 gx_fov2box,time,center_arcsec=center_arcsec,size_pix=size_pix,dx_km=dx_km,out_dir=out_dir,tmp_dir=tmp_dir,/cea,$
                     /save_empty_box,/save_potential,/save_bounds,/use_potential,/uv,/euv,/empty_box
end