;+
  ; :Description:
  ;    Adds the reference map to the box structure. The map will be cut out to
  ;    to have th field of view 10% larger then that of the box's basemap.
  ;    Spatial resolution of the remains the same as in given FITS
  ;
  ; :Params:
  ;    box - GX-Simulator Compatible box structure
  ;    fits_file - file name of the FITS containing the base map
  ;
  ; :Keywords:
  ;    id - ID of the reference map (e.g. "Bz reference")
  ;
  ; :Author: Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
  ;-
pro gx_box_add_refmap, box, fits_file, id = id

  wcs = fitshead2wcs(box.index)
  read_sdo,fits_file, index, data, /uncomp_delete;, /use_shared_lib
  wcs_input = fitshead2wcs(index)
  
  ;trying to correct position bug
  wcs2map,data[*,*,0], wcs_input, map
  map2wcs, map,wcs_input
  
  
  if not keyword_set(id) then get_fits_par,index, id =id
  
  
  nx = wcs.naxis[0]
  ny = wcs.naxis[1]
  
  
  pix = lonarr(2, 4)
  pix[0,*] = [0, 0, nx - 1, nx -1]
  pix[1,*] = [0, ny -1, ny - 1, 0]
  
  
  
  crd = wcs_get_coord(wcs, pix)
  wcs_convert_from_coord,wcs,crd,'HG', lon, lat, /carrington
  wcs_convert_to_coord,wcs_input,crd_ref,'HG', lon, lat, /carrington
  
  
  wcs_convert_from_coord,wcs,wcs.crval,'HG', lon, lat, /carrington
  wcs_convert_to_coord,wcs_input,crval_ref,'HG', lon, lat, /carrington
  
  
  
  
  pix_ref = wcs_get_pixel(wcs_input, crd_ref)
  xrange = minmax(pix_ref[0,*])
  yrange =  minmax(pix_ref[1,*])
  
  nx_ref = round((xrange[1] - xrange[0])*1.1)
  ny_ref = round((yrange[1] - yrange[0])*1.1)
  
  ;WCS_ref = WCS_2D_SIMULATE(nx_ref, ny_ref, CDELT=wcs_input.cdelt, DSUN_OBS=wcs_input.position.dsun_obs,$
  ;   date_obs = wcs_input.time.observ_date, crval = crval_ref, crlt_obs =wcs_input.position.crlt_obs, crln_obs =wcs_input.position.crln_obs)
     
  WCS_ref = WCS_2D_SIMULATE(nx_ref, ny_ref, CDELT=wcs_input.cdelt, DSUN_OBS=wcs_input.position.dsun_obs,$
     date_obs = wcs_input.time.observ_date, crval = crval_ref)
     
  foo = wcs_remap(data, wcs_input, wcs_ref)
  wcs2map,foo,wcs_ref, map
  map.id = id
  map.b0 = wcs_ref.position.solar_b0

  
  ind = (*box.refmaps).get(/count)
  (*box.refmaps).set,ind, map = map
     
  
  
 ; stop

end