;+
  ; :Description:
  ;    Adds vertical current map to the specified GX-simulator compatible box structure
  ;
  ; :Params:
  ;    box - box structure
  ;    file_field       - filename of the SDO/HMI fits with the magnetic field absolute value
  ;    file_inclination - filename of the SDO/HMI fits with the magnetic field inclination
  ;    file_azimuth     - filename of the SDO/HMI fits with the magnetic field azimuth
  ;    file_disambig    - filename of the SDO/HMI fits with the disambiguation information
  ;
  ;
  ;
  ; :Author: Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
  ;-
pro gx_box_add_vertical_current_map, box, file_field, file_inclination, file_azimuth, file_disambig
  if !version.os_family eq 'unix' then use_shared_lib = 1
  
  files = [file_field, file_inclination, file_azimuth]
  read_sdo,files, index, data, /uncomp_delet, use_shared_lib = use_shared_lib
  wcs0 = FITSHEAD2WCS( index[0] )
  
  ind = where(finite(data,/nan))
  if ind ne -1 then data[ind] = 0
  
  read_sdo, file_disambig, index, disambig, /uncomp_delete;, /use_shared_lib
  azimuth  =reform(data[*,*,2])
  hmi_disambig, azimuth, disambig, 0
  data[*,*,2] = azimuth
  
  wcs = fitshead2wcs(box.index)
  nx = wcs.naxis[0]
  ny = wcs.naxis[1]
  pix = lonarr(2, 4)
  pix[0,*] = [0, 0, nx - 1, nx -1]
  pix[1,*] = [0, ny -1, ny - 1, 0]
  crd = wcs_get_coord(wcs, pix)
  wcs_convert_from_coord,wcs,crd,'HG', lon, lat, /carrington
  wcs_convert_to_coord,wcs0,crd_ref,'HG', lon, lat, /carrington
  pix_ref = wcs_get_pixel(wcs0, crd_ref)
  
  xrange = minmax(pix_ref[0,*])
  yrange =  minmax(pix_ref[1,*])
  
  wcs_convert_from_coord,wcs,wcs.crval,'HG', lon, lat, /carrington
  wcs_convert_to_coord,wcs0,crval_ref,'HG', lon, lat, /carrington
  
  nx_ref = round((xrange[1] - xrange[0])*1.1)
  ny_ref = round((yrange[1] - yrange[0])*1.1)
  
  ;WCS_ref = WCS_2D_SIMULATE(nx_ref, ny_ref, CDELT=wcs_input.cdelt, DSUN_OBS=wcs_input.position.dsun_obs,$
  ;   date_obs = wcs_input.time.observ_date, crval = crval_ref, crlt_obs =wcs_input.position.crlt_obs, crln_obs =wcs_input.position.crln_obs)
  
  WCS_ref = WCS_2D_SIMULATE(nx_ref, ny_ref, CDELT=wcs0.cdelt, DSUN_OBS=wcs0.position.dsun_obs,$
    date_obs = wcs0.time.observ_date, crval = crval_ref,$
      crlt_obs =wcs0.position.crlt_obs, crln_obs =wcs0.position.crln_obs)
    
  field       = data[*,*,0]
  inclination = data[*,*,1]
  azimuth     = data[*,*,2]
  
  bz = field*cos(inclination*(!dpi/180d))
  bx = field*sin(inclination*(!dpi/180d))*sin(azimuth*!dpi/180d)
  by = -field*sin(inclination*(!dpi/180d))*cos(azimuth*!dpi/180d)

  bx      = wcs_remap(bx, wcs0, wcs_ref)
  by      = wcs_remap(by, wcs0, wcs_ref)
  bz      = wcs_remap(bz, wcs0, wcs_ref)
  
  rsun = (pb0r(wcs_ref.time.observ_date,/arcsec))[2]
  current_density = GX_box_Current_Density(bz, bx, by ,wcs_ref.crpix[0], wcs_ref.crpix[1],$
                           wcs.cdelt[0], wcs.cdelt[1] , RSun)
  
  wcs2map, current_density, wcs_ref, map
  map.id = 'Vert_current'
  map.b0 = wcs_ref.position.solar_b0
  
  ind = (*box.refmaps).get(/count)
  (*box.refmaps).set,ind, map = map
  
  
end