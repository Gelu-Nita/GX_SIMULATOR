;+
  ; :Description:
  ;    Generate magnetic field maps in Cylindrical Equal Area (CEA) projection from SDO/HMI magnetograms
  ;
  ; :Params:
  ;    file_field - file name of the FIELD fits file
  ;    file_inclination - file name of the INCLINATION fits file
  ;    file_azimuth - file name of the AZIMUTH fits file
  ;    file_disambig - file name of the DISAMBIGuation fits file
  ;    center_arcsec - center of the patch to be mapped into CEA projection
  ;    size_pix - size of the resulting CEA map in pixels
  ;    dx_deg - spatial resolution of the resulting CEA map in heliographic degrees
  ; :Keywords:
  ;   WCS - (optional input), WCS structure, describing the CEA projection where to map field to
  ;   Carrington - set this if the center of the patch is given as longitude and latitude (degrees) in carrington coordinate system  
  ;
  ;
  ;
  ; :Author: Sergey Anfinogentov
  ;-
function prepare_cea_map, file_field, file_inclination, file_azimuth, file_disambig, center_arcsec, size_pix, dx_deg, WCS = WCS, carrington = carrington

  files = [file_field, file_inclination, file_azimuth]
  read_sdo,files, index, data, /uncomp_delete, /use_shared_lib
  
  wcs0 = FITSHEAD2WCS( index[0] )
  
  ;Apply disambigution--------------------------------
  read_sdo, file_disambig, index, disambig, /uncomp_delete, /use_shared_lib
  azimuth  =reform(data[*,*,2])
  hmi_disambig, azimuth, disambig, 0
  data[*,*,2] = azimuth
  ;-------------------------------------
  
  ;Converting field to spherical coordinates
  hmi_b2ptr, index[0], data, bptr, lonlat=lonlat
  
  ;Calculating reference point in Carrington  coordinate system
  if not keyword_set(carrington) then begin
  wcs_convert_from_coord,wcs0,center_arcsec,'HG', lon, lat, /carrington
  endif else begin
    lon = center_arcsec[0]
    lat = center_arcsec[1]
  endelse
  

  
  ;Make WCS for resulting CEA map
  if not keyword_set(wcs) then begin
    wcs = WCS_2D_SIMULATE( size_pix[0], size_pix[1],cdelt = dx_deg, crval =[lon,lat], type ='CR', projection = 'cea',$
       date_obs = index.date_obs)
  endif
  
 ;Coordinate transformnation----
  cc = wcs_get_coord( wcs )
  wcs_convert_from_coord,wcs,cc,'HG', lon, lat, /carrington
  wcs_convert_to_coord,wcs0,cc0,'HG', lon, lat, /carrington
  pix = wcs_get_pixel(wcs0, cc0)
  ;------------------------------
  
  ;Interpolating data
  bp =  reform(interpolate(bptr[*,*,0],pix[0,*,*],pix[1,*,*], cubic=-0.5))
  bt =  reform(interpolate(bptr[*,*,1],pix[0,*,*],pix[1,*,*], cubic=-0.5))
  br =  reform(interpolate(bptr[*,*,2],pix[0,*,*],pix[1,*,*], cubic=-0.5))
  
  return, {wcs:wcs, bp:bp, bt:bt, br:br}


end
