;+
  ; :Description:
  ;    Generate magnetic field maps in Cylindrical Equal Area (CEA) or Top View projection from SDO/HMI magnetograms
  ;
  ; :Output:
  ;   returns a structure containing the following fields:
  ;     -Bp  - longitudinal component of the magnetic field
  ;     -Bt  - latitudinal component of the magnetic field
  ;     -Br  - radial component of the magnetic field
  ;     -Ic  - continuum intencity
  ;     -wcs - WCS structure describing the projection
  ;
  ; :Params:
  ;    file_field - file name of the FIELD fits file
  ;    file_inclination - file name of the INCLINATION fits file
  ;    file_azimuth - file name of the AZIMUTH fits file
  ;    file_disambig - file name of the DISAMBIGuation fits file
  ;    file_continuum - file name of the continuum with removed limb darkening fits file
  ;    center_arcsec - center of the patch to be mapped into CEA projection
  ;    size_pix - size of the resulting CEA map in pixels
  ;    dx_km - spatial resolution of the resulting  map in kilometers
  ; :Keywords:
  ;   WCS - (optional input), WCS structure, describing the projection where to map field to
  ;   Carrington - set this if the center of the patch is given as longitude and latitude (degrees) in carrington coordinate system  
  ;   Top - set this keyword to create the "Top view" map
  ;
  ;
  ; :Author: Sergey Anfinogentov  (anfinogentov@iszf.irk.ru)
  ;-
function prepare_basemaps, file_field, file_inclination, file_azimuth, file_disambig, file_continuum,$
 center_arcsec, size_pix, dx_km, WCS = WCS, carrington = carrington, cea = cea, top = top, sfq = sfq
 compile_opt idl2
   
   
   
   if (not keyword_set(cea)) and (not keyword_set(top)) then cea = 1

  files = [file_field, file_inclination, file_azimuth]
  read_sdo,files, index, data, /uncomp_delete;, /use_shared_lib
  
  ind = where(finite(data,/nan))
  data[ind] = 0
  
  wcs0 = FITSHEAD2WCS( index[0] )
  
  ;trying to correct position bug
  wcs2map,data[*,*,0], wcs0, map
  map2wcs, map,wcs0
  
  
  DSUN_OBS  = wcs0.position.dsun_obs;
  dx_deg = dx_km*1d3 / WCS_RSUN() * 180d/!dpi
  dx_arcsec = dx_km*1d3 / (DSUN_OBS - wcs_rsun() ) * 180d/!dpi * 3600d
  
  

  
  ;Calculating reference point in Carrington  coordinate system
  if not keyword_set(carrington) then begin
    wcs_convert_from_coord,wcs0,center_arcsec,'HG', lon, lat, /carrington
  endif else begin
    lon = center_arcsec[0]
    lat = center_arcsec[1]
  endelse
  
  ;Seting up the basemap projection as a WCS structure
  if not keyword_set(wcs) then begin
    if keyword_set(cea) then begin
      wcs = WCS_2D_SIMULATE( size_pix[0], size_pix[1],cdelt = dx_deg, crval =[lon,lat],$
        type ='CR', projection = 'cea', date_obs = index[0].date_obs)
    endif
    if keyword_set(top) then begin
      WCS = WCS_2D_SIMULATE(size_pix[0], size_pix[1], CDELT=dx_arcsec, DSUN_OBS=DSUN_OBS ,$
        CRLN_OBS=lon, CRLT_OBS=lat, date_obs = index[0].date_obs)
    endif

  endif

  if keyword_set(sfq) then begin
    nx = wcs.naxis[0]
    ny = wcs.naxis[1]
    pix = lonarr(2, 4)
    pix[0,*] = [0, 0, nx - 1, nx -1]
    pix[1,*] = [0, ny -1, ny - 1, 0]
    crd = wcs_get_coord(wcs, pix)
    wcs_convert_from_coord,wcs,crd,'HG', lon, lat, /carrington
    wcs_convert_to_coord,wcs0,crd_ref,'HG', lon, lat, /carrington
    pix_ref = wcs_get_pixel(wcs0, crd_ref)
    xrange = round(minmax(pix_ref[0,*]))+[-1,1]
    yrange = round(minmax(pix_ref[1,*]))+[-1,1]
    field_s = data[xrange[0]:xrange[1],yrange[0]:yrange[1],0]
    inclination_s = data[xrange[0]:xrange[1],yrange[0]:yrange[1],1]
    azimuth_s = data[xrange[0]:xrange[1],yrange[0]:yrange[1],2]
    bz = field_s*cos(inclination_s*(!dpi/180d))
    bx = field_s*sin(inclination_s*(!dpi/180d))*sin(azimuth_s*!dpi/180d)
    by = -field_s*sin(inclination_s*(!dpi/180d))*cos(azimuth_s*!dpi/180d)
    
    bx = rotate(bx,2)
    by = rotate(by,2)
    bz = rotate(bz,2)
    
    pos = [min(crd_ref[0,*]),min(crd_ref[1,*]),max(crd_ref[0,*]),max(crd_ref[1,*])]
    rsun_arcsec = wcs_rsun()/wcs0.position.dsun_obs*180d*60d*60d/!dpi
    
    sfq_disambig,bx,by,bz,pos, rsun_arcsec,/hmi
    
    by = -rotate(by,2)
    bx = rotate(bx,2)
    az = atan(bx,by)
    data[xrange[0]:xrange[1],yrange[0]:yrange[1],2] = az*180d/!dpi



    
   ;stop
    
  endif else begin
  
  ;Apply disambigution--------------------------------
  read_sdo, file_disambig, index, disambig, /uncomp_delete;, /use_shared_lib
  azimuth  =reform(data[*,*,2])
  hmi_disambig, azimuth, disambig, 0
  data[*,*,2] = azimuth
  ;-------------------------------------
  endelse
  
  ;Converting field to spherical coordinates
  hmi_b2ptr, index[0], data, bptr, lonlat=lonlat
  
  
  

  

  
  ; remapping data to the basmap projection
  bp = wcs_remap(bptr[*,*,0],wcs0, wcs, /ssaa)
  bt = wcs_remap(bptr[*,*,1],wcs0, wcs, /ssaa)
  br = wcs_remap(bptr[*,*,2],wcs0, wcs, /ssaa)
  
  read_sdo, file_continuum, index, data, /uncomp_delete;,/use_shared_lib
  wcs0 = FITSHEAD2WCS( index[0] )
  wcs2map,data, wcs0, map
  map2wcs, map,wcs0
  ic = wcs_remap(data, wcs0, wcs, /ssaa)
  
 
 
  
  return, {wcs:wcs, bp:bp, bt:bt, br:br, ic: ic}


end
