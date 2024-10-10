pro gx_los2base,base_index,losfile,basemap,id=id,pixel=pixel
  setenv, 'WCS_RSUN=6.96d8'
  if !version.os_family eq 'Windows' then noshell=1 else noshell=0
  wcs1 = fitshead2wcs( base_index)
  read_sdo, losfile, index0, data0,noshell=noshell
  ind = where(finite(data0,/nan))
  data0[ind] = 0
  
  if n_elements(id) eq 0 then begin
    index2map,index0,data0,map0
    id=map0.id
  endif
  if n_elements(pixel) eq 0 then begin
    wcs0 = FITSHEAD2WCS( index0[0] )
    ;trying to correct position bug
    wcs2map,data0[*,*,0], wcs0, map
    map2wcs, map,wcs0
    coord1 = wcs_get_coord(wcs1)
    wcs_convert_from_coord, wcs1, coord1, 'hg', lon, lat,/carrington
    wcs_convert_to_coord, wcs0, coord0, 'hg', lon, lat,/carrington
    pixel = wcs_get_pixel( wcs0, coord0 )
  end
  data = reform( interpol( data0, pixel[0,*,*], pixel[1,*,*],missing=0 ))
  wcs1.simple=1
  wcs2map,data,wcs1,basemap,id='BASE '+id
end