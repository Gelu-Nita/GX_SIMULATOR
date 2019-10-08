;+
  ; :Description:
  ;    Prepares a GX-simulator comptible box filled with the potential field.
  ;    All required data are download automatically. Normally, the full magnetic
  ;    field vector at the lower boundary is preserved to be used for subsequent
  ;    NLFFF extrapolation. If you need the field at the lower boundary to be replaced
  ;    with the potential field, use the 'make_pbox' keyword.
  ;
  ; :Params:
  ;    time - Requested time
  ;    centre - [x0,y0], position of the box centre in arcseconds from the disk centre
  ;    size_pix - [nx,ny,nz], size of the box in voxel
  ;    dx_km - spatial resolution in kilometers
  ;
  ; :Keywords:
  ;    out_dir - directory where to save the data (default: current directory)
  ;    tmp_dir - temporary dierectory where to keep downloaded data segments
  ;                (default: IDL temporary directory as returned by GETENV('IDL_TMPDIR'))
  ;    aia_euv - Download images in AIA EUV channels and add them to the box as reference maps
  ;    aia_uv  - Download images in AIA UV  channels and add them to the box as reference maps
  ;    carrington - set this keyword if the box center is given as carrington longitude and latitude in degrees
  ;    cea - set this keyword to use the CEA projection for the base of the box
  ;    top - set this keyword to use the TOP VIEW projection for the base of the box
  ;    make_pbox - set this keyword to produce additional GX-simulator compatible box structure
  ;            to hold the potential field solution in the whole box including lower boundary.
  ;    sfq - perform SFQ disambiguation (see http://adsabs.harvard.edu/abs/2014SoPh..289.1499R)
  ;    box - set this keyword to a variable which will contain generated box structure with starting
  ;           field for NLFFF extrapolation
  ;    pbox - set this keyword to a variable which will contain generated box structure with potential field solution
  ;
  ; :Author: Sergey Anfinigentov (sergey.istp@gmail.com)
  ;-
pro gx_box_prepare_box, time, centre, size_pix, dx_km, out_dir = out_dir, tmp_dir = tmp_dir,$
                  aia_euv = aia_euv, aia_uv = aia_uv, top = top, cea = cea,$
                  carrington = carrington, sfq = sfq, make_pbox = make_pbox,$
                  HMI_time_window = HMI_time_window, AIA_time_window = AIA_time_window,$
                  box = box, pbox = pbox, hmi_files = hmi_files
  if not keyword_set(out_dir) then cd, current = out_dir
  if not file_test(out_dir) then file_mkdir, out_dir
  if not keyword_set(tmp_dir) then tmp_dir = filepath('jsoc_cache',root = GETENV('IDL_TMPDIR'))
  if not file_test(tmp_dir) then file_mkdir, tmp_dir
  if not keyword_set(dx_km) then dx_km = 1000d
  if not keyword_Set(size_pix) then size_pix = [128,128,64]
  
  if not keyword_set(hmi_files) then begin
    files = gx_box_download_hmi_data(time, tmp_dir, time_window = HMI_time_window)
  endif else begin
    files = hmi_files
  endelse

  box = gx_box_create(files.field, files.inclination, files.azimuth,files.disambig,$
     files.continuum, centre, size_pix, dx_km,top = top, cea = cea, carrington = carrington, sfq = sfq)
  gx_box_add_refmap, box, files.continuum, id = 'Continuum'
  gx_box_add_refmap, box, files.magnetogram, id = 'LOS_magnetogram'
  gx_box_add_vertical_current_map, box, files.field, files.inclination, files.azimuth, files.disambig
  
  if keyword_set(make_pbox) then begin
    gx_box_make_potential_field, box, pbox
  endif else begin
    gx_box_make_potential_field, box
  endelse
  
  
  ;Downloading AIA data in EUV channels
  if keyword_set(AIA_EUV) then begin
    files = gx_box_download_AIA_data(time, out_dir, cache_dir = tmp_dir, /euv, time_window = AIA_time_window)
    
   
    
    if have_tag(files,'aia_94') then gx_box_add_refmap, box, files.aia_94,  id = 'AIA_94'
    if have_tag(files,'aia_131') then gx_box_add_refmap, box, files.aia_131, id = 'AIA_131'
    if have_tag(files,'aia_171') then gx_box_add_refmap, box, files.aia_171, id = 'AIA_171'
    if have_tag(files,'aia_193') then gx_box_add_refmap, box, files.aia_193, id = 'AIA_193'
    if have_tag(files,'aia_211') then gx_box_add_refmap, box, files.aia_211, id = 'AIA_211'
    if have_tag(files,'aia_304') then gx_box_add_refmap, box, files.aia_304, id = 'AIA_304'
    if have_tag(files,'aia_335') then gx_box_add_refmap, box, files.aia_335, id = 'AIA_335'
    
  endif
  
  if keyword_set(AIA_UV) then begin
    files=gx_box_download_AIA_data(time, out_dir, cache_dir = tmp_dir, /uv, time_window = AIA_time_window)

    if have_tag(files,'aia_1600') then gx_box_add_refmap, box, files.aia_1600,  id = 'AIA_1600'
    if have_tag(files,'aia_1700') then gx_box_add_refmap, box, files.aia_1700,  id = 'AIA_1700'
    

  endif
  
  save, box, file =filepath(box.id+".sav",root_dir = out_dir)
  if keyword_set(make_pbox) then begin
    box = pbox
    save, box, file =filepath(box.id+".sav",root_dir = out_dir)
  endif
  ;stop
end
