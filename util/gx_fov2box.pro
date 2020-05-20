;center_arcsec: FOV center cordinates, [xc,yc] in arcseconds
;centre: deprecated keyword used in previous version to provide the center_arcsec coordinates
;size_pix: size of the extrapolation box, [nx,ny,nz] 
;dx_km: voxel size in Km (assumed to be uniform, i.e. dx=dy=dz)  
;out_dir: user defined local directory to save the output models in
;tmp_dir: user defined local directory to save the nput files downloaded from the SDO repository
;/empty_box_only: set this keyword to stop the script after the empty box structure is generated (no extrapolation yet performed, just the boundary conditions stored in the box)
;/save_empty_box: set this keyword to save the boundary condition box
;/potential_only: set this keyword to gcompute only the potential extrapolation
;/save_potential: set this keyword to save the potential extrapolation box (which used to generate the boundary box for the following pipeline steps)
;/save_bounds: set this keyword to save the boundary box
;/use_potential: set this keyword to skip the NLFFF extrapolation step (which is skip regardless this keyword on UNIX/LINUX platforms)
;/nlfff_only: set this keyword to stop the pipeline script after the NLFFF box is computed 
;/use_idl: set this keyword to use the IDL implementation of the magnetic field lines intersecting each volume voxel instead of using the NLFFFF DLL (which is skip regardless this keyword on UNIX/LINUX platforms)
;/generic_only: set this keyword to stop the script after the information related the computed volume magnetic lines are added to the box
;/euv: use this keyword to download the context SDO EUV maps and add tem as reference maps in the box structure
;/uv: use this keyword to download the context SDO UV maps and add tem as reference maps in the box structure
;hmifiles: use this keyword to suply a valid hmi filepath structure pinting to a local repository, in order to bypass the attempt to download them 
;_extra: use this keyword to provide additional keywards that may be used by various routines called by the pipeline , such as
;_extra keywords that may be used by gx_box_create.pro
; /cea: to use CEA projection
; /top: to use TOP_VIEW projection
; /sfq: to use SFQ disambiguation
; /old_combo_format: to generate combo models using the format used before May 2020 (for testing purposes)
;_extra keywords that may be used when computing the coronal magnetic field lines properties
; Options available on LInux/Uix platforms, or if /use_idl is set:
;   /center_vox: use this keyword to compute lines that pass exactly through the center of each volume voxel (more computationaly intensive) 
;   as oposed to the faster option (default on Unix/LINUX), which assigns the same <B>-L properties to all voxels crossed by an already computed field line
; Options availble only on indows platforms, id use_idl=0 (default on Windows OS)
;   reduce_passed=0 (default); equivalent to /center_vox described above
;   reduce_passed=1; mark as already passed all voxels intersected by some closed field line
;   reduced_passed=2; mark as already passed all voxels intersected by some open field line
;   reduced_passed=3; mark as already passed all voxels intersected ny a closed or open field line



pro gx_fov2box,time, center_arcsec=center_arcsec, size_pix=size_pix, dx_km=dx_km, out_dir = out_dir, tmp_dir = tmp_dir,$
                        empty_box_only=empty_box_only,save_empty_box=save_empty_box,potential_only=potential_only,$
                        save_potential=save_potential,save_bounds=save_bounds,use_potential=use_potential, use_idl=use_idl,$
                        nlfff_only=nlfff_only, generic_only=generic_only,centre=centre,euv=euv,uv=uv,hmifiles=hmifiles,old_combo_format=old_combo_format,_extra=_extra
  setenv, 'WCS_RSUN=6.96d8'
  
  break_file, ROUTINE_FILEPATH(), dsk_log, dir, routine_name, ext
  par=ROUTINE_INFO(routine_name,/par)
  exec="gx_fov2box, '"+time+"'"
  for i=0, par.num_kw_args-2 do begin
    dummy=execute('set=keyword_set('+par.kw_args[i]+')')
    if set eq 1 then begin
      dummy=execute('val='+par.kw_args[i])
      if size(val,/tname) ne 'STRUCT' then begin
        if size(val,/tname) ne 'STRING' then begin
        val=n_elements(val) eq 1?arr2str(val):'['+arr2str(val)+']'
        endif else val="'"+val+"'"
        val=strcompress(val)
        exec+=', '+par.kw_args[i]+'='+val   
      end
    endif
  endfor
  if size(_extra,/tname) eq 'STRUCT' then begin
    tnames=tag_names(_extra)
    for i=0,n_elements(tnames)-1 do begin
      val=_extra.(i)
      val=n_elements(val) eq 1?arr2str(val):'['+arr2str(val)+']'
      val=strcompress(val)
      exec+=', '+tnames[i]+'='+val
    endfor
  endif
  
  t0=systime(/seconds)
  message,'Downloading data',/cont
  if not keyword_set(tmp_dir) then tmp_dir = filepath('jsoc_cache',root = GETENV('IDL_TMPDIR'))
  if not file_test(tmp_dir) then file_mkdir, tmp_dir
  if not keyword_set(out_dir) then cd, current = out_dir
  out_dir=out_dir+path_sep()+anytim(strreplace(time,'.','-'),/ccsds,/date)
  if not file_test(out_dir) then file_mkdir, out_dir
  
  if not keyword_set(dx_km) then dx_km = 1000d
  if not keyword_Set(size_pix) then size_pix = [128,128,64]
  if size(hmifiles,/tname) eq 'STRUCT' then begin
   if tag_exist(hmifiles,'field') $
              and tag_exist(hmifiles,'inclination')  $
              and tag_exist(hmifiles,'azimuth') $
              and tag_exist(hmifiles,'disambig') $
   then files=hmifiles else files = gx_box_download_hmi_data(time, tmp_dir)                  
  endif else files = gx_box_download_hmi_data(time, tmp_dir)
  data=readfits(files.continuum,header)
  time=atime(sxpar(header,'date_obs'))              
  if keyword_set(uv) or keyword_set(euv) then aia_files=gx_box_download_AIA_data(time, cache_dir = tmp_dir, uv=uv, euv=euv, _extra=_extra)
  if size(aia_files,/tname) eq 'STRUCT' then files = create_struct(files,aia_files)
  message,strcompress(string(systime(/seconds)-t0,format="('Data already found in the local repository or downloaded in ',g0,' seconds')")),/cont
  
  t0=systime(/seconds)
  message,'Creating the box structure',/cont
  ;for backward compatibility with deprecated "centre" input
  if n_elements(center_arcsec) ne 2 then if n_elements(centre) eq 2 then center_arcsec=centre
  if n_elements(center_arcsec) ne 2 then begin
    message,'Required center_arcsec input is missing or incorrect. Action aborted!',/cont
    return
  endif
  
  box = gx_box_create(files.field, files.inclination, files.azimuth,files.disambig, files.continuum, center_arcsec, size_pix, dx_km,_extra=_extra)
  box=create_struct(box,'execute',exec)
  if n_elements(files) gt 0   then begin
    if tag_exist(files,'magnetogram') then if file_test(files.magnetogram) then gx_box_add_refmap, box, files.magnetogram, id = 'Bz_reference'
    if tag_exist(files,'continuum') then if file_test(files.continuum) then gx_box_add_refmap, box, files.continuum, id = 'Ic_reference'
    if tag_exist(files,'AIA_94') then if file_test(files.AIA_94) then gx_box_add_refmap, box, files.aia_94,  id = 'AIA_94'
    if tag_exist(files,'AIA_131') then if file_test(files.AIA_131) then gx_box_add_refmap, box, files.aia_131, id = 'AIA_131'
    if tag_exist(files,'AIA_171') then if file_test(files.AIA_171) then gx_box_add_refmap, box, files.aia_171, id = 'AIA_171'
    if tag_exist(files,'AIA_193') then if file_test(files.AIA_193) then gx_box_add_refmap, box, files.aia_193, id = 'AIA_193'
    if tag_exist(files,'AIA_211') then if file_test(files.AIA_211) then gx_box_add_refmap, box, files.aia_211, id = 'AIA_211'
    if tag_exist(files,'AIA_304') then if file_test(files.AIA_304) then gx_box_add_refmap, box, files.aia_304, id = 'AIA_304'
    if tag_exist(files,'AIA_335') then if file_test(files.AIA_335) then gx_box_add_refmap, box, files.aia_335, id = 'AIA_335'
    if tag_exist(files,'AIA_1600') then if file_test(files.AIA_1600) then gx_box_add_refmap, box, files.aia_1600, id = 'AIA_1600'
    if tag_exist(files,'AIA_1700') then if file_test(files.AIA_1700) then gx_box_add_refmap, box, files.aia_1700, id = 'AIA_1700'
  endif   
  message,strcompress(string(systime(/seconds)-t0,format="('Box structure created in ',g0,' seconds')")),/cont  
  
  if keyword_set(empty_box_only) or keyword_set(save_empty_box) then begin
    save,box,file=out_dir+path_sep()+box.id+'.sav'
    message,'Empty box structure saved to '+out_dir+path_sep()+box.id+'.sav',/cont
  endif
  if keyword_set(empty_box_only) then return
  
  t0=systime(/seconds)
  message,'Performing initial potential extrapolation',/cont
  gx_box_make_potential_field, box,pbox
  message,strcompress(string(systime(/seconds)-t0,format="('Potential extrapolation performed in ',g0,' seconds')")),/cont
  
  if size(pbox,/tname) eq 'STRUCT' and ( keyword_set(save_potential) or keyword_set(potential_only) ) then begin
    save,pbox,file=out_dir+path_sep()+pbox.id+'.sav'
    message,'Potential box structure saved to '+out_dir+path_sep()+pbox.id+'.sav',/cont
  end
  
  if keyword_set(save_bounds) then begin
    save,box,file=out_dir+path_sep()+box.id+'.sav'
    message,'Bound Box structure saved to '+out_dir+path_sep()+box.id+'.sav',/cont
  endif
 
  if keyword_set(potential_only) then return
  
  if !VERSION.OS_FAMILY NE 'Windows' or keyword_set(use_potential) then begin
    if size(pbox,/tname) eq 'STRUCT' then box=temporary(pbox)
    goto,skip_nlfff
  endif
  
  t0=systime(/seconds)
  
  dirpath=file_dirname((ROUTINE_INFO('gx_box_make_potential_field',/source)).path,/mark)
  path=dirpath+'WWNLFFFReconstruction.dll'
  
  message,'Performing NLFFF extrapolation',/cont
  return_code = gx_box_make_nlfff_wwas_field(path, box)
  message,strcompress(string(systime(/seconds)-t0,format="('NLFFF extrapolation performed in ',g0,' seconds')")),/cont
  save,box,file=out_dir+path_sep()+box.id+'.sav'
  message,'NLFFF box structure saved to '+out_dir+path_sep()+box.id+'.sav',/cont
  
  if keyword_set(nlfff_only) then return
  
  skip_nlfff:
  
  message,'Computing field lines for each voxel in the model..',/cont
  tr_height_km=1000
  tr_height_sunradius=tr_height_km/(gx_rsun(unit='km'))
  if keyword_set(use_idl) or !version.os_family ne 'Windows' then begin
    t0=systime(/seconds)
    model=gx_importmodel(box)
    model->computecoronalmodel,tr_height=tr_height_sunradius,/compute,_extra=_extra
    gx_copylines2box,model,box
    obj_destroy,model
    message,strcompress(string(systime(/seconds)-t0,format="('Field line computation performed in ',g0,' seconds')")),/cont
  endif else begin
    reduce_passed=1
    if tag_exist(_extra,'center_vox') then begin
      reduce_passed=~keyword_set(_extra.center_vox)
    endif else if tag_exist(_extra,'reduce_passed') then reduce_passed=_extra.reduce_passed
    gx_addlines2box, box,tr_height_km,reduce_passed=reduce_passed
  endelse
  
  box.id=box.id+'.GEN'
  save,box,file=out_dir+path_sep()+box.id+'.sav'
  message,'Box structure saved to '+out_dir+path_sep()+box.id+'.sav',/cont
  
  
  if keyword_set(generic_only) then return 
  
  t0=systime(/seconds)
  message,'Generating chromo model..',/cont
  chromo_mask=decompose(box.base.bz,box.base.ic)
  box=keyword_set(old_combo_format)?combo_model_deprecated(box,chromo_mask):combo_model(box,chromo_mask)
  message,strcompress(string(systime(/seconds)-t0,format="('Chromo model generated in ',g0,' seconds')")),/cont
  box.id=box.id+'.CHR'
  save,box,file=out_dir+path_sep()+box.id+'.sav'
  message,'Box structure saved to '+out_dir+path_sep()+box.id+'.sav',/cont
end