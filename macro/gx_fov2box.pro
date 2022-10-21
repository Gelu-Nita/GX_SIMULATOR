;center_arcsec: FOV center cordinates, [xc,yc] in arcseconds
;centre: deprecated keyword used in previous version to provide the center_arcsec coordinates
;size_pix: size of the extrapolation box, [nx,ny,nz] 
;dx_km: voxel size in Km (assumed to be uniform, i.e. dx=dy=dz)  
;out_dir: user defined local directory to save the output models in
;tmp_dir: user defined local directory to save the nput files downloaded from the SDO repository
;entry_box: use this keyword argument to provide a preexisting box to be used instead of an empty box
;                      using this argument results in skipping the adata download section of the code
;/empty_box_only: set this keyword to stop the script after the empty box structure is generated (no extrapolation yet performed, just the boundary conditions stored in the box)
;/save_empty_box: set this keyword to save the boundary condition box
;/potential_only: set this keyword to gcompute only the potential extrapolation
;/save_potential: set this keyword to save the potential extrapolation box (which used to generate the boundary box for the following pipeline steps)
;/save_bounds: set this keyword to save the boundary box
;/use_potential: set this keyword to skip the NLFFF extrapolation step
;/nlfff_only: set this keyword to stop the pipeline script after the NLFFF box is computed 
;/generic_only: set this keyword to stop the script after the information related the computed volume magnetic lines are added to the box
;/euv: use this keyword to download the context SDO EUV maps and add tem as reference maps in the box structure
;/uv: use this keyword to download the context SDO UV maps and add tem as reference maps in the box structure
; use the following keywords only in conjuction with the entry_box keyword argument
;             to indicate the stage in the AMPP where the empty_box will be used as start downstream computations
;   /jump2potential
;   /jump2nlfff
;   /jump2lines
;   /jump2chromo
; the above keywords are take precedence in the order listed above, all other, if provided, being ignored. 
; If entry_box is not provided, any "jump2*' keywords is ignored. 
;hmifiles: use this keyword to suply a valid hmi filepath structure pinting to a local repository, in order to bypass the attempt to download them 
;_extra: use this keyword to provide additional keywords that may be used by various routines called by the pipeline , such as
;_extra keywords that may be used by gx_box_create.pro
; /cea: to use CEA projection
; /top: to use TOP_VIEW projection
; /sfq: to use SFQ disambiguation
; /old_combo_format: to generate combo models using the format used before May 2020 (for testing purposes)
;_extra keywords that may be used when computing the coronal magnetic field lines properties
;   reduce_passed=0; computes lines that pass exactly through the center of each volume voxel (more computationally intensive)
;   reduce_passed=1; marks as already passed all voxels intersected by some closed field line
;   reduce_passed=2; marks as already passed all voxels intersected by some open field line
;   reduce_passed=3; marks as already passed all voxels intersected by a closed or open field line
;   /center vox: the same as reduce_passed=0: use this keyword to compute lines that pass exactly through the center of each volume voxel (more computationaly intensive)
;   as opposed to the faster option (default on Unix/LINUX), which assigns the same <B>-L properties to all voxels crossed by an already computed field line

pro gx_fov2box,time, center_arcsec=center_arcsec, size_pix=size_pix, dx_km=dx_km, out_dir = out_dir, tmp_dir = tmp_dir,$
                        empty_box_only=empty_box_only,save_empty_box=save_empty_box,potential_only=potential_only,$
                        save_potential=save_potential,save_bounds=save_bounds,use_potential=use_potential,$
                        nlfff_only=nlfff_only, generic_only=generic_only,centre=centre,euv=euv,uv=uv,hmifiles=hmifiles,$
                        tr_height_km=tr_height_km,old_combo_format=old_combo_format,out_files=out_files,wConsole=wConsole,$
                        entry_box=entry_box,jump2potential=jump2potential,jump2nlfff=jump2nlfff,jump2lines=jump2lines,$
                        jump2chromo=jump2chromo,info=info,lib_path=lib_path,_extra=_extra
 CATCH, Error_status
 IF Error_status NE 0 THEN BEGIN
    if n_elements(wConsole) ne 0 then console= long(wConsole)
    if widget_valid(wConsole) then begin
      widget_control,wConsole,get_value=txt
      widget_control,wConsole,set_value=[txt,'% '+!ERROR_STATE.MSG,'% ABORTED!']
    endif else print,[['% '+!ERROR_STATE.MSG],['% GX_FOV2BOX: ABORTED!']]
    CATCH, /CANCEL
    return
 ENDIF
 setenv, 'WCS_RSUN=6.96d8'
 par=ROUTINE_INFO('gx_fov2box',/par)
 exec=''
 parms=n_elements(time)?{time:time}:{time:''}
 for i=0, par.num_kw_args-2 do begin
   if par.kw_args[i] ne 'WCONSOLE' then begin
     dummy=execute('set=n_elements('+par.kw_args[i]+') ne 0')
     if set eq 1 then begin
       dummy=execute('val='+par.kw_args[i])
       parms=create_struct(parms,par.kw_args[i],val)
       if size(val,/tname) ne 'STRUCT' then begin
         if size(val,/tname) ne 'STRING' then begin
           val=n_elements(val) eq 1?arr2str(val):'['+arr2str(val)+']'
         endif else val="'"+val+"'"
         val=strcompress(val)
         exec+=', '+par.kw_args[i]+'='+val
       endif
     endif
   end
 endfor
 if size(_extra,/tname) eq 'STRUCT' then begin
   parms=create_struct(parms,_extra)
   tnames=tag_names(_extra)
   for i=0,n_elements(tnames)-1 do begin
     val=_extra.(i)
     val=n_elements(val) eq 1?arr2str(val):'['+arr2str(val)+']'
     val=strcompress(val)
     exec+=', '+tnames[i]+'='+val
   endfor
 endif
 
 if arg_present(info) then begin
  info=temporary(parms)
  if ~valid_time(info.time) then info=rem_tag(info,'time')
  return
 endif
  
  out_files=[]
  t_start=systime(/s)
  validbox=0
  if n_elements(entry_box) ne 0 then begin
    if file_exist(entry_box) then begin
      entry_box=gx_readbox(entry_box,info=info)
      time=info.time
      validbox=1
    endif else begin
      if gx_validbox(entry_box) then begin
        file=entry_box.id+'.sav'
        time=gx_id2time(entry_box.id)
        validbox=1
      endif
    endelse
    if ~validbox then gx_message, 'the entry_box argument provided is not a compatible AMPP box structure!',wConsole
  endif else validbox=0  
  
  if n_elements(time) eq 0 then gx_message,'Required time input is missing!',wConsole
  if ~valid_time(time,err) then gx_message,err,wConsole 
  
  if not keyword_set(tmp_dir) then tmp_dir = (!version.os_family eq 'Windows')?'C:\jsoc_cache':filepath('jsoc_cache',root = curdir())
  if not file_test(tmp_dir) then file_mkdir, tmp_dir
  if not keyword_set(out_dir) then out_dir = (!version.os_family eq 'Windows')?'C:\gx_models':filepath('gx_models',root = curdir())
  out_dir=out_dir+path_sep()+anytim(strreplace(time,'.','-'),/ccsds,/date)
  if not file_test(out_dir) then file_mkdir, out_dir 
  
  if validbox then goto,entry_line
 
  
  ;for backward compatibility with deprecated "centre" input
  if n_elements(center_arcsec) ne 2 then if n_elements(centre) eq 2 then center_arcsec=centre
  if n_elements(center_arcsec) ne 2 then begin
    gx_message,'Required center_arcsec input is missing or incorrect!',wConsole
  endif
  
  setenv, 'WCS_RSUN=6.96d8'
  exec="gx_fov2box, '"+time+"'"+exec
 
  t0=systime(/seconds)
  gx_message,'Downloading data...',wConsole
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
  gx_message, strcompress(string(systime(/seconds)-t0,format="('Data already found in the local repository or downloaded in ',g0,' seconds')")), wConsole
  t0=systime(/seconds)
  gx_message,'Creating the box structure...', wConsole
  
  box = gx_box_create(files.field, files.inclination, files.azimuth,files.disambig, files.continuum, center_arcsec, size_pix, dx_km,_extra=_extra)
  box=create_struct(box,'execute',exec)
  
  gx_box_add_vertical_current_map, box, files.field, files.inclination, files.azimuth, files.disambig
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
  gx_message,strcompress(string(systime(/seconds)-t0,format="('Box structure created in ',g0,' seconds')")), wConsole  

  if keyword_set(empty_box_only) or keyword_set(save_empty_box) then begin
    file=out_dir+path_sep()+box.id+'.sav'
    save,box,file=file
    out_files=[out_files,file]
    gx_message,'Empty box structure saved to '+file, wConsole
  endif
  if keyword_set(empty_box_only) then goto,exit_point
  
  entry_line:
  if n_elements(entry_box) ne 0 then begin
      box=temporary(entry_box)
      if tag_exist(box,'bcube') then begin
        bx=box.bcube[*,*,*,0]
        by=box.bcube[*,*,*,1]
        bz=box.bcube[*,*,*,2]
        box=rem_tag(box,'Bcube')
        box=add_tag(box,temporary(bx),'bx',/no_copy,/duplicate)
        box=add_tag(box,temporary(by),'by',/no_copy,/duplicate)
        box=add_tag(box,temporary(bz),'bz',/no_copy,/duplicate)
      endif 
      box.execute+=' & gx_fov2box'+(size(file,/tname) eq 'STRING'?',empty_box='+"'"+file+"'":'')+exec
      if keyword_set(jump2potential) then begin
        box.id=strjoin((strsplit(box.id,'.',/extract))[0:4],'.')
        goto,compute_potential
      endif
      if keyword_set(jump2nlfff) then begin
        box.id=strjoin((strsplit(box.id,'.',/extract))[0:4],'.')
        goto,compute_nlfff
      endif
      if keyword_set(jump2lines) then goto,compute_lines
      if keyword_set(jump2chromo) then goto,compute_chromo
  endif
  
  compute_potential:
  t0=systime(/seconds)
  gx_message,'Performing initial potential extrapolation', wConsole
  gx_box_make_potential_field, box,pbox
  gx_message,strcompress(string(systime(/seconds)-t0,format="('Potential extrapolation performed in ',g0,' seconds')")), wConsole
  
  if size(pbox,/tname) eq 'STRUCT' and ( keyword_set(save_potential) or keyword_set(potential_only) ) then begin
    file=out_dir+path_sep()+pbox.id+'.sav'
    save,pbox,file=file
    out_files=[out_files,file]
    gx_message,'Potential box structure saved to '+file, wConsole
  end
  
  if keyword_set(save_bounds) then begin
    file=out_dir+path_sep()+box.id+'.sav'
    save,box,file=file
    out_files=[out_files,file]
    gx_message,'Bound Box structure saved to '+file, wConsole
  endif
 
  if keyword_set(potential_only) then goto,exit_point
  
  if keyword_set(use_potential) then begin
    if size(pbox,/tname) eq 'STRUCT' then box=temporary(pbox)
    goto,compute_lines
  endif else begin
    pbox=rem_tag(pbox,'refmaps')
    free_var,pbox,/delete
  endelse
  
  compute_nlfff:
  lib_path=gx_libpath('nlfff')
  t0=systime(/seconds)
  gx_message,'Performing NLFFF extrapolation', wConsole
  return_code = gx_box_make_nlfff_wwas_field(lib_path, box,_extra=_extra)
  gx_message,strcompress(string(systime(/seconds)-t0,format="('NLFFF extrapolation performed in ',g0,' seconds')")), wConsole
  file=out_dir+path_sep()+box.id+'.sav'
  save,box,file=file
  out_files=[out_files,file]
  gx_message,'NLFFF box structure saved to '+file, wConsole
  
  if keyword_set(nlfff_only) then goto,exit_point
  
  compute_lines:
  lib_path=gx_libpath('nlfff')
  gx_message,'Computing field lines for each voxel in the model..', wConsole
  default,tr_height_km,1000
  tr_height_sunradius=tr_height_km/(gx_rsun(unit='km'))
  reduce_passed=1b
  if size(_extra,/tname) eq 'STRUCT' then begin
    if tag_exist(_extra,'center_vox') then begin
      reduce_passed=byte(~keyword_set(_extra.center_vox))
    endif else if tag_exist(_extra,'reduce_passed') then reduce_passed=byte(_extra.reduce_passed)
  end  
  gx_addlines2box, box,tr_height_km,reduce_passed=reduce_passed,lib_path=lib_path
  box.id=box.id+'.GEN'
  file=out_dir+path_sep()+box.id+'.sav'
  save,box,file=file
  out_files=[out_files,file]
  gx_message,'Box structure saved to '+file, wConsole
  
  if keyword_set(generic_only) then goto,exit_point 
  
  compute_chromo:
  t0=systime(/seconds)
  if gx_boxhaschromo(box,tags=tags) then begin
    gx_message,'This box already has a chromo model, removing it to create a new one..', wConsole
    box=rem_tag(box,tags)
  endif
  gx_message,'Generating chromo model..', wConsole
  chromo_mask=decompose(box.base.bz,box.base.ic)
  box=keyword_set(old_combo_format)?combo_model_deprecated(box,chromo_mask):combo_model(box,chromo_mask)
  gx_message,strcompress(string(systime(/seconds)-t0,format="('Chromo model generated in ',g0,' seconds')")), wConsole
  file=out_dir+path_sep()+box.id+'.sav'
  save,box,file=file
  out_files=[out_files,file]
  gx_message,'Box structure saved to '+file, wConsole
  exit_point:
  gx_message,[string(systime(/seconds)-t_start,format="('This AMPP script has been executed in ',g0,' seconds and generated the following files:')"),'',out_files], wConsole
  if n_elements(wConsole) ne 0 then gx_message,'', wConsole else print,' '
  gx_message,'You may use "Import Model Data" file menu option to import any of these models in GX_Simulator.', wConsole
end