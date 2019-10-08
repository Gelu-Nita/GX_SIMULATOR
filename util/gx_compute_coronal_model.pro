pro gx_compute_coronal_model,tr_height_km,use_idl=use_idl,_extra=_extra
  default,tr_height_km,1000
  tr_height=tr_height_km/(gx_rsun(unit='km'))
  file=dialog_pickfile(filter=['*.gxm','*.sav'])
  if file eq '' then return
  break_file, file, disk_log, dir, filnam, ext, fversion, node, /last_dot
  if keyword_set(use_idl) or !version.os_family ne 'Windows' then begin
    if strupcase(ext) eq '.SAV' then begin
      model=gx_importmodel(box=box)
    endif else model=gx_read(file)
    model->computecoronalmodel,tr_height=tr_height,/compute,_extra=_extra
    tr_height_km=tr_height*(gx_rsun(unit='km'))
    if n_elements(box) eq 0 then begin
      save,model,file=file
    endif else begin
      gx_copylines2box,model,box
      save,box,file=file
    endelse
  endif else begin
    if strupcase(ext) eq '.SAV' then begin
      restore,file
      gx_addlines2box, box,tr_height_km
      save,box,file=file
    endif else begin
      model=gx_read(file)
      dummy=model->GetB(Bx=Bx,By=By,Bz=Bz)
      model->GetProperty,dr=dr
      volume=model->GetVolume()
      box={bx:bx,by:by,bz:bz,dr:dr}
      gx_addlines2box, box,tr_height_km
      if tag_exist(box,'idx') then begin
        volume->SetVertexAttributeData,'idx',box.idx
      end
      if tag_exist(box,'bmed') then begin
        volume->SetVertexAttributeData,'bmed',box.bmed
      end
      if tag_exist(box,'length') then begin
        volume->SetVertexAttributeData,'length',box.length
      end
      if tag_exist(box,'alpha') then begin
        volume->SetVertexAttributeData,'alpha',box.alpha
      end
      if tag_exist(box,'curlb') then begin
        volume->SetVertexAttributeData,'curlb',box.curlb
      end
      if tag_exist(box,'foot1') then begin
        volume->SetVertexAttributeData,'foot1',box.foot1
      end
      if tag_exist(box,'foot2') then begin
        volume->SetVertexAttributeData,'foot2',box.foot2
      end
      if tag_exist(box,'oidx') then begin
        volume->SetVertexAttributeData,'oidx',box.oidx
      end
      if tag_exist(box,'obmed') then begin
        volume->SetVertexAttributeData,'obmed',box.obmed
      end
      if tag_exist(box,'olength') then begin
        volume->SetVertexAttributeData,'olength',box.olength
      end
      if tag_exist(box,'ofoot1') then begin
        volume->SetVertexAttributeData,'ofoot1',box.ofoot1
      end
      if tag_exist(box,'ofoot2') then begin
        volume->SetVertexAttributeData,'ofoot2',box.ofoot2
      end
      save,model,file=file
    endelse
  endelse
end