pro gx_addlines2box, box,tr_height_km, status=status, physLength=physLength, avField=avField, startIdx=startIdx, endIdx=endIdx,lib_path=lib_path,elapsed_time=elapsed_time,_extra = _extra
  t0=systime(/seconds)
  if tag_exist(box,'bx') then begin
    dim=size(box.bx,/dim)
  endif else begin
    if tag_exist(box,'bcube') then begin
      ;this is needed to recompute lines for a box obtained from an already existing gx model because gx_model2box creates a box having a Bcube tag
      bx=box.bcube[*,*,*,0]
      by=box.bcube[*,*,*,1]
      bz=box.bcube[*,*,*,2]
      box=rem_tag(box,'Bcube')
      box=add_tag(box,temporary(bx),'bx',/no_copy,/duplicate)
      box=add_tag(box,temporary(by),'by',/no_copy,/duplicate)
      box=add_tag(box,temporary(bz),'bz',/no_copy,/duplicate)
    endif else message,'Required [Bx,By,Bz] or Bcube tags missing from the provided box argument'
  endelse 
  if not tag_exist(box,'dr') then box=add_tag(box,[1000,1000,1000]/gx_rsun(unit='km'),'dr',/no_copy,/duplicate)
  default,tr_height_km,1000
  chromo_level=tr_height_km
  if n_elements(lib_path) eq 0 then lib_path=gx_nlfff_libpath()
  rc=gx_box_calculate_lines(lib_path, box, status=status, physLength=physLength, avField=avField, startIdx=startIdx, endIdx=endIdx,chromo_level=chromo_level,_extra = _extra) 
  elapsed_time=systime(/seconds)-t0
  message,strcompress(string(elapsed_time,format="('Field line computation performed using DLL implementation in ',g0,' seconds')")),/cont
  idx=where((status and 4L) eq 4L)
  oidx=where(((status and 2L) eq 2l) and ((status and 4L) eq 0))
  bmed=avField[idx]
  obmed=avField[oidx]
  length=physlength[idx]
  olength=physlength[oidx]
  foot1=startIdx[idx]
  foot2=endIdx[idx]
  ofoot1=startIdx[oidx]
  ofoot2=endIdx[oidx]
  if n_elements(alpha) ne 0 then box=rep_tag_value(box,alpha,'alpha',/no_copy,/duplicate)
  if n_elements(curlb) ne 0 then box=rep_tag_value(box,curlb,'curlb',/no_copy,/duplicate)
  if n_elements(idx) ne 0 then box=rep_tag_value(box,idx,'idx',/no_copy,/duplicate)
  if n_elements(bmed) ne 0 then box=rep_tag_value(box,bmed,'bmed',/no_copy,/duplicate)
  if n_elements(length) ne 0 then box=rep_tag_value(box,length,'length',/no_copy,/duplicate)
  if n_elements(foot1) ne 0 then box=rep_tag_value(box,foot1,'foot1',/no_copy,/duplicate)
  if n_elements(foot2) ne 0 then box=rep_tag_value(box,foot2,'foot2',/no_copy,/duplicate)
  if n_elements(oalpha) ne 0 then box=rep_tag_value(box,alpha,'oalpha',/no_copy,/duplicate)
  if n_elements(ocurlb) ne 0 then box=rep_tag_value(box,curlb,'ocurlb',/no_copy,/duplicate)
  if n_elements(oidx) ne 0 then box=rep_tag_value(box,oidx,'oidx',/no_copy,/duplicate)
  if n_elements(obmed) ne 0 then box=rep_tag_value(box,obmed,'obmed',/no_copy,/duplicate)
  if n_elements(olength) ne 0 then box=rep_tag_value(box,olength,'olength',/no_copy,/duplicate)
  if n_elements(ofoot1) ne 0 then box=rep_tag_value(box,ofoot1,'ofoot1',/no_copy,/duplicate)
  if n_elements(ofoot2) ne 0 then box=rep_tag_value(box,ofoot2,'ofoot2',/no_copy,/duplicate)  
  bx=box.bx
  box=rem_tag(box,'bx')
  by=box.by
  box=rem_tag(box,'by')
  bz=box.bz
  box=rem_tag(box,'bz')
  dim=size(bx,/dim)
  bcube=fltarr(dim[0],dim[1],dim[2],3)
  bcube[*,*,*,0]=temporary(bx)
  bcube[*,*,*,1]=temporary(by)
  bcube[*,*,*,2]=temporary(bz)
  box=add_tag(box,temporary(bcube),'bcube',/no_copy,/duplicate)
end