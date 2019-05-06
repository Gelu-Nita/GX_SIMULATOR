pro gx_model2box,model,box,destroy_model=destroy_bodel
  volume=model->GetVolume()
  volume->GetVertexAttributeData,'bmed',bmed
  volume->GetVertexAttributeData,'length',length
  volume->GetVertexAttributeData,'alpha',alpha
  volume->GetVertexAttributeData,'curlb',curlb
  volume->GetVertexAttributeData,'foot1',foot1
  volume->GetVertexAttributeData,'foot2',foot2
  volume->GetVertexAttributeData,'idx',idx
  volume->GetVertexAttributeData,'obmed',obmed
  volume->GetVertexAttributeData,'olength',olength
  volume->GetVertexAttributeData,'oalpha',oalpha
  volume->GetVertexAttributeData,'ocurlb',ocurlb
  volume->GetVertexAttributeData,'ofoot1',ofoot1
  volume->GetVertexAttributeData,'ofoot2',ofoot2
  volume->GetVertexAttributeData,'oidx',oidx
  if keyword_set(destroy_model) then obj_destroy,model
  if n_elements(alpha) ne 0 then box=add_tag(box,alpha,'alpha',/no_copy,/duplicate)
  if n_elements(curlb) ne 0 then box=add_tag(box,curlb,'curlb',/no_copy,/duplicate)
  if n_elements(idx) ne 0 then box=add_tag(box,idx,'idx',/no_copy,/duplicate)
  if n_elements(bmed) ne 0 then box=add_tag(box,bmed,'bmed',/no_copy,/duplicate)
  if n_elements(length) ne 0 then box=add_tag(box,length,'length',/no_copy,/duplicate)
  if n_elements(foot1) ne 0 then box=add_tag(box,foot1,'foot1',/no_copy,/duplicate)
  if n_elements(foot2) ne 0 then box=add_tag(box,foot2,'foot2',/no_copy,/duplicate)
  if n_elements(oidx) ne 0 then box=add_tag(box,oidx,'oidx',/no_copy,/duplicate)
  if n_elements(obmed) ne 0 then box=add_tag(box,obmed,'obmed',/no_copy,/duplicate)
  if n_elements(olength) ne 0 then box=add_tag(box,olength,'olength',/no_copy,/duplicate)
  if n_elements(ofoot1) ne 0 then box=add_tag(box,ofoot1,'ofoot1',/no_copy,/duplicate)
  if n_elements(ofoot2) ne 0 then box=add_tag(box,ofoot2,'ofoot2',/no_copy,/duplicate)
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