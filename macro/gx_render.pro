;this is general purpose macro that may be used to compute radiation transfer images programatically
function gx_render,model,renderer,_extra=_extra
  t0=systime(/s)
  if ~isa(model) then begin
    message,'None or nvalid model provided! Operation aborted!',/cont
    return,!null
  endif else begin
    if ~obj_valid(model) then begin
      message,'None or nvalid model provided! Operation aborted!',/cont
      return,!null
    endif
  endelse
  info=gx_rendererinfo(renderer)
  if ~isa(info) then begin
    message,'Invalid renderer routine! Operation aborted!',/cont
    return,!null
  endif
  if isa(_extra) then begin
    names=tag_names(_extra)
    for k=0,n_elements(names)-1 do begin
      idx=gx_name2idx(info.parms,names[k])
      if idx ge 0 and isa(_extra.(k),/number) then info.parms[idx].value=_extra.(k)
    endfor
  endif
  dr=model->GetFovPixSize(unit='cm')
  info.parms[gx_name2idx(info.parms,'dS')].value=dr[0]*dr[1]
  info=gx_rendererinfo(renderer,info=info)
  fovmap=model->GetFovMap()
  sz=size(fovmap->get(/data))
  nx=sz[1]
  ny=sz[2]
  rowdata=make_array([nx,info.pixdim],/float)
  dim=[nx,ny,info.pixdim]
  data=make_array(dim,/float)
  t0=systime(/s)
  for row=0, ny-1 do begin
    print,strcompress(string(row+1,ny,format="('computing image row ', i5,' out of', i5)"))
    rowdata[*]=0
    model->Slice,info.parms,row,scanner=scanner
    parms=(*scanner).parms
    result=execute(info.execute)
    data[*,row,*,*,*]=rowdata
  endfor
  model->getproperty,xcoord_conv=dx,ycoord_conv=dy
  dx=dx[1]
  dy=dy[1]
  dim=model->Size()
  dz=model->GetVertexData('dz')
  if n_elements(dz) eq 0 then begin
    model->getproperty,zcoord_conv=dz
    dz=dz[1]
  endif
  gxcube={info:info,data:data,renderer:renderer,fovmap:fovmap,model:{dx:dx,dy:dy,dz:dz,dim:dim[1:3]}}
  print,strcompress(string(systime(/s)-t0,format="('Computation done in ',f10.3,' seconds')"))
  return,gxcube
end