;this macro may be used to compute EUV emission from adjustable EBTEL models programatically, outside the GX_Simulator GUI
function gx_euvrender_ebtel,model,renderer,info=info,ebtel_path=ebtel_path,q_parms=q_parms,$
  q0_formula=q0_formula,q_formula=q_formula,gxcube=gxcube,map=map,use_dem=use_dem,has_used_ddm=has_used_ddm,_extra=_extra
  if ~isa(model) then begin
    message,'None or nvalid model provided! Operation aborted!',/info
    goto,skip
  endif else begin
    if ~obj_valid(model) then begin
      message,'None or nvalid model provided! Operation aborted!',/info
      goto,skip
    endif
  endelse
  default,renderer,'aia.pro'
  path=gx_ebtel_path(path)
  if isa(ebtel_path,/string) then begin
    if gx_ebtel_valid_path(ebtel_path) then if path ne ebtel_path then ebtel_path=gx_ebtel_path(ebtel_path)
  endif else ebtel_path=path
  message,'EBTEL path in use: '+file_basename(ebtel_path),/info
  default,q_parms,[0.000495, 100.00000, 1.0000000e+009, 1, 0.75]
  model->SetVertexData,'q0_coeff',q_parms
  volume=model->GetVolume()
  flags=volume->Setflags(/NTDEM)
  default, q0_formula, 'q[0]'
  q0_formula=volume->SetQ0(q0_formula,/quiet)
  default, q_formula, 'q[0]*(B/q[1])^q[3]*(L/q[2])^q[4]'
  q_formula=volume->SetQ(q_formula,/quiet)
  message,'EBTEL heating rate formula in use: '+q_formula,/info
  if volume->NewNT(NTkey) then volume->SetVertexAttributeData,'NTkey',NTkey
  gxcube=gx_render(model,renderer,_extra=_extra)
  fovmap=(model->GetFovMap())->get(/map)
  add_prop,fovmap,gx_key=string(model->GetVertexData('NTkey')),/replace
  (model->GetFovMap())->setmap,0,fovmap
  return,gx_gxcube2maps(gxcube)
  skip:
  return,isa(map)?map:!null
end