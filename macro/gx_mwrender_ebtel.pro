;this macro may be used to compute MW emission from adjustable EBTEL models programatically, outside the GX_Simulator GUI
function gx_mwrender_ebtel,model,renderer,ebtel_path=ebtel_path,ss=ss,q_parms=q_parms,$
  q0_formula=q0_formula,q_formula=q_formula,gxcube=gxcube,map=map,use_dem=use_dem,has_used_ddm=has_used_ddm,_extra=_extra
  if ~isa(model) then begin
    message,'None or nvalid model provided! Operation aborted!',/cont
    goto,skip
  endif else begin
    if ~obj_valid(model) then begin
      message,'None or nvalid model provided! Operation aborted!',/cont
      goto,skip
    endif
  endelse
  info=gx_rendererinfo(renderer)
  if ~isa(info) then begin
    message,'Invalid renderer routine! Operation aborted!',/cont
    goto,skip
  endif
  path=gx_ebtel_path(path,ss=ss)
  if isa(ebtel_path,/string) then begin
    if gx_ebtel_valid_path(ebtel_path) then if path ne ebtel_path then ebtel_path=gx_ebtel_path(ebtel_path,ss=ss)
  endif else ebtel_path=path
  message,'EBTEL path in use: '+file_basename(ebtel_path),/cont
  default,q_parms,[0.000415, 100.00000, 1.0000000e+009, 1, 0.75]
  model->SetVertexData,'q0_coeff',q_parms
  volume=model->GetVolume()
  flags=volume->Setflags(/NTDEM,NTSSDEM=keyword_set(ss))
  default, q0_formula, 'q[0]'
  q0_formula=volume->SetQ0(q0_formula,/quiet)
  default, q_formula, 'q[0]*(B/q[1])^(q[3])*(L/q[2])^(q[4])'
  q_formula=volume->SetQ(q_formula,/quiet)
  message,'EBTEL heating rate formula in use: '+q_formula,/cont
  volume->Update,/nt,use_dem=use_dem,has_used_ddm=has_used_ddm
  volume->Update,/force
  fovmap=(model->GetFovMap())->get(/map)
  add_prop,fovmap,gx_key=string(model->GetVertexData('NTkey')),/replace
  (model->GetFovMap())->setmap,0,fovmap
  return,gx_mwrender(model,renderer,map=map,gxcube=gxcube,_extra=_extra)
  skip:
  return,isa(map)?map:!null
end