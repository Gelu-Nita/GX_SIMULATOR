function gx_euvrender_ebtel_script,model,renderer=renderer,a=a,b=b,q=q,formula_q0=q0_formula,formula_q=q_formula,$
                                   xc=xc,yc=yc,xfov=xfov, yfov=yfov,nx=nx,ny=ny,gxcube=gxcube,_extra=_extra
if ~obj_valid(model) then begin
  file=dialog_pickfile(filter=['*.gxm','*.sav'],/read,/must_exist,title='Please select a file to restore a saved gxModel structure or object')
  break_file, file, disk_log, dir, filnam, ext, fversion, node, /last_dot
  case ext of
    '.gxm':model=gx_read(file)
    '.sav':model=gx_importmodel(file)
    else: begin
            message,'Unknown file type selected! Returning an invalid map object',/info
            return,obj_new()
          end  
  endcase
endif
;++++++++++++++++++++++++
model->ResetPosition
fovdata=model->SetFOV(xc=xc,yc=yc,xfov=xfov, yfov=yfov,nx=nx,ny=ny,_extra=_extra,/compute_grid)
;++++++++++++++++++++++++++++++
default,a,1
default,b,1
default,q,0.001
default,q0_formula,'q[0]'
default,q_formula,string(a,b,format="('q0*(B/q[1])^',g0,'/(L/q[2])^',g0)")
q_parms=[q, 100.0, 1.0000000d+009, 0.0, 0.0]
;+++++++++++++++++++++++++++++++++++++
default,renderer,'aia_lib.pro'
;+++++++++++++++++++++++++++++++++
return,gx_euvrender_ebtel(model,renderer,ebtel_path=ebtel_path, libpath=libpath,q_parms=q_parms,q_formula=q_formula,q0_formula=q0_formula,gxcube=gxcube,/all,_extra=_extra)

end