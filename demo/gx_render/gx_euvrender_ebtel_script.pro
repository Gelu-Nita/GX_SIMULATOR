function gx_euvrender_ebtel_script,model,renderer,xc=xc,yc=yc,xfov=xfov, yfov=yfov,nx=nx,ny=ny,gxcube=gxcube
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
;+++++++++++++++++++++++++++++
;just as an example
default,xc,-86.68
default,yc,-320.0
default,xfov,200.0
default,yfov,200.0
default,nx,100
default,ny,100
;++++++++++++++++++++++++
fovdata=model->SetFOV(xc=xc,yc=yc,xfov=xfov, yfov=yfov,nx=nx,ny=ny,/compute_grid)
;++++++++++++++++++++++++++++++
a=2
b=1
q=0.001
q0_formula='q[0]'
q_formula=string(a,b,format="('q0*(B/q[1])^',g0,'/(L/q[2])^',g0)")
q_parms=[q, 100.0, 1.0000000d+009, 0.0, 0.0]
;+++++++++++++++++++++++++++++++++++++
default,renderer,'aia.pro'
;+++++++++++++++++++++++++++++++++
return,gx_euvrender_ebtel(model,renderer,ebtel_path=ebtel_path,q_parms=q_parms,q_formula=q_formula,q0_formula=q0_formula,gxcube=gxcube)

end