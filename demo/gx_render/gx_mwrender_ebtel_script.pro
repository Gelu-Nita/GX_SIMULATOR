function gx_mwrender_ebtel_script,model,renderer,xc=xc,yc=yc,xfov=xfov, $
         yfov=yfov,nx=nx,ny=ny,gxcube=gxcube,a=a,b=b,q=q,use_dem=use_dem
if ~obj_valid(model) then begin
  file=dialog_pickfile(filter=['*.gxm','*.sav'],/read,/must_exist,title='Please select a file to restore a saved gxModel striucture or object')
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
default,a,0.4
default,b,1.4
default,q,0.00261868
default,use_dem,0
q0_formula='q[0]'
q_formula=string(a,b,format="('q0*(B/q[1])^',g0,'/(L/q[2])^',g0)")
q_parms=[q, 100.0, 1.0000000d+009, 0.0, 0.0]
;+++++++++++++++++++++++++++++++++++++
default,renderer,gx_findfile((!version.os_family eq 'Windows')?'AR_GRFF_nonLTE.pro':'grff_dem_transfer.pro',folder='')
;+++++++++++++++++++++++++++++++++
return,gx_mwrender_ebtel(model,renderer,ebtel_path=ebtel_path,q_parms=q_parms,q_formula=q_formula,q0_formula=q0_formula,f_min=17d9,n_freq=1,gxcube=gxcube,use_dem=use_dem)

end