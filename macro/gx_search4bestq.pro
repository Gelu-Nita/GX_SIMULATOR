pro gx_search4bestq, gxm_path=gxm_path,a_arr=a_arr,b_arr=b_arr,q_start=q_start, $
                     modDir=modDir,psDir=psDir,tmpDir=tmpDir,refdatapath=refdatapath,$
                     ebtel_path=ebtel_path,renderer=renderer,$
                     xc=xc,yc=yc,xfov=xfov,yfov=yfov,nx=nx,ny=ny,$
                     levels=levels,resize=resize,result=result,save_gxc=save_gxc,redo=redo,$
                     save_result=save_result,plot_best=plot_best
  final_result=[]
  ;+++++++++++++++++++++++++++++++++++++++++++
  default,levels,[12,20,30,50,80]
  default,resize,100
  ;++++++++++++++++++++++++++++++++++++++++++++
  default,a_arr,[0.5,0.75,1,1.25,1.5]
  default,b_arr,[0.5,0.75,1,1.25,1.5]
  ;++++++++++++++++++++++++++++++++++++++++++++
  default,q_start,[0.0001,0.001]
  ;++++++++++++++++++++++++++++++++++++++++++++
  default,gxm_path,curdir()+'\gxmDIR\AR11520.gxm'
  default,xc,-86.68
  default,yc,-320.0
  default,xfov,200.0
  default,yfov,200.0
  default,nx,200
  default,ny,200
  ;++++++++++++++++++++++++++++++++++++++++++++
  default,modDir,curdir()+'\modDir'
  default,psDir,curdir()+'\psDir'
  default,tmpDir,curdir()+'\tmpDir'
  ;++++++++++++++++++++++++++++++++++++++++++++++
  default,ebtel_path, gx_findfile('ebtel.sav',folder='')
  ;+++++++++++++++++++++++++++++++++++++++++++++
  default,renderer,'AR_GRFF_nonLTE'
  ;+++++++++++++++++++++++++++++++++++++++++++++
  default,refdatapath,'norh_ref.sav'
  restore,refdatapath
  ;+++++++++++++++++++++++++++++++++++++++++++++
  if not file_test(modDir) then file_mkdir,modDir
  if keyword_set(save_gxc) then begin
    gxcDir=modDir+'\gxc'
    if not file_test(gxcDir) then file_mkdir,gxcDir
  end
  if not file_test(psDir) then file_mkdir,psDir
  
  map=obj_new('map')
  q=q_start
  for ii=0,n_elements(a_arr)-1 do begin
  for jj=0,n_elements(b_arr)-1 do begin
      ;+++++++++++++++++++++++++++++++++++++
     a=a_arr[ii]
     b=b_arr[jj]
     t0=systime(/s)
     done=0b
     almost_done=0
     done_res=0b
     done_chi=0b
     apply2=1
     force_done=0
     repeat begin; until done  
        for j=0,n_elements(q)-1 do begin
          modfile=modDir+strcompress(string(a,b,q[j],format="('\i_a',f7.2,'b',f7.2,'q',g0,'.map')"),/rem)
          if ~file_exist(modfile) or keyword_set(redo)then begin
            if ~isa(model,'gxmodel') then begin
              model=gx_read(gxm_path)
              fovdata=model->SetFOV(xc=xc,yc=yc,xfov=xfov, yfov=yfov,nx=nx,ny=ny,/compute_grid)
              end
            q0_formula='q[0]'
            q_formula=string(a,b,format="('q0*(B/q[1])^',g0,'/(L/q[2])^',g0)")
            q_parms=[q[j], 100.0, 1.0000000d+009, 0.0, 0.0]
            omap=gx_mwrender_ebtel(model,renderer,ebtel_path=ebtel_path,q_parms=q_parms,q_formula=q_formula,q0_formula=q0_formula,f_min=17d9,n_freq=1,gxcube=gxcube)
            if obj_valid(omap) then begin
                map->setmap,0,omap->get(0,/map)
                save,map,file=modfile
                obj_destroy,omap
            endif
            if (isa(gxcube) and keyword_set(save_gxc)) then save,gxcube,file=gxcDir+strcompress(string(a,b,q[j],format="('\a',f7.2,'b',f7.2,'q',g0,'.gxc')"),/rem)
          endif else gx_message, modfile+' already exists, no reprocessing requested!',/info,/cont
        endfor
        result=gx_processmwmodels_ebtel(ab=[a,b],ref=ref,$
                           modDir=modDir,obsDir=obsDir,psDir=psDir,$
                           levels=levels,resize=resize,$
                           file_arr=file_arr,apply2=apply2,done=force_done)

        if result.res_done eq 0 or result.chi_done  eq 0 then begin
          nq=n_elements(q)
          if result.res_done eq 0 then q=[q,result.q_res_best] else q=[q,result.q_chi_best]
          q=q[uniq(q,sort(q))]  
         if n_elements(q) eq nq then begin
          if apply2 eq 1 then apply2=3 else force_done=1
         endif
       endif else if apply2 eq 3 then done=1 else apply2=3        
     endrep until done
     final_result=[final_result,result]   
 endfor
 endfor
 gx_message, string(n_elements(a_arr)*n_elements(b_arr),(systime(/s)-t0)/3600,format="('Computed ',i2,' sets in ',g0,' hours')"),/info,/cont
 obj_destroy,map
 result=final_result
 if size(save_result,/tname) eq 'STRING' then save,result,file=strcompress(tmpDir+path_sep()+save_result)
 if keyword_set(plot_best) then gx_plotbestmwmodels_ebtel, result, psDir
 end