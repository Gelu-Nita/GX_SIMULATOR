;+
; Returns an array of structs (one per a,b). Legacy image tags are always present.
; Spectrum mode adds search_mode, spec_axis, S_obs, S_sdev, S_mod_*_best, and
; spec_allmetrics — see header of gx_processmodels_ebtel.pro. To display one
; spectrum channel with legacy plotters/GUI, use gx_result_select_channel.
;-
function gx_search4bestq, gxmpath=gxmpath,a_arr=a_arr,b_arr=b_arr,q_start=q_start, $
                     modDir=modDir,psDir=psDir,tmpDir=tmpDir,refdatapath=refdatapath,$
                     ebtel_path=ebtel_path,renderer=renderer,info=info,$
                     xc=xc,yc=yc,xfov=xfov,yfov=yfov,nx=nx,ny=ny,$
                     levels=levels,resize=resize,save_gxc=save_gxc,redo=redo,$
                     save_result=save_result,plot_best=plot_best,freq=freq,chan=chan,$
                     search_mode=search_mode,spec_freq=spec_freq,spec_chan=spec_chan,$
                     nobackground=nobackground,_extra=_extra
  final_result=[]
  catch, error_status
  if error_status ne 0 then begin
    catch, /cancel
    gx_message,!error_state.msg + ' ' + !error_state.sys_msg
    result=final_result
    return, result
  endif
  ;+++++++++++++++++++++++++++++++++++++++++++
  default,levels,[12,20,30,50,80]
  ;default,resize,100
  ;++++++++++++++++++++++++++++++++++++++++++++
  default,a_arr,[0.5,0.75,1,1.25,1.5]
  default,b_arr,[0.5,0.75,1,1.25,1.5]
  ;++++++++++++++++++++++++++++++++++++++++++++
  default,q_start,[0.0001,0.001]
  ;++++++++++++++++++++++++++++++++++++++++++++
  default,xc,-86.68
  default,yc,-320.0
  default,xfov,200.0
  default,yfov,200.0
  default,nx,200
  default,ny,200
  ;++++++++++++++++++++++++++++++++++++++++++++
  default,modDir,curdir()+path_sep()+'modDir'
  default,psDir,curdir()+path_sep()+'psDir'
  default,tmpDir,curdir()+path_sep()+'tmpDir'
  ;++++++++++++++++++++++++++++++++++++++++++++++
  default,ebtel_path, gx_findfile('ebtel.sav',folder='')
  ;+++++++++++++++++++++++++++++++++++++++++++++
  default,renderer,gx_findfile((!version.os_family eq 'Windows')?'AR_GRFF_nonLTE.pro':'grffdemtransfer.pro',folder='')
  ;+++++++++++++++++++++++++++++++++++++++++++++
  default,search_mode,'image'
  search_mode=strlowcase(strcompress(search_mode,/rem))
  spectrum_mode=search_mode eq 'spectrum'
  ;
  ; Frequency / channel roles:
  ;   Image mode:
  ;     freq= (MW) or chan= (EUV) = single-channel image metric axis.
  ;     Optional _extra.freqlist may synthesize more MW frequencies.
  ;   Spectrum mode (MW):
  ;     Synthesis list = _extra.freqlist if set, else freq=.
  ;     Metric axis     = spec_freq= if set, else the full synthesis list.
  ;   Spectrum mode (EUV):
  ;     Renderer always synthesizes all instrument channels.
  ;     chan= omitted  → metric/ref axis = all channels in the reference set.
  ;     chan=[...]     → must list 2+ channels (a single chan= is an error).
  ;     spec_chan= is not used (avoids confusion with image-mode chan=).
  ;
  mw_mode=0b
  if spectrum_mode then begin
    if isa(_extra,'STRUCT') and tag_exist(_extra,'freqlist') then begin
      synth_freq=double(_extra.freqlist)
      mw_mode=1b
    endif else if n_elements(freq) gt 0 then begin
      synth_freq=double(freq)
      mw_mode=1b
    endif

    ; Same ref loader as image mode: objarr (or single) of CHMP map objects
    ref_all=gx_ref2chmp(refdatapath,err_msg=err_msg,_extra=_extra)
    if size(ref_all,/tname) ne 'OBJREF' then $
      message,'Failed to load reference maps via gx_ref2chmp: '+ $
        (size(err_msg,/tname) eq 'STRING'?strjoin(err_msg,' '):'unknown error')

    if mw_mode then begin
      if n_elements(spec_freq) gt 0 then metric_freq=double(spec_freq) else metric_freq=synth_freq
      if n_elements(metric_freq) eq 1 then $
        message,'WARNING: spectrum mode with a single metric frequency degenerates to one-point spectral metrics.',/info
      if isa(_extra,'STRUCT') then begin
        if tag_exist(_extra,'freqlist') then _extra.freqlist=synth_freq $
        else _extra=create_struct(_extra,'freqlist',synth_freq)
      endif else _extra={freqlist:synth_freq}
      ref=gx_ref_select_axis(ref_all,freq=metric_freq,err_msg=err_msg,is_chan=spec_is_chan,axis=spec_axis)
    endif else begin
      if n_elements(spec_chan) gt 0 then $
        message,'WARNING: spec_chan= is ignored for EUV spectrum mode; use chan=[...] (2+) or omit chan= to use all reference channels.',/info
      if n_elements(chan) eq 1 then $
        message,"search_mode=spectrum cannot use a single chan= value; omit chan= to use all channels in the reference set, or pass chan=[...] with at least two channels"
      if n_elements(chan) ge 2 then begin
        ref=gx_ref_select_axis(ref_all,chan=chan,err_msg=err_msg,is_chan=spec_is_chan,axis=spec_axis)
      endif else begin
        ref=gx_ref_select_axis(ref_all,err_msg=err_msg,is_chan=spec_is_chan,axis=spec_axis)
      endelse
      if size(ref,/tname) eq 'OBJREF' then begin
        if n_elements(ref) lt 2 then $
          message,'search_mode=spectrum needs at least two EUV reference channels; the reference set has fewer than two'
      endif
    endelse
    if size(ref,/tname) ne 'OBJREF' then $
      message,'Failed to select spectrum reference axis: '+ $
        (size(err_msg,/tname) eq 'STRING'?strjoin(err_msg,' '):'unknown error')
    ref_geom=ref[0]
  endif else begin
    ref=gx_ref2chmp(refdatapath,freq=freq,chan=chan,err_msg=err_msg,_extra=_extra)
    ref_geom=ref
    mw_mode=n_elements(freq) gt 0
  endelse
  ;+++++++++++++++++++++++++++++++++++++++++++++
  if not file_test(modDir) then file_mkdir,modDir
  if keyword_set(save_gxc) then begin
    gxcDir=modDir+path_sep()+'gxc'
    if not file_test(gxcDir) then file_mkdir,gxcDir
  end
  if not file_test(psDir) then file_mkdir,psDir
  
  map=obj_new('map')
  for ii=0,n_elements(a_arr)-1 do begin
  for jj=0,n_elements(b_arr)-1 do begin
      ;+++++++++++++++++++++++++++++++++++++
     a=a_arr[ii]
     b=b_arr[jj]
     q=q_start
     t0=systime(/s)
     done=0b
     almost_done=0
     done_res=0b
     done_chi=0b
     default,apply2,3
     force_done=0
     if ~spectrum_mode and n_elements(freq) ne 0 then begin
       if isa(_extra,'STRUCT') then begin
         if ~tag_exist(_extra,'f_min') then _extra=create_struct(_extra,'f_min',freq*1d9)
         if ~tag_exist(_extra,'n_freq') then _extra=create_struct(_extra,'n_freq',1)
       endif else _extra={f_min:freq*1d9,n_freq:1}
     endif
     counter=0
     REPEAT BEGIN; until done       
        for j=0,n_elements(q)-1 do begin
          modfile=modDir+path_sep()+strcompress(string(a,b,q[j],format="('i_a',f7.2,'b',f7.2,'q',g0,'.map')"),/rem)
          if ~file_exist(modfile) or keyword_set(redo)then begin
            if ~isa(model,'gxmodel') then begin
              model=gx_read(gxmpath)
              (model->Corona())->SetProperty,ignore=keyword_set(nobackground)
              fovdata=model->SetFOV(b0=ref_geom->get(/b0),l0=ref_geom->get(/l0),rsun=ref_geom->get(/rsun),$
                                    xc=xc,yc=yc,xfov=xfov, yfov=yfov,nx=nx,ny=ny,/compute_grid,_extra=_extra)
              end
            if tag_exist(_extra,'q0_formula') then q0_formula=_extra.q0_formula
            default,q0_formula,'q[0]'
            q_formula=string(a,b,format="('q0*(B/q[1])^(',g0,')/(L/q[2])^(',g0,')')")
            q_parms=[q[j], 100.0, 1.0000000d+009, 0.0, 0.0]
            if keyword_set(mw_mode) or (n_elements(freq) gt 0) or $
               (isa(_extra,'STRUCT') and tag_exist(_extra,'freqlist')) then begin
             map=gx_mwrender_ebtel(model,renderer,info=info,ebtel_path=ebtel_path,$
                 q_parms=q_parms,q_formula=q_formula,q0_formula=q0_formula,$
                 gxcube=gxcube,_extra=_extra)
            endif else begin
              map=gx_euvrender_ebtel(model,renderer,info=info,ebtel_path=ebtel_path,$
                q_parms=q_parms,q_formula=q_formula,q0_formula=q0_formula,$
                gxcube=gxcube,/all,_extra=_extra)
            endelse
            if obj_valid(map) then begin
                save,map,file=modfile
                obj_destroy,map
            endif
            if (isa(gxcube) and keyword_set(save_gxc)) then begin
              if n_elements(synth_freq) gt 0 then $
                save,gxcube,file=gxcDir+path_sep()+strcompress(string(a,b,q[j],synth_freq[0],format="('a',f7.2,'b',f7.2,'q',g0,'-',g0,' GHz.gxc')"),/rem) $
              else if n_elements(freq) gt 0 then $
                save,gxcube,file=gxcDir+path_sep()+strcompress(string(a,b,q[j],freq[0],format="('a',f7.2,'b',f7.2,'q',g0,'-',g0,' GHz.gxc')"),/rem)
              if n_elements(chan) gt 0 then save,gxcube,file=gxcDir+path_sep()+strcompress(string(a,b,q[j],chan[0],format="('a',f7.2,'b',f7.2,'q',g0,'-',g0,' A.gxc')"),/rem)
            endif
          endif else gx_message, modfile+' already exists, no reprocessing requested!',/info
        endfor
        result=gx_processmodels_ebtel(ab=[a,b],ref=ref,$
          modDir=modDir,psDir=psDir,$
          levels=levels,resize=resize,$
          file_arr=file_arr,apply2=apply2,done=force_done,$
          search_mode=search_mode,$
          refdatapath=refdatapath,gxmpath=gxmpath,q_start=q_start,counter=counter,_extra=_extra)
        if size(result,/tname) eq 'STRUCT' then begin
          add_q=(apply2 eq 1)?((result.res2_done eq 0) and (result.chi2_done  eq 0)):((result.res2_done eq 0) or (result.chi2_done  eq 0))
          if add_q eq 1 then begin
            nq=n_elements(q)
            if apply2 eq 1 then begin
              if result.res2_done eq 0 then q=[q,result.q_res2_best] else q=[q,result.q_chi2_best]
            endif else begin
              q=[q,result.q_res2_best,result.q_chi2_best]
            endelse
            q=q[uniq(q,sort(q))]
            q=q[where(finite(q) eq 1)]
            if n_elements(q) eq nq then begin
              if apply2 eq 1 then apply2=3 else force_done=1
            endif
          endif else if apply2 eq 3 then done=1 else apply2=3
        endif else done=1
     ENDREP UNTIL done
     final_result=[final_result,result]   
 endfor
 endfor
 gx_message, string(n_elements(a_arr)*n_elements(b_arr),(systime(/s)-t0)/3600,format="('Computed ',i2,' sets in ',g0,' hours')"),/info
 obj_destroy,map
 result=final_result
 if size(save_result,/tname) eq 'STRING' then save,result,file=strcompress(tmpDir+path_sep()+save_result)
 if keyword_set(plot_best) then gx_plotbestmwmodels_ebtel, result, psDir
 return,result
 end
