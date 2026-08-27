;+
; :Description:
;    Search for the best EBTEL GX model-to-data Q match in a set of maps from
;    gx_mwrender_ebtel / gx_euvrender_ebtel, looked up in modDir or modFiles.
;
;    search_mode='image' (default): minimize image metrics at one FREQ/CHAN.
;    search_mode='spectrum': minimize ROI-integrated spectral metrics over a
;    FREQ/CHAN set (ref = objarr of CHMP map objects from gx_ref2chmp). Spectra
;    are prepared at metric time by gx_maps2spectrum using the same ROI mask as
;    image mode.
;
;    Both modes run independent golden-section searches of RES2 and CHI2.
;    Spectrum mode uses gx_metrics_spectrum: RES2 = res2_norm (relative),
;    CHI2 = mean(((S_mod-S_obs)/S_sdev)^2). S_sdev is the ROI-integrated
;    reference uncertainty from gx_fov_integral_map (independent pixels:
;    S_sdev = sqrt(total(sdev^2)) * per-pixel scale). If gx_ref2chmp found no
;    real sdev, the intensity map is the placeholder, so S_sdev is not S_obs.
;
;    RESULT (per a,b): shares the legacy image-search tags (a,b,q_*_best,
;    *_best_file, *_best_metrics, allmetrics, ...). In spectrum mode those
;    *_best / q_*_best / *_best_file values come from spectral RES2/CHI2;
;    *_best_metrics are still single map objects (first in-search channel
;    image metrics at the winning Q) for legacy PS/plotters.
;
;    Spectrum-only convenience tags:
;      search_mode, spec_axis, S_obs, S_sdev, S_mod_res2_best, S_mod_chi2_best
;        → ROI-integrated spectra on the SEARCH subset only (drive Q metrics)
;      spec_axis_all, S_obs_all, S_sdev_all, S_mod_res2_best_all,
;        S_mod_chi2_best_all
;        → same integrals over EVERY synthesized/ref channel that matches the
;          model map (full spectra for plotting in-/out-of-search points)
;      spec_allmetrics → per-Q samples with both subset and *_all vectors
;
;    levels - percentile contour levels for PS overlays. If mask is omitted,
;      levels[0] is also the default scalar ROI brightness threshold (%).
;    mask - image/ROI selector for metrics and ROI-integrated spectra (same
;      semantics as gx_metrics_image): scalar % of max, 2-D bit mask, LONG
;      pixel indices, or string path of a .sav containing mask. NOT used to
;      pick spectral channels — that is spec_weights= only (binary or soft).
;      chan=/freq= are image-mode only (enforced in gx_search4bestq).
;
;    spec_allmetrics (spectrum mode): pointer to an array of structs, one per
;    sampled Q:
;      .q                      Q0 for that sample
;      .S_mod, .S_obs, .S_sdev ROI-integrated spectra (metric / search subset)
;      .spec_axis_all, .S_mod_all, .S_obs_all, .S_sdev_all
;                              full ROI-integrated spectra (all matched chans)
;      .smetrics               gx_metrics_spectrum result (drives the Q search)
;      .channel_image_metrics  objarr(n_all) of gx_metrics_map objects
;                             (all matched channels; search subset marked in
;                             the PS titles)
;    Image mode leaves spec_allmetrics as a null pointer and *_all as 0d.
;
;    Future channel-aware display:
;      Use gx_result_select_channel(result, index=k) or freq=/chan= to build a
;      temporary result with RES2_BEST_METRICS / CHI2_BEST_METRICS swapped to
;      that channel's image metrics (at the spectral q_*_best Qs), then pass
;      it to gx_plotbestchmpmodels_ebtel / gx_chmp2grid / the CHMP GUI unchanged.
;      Manual extract (same logic as the wrapper):
;      sam = *result[i].spec_allmetrics
;      jr = where(abs(sam.q - result[i].q_res2_best) eq min(abs(sam.q - result[i].q_res2_best)), /null)
;      jc = where(abs(sam.q - result[i].q_chi2_best) eq min(abs(sam.q - result[i].q_chi2_best)), /null)
;      cim_res2 = sam[jr[0]].channel_image_metrics   ; objarr(n_chan)
;      cim_chi2 = sam[jc[0]].channel_image_metrics
;
; :Author: Gelu Nita (gnita@njit.edu)
;-
function q_sigma,x,y
  ; Fit the data to a parabolic curve y = a*x^2 + b*x + c
  fit = POLY_FIT(x, y, 2,yfit=yfit)
  a = fit[2]
  b = fit[1]
  c = fit[0]

  ; Calculate the vertex (minimum) of the parabola
  ; The vertex x-coordinate is at x = -b / (2*a)
  estimated_param = -b / (2*a)

  ; Calculate the second derivative of the parabola at the vertex
  ; The second derivative is 2*a
  second_derivative = 2*a

  ; Estimate the uncertainty using the curvature
  ; Assuming that the uncertainty in y (dy) is uniform and given by the standard deviation of y
  dy = STDDEV(y)

  ; The uncertainty in the estimated parameter can be approximated by
  ; uncertainty = dy / sqrt(2*a)
  return, dy / SQRT(second_derivative)
end

function metrics_min,Qgrid,metrics,acc=acc,done=done
 chi2=metrics;call it chi2 for convenience
 default, acc,1d-1
 G=(1d0+sqrt(5d0))/2
 ; All-Inf/NaN metrics (e.g. broken ROI) would otherwise always pick the
 ; low-Q edge and densify Q → 0 forever.
 good=where(finite(chi2),ngood)
 if ngood eq 0 then begin
   message,'metrics_min: no finite metric samples; stopping Q densification.',/info
   return,{acc:acc,q_best:Qgrid[0],q_range:minmax(double(Qgrid)),$
     metrics_best:!values.d_nan,done:1,metrics_best_idx:0,tol:!values.d_nan}
 endif
 chi2_b=min(chi2,ib)
  case ib of
  0: begin
      Qa=Qgrid[0]/G 
      Qb=Qgrid[ib]
      Qc=Qgrid[ib+1]
      q_best=Qa
      q_range=[-!values.d_infinity,Qc]
      done=0 or keyword_set(done)
     end  
  n_elements(chi2)-1: begin
                        Qa=Qgrid[ib-1]
                        Qb=Qgrid[ib]
                        Qc=Qgrid[ib]*G 
                        q_best=Qc
                        q_range=[Qa,!values.d_infinity]
                        done=0 or keyword_set(done) 
                       end              
  else: begin
          Qa=Qgrid[ib-1]
          Qb=Qgrid[ib]
          Qc=Qgrid[ib+1]
          if ((Qc-Qa)/(Qc+Qa)) lt acc or keyword_set(done) then begin
            q_best=Qb
            done=1
          endif else begin
            q_best=((Qc-Qb) gt (Qb-Qa)) ? Qb+(Qc-Qb)*(1d0-1d0/G) : Qb-(Qb-Qa)*(1d0-1d0/G)
            done=0 or keyword_set(done)
          endelse
;          q_range=[Qa,Qc] 
          sigma=q_sigma(Qgrid,chi2)
          q_range=[(Qb-sigma)<Qa,(Qb+sigma)>Qc]
        end  
 endcase
  return,{acc:acc,q_best:q_best,q_range:q_range,metrics_best:chi2_b,done:done,metrics_best_idx:ib,tol:(Qc-Qa)/(Qc+Qa)}
end


function gx_processmodels_ebtel,ab=ab,ref=ref,$
                       modDir=modDir,modFiles=modFiles,psDir=psDir,$
                       levels=levels,mask=mask,resize=resize,$
                       file_arr=file_arr,q_arr=q_arr,corr_beam=corr_beam,$
                       apply2=apply2,charsize=charsize,counter=counter,$
                       search_mode=search_mode,all_refs=all_refs,$
                       spec_weights=spec_weights,_extra=_extra
 ;check validity of input data
 default,search_mode,'image'
 search_mode=strlowcase(strcompress(search_mode,/rem))
 spectrum_mode=search_mode eq 'spectrum'
 ; Optional soft per-channel weights for spectrum metrics (see metrics call).
 if n_elements(spec_weights) eq 0 and isa(_extra,'STRUCT') then $
   if tag_exist(_extra,'spec_weights') then spec_weights=_extra.spec_weights
 if n_elements(spec_weights) eq 0 and isa(_extra,'STRUCT') then $
   if tag_exist(_extra,'weights') then spec_weights=_extra.weights
 resolve_routine, 'gx_plot_chmp_chanmaps', /compile_full_file, /either
 resolve_routine, 'gx_chmp_refs_on_map', /compile_full_file, /either
 resolve_routine, 'gx_chmp_axis_selmask', /compile_full_file, /either
 if ~isa(modDir,'STRING') then begin
   message,'Undefined model repository, operation aborted!',/info
   return, !null
 endif else begin
   if ~(file_info(modDir)).exists or ~(file_info(modDir)).directory then begin
     message,moddir+' is not a valid model repository path, operation aborted!',/info
     return, !null
   endif
 endelse

 if ~isa(psDir,'STRING') then begin
   message,'Undefined output postcript directory, operation aborted!',/info
   return, !null
 endif else begin
   if ~(file_info(psDir)).exists or ~(file_info(psDir)).directory then begin
     message,psDir+' is not a valid directory path, operation aborted!',/info
     return, !null
   endif
 endelse

 if spectrum_mode then begin
   ; ref is objarr (or single) of standard CHMP map objects from gx_ref2chmp
   if size(ref,/tname) ne 'OBJREF' then goto, invalid_ref
   nref=n_elements(ref)
   if nref lt 1 then goto, invalid_ref
   for ir=0,nref-1 do if ~obj_valid(ref[ir]) or ref[ir]->get(/count) lt 2 then goto, invalid_ref
   spec_refs=ref
   ref_axis_all=dblarr(nref)
   spec_is_chan=0b
   for ir=0,nref-1 do begin
     rf=ref[ir]->get(0,/freq)
     rc=ref[ir]->get(0,/chan)
     if n_elements(rf) gt 0 && finite(rf[0]) then begin
       ref_axis_all[ir]=double(rf[0])
       if ir eq 0 then spec_is_chan=0b
     endif else if n_elements(rc) gt 0 && finite(rc[0]) then begin
       ref_axis_all[ir]=double(rc[0])
       if ir eq 0 then spec_is_chan=1b
     endif else goto, invalid_ref
   endfor
   ; Weights on the full reference axis (gx_ref2chmp order). Omit → all ones.
   w_ref=replicate(1d,nref)
   if n_elements(spec_weights) gt 0 then begin
     if n_elements(spec_weights) ne nref then begin
       message,'spec_weights length must equal n_elements(ref) ('+ $
         strtrim(nref,2)+'); got '+strtrim(n_elements(spec_weights),2),/info
       return,!null
     endif
     w_ref=double(spec_weights)
   endif
   iw=where(w_ref gt 0d,nsw)
   if nsw lt 1 then begin
     message,'spec_weights has no positive entries; operation aborted!',/info
     return,!null
   endif
   ; Search axis for result tags / "in search" plots = positive-weight refs
   spec_axis=ref_axis_all[iw]
   ; Spectra (S_obs/S_mod/…) are computed per model under mask via gx_maps2spectrum
   S_obs=dblarr(nsw) ; filled after first successful prep (for PS); updated each Q
   S_sdev=dblarr(nsw)
   ref0=ref[0]
   _obsI=ref0->get(0,/map)
   _obsIsdev=ref0->get(1,/map)
   a_beam=ref0->get(0,/a_beam)
   b_beam=ref0->get(0,/b_beam)
   phi_beam=ref0->get(0,/phi_beam)
   if spec_is_chan then begin
     ref_freq=!null
     ref_chan=spec_axis[0]
   endif else begin
     ref_freq=spec_axis[0]
     ref_chan=!null
   endelse
   corr_beam=~is_number(corr_beam)?ref0->get(0,/corr_beam):1
 endif else begin
   if ~valid_map(ref) then begin
    invalid_ref:
    err_msg=spectrum_mode? $
      ['Undefined spectrum reference objarr, operation aborted!',$
       'Use gx_ref2chmp.pro on a multi-freq/chan ref path (dir or file list)']: $
      ['Undefined reference data map object, operation aborted!',$
       'Use gx_ref2chmp.pro to create a valid reference data map object']
    message,'',/info
    box_message,err_msg
    return, !null
   endif
   if ref->get(0,/count) lt 2 then goto, invalid_ref
   _obsI=ref->get(0,/map)
   _obsIsdev=ref->get(1,/map)
   ; Keep uncropped masters: the per-Q loop must not mutate these in place.
   obsI_ref=_obsI
   obsIsdev_ref=_obsIsdev
   a_beam=ref->get(0,/a_beam)
   b_beam=ref->get(0,/b_beam)
   phi_beam=ref->get(0,/phi_beam)
   ref_freq=ref->get(0,/freq)
   ref_chan=ref->get(0,/chan)
   corr_beam=~is_number(corr_beam)?ref->get(0,/corr_beam):1
   if n_elements(ref_freq) eq 0 and n_elements(ref_chan) eq 0 then begin
     message,'Required FREQ or CHAN reference data are missing!',/info
     goto,invalid_ref
   endif
 endelse
 ;+++++++++++++++++++++++++++++++++++
 default,counter,0l
 counter+=1
 G=(1d0+sqrt(5d0))/2;golden ratio
 if ~isa(levels) then levels=[12,20,30,50,80]
 ; Image ROI for metrics / gx_maps2spectrum (legacy mask= rules).
 ; Default scalar % = levels[0] only when mask is omitted.
 if isa(mask) then begin
  if size(mask, /tname) eq 'STRING' then begin
     CATCH, Error_status
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      ; Handle the error by ignoring the mask
      dummy=temporary(mask)
      CATCH, /CANCEL
      goto,skip_mask_string
   ENDIF
   restore,mask
   skip_mask_string:
  endif
 endif
 if ~isa(mask) then mask=levels[0]
 ; Scalar % stored on the result (not a 2-D array). Contours still use levels.
 if size(mask, /n_dimensions) eq 0 then mask_stored=double(mask) else mask_stored=double(levels[0])
 if n_elements(ab) eq 2 then begin
   if moddir ne '' then modFiles=find_files(string(ab,format="('*a',f0.2,'b',f0.2,'*.map')"),modDir)
 endif else if moddir ne '' then modFiles=find_files('*.map',modDir)
 ;----------------------------------------------------------------------------
 nmod=n_elements(modFiles)
 if nmod eq 1 and modFiles[0] eq '' then return,[]
 a0=(b0=(q0=fltarr(nmod)))
 formula0=(id0=(setfiles0=strarr(nmod)))
 dx0=(dy0=(width0=0))
 thisDevice = !D.Name
 tvlct,rgb,/get
 loadct,39
 set_plot,'ps'
 map=obj_new()
 for i=0,nmod-1 do begin
  restore,modFiles[i]
  keys=gx_getEBTELparms(map->get(/gx_key),aa,bb,qq,f=f)
  id0[i]=map->get(0,/id)
  a0[i]=aa
  b0[i]=bb
  q0[i]=qq
  formula0[i]=f
  obj_destroy,map
 endfor
 set=1
 result=[]
 repeat begin
 if n_elements(ab) eq 2 then begin
  good=where(a0 eq float(ab[0]) and b0 eq float(ab[1]),count)
  comp=-1
  ncomp=0
 endif else begin
  good=where(a0 eq a0[0] and b0 eq b0[0],count,comp=comp,ncomp=ncomp)
 endelse
 if count gt 1 then begin
   q=q0[good]
   a=a0[good]
   b=b0[good]
   id=id0[good]
   setfiles=modFiles[good]
   formula=formula0[good]
   if (apply2 ne 3) then Filename=psDir+path_sep()+strcompress(string(a[0],b[0],format="('set_a',g0,'b',g0,'.ps')"),/rem) else Filename=psDir+path_sep()+strcompress(string(a[0],b[0],format="('set_a',g0,'b',g0,'_final.ps')"),/rem)
   filename_copy=filename
   default,charsize,!p.charsize
   psObject = Obj_New("FSC_PSConfig", /Color, /Times, /Bold, Filename=Filename,xoffset=0.5,yoffset=0.25,xsize=6.4,ysize=9.5,landscape=0,bits=8)
   psKeys=psObject->GetKeywords()
   psKeys.filename=filename_copy
   Device, _Extra= psKeys
   !p.multi=[0,2,3,0,1]
   print,string(set,count,format="('Processing SET ',i2, ' file count=',i2)")
   print,string(a[0],b[0],format="('a= ',g0, ' b=',g0)")

   res2=dblarr(count)
   chi2=dblarr(count)
   res2_best=dblarr(count)
   chi2_best=dblarr(count)
   obj_metrics_arr=objarr(count)
   spec_diag=ptr_new()
 
   for i=0,count-1 do begin
    print,'restoring{ ',modFiles[good[i]]
    obj_destroy,map
    restore,modFiles[good[i]]
    
    if spectrum_mode then begin
      ;----- spectrum minimization path -----
      ; Full ROI spectra: all refs that match layers on this model map
      ; (ALL_REFS from gx_search4bestq when the ref dir has more channels than
      ; the search subset). Search metrics then slice that full spectrum.
      refs_cand=spec_refs
      if size(all_refs,/tname) eq 'OBJREF' then $
        if n_elements(all_refs) ge 1 then refs_cand=all_refs
      refs_use=gx_chmp_refs_on_map(refs_cand,map,axis=full_axis,err_msg=em)
      if size(refs_use,/tname) ne 'OBJREF' then begin
        message,'Spectrum ref/model match failed for '+modFiles[good[i]]+': '+ $
          ((size(em,/tname) eq 'STRING')?em:'unknown'),/info
        res2[i]=!values.d_nan
        chi2[i]=!values.d_nan
        obj_metrics_arr[i]=obj_new()
        obj_destroy,map
        continue
      endif
      spec=gx_maps2spectrum(map,refs_use,mask=mask,apply2=apply2,resize=resize,$
        corr_beam=corr_beam,err_msg=em,mod_maps=mod_maps)
      obj_destroy,map
      if ~isa(spec,'STRUCT') then begin
        message,'Spectrum build failed for '+modFiles[good[i]]+': '+em,/info
        res2[i]=!values.d_nan
        chi2[i]=!values.d_nan
        obj_metrics_arr[i]=obj_new()
        if n_elements(mod_maps) gt 0 then obj_destroy,mod_maps
        continue
      endif
      ; Spectral weights (NOT image mask=): map w_ref (full ref order) onto
      ; this model's ROI spectrum axis. Positive weight → in search / metrics.
      n_all=n_elements(spec.axis)
      w_metric=dblarr(n_all)
      for kk=0,n_all-1 do begin
        m=min(abs(ref_axis_all-spec.axis[kk]),jj)
        thr=(1d-3*abs(spec.axis[kk]))>1d-6
        if m le thr then w_metric[kk]=w_ref[jj]
      endfor
      isel=where(w_metric gt 0d,nsel)
      if nsel eq 0 then begin
        message,'No positive-weight channels on spectrum axis for '+ $
          modFiles[good[i]],/info
        res2[i]=!values.d_nan
        chi2[i]=!values.d_nan
        obj_metrics_arr[i]=obj_new()
        if n_elements(mod_maps) gt 0 then obj_destroy,mod_maps
        continue
      endif
      if nsel lt n_elements(spec_axis) then $
        message,'WARNING: only '+strtrim(nsel,2)+' of '+ $
          strtrim(n_elements(spec_axis),2)+ $
          ' positive-weight channels matched the model map.',/info
      S_obs_sel=spec.S_obs[isel]
      S_sdev_sel=spec.S_sdev[isel]
      S_mod_sel=spec.S_mod[isel]
      S_obs=S_obs_sel
      S_sdev=S_sdev_sel
      spec_sdev_ok=total(spec.has_sdev[isel]) eq nsel
      if keyword_set(spec_sdev_ok) then $
        smetrics=gx_metrics_spectrum(spec.S_mod,spec.S_obs,spec.S_sdev,weights=w_metric) $
      else smetrics=gx_metrics_spectrum(spec.S_mod,spec.S_obs,weights=w_metric)
      if ~isa(smetrics,'STRUCT') then begin
        res2[i]=!values.d_nan
        chi2[i]=!values.d_nan
      endif else begin
        res2[i]=smetrics.res2_norm
        if tag_exist(smetrics,'chi2') then chi2[i]=smetrics.chi2 else chi2[i]=!values.d_nan
      endelse
      ; diagnostic image metrics for every full-spectrum channel
      ; (do not drive search). Parallel to spec.axis / spec_axis_all.
      n_all=n_elements(spec.axis)
      if i eq 0 then $
        message,string(n_all,n_elements(spec_axis),total(w_metric gt 0),$
          format="('Storing full ROI spectra (',i0,' channels); search axis ',i0,' channels; metrics use ',i0,' weighted points.')"),/info
      chan_metrics=objarr(n_all)
      for kk=0,n_all-1 do begin
        if ~obj_valid(mod_maps[kk]) or ~obj_valid(refs_use[kk]) then continue
        modI=mod_maps[kk]->get(0,/map)
        obsI=refs_use[kk]->get(0,/map)
        obsIsdev=refs_use[kk]->get(1,/map)
        if keyword_set(spec_is_chan) then begin
          sub_map,obsI,obsI,ref=modI
          sub_map,obsIsdev,obsIsdev,ref=modI
          sz=size(modI.data)
          obsI=gx_rebin_map(obsI,sz[1],sz[2],/total)
          obsIsdev=gx_rebin_map(obsIsdev,sz[1],sz[2],/total)
        endif
        chan_metrics[kk]=gx_metrics_map(modI,obsI,obsIsdev,mask=mask,metrics=imetrics,$
          apply2=apply2,/no_renorm,_extra=_extra)
      endfor
      obj_destroy,mod_maps
      ; keep representative (first in-search channel) for legacy PS/plotters
      if obj_valid(chan_metrics[isel[0]]) then $
        obj_metrics_arr[i]=obj_clone(chan_metrics[isel[0]]) $
      else obj_metrics_arr[i]=obj_new()
      if ~ptr_valid(spec_diag) then begin
        spec_diag=ptr_new(replicate({q:0d,S_mod:dblarr(n_elements(spec_axis)),$
          S_obs:dblarr(n_elements(spec_axis)),S_sdev:dblarr(n_elements(spec_axis)),$
          spec_axis_all:dblarr(n_all),$
          S_mod_all:dblarr(n_all),$
          S_obs_all:dblarr(n_all),$
          S_sdev_all:dblarr(n_all),$
          channel_image_metrics:objarr(n_all),$
          smetrics:smetrics},count))
      endif
      (*spec_diag)[i].q=q[i]
      (*spec_diag)[i].S_mod=S_mod_sel
      (*spec_diag)[i].S_obs=S_obs_sel
      (*spec_diag)[i].S_sdev=S_sdev_sel
      (*spec_diag)[i].spec_axis_all=spec.axis
      (*spec_diag)[i].S_mod_all=spec.S_mod
      (*spec_diag)[i].S_obs_all=spec.S_obs
      (*spec_diag)[i].S_sdev_all=spec.S_sdev
      (*spec_diag)[i].channel_image_metrics=chan_metrics
      (*spec_diag)[i].smetrics=smetrics
    endif else begin
      ;----- image minimization path (unchanged) -----
      if n_elements(resize) ne 0 then begin
        if n_elements(resize) eq 1 then resize=[resize,resize]
        for k=0, map->get(/count)-1 do begin
          ;added option of preserving total flux if the map is an EUV map, which is expected to have a CHAN tag
          rmap=gx_rebin_map(map->get(k,/map),resize[0],resize[1],total=is_number(map->get(k,/chan)))
          rmap.id='rebinned_'+rmap.id
          map->setmap,k,rmap
        endfor
      endif
      
      if ~isa(obsBeam) then begin
        if isa(a_beam) and isa(b_beam) and isa(phi_beam)then begin
          dx=map->get(/dx)
          dy=map->get(/dy)
          width=size(map->get(/data),/dimensions)
          ;ensure that width is odd
          if width[0] mod 2 eq 0 then width[0]+=1
          if width[1] mod 2 eq 0 then width[1]+=1
          if ~is_number(corr_beam) then corr_beam=1
          obsBeam=gx_psf(corr_beam*[a_beam,b_beam]/[dx,dy],phi_beam,width)
        endif
      end  
      
      if n_elements(ref_freq) gt 0 then begin
        freq=map->get(/freq)
        for k=1,map->get(/count)-1 do freq=[freq,map->get(k,/freq)]
        m=min(abs(freq-ref_freq),modidx)
      endif
      
      if n_elements(ref_chan) gt 0 then begin
        chan=map->get(/chan)
        for k=1,map->get(/count)-1 do chan=[chan,map->get(k,/chan)]
        m=min(abs(chan-ref_chan),modidx)
      endif
      
      modI=map->get(modidx,/map)
      obj_destroy,map
      
      ;here handle the _obsI and _obsIsdev maps if tey are EUV maps, to conserve flux
      ; Fresh copy from the uncropped reference each Q (never sub_map in place).
      if n_elements(ref_chan) gt 0 then begin
        _obsI=obsI_ref
        _obsIsdev=obsIsdev_ref
        sub_map,_obsI,_obsI,ref=modI
        sub_map,_obsIsdev,_obsIsdev,ref=modI
        sz=size(modI.data)
        _obsI=gx_rebin_map(_obsI,sz[1],sz[2],/total)
        _obsIsdev=gx_rebin_map(_obsIsdev,sz[1],sz[2],/total)
      endif
      ;EUV special handling of flux conservation done
      
      if n_elements(ObsBeam) gt 0 then modI.data=convol_fft(modI.data, ObsBeam)  
      obj_metrics_arr[i]=gx_metrics_map(modI, _obsI,_obsIsdev,mask=mask,metrics=metrics,apply2=apply2,/no_renorm,_extra=_extra)
      res2[i]=metrics.res2_norm
      chi2[i]=metrics.chi2
    endelse
   endfor
   sort_idx=sort(q)
;  =================chi2=========================
   !p.font=2
   chi2_idx=sort([abs(chi2)])
   case n_elements(chi2_idx) of
     1:chi2_idx=replicate(chi2_idx[0],3)
     2:chi2_idx=[chi2_idx,chi2_idx[0:1]]
     else:chi2_idx=chi2_idx[0:2]
   endcase
   
   chi2_solution=metrics_min(q[sort_idx], chi2[sort_idx],_extra=_extra)
   q_chi2_best=chi2_solution.q_best
   q_chi2_range=chi2_solution.q_range
   chi2_best=chi2_solution.metrics_best
   chi2_thresh=chi2_solution.acc
   chi2_done=chi2_solution.done
   q_chi2_best=chi2_solution.q_best
   chi2_best=double(chi2_solution.metrics_best)
   chi2_best_file=(setfiles[sort_idx])[chi2_solution.metrics_best_idx]
   chi2_best_metrics=obj_clone((obj_metrics_arr[sort_idx])[chi2_solution.metrics_best_idx])
   
   chi2_range_idx=sort(chi2)
   chi2_range_idx=chi2_range_idx[0:(n_elements(chi2_range_idx)-1)<5]
   ;=================res2=========================
   res2_idx=sort([abs(res2)])
   case n_elements(res2_idx) of
     1:res2_idx=replicate(res2_idx[0],3)
     2:res2_idx=[res2_idx,res2_idx[0:1]]
     else:res2_idx=res2_idx[0:2]
   endcase
   res2_solution=metrics_min(Q[sort_idx], res2[sort_idx],done=chi2_solution.done);,_extra=_extra
   q_res2_best=res2_solution.q_best
   q_res2_range=res2_solution.q_range
   res2_best=res2_solution.metrics_best
   res2_thresh=res2_solution.acc
   res2_done=res2_solution.done
   q_res2_best=res2_solution.q_best
   res2_best=double(res2_solution.metrics_best)
   res2_best_file=(setfiles[sort_idx])[res2_solution.metrics_best_idx]
   res2_best_metrics=obj_clone((obj_metrics_arr[sort_idx])[res2_solution.metrics_best_idx])
   res2_range_idx=sort(res2)
   res2_range_idx=res2_range_idx[0:(n_elements(res2_range_idx)-1)<5]   
   
   !p.multi=[0,1,2]
   !p.font=2
   if spectrum_mode then begin
     ax0=min(spec_axis,max=ax1)
     if keyword_set(spec_is_chan) then $
       metrics_title=string(n_elements(spec_axis),ax0,ax1,$
         format="('ROI spectrum, ',i0,' channels (',g0,'–',g0,' A)')") $
     else $
       metrics_title=string(n_elements(spec_axis),ax0,ax1,$
         format="('ROI spectrum, ',i0,' frequencies (',g0,'–',g0,' GHz)')")
   endif else metrics_title=_obsI.ID
;   xrange=minmax(q[[res2_range_idx,chi2_range_idx]])*[1/G,G]
;   yrange=minmax([0,res2[res2_range_idx],2*res2[res2_range_idx[0]]])
   yrange=[0,max(res2,/nan)]
   plot, Q[sort_idx], res2[sort_idx], psym=-4, xlog=xlog, xstyle=0, ystyle=1, xticks=4,$
     xrange=xrange, yrange=yrange, $
     xtitle='!18Q!3', ytitle='!17 RES!S!U2!N!R!Dnorm!N!3', thick=2,charsize=1.2*charsize,title=metrics_title
   oplot,q_res2_best[[0,0]],!y.crange,color=250,thick=3,linesty=res2_done?0:2
   oplot,q_res2_range[[0,0]],!y.crange,color=250,thick=3,linesty=1
   oplot,q_res2_range[[1,1]],!y.crange,color=250,thick=3,linesty=1
   gx_plot_label,0.01,0.9,xlog=xlog, string(a[0],b[0],format="('a=',f5.2,'; ','b=',f5.2)"),charsize=charsize
   gx_plot_label,0.01,0.8, 'PROJECTED SOLUTION:',xlog=xlog,charsize=charsize
   gx_plot_label,0.01,0.7, string([q_res2_best,q_res2_range-q_res2_best], format="('Q!Dres2_best!N = ',g0,'!S!D',g0,'!R!U+',g0)") ,xlog=xlog,charsize=charsize
   if res2_done eq 1 then begin
     gx_plot_label,0.01,0.3, 'FINAL SOLUTION:',xlog=xlog,charsize=charsize
     gx_plot_label,0.01,0.1, string([q_res2_best,q_res2_range-q_res2_best], format="('Q = ',g0,'!S!D',g0,'!R!U+',g0)") ,xlog=xlog,charsize=charsize
   end
   gx_plot_label,0.01,0.2, string(res2_best, format="('RES!S!U2!N!R!Dnorm!N = ',g0)") ,xlog=xlog,charsize=charsize
   gx_plot_label,0.7,0.2, string(res2_solution.tol, format="('tol = ',g0)") ,xlog=xlog,charsize=charsize
   gx_plot_label,0.7,0.1, string(counter,format="('Run#: ',g0)"),xlog=xlog,charsize=charsize

;   xrange=minmax(q[chi2_range_idx])*[1/G,G]
;   yrange=minmax([0,chi2[chi2_range_idx],2*chi2[chi2_range_idx[0]]])
   yrange=[0,max(chi2,/nan)]
   plot, q[sort_idx], chi2[sort_idx], psym=-4, xlog=xlog,  xstyle=0, ystyle=1, xticks=4,$
     xrange=xrange, yrange=yrange, $
     xtitle='!18Q!3', ytitle='!17 Chi!U2!N!3', thick=2,charsize=1.2*charsize,title=metrics_title
   oplot,q_chi2_best[[0,0]],!y.crange,color=250,thick=3,linesty=chi2_done?0:2
   oplot,q_chi2_range[[0,0]],!y.crange,color=250,thick=3,linesty=1
   oplot,q_chi2_range[[1,1]],!y.crange,color=250,thick=3,linesty=1
   !p.font=2
   gx_plot_label,0.01,0.9,xlog=xlog, string(a[0],b[0],format="('a=',f5.2,'; ','b=',f5.2)"),charsize=charsize
   gx_plot_label,0.01,0.8, 'PROJECTED SOLUTION:',xlog=xlog,charsize=charsize
   gx_plot_label,0.01,0.7, string([q_chi2_best,q_chi2_range-q_chi2_best], format="('Q!Dchi2_best!N = ',g0,'!S!D',g0,'!R!U+',g0)") ,xlog=xlog,charsize=charsize
   if chi2_done then begin
     gx_plot_label,0.01,0.3, 'FINAL SOLUTION:',xlog=xlog,charsize=charsize
     gx_plot_label,0.01,0.1, string([q_chi2_best,q_chi2_range-q_chi2_best], format="('Q = ',g0,'!S!D',g0,'!R!U+',g0)") ,xlog=xlog,charsize=charsize
   end
     gx_plot_label,0.01,0.2, string(chi2_best,format="('Chi!U2!N=',g0)") ,xlog=xlog,charsize=charsize
     gx_plot_label,0.7,0.2, string(chi2_solution.tol, format="('tol = ',g0)") ,xlog=xlog,charsize=charsize
     gx_plot_label,0.7,0.1, string(counter,format="('Run#: ',g0)"),xlog=xlog,charsize=charsize
   !p.font=-1

   if spectrum_mode and ptr_valid(spec_diag) then begin
     ; ROI-integrated spectrum comparison for the current best Q samples
     !p.multi=[0,1,2]
     ib_res2=res2_solution.metrics_best_idx
     ib_chi2=chi2_solution.metrics_best_idx
     gx_plot_chmp_spectrum, spec_axis, $
       (*spec_diag)[sort_idx[ib_res2]].S_obs, (*spec_diag)[sort_idx[ib_res2]].S_sdev, $
       (*spec_diag)[sort_idx[ib_res2]].S_mod, $
       (*spec_diag)[sort_idx[ib_chi2]].S_obs, (*spec_diag)[sort_idx[ib_chi2]].S_sdev, $
       (*spec_diag)[sort_idx[ib_chi2]].S_mod, $
       aval=a[0], bval=b[0], q_res2_best=q_res2_best, res2_best=res2_best, $
       q_chi2_best=q_chi2_best, chi2_best=chi2_best, $
       is_chan=spec_is_chan, charsize=charsize, $
       samp_res2=(*spec_diag)[sort_idx[ib_res2]], $
       samp_chi2=(*spec_diag)[sort_idx[ib_chi2]], _extra=_extra
     samp_r=(*spec_diag)[sort_idx[ib_res2]]
     samp_c=(*spec_diag)[sort_idx[ib_chi2]]
     gx_plot_chmp_chanmaps, samp_r.channel_image_metrics, samp_r.spec_axis_all, spec_axis, $
       header=string(q_res2_best, format="('RES!U2!N Q=',g0)"), $
       levels=levels, charsize=charsize, is_chan=spec_is_chan, _extra=_extra
     gx_plot_chmp_chanmaps, samp_c.channel_image_metrics, samp_c.spec_axis_all, spec_axis, $
       header=string(q_chi2_best, format="('CHI!U2!N Q=',g0)"), $
       levels=levels, charsize=charsize, is_chan=spec_is_chan, _extra=_extra
   endif
 endif
 
 if ~spectrum_mode then begin
 range_idx=[chi2_range_idx,res2_range_idx]
 range_idx=range_idx[uniq(range_idx,sort(range_idx))]
 
 !p.multi=[0,2,3,0,1]
 for k=0,n_elements(range_idx)-1 do begin
   obj_metrics=obj_metrics_arr[range_idx[k]]
   if ~obj_valid(obj_metrics) then continue
   modI=obj_metrics->get(0,/map)
   modI.id=strmid(modI.id,strpos(modI.id,'GX'))
   obsI=obj_metrics->get(1,/map)
   dx=tag_exist(obsI,'orig_xc')?(obsI.xc-obsI.orig_xc):0.0
   dy=tag_exist(obsI,'orig_yc')?(obsI.yc-obsI.orig_yc):0.0
   obsIsdev=obj_metrics->get(2,/map)
   want_log=0b
   if isa(_extra,'STRUCT') then begin
     if tag_exist(_extra,'log_scale') then want_log=keyword_set(_extra.log_scale) $
     else if tag_exist(_extra,'log') then want_log=keyword_set(_extra.log)
   endif
   plot_map,modI,charsize=charsize,title=modI.id,log_scale=want_log
   plot_map,modI,/over,levels=levels,/perc,color=0,thick=3
   plot_map,obsI,/over,levels=levels,/perc,color=200,thick=3
   drew_mask=0b
   if n_elements(mask) eq n_elements(modI.data) then begin
    mask_map=modI
    mask_map.data=mask
    plot_map,mask_map,/over,levels=1,color=100,thick=4
    drew_mask=1b
   endif
   gx_plot_chmp_contour_legend, charsize=charsize, mask=drew_mask
   get_map_coord,modI,x,y
   sz=size(modI.data)
   sx=sz[1]/100.
   sy=sz[2]/100.
   xyouts,x[10*sx,90*sy],y[10*sx,90*sy],string(dx,dy,format="('!4D!3x=',f7.3,' !4D!3y=',f7.3)"),charsize=charsize,color=255
   xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(q[range_idx[k]],format="('q=',g0)"),charsize=charsize,color=255
   xyouts,x[10*sx,90*sy],y[10*sx,20*sy],string(res2[range_idx[k]],format="(' Res!U2!N=',g0)")+(obj_metrics->get(7,/roi_metrics) eq res2_best?' BEST':''),charsize=charsize,color=255
   xyouts,x[10*sx,90*sy],y[10*sx,10*sy],string(chi2[range_idx[k]],format="(' Chi!U2!N=',g0)")+(obj_metrics->get(9,/roi_metrics) eq chi2_best?' BEST':''),charsize=charsize,color=255
 end
 endif
  
 device,/close
 file_arr=setfiles[[res2_idx,chi2_idx]]
 q_arr=q[[res2_idx,chi2_idx]]
 idx=uniq(file_arr,sort(file_arr))
 file_arr=file_arr[idx]
 q_arr=q_arr[idx]

 ; spectrum diagnostics (empty placeholders keep image-mode struct shape compatible within one call)
 ; Per-channel image metrics live only under spec_allmetrics[j].channel_image_metrics
 ; (see header); no top-level channel_image_metrics tag.
 if spectrum_mode and ptr_valid(spec_diag) then begin
   irb=sort_idx[res2_solution.metrics_best_idx]
   icb=sort_idx[chi2_solution.metrics_best_idx]
   S_mod_res2_best=(*spec_diag)[irb].S_mod
   S_mod_chi2_best=(*spec_diag)[icb].S_mod
   S_obs=(*spec_diag)[irb].S_obs
   S_sdev=(*spec_diag)[irb].S_sdev
   spec_axis_all=(*spec_diag)[irb].spec_axis_all
   S_obs_all=(*spec_diag)[irb].S_obs_all
   S_sdev_all=(*spec_diag)[irb].S_sdev_all
   S_mod_res2_best_all=(*spec_diag)[irb].S_mod_all
   S_mod_chi2_best_all=(*spec_diag)[icb].S_mod_all
   spec_all=ptr_new(*spec_diag)
 endif else begin
   S_mod_res2_best=0d
   S_mod_chi2_best=0d
   spec_axis_all=0d
   S_obs_all=0d
   S_sdev_all=0d
   S_mod_res2_best_all=0d
   S_mod_chi2_best_all=0d
   spec_all=ptr_new()
   if ~spectrum_mode then begin
     spec_axis=0d
     S_obs=0d
     S_sdev=0d
   endif
 endelse

 result=[result,{a:double(a[0]),b:double(b[0]),$
  q_res2_best:double(q_res2_best),q_res2_range:double(q_res2_range), res2_best:double(res2_best),$
  q_chi2_best:double(q_chi2_best),q_chi2_range:double(q_chi2_range), chi2_best:double(chi2_best),$
  use_mean:keyword_set(use_mean),modDir:modDir,psDir:psdir,psfile:file_basename(filename),$
  res2_threshold:double(res2_thresh),chi2_threshold:double(chi2_thresh),chi2_done:chi2_done,res2_done:res2_done,$
  res2_best_file:file_basename(res2_best_file),chi2_best_file:file_basename(chi2_best_file),$
  res2_best_metrics:res2_best_metrics,chi2_best_metrics:chi2_best_metrics,mask:mask_stored,$
  refdatapath:tag_exist(_extra,'refdatapath',/quiet)?_extra.refdatapath:'',$
  gxmpath:tag_exist(_extra,'gxmpath',/quiet)?_extra.gxmpath:'',$
  q_start:tag_exist(_extra,'q_start',/quiet)?_extra.q_start:[0.0001,0.001],counter:counter,$
  allmetrics:ptr_new({q:q[sort_idx],res2:res2[sort_idx],chi2:chi2[sort_idx]}),$
  search_mode:search_mode,spec_axis:double(spec_axis),S_obs:double(S_obs),S_sdev:double(S_sdev),$
  S_mod_res2_best:double(S_mod_res2_best),S_mod_chi2_best:double(S_mod_chi2_best),$
  spec_axis_all:double(spec_axis_all),S_obs_all:double(S_obs_all),S_sdev_all:double(S_sdev_all),$
  S_mod_res2_best_all:double(S_mod_res2_best_all),S_mod_chi2_best_all:double(S_mod_chi2_best_all),$
  spec_allmetrics:spec_all}]

 if ptr_valid(spec_diag) then ptr_free,spec_diag
 obj_destroy,obj_metrics_arr
 if ncomp gt 1 then begin
  a0=a0[comp]
  b0=b0[comp]
  q0=q0[comp]
  id0=id0[comp]
  formula0=formula0[comp]
  modFiles=modfiles[comp]
  set+=1
 endif else ncomp=0
 endrep until ncomp eq 0
 close_lun,/all
 set_plot,thisDevice
 tvlct,rgb
 ;if !version.os_family eq 'Windows' then cgPS2PDF,filename_copy,/delete_ps; convert to pdf if on windows platform as GSVIEW stopped being supported
 return,result
end

