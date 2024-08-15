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

function metrics_min,Qgrid,metrics,acc,done=done
 chi2=metrics;call it chi2 for convenience
 default, acc,1d-1
 G=(1d0+sqrt(5d0))/2
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
                       apply2=apply2,charsize=charsize,counter=counter,_extra=_extra
 ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 ;This routine provides a means to search for the best EBTEL GX model to data match 
 ;in a set of GX microwave maps obtained using the gx_mwrender_ebtel gx_euvrender_ebtel macros, which are looked for 
 ;either in a modDir directory, or in an modFiles path array pointing to a subset of selected models
 ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      
 ;check validity of input data
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

 if ~valid_map(ref) then begin
  invalid_ref:
  err_msg=['Undefined reference data map object, operation aborted!',$
     'Use gx_ref2chmp.pro to create a valid reference data map object']  
  message,'',/info
  box_message,err_msg
  return, !null
 endif else begin
  if ref->get(0,/count) lt 2 then goto, invalid_ref
  _obsI=ref->get(0,/map)
  _obsIsdev=ref->get(1,/map)
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
 if isa(mask) then begin
  if size('str',/tname) then begin
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
 
   for i=0,count-1 do begin
    print,'restoring{ ',modFiles[good[i]]
    obj_destroy,map
    restore,modFiles[good[i]]
    
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
    if n_elements(ref_chan) gt 0 then begin
      sub_map,_obsI,_obsI,ref=modI
      sub_map,_obsIsdev,_obsIsdev,ref=modI
      sz=size(modI.data)
      _obsI=gx_rebin_map(_obsI,sz[1],sz[2],/total)
      _obsIsdev=gx_rebin_map(_obsIsdev,sz[1],sz[2],/total)
    endif
    ;EUV special handling of flix conservation done
    
    if n_elements(ObsBeam) gt 0 then modI.data=convol_fft(modI.data, ObsBeam)  
    obj_metrics_arr[i]=gx_metrics_map(modI, _obsI,_obsIsdev,mask=mask,metrics=metrics,apply2=apply2,/no_renorm,_extra=_extra)
    res2[i]=metrics.res2_norm
    chi2[i]=metrics.chi2
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
;   xrange=minmax(q[[res2_range_idx,chi2_range_idx]])*[1/G,G]
;   yrange=minmax([0,res2[res2_range_idx],2*res2[res2_range_idx[0]]])
   yrange=[0,max(res2,/nan)]
   plot, Q[sort_idx], res2[sort_idx], psym=-4, xlog=xlog, xstyle=0, ystyle=1, xticks=4,$
     xrange=xrange, yrange=yrange, $
     xtitle='!18Q!3', ytitle='!17 RES!S!U2!N!R!Dnorm!N!3', thick=2,charsize=1.2*charsize,title=_obsI.ID
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
     xtitle='!18Q!3', ytitle='!17 Chi!U2!N!3', thick=2,charsize=1.2*charsize,title=_obsI.ID
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
 endif
 
 range_idx=[chi2_range_idx,res2_range_idx]
 range_idx=range_idx[uniq(range_idx,sort(range_idx))]
 
 !p.multi=[0,2,3,0,1]
 for k=0,n_elements(range_idx)-1 do begin
   obj_metrics=obj_metrics_arr[range_idx[k]]
   modI=obj_metrics->get(0,/map)
   modI.id=strmid(modI.id,strpos(modI.id,'GX'))
   obsI=obj_metrics->get(1,/map)
   dx=tag_exist(obsI,'orig_xc')?(obsI.xc-obsI.orig_xc):0.0
   dy=tag_exist(obsI,'orig_yc')?(obsI.yc-obsI.orig_yc):0.0
   obsIsdev=obj_metrics->get(2,/map)
   plot_map,modI,charsize=charsize,title=modI.id
   plot_map,modI,/over,levels=levels,/perc,color=0,thick=3
   plot_map,obsI,/over,levels=levels,/perc,color=200,thick=3
   get_map_coord,modI,x,y
   sz=size(modI.data)
   sx=sz[1]/100.
   sy=sz[2]/100.
   xyouts,x[10*sx,90*sy],y[10*sx,90*sy],string(dx,dy,format="('!4D!3x=',f7.3,' !4D!3y=',f7.3)"),charsize=charsize,color=255
   xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(q[range_idx[k]],format="('q=',g0)"),charsize=charsize,color=255
   xyouts,x[10*sx,90*sy],y[10*sx,20*sy],string(res2[range_idx[k]],format="(' Res!U2!N=',g0)")+(obj_metrics->get(7,/roi_metrics) eq res2_best?' BEST':''),charsize=charsize,color=255
   xyouts,x[10*sx,90*sy],y[10*sx,10*sy],string(chi2[range_idx[k]],format="(' Chi!U2!N=',g0)")+(obj_metrics->get(9,/roi_metrics) eq chi2_best?' BEST':''),charsize=charsize,color=255
 end  
  
 device,/close
 file_arr=setfiles[[res2_idx,chi2_idx]]
 q_arr=q[[res2_idx,chi2_idx]]
 idx=uniq(file_arr,sort(file_arr))
 file_arr=file_arr[idx]
 q_arr=q_arr[idx]

 result=[result,{a:double(a[0]),b:double(b[0]),$
  q_res2_best:double(q_res2_best),q_res2_range:double(q_res2_range), res2_best:double(res2_best),$
  q_chi2_best:double(q_chi2_best),q_chi2_range:double(q_chi2_range), chi2_best:double(chi2_best),$
  use_mean:keyword_set(use_mean),modDir:modDir,psDir:psdir,psfile:file_basename(filename),$
  res2_threshold:double(res2_thresh),chi2_threshold:double(chi2_thresh),chi2_done:chi2_done,res2_done:res2_done,$
  res2_best_file:file_basename(res2_best_file),chi2_best_file:file_basename(chi2_best_file),$
  res2_best_metrics:res2_best_metrics,chi2_best_metrics:chi2_best_metrics,mask:levels[0],$
  refdatapath:tag_exist(_extra,'refdatapath',/quiet)?_extra.refdatapath:'',$
  gxmpath:tag_exist(_extra,'gxmpath',/quiet)?_extra.gxmpath:'',$
  q_start:tag_exist(_extra,'q_start',/quiet)?_extra.q_start:[0.0001,0.001],counter:counter}]

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

