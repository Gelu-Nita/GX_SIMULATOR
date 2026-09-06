;+
; Top-level CHMP plot / replot.
;
; Default: write every cell's set_a*b*_final.ps, then Best of Bests.ps if
; more than one cell was plotted (plot_best=0 inhibits that last step).
; One cell: those pages are Best of Bests; no separate Best of Bests.ps.
;
; /bob_only: skip cell files; only Best of Bests (no-op if a single cell).
; /overwrite: replace existing PS without asking. Else one confirm dialog
; if any target already exists.
; psDir omitted → result.psDir (created if needed; cwd/psDir if mkdir fails).
; result may be a struct array or a .sav path that contains RESULT.
; /debug: extra neighborhood Q maps/spectra (best few); see gx_plot_chmp_cell.
; /replot_final: historical alias that forces the cell pass (overrides /bob_only).
;
; gx_plot_chmp_bestofbests (same file) writes only Best of Bests.ps.
;-
pro gx_plotbestchmpmodels_ebtel, result, psDir, res2_best=res2_best, chi2_best=chi2_best, $
  q_res2_best=q_res2_best, q_chi2_best=q_chi2_best, a=a, b=b, levels=levels, $
  renorm_q=renorm_q, charsize=charsize, maps_best=maps_best, plot_chi=plot_chi, $
  plot_res=plot_res, replot_final=replot_final, bob_only=bob_only, overwrite=overwrite, $
  plot_best=plot_best, debug=debug, refs_all=refs_all, _extra=_extra

  compile_opt idl2
  resolve_routine, 'gx_plot_chmp_cell', /compile_full_file, /either
  resolve_routine, 'gx_plot_chmp_chanmaps', /compile_full_file, /either
  if size(result, /tname) eq 'STRING' then begin
    if ~file_test(result) then begin
      message, 'Cannot restore ' + result, /info
      return
    endif
    restore, result
  endif
  if ~isa(result, 'STRUCT') then begin
    message, 'No input structure provided!', /info
    return
  endif
  if n_elements(_extra) gt 0 then user_extra = _extra else user_extra = !null
  if n_elements(bob_only) eq 0 and isa(user_extra, 'STRUCT') then $
    if tag_exist(user_extra, 'bob_only') then bob_only = keyword_set(user_extra.bob_only)
  if n_elements(overwrite) eq 0 and isa(user_extra, 'STRUCT') then $
    if tag_exist(user_extra, 'overwrite') then overwrite = keyword_set(user_extra.overwrite)
  if n_elements(debug) eq 0 and isa(user_extra, 'STRUCT') then $
    if tag_exist(user_extra, 'debug') then debug = keyword_set(user_extra.debug)
  if n_elements(replot_final) eq 0 and isa(user_extra, 'STRUCT') then $
    if tag_exist(user_extra, 'replot_final') then replot_final = keyword_set(user_extra.replot_final)
  if n_elements(plot_best) eq 0 and isa(user_extra, 'STRUCT') then $
    if tag_exist(user_extra, 'plot_best') then plot_best = keyword_set(user_extra.plot_best)
  if n_elements(levels) eq 0 and isa(user_extra, 'STRUCT') then $
    if tag_exist(user_extra, 'levels') then levels = user_extra.levels
  default, levels, [20, 50, 80]
  default, charsize, !p.charsize
  gx_chmp_psdir_resolve, result, psDir
  n = n_elements(result)
  do_cells = ~keyword_set(bob_only)
  if keyword_set(replot_final) then do_cells = 1
  do_bob = (n gt 1)
  if n_elements(plot_best) gt 0 then do_bob = do_bob and keyword_set(plot_best)
  if keyword_set(bob_only) and (n eq 1) then begin
    message, 'One cell: Best of Bests is the cell PS; /bob_only writes nothing extra.', /info
    return
  endif
  files = !null
  if do_cells then for i = 0L, n - 1 do $
    files = [files, psDir + path_sep() + gx_chmp_cell_psname(result[i])]
  if do_bob then files = [files, psDir + path_sep() + 'Best of Bests.ps']
  gx_chmp_confirm_overwrite, files, overwrite=overwrite
  if ~keyword_set(overwrite) then begin
    message, 'Plot cancelled (existing PS not overwritten).', /info
    return
  endif
  if do_cells then for i = 0L, n - 1 do $
    gx_plot_chmp_cell, result[i], psDir, /overwrite, charsize=charsize, $
      levels=levels, debug=debug, refs_all=refs_all, _extra=user_extra
  if do_bob then $
    gx_plot_chmp_bestofbests, result, psDir, res2_best=res2_best, chi2_best=chi2_best, $
      q_res2_best=q_res2_best, q_chi2_best=q_chi2_best, a=a, b=b, levels=levels, $
      renorm_q=renorm_q, charsize=charsize, maps_best=maps_best, plot_chi=plot_chi, $
      plot_res=plot_res, /overwrite, refs_all=refs_all, _extra=user_extra
end

; Write Best of Bests.ps only (comparison page + winning cell pages).
pro gx_plot_chmp_bestofbests, result, psDir, res2_best=res2_best, chi2_best=chi2_best, $
  q_res2_best=q_res2_best, q_chi2_best=q_chi2_best, a=a, b=b, levels=levels, $
  renorm_q=renorm_q, charsize=charsize, maps_best=maps_best, plot_chi=plot_chi, $
  plot_res=plot_res, overwrite=overwrite, refs_all=refs_all, _extra=_extra

compile_opt idl2
resolve_routine, 'gx_plot_chmp_chanmaps', /compile_full_file, /either
resolve_routine, 'gx_plot_chmp_cell', /compile_full_file, /either
if ~isa(result, 'STRUCT') then begin
  message,'No input structure provided!',/info
  return
endif
default,charsize,!p.charsize
gx_chmp_psdir_resolve, result, psDir
if n_elements(_extra) gt 0 then user_extra=_extra else user_extra=!null
if n_elements(levels) eq 0 and isa(user_extra,'STRUCT') then $
  if tag_exist(user_extra,'levels') then levels=user_extra.levels
default,levels,[20,50,80]
want_log=0b
if isa(user_extra,'STRUCT') then begin
  if tag_exist(user_extra,'log_scale') then want_log=keyword_set(user_extra.log_scale) $
  else if tag_exist(user_extra,'log') then want_log=keyword_set(user_extra.log)
endif
spectrum_mode=tag_exist(result,'search_mode') && $
  strlowcase(strcompress(result[0].search_mode,/rem)) eq 'spectrum'
if arg_present(maps_best) then return_best_maps=1 
 ;----------------------------------------------------------------------------
 objMetricsArr=[[result.RES2_BEST_METRICS],[result.CHI2_BEST_METRICS]]
 psFilesArr=[psDir+path_sep()+'BestRes.ps',psDir+path_sep()+'BestChi.ps']
 psPlotsArr=[keyword_set(plot_res),keyword_set(plot_chi)]
 modFilesArr=[[result.res2_best_file],[result.chi2_best_file]]
 a0=result.a
 b0=result.b
 q_best= [[result.Q_RES2_BEST],[result.Q_CHI2_BEST]]
 
 a=a0[uniq(a0,sort(a0))]
 b=b0[uniq(b0,sort(b0))]
 chi2_img=(res2_img=(res_img=(chi_img=(q0_img=dblarr(n_elements(a),n_elements(b),2)*!values.d_nan))))
 obj_img=objarr(n_elements(a),n_elements(b),2)
 thisDevice = !D.Name
 tvlct,rgb,/get
 loadct,39
 set_plot,'ps'
 for k=0,1 do begin
   nmod=n_elements(result)
   R=(npix=(q0=(res=(res2=fltarr(nmod)))))
   chi2=res2
   chi=chi2
   if psPlotsArr[k] then begin
     ; Portrait: IDL landscape PS is 270-rotated and shows upside-down in Preview
     psObject = Obj_New("FSC_PSConfig", /Color, /Times, Filename=psFilesArr[k],xoffset=0.5,yoffset=0.25,xsize=6.4,ysize=9.5,landscape=0,bits=8)
     psKeys=psObject->GetKeywords()
     psKeys.filename=psFilesArr[k]
     Device, _Extra= psKeys
   end
   default,pmulti,[0,3,2,0,0]
   !p.multi=pmulti
   !p.font=2
   modidx=0  
   smaxm=0
   d_res=1 
   q0=q_best[*,k]
  for i=0,nmod-1 do begin
   obj_metrics=objMetricsArr[i,k]
   
  ;      0 MAP:R ALLIGNED [-5.15,-0.74] (R= 0.98)GX Tb_I 17 GHz q=0.00266136*(B/q[1])^0/(L/q[2])^1.1
  ;      1 REF REMAPPED REFMAP NORH I 17 GHz
  ;      2 SDEV REMAPPED SDEV NORH I 17 GHz
  ;      3 ROI:NPIX ROI MASK (839/10000 pixels)
  ;      4 ROI:RES RESIDUAL (7815.44)
  ;      5 ROI:RES_NORM NORMALIZED RESIDUAL (-0.00154924)
  ;      6 ROI:RES2 SQUARED RESIDUAL (1.03101e+009)
  ;      7 ROI:RES2_NORM NORMALIZED SQUARED RESIDUAL (0.0960858)
  ;      8 ROI:CHI CHI (-0.00991784)
  ;      9 ROI:CHI2 CHI2 (63.5342)
  ;     metrics structure:
  ;       R - Pearson correlation coefficient
  ;       res_img= data_model - data_obs
  ;       res= total(res_img[mask_pix])
  ;       res_img_norm=res_img/data_obs
  ;       res_norm=total(res_img_norm[mask_pix])/n_mask_pix
  ;       res2_img=res_img^2
  ;       res2=total(res2_img[mask_pix])-res^2/n_mask_pix
  ;       res2_img_norm=res_img_norm^2
  ;       res2_norm=total(res2_img_norm[mask_pix])-res_norm^2
  ;       chi_img=res_img/data_sdev
  ;       chi=total(chi_img[mask_pix])/n_mask_pix
  ;       chi2_img=chi_img^2
  ;       chi2=total(chi2_img[mask_pix])/(n_mask_pix-n_free)-chi^2
        modI=obj_metrics->get(0,/map)
        R[i]=modI.roi_metrics
              
        obsI=obj_metrics->get(1,/map)
        dx=tag_exist(obsI,'orig_xc')?(obsI.xc-obsI.orig_xc):0.0
        dy=tag_exist(obsI,'orig_yc')?(obsI.yc-obsI.orig_yc):0.0
        obsIsdev=obj_metrics->get(2,/map)
             
  ;       0 MAP:R
  ;       1 REF
  ;       2 SDEV
  ;       3 ROI:NPIX
  ;       4 ROI:RES
  ;       5 ROI:RES_NORM
  ;       6 ROI:RES2
  ;       7 ROI:RES2_NORM
  ;       8 ROI:CHI
  ;       9 ROI:CHI2

        mod_dS=modI.dx*modI.dy
        npix[i]=obj_metrics->get(3,/roi_metrics)
        
        RES_NORM_MAP=obj_metrics->get(5,/map)
        res[i]=res_norm_map.roi_metrics
        
        bad=where(RES_NORM_MAP.data eq 1,nbad,ncomp=ncomp)
        if nbad gt 0 then RES_NORM_MAP.data[bad]=0
        
        RES2_MAP=obj_metrics->get(7,/map)
        res2[i]=RES2_MAP.roi_metrics
        
        CHI_MAP=obj_metrics->get(8,/map)
        chi[i]=CHI_MAP.roi_metrics
                
        CHI2_MAP=obj_metrics->get(9,/map)
        chi2[i]=CHI2_MAP.roi_metrics
      
      filnam='rbin_'+file_basename(modFilesArr[i,k])
      if psPlotsArr[k] then begin
        plot_map,modI,charsize=charsize,title=filnam,log_scale=want_log
        plot_map,modI,/over,levels=levels,/perc,color=0,thick=3
        plot_map,obsI,/over,levels=levels,/perc,color=200,thick=3
        gx_plot_chmp_contour_legend, charsize=charsize
        get_map_coord,modI,x,y
        sz=size(modI.data)
        sx=sz[1]/100.
        sy=sz[2]/100.
        !p.font=-1
        xyouts,x[10*sx,90*sy],y[10*sx,90*sy],strcompress(string(dx,dy,format="('!4D!3x=',f7.2,'; !4D!3y=',f7.2)"),/rem),charsize=1.1*charsize,color=255
        !p.font=2
        xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(R[i],format="(' R=',g0)"),charsize=charsize,color=255
        xyouts,x[10*sx,90*sy],y[10*sx,70*sy],string(Q0[i],format="(' Q0=',g0)"),charsize=charsize,color=255
        xyouts,x[10*sx,90*sy],y[10*sx,60*sy],string(a0[i],b0[i],format="(' (a; b)=(',g0,'; ',g0,')')"),charsize=charsize,color=255
        xyouts,x[10*sx,90*sy],y[10*sx,10*sy],string(total(npix[i]),format="(' Mask_Npix=',I0)"),charsize=charsize,color=255
        plot_map,RES_NORM_MAP,charsize=charsize,title='res_'+filnam ,dmax=d_res, dmin=-d_res 
        xyouts,x[10*sx,90*sy],y[10*sx,90*sy],string(res[i],format="(' Res=',g0)"),charsize=charsize,color=25
        xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(res2[i],format="(' Res!U2!N=',g0)"),charsize=charsize,color=25
        xyouts,x[10*sx,90*sy],y[10*sx,70*sy],string(Q0[i],format="(' Q0=',g0)"),charsize=charsize,color=25
        plot_map,CHI2_MAP,charsize=charsize,title='chi!U2!N_'+filnam ,dmax=d_res*20, dmin=0 ;-d_res*10
        xyouts,x[10*sx,90*sy],y[10*sx,90*sy],string(chi[i],format="(' Chi=',g0)"),charsize=charsize,color=200
        xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(chi2[i],format="(' Chi!U2!N=',g0)"),charsize=charsize,color=200
      end
     endfor

     for l=0,nmod-1 do begin
      ii=where(a0[l] eq a)
      jj=where(b0[l] eq b)
      res2_img[ii,jj,k]=res2[l]
      chi2_img[ii,jj,k]=chi2[l]
      res_img[ii,jj,k]=res[l]
      chi_img[ii,jj,k]=chi[l]
      q0_img[ii,jj,k]=q0[l]
      obj_img[ii,jj,k]=ObjMetricsArr[l,k]
     endfor
  
   if min(a0) lt max(a0) and min(b0) lt max(b0) and (psPlotsArr[k] eq 1) then begin
    !p.multi=[0,3,4,0,0]
    ymargin=[3,1]
    plot,a,res2_img[*,0,k],psym=-1,charsize=charsize,xtitle='a',ytitle='RES!U2!N',yrange=minmax(res2_img),/nodata,/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(b)-1 do oplot,a,res2_img[*,l,k],psym=-1,color=50+l*30,thick=2
    min_res2=min(res2_img[*,*,k],imin_res2)
    idx_res2=array_indices(res2_img[*,*,k],imin_res2)
    plots,a[idx_res2[0]],min_res2,psym=2,color=250,symsize=symsize,thick=3
    minres2=min(res2,imin)
    gx_chmp_al_legend, [string(minres2,format="('RES!U2!N=',g0)"), $
      string(a[idx_res2[0]],b[idx_res2[1]],format="('a=',f5.2,'; b=',f5.2)"), $
      string(q0[imin],format="('q=',g0)")], /top, /left, charsize=charsize, box=1
  
    plot,a,chi2_img[*,0,k],psym=-1,charsize=charsize,xtitle='a',ytitle='Chi!U2!N',yrange=minmax(chi2_img),/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(b)-1 do oplot,a,chi2_img[*,l,k],psym=-1,color=50+l*30,thick=2
    min_chi2=min(chi2_img[*,*,k],imin_chi2)
    idx_chi2=array_indices(chi2_img[*,*,k],imin_chi2)
    plots,a[idx_chi2[0]],min_chi2,psym=2,color=250,symsize=symsize,thick=3
    minchi2=min(chi2,imin)
    gx_chmp_al_legend, [string(minchi2,format="('Chi!U2!N=',g0)"), $
      string(a[idx_chi2[0]],b[idx_chi2[1]],format="('a=',f5.2,'; b=',f5.2)"), $
      string(q0[imin],format="('q=',g0)")], /top, /left, charsize=charsize, box=1
  
    plot,a,res_img[*,0,k],psym=-1,charsize=charsize,xtitle='a',ytitle='RES',yrange=max(abs(minmax(res_img)))*[-1,1],/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(b)-1 do oplot,a,res_img[*,l,k],psym=-1,color=50+l*30,thick=2
     oplot,a,result.res2_threshold/npix/mod_dS,color=250, thick=2
     oplot,a,-result.res2_threshold/npix/mod_dS, color=250, thick=2

    plot,b,res2_img[0,*,k],psym=-1,charsize=charsize,xtitle='b',ytitle='RES!U2!N',yrange=minmax(res2_img),/nodata,/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(a)-1 do oplot,b,res2_img[l,*,k],psym=-1,color=50+l*30,thick=2
    plots,b[idx_res2[1]],min_res2,psym=2,color=250,symsize=symsize,thick=3
  
    plot,b,chi2_img[0,*,k],psym=-1,charsize=charsize,xtitle='b',ytitle='Chi!U2!N',yrange=minmax(chi2_img),/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(a)-1 do oplot,b,chi2_img[l,*,k],psym=-1,color=50+l*30,thick=2
    plots,b[idx_chi2[1]],min_chi2,psym=2,color=250,symsize=symsize,thick=3
  
    plot,b,res_img[0,*,k],psym=-1,charsize=charsize,xtitle='b',ytitle='RES',yrange=max(abs(minmax(res_img)))*[-1,1],/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(a)-1 do oplot,b,res_img[l,*,k],psym=-1,color=50+l*30,thick=2
    oplot,b,result.res2_threshold/npix/mod_dS, color=250, thick=2
    oplot,b,-result.res2_threshold/npix/mod_dS, color=250, thick=2
  
  
    !p.multi=[3,3,2,0,0]
    img_res2=res2_img[*,*,k]
    bad=where(finite(img_res2) eq 0,count)
    if count gt 0 then img_res2[bad]=max(img_res2,/nan)*1.02
    tvplot,img_res2,a,b,charsize=charsize,title='RES!U2!N',xtitle='a',ytitle='b',/sample
    plots,a[idx_res2[0]],b[idx_res2[1]],psym=2,color=250,symsize=symsize,thick=3
    img_chi2=chi2_img[*,*,k]
    bad=where(finite(img_chi2) eq 0,count)
    if count gt 0 then img_chi2[bad]=max(img_chi2,/nan)*1.02
    tvplot,img_chi2,a,b,charsize=charsize,title='Chi!U2!N',xtitle='a',ytitle='b',/sample
    plots,a[idx_chi2[0]],b[idx_chi2[1]],psym=2,color=250,symsize=symsize,thick=3
  
    !p.multi=[4,3,4,0,0]
    plot,a,chi_img[*,0,k],psym=-1,charsize=charsize,xtitle='a',ytitle='CHI',yrange=(max(abs(minmax(chi_img)))>0.25)*[-1,1],/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(b)-1 do oplot,a,chi_img[*,l,k],psym=-1,color=50+l*30,thick=2
    oplot,a,result.chi2_threshold/npix/mod_dS, color=250,thick=2
    oplot,a,-result.chi2_threshold/npix/mod_dS, color=250,thick=2
    
    !p.multi=[1,3,4,0,0]
    plot,b,chi_img[0,*,k],psym=-1,charsize=charsize,xtitle='b',ytitle='CHI',yrange=(max(abs(minmax(chi_img)))>0.25)*[-1,1],/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(a)-1 do oplot,b,chi_img[l,*,k],psym=-1,color=50+l*30,thick=2
    oplot,b,result.chi2_threshold/npix/mod_dS, color=250,thick=2
    oplot,b,-result.chi2_threshold/npix/mod_dS, color=250,thick=2
  end
   device,/close
 end
 ; Spectrum mode: rank (a,b) by spectral RES2/CHI2, not channel-0 image metrics
 if keyword_set(spectrum_mode) then begin
  for l=0,n_elements(result)-1 do begin
    ii=where(a0[l] eq a)
    jj=where(b0[l] eq b)
    if ii[0] lt 0 or jj[0] lt 0 then continue
    ri=result[l]
    if ptr_valid(ri.allmetrics) then begin
      am=*ri.allmetrics
      void=min(abs(am.q-ri.q_res2_best),ir)
      void=min(abs(am.q-ri.q_chi2_best),ic)
      res2_img[ii,jj,0]=am.res2[ir]
      res2_img[ii,jj,1]=am.res2[ic]
      chi2_img[ii,jj,0]=am.chi2[ir]
      chi2_img[ii,jj,1]=am.chi2[ic]
    endif else begin
      res2_img[ii,jj,0]=ri.res2_best
      res2_img[ii,jj,1]=ri.res2_best * 1.01d + 1d-30
      chi2_img[ii,jj,0]=ri.chi2_best * 1.01d + 1d-30
      chi2_img[ii,jj,1]=ri.chi2_best
    endelse
  endfor
 endif
 a_arr=a
 b_arr=b
 q_chi2_best=(q_res2_best=(chi2_best=(res2_best=(dblarr(n_elements(a),n_elements(b))))))
 obj_chi2_best=(obj_res2_best=objarr(n_elements(a),n_elements(b)))
 for i=0,n_elements(a)-1 do begin
  for j=0,n_elements(b) -1 do begin
    renorm=keyword_set(renorm_q)?(1e9)^b[i]/(100^a[i]):1
    res2_best[i,j]=min(reform(res2_img[i,j,*]),imin)
    q_res2_best[i,j]=q0_img[i,j,imin]*renorm
    obj_res2_best[i,j]=obj_img[i,j,imin]
    chi2_best[i,j]=min(reform(chi2_img[i,j,*]),jmin)
    q_chi2_best[i,j]=q0_img[i,j,jmin]*renorm
    obj_chi2_best[i,j]=obj_img[i,j,jmin]
  endfor
 end
 filename=psDir+path_sep()+'Best of Bests.ps'
 ; Portrait: IDL landscape PS is 270-rotated and shows upside-down in Preview
 psObject = Obj_New("FSC_PSConfig", /Color, /Times, Filename=Filename,xoffset=0.4,yoffset=0.25,xsize=7.5,ysize=9.5,landscape=0,bits=8)
 _Extra=psObject->GetKeywords()
 _Extra.filename=psDir+path_sep()+'Best of Bests.ps'
 Device, _Extra=_Extra
 default,symsize,2
 
 best_res2=min(res2_best,imin)
 best_res2_q=q_res2_best[imin]
 idx_res2=array_indices(res2_best,imin)
 
 best_chi2=min(chi2_best,imin)
 best_chi2_q=q_chi2_best[imin]
 idx_chi2=array_indices(chi2_best,imin)
 
 bad=where(finite(res2_best) eq 0,count)
 if count gt 0 then begin
  res2_best[bad]=max(res2_best,/nan)*1.01
  chi2_best[bad]=max(chi2_best,/nan)*1.01
  q_res2_best[bad]=max(q_res2_best,/nan)*1.01
  q_chi2_best[bad]=max(q_chi2_best,/nan)*1.01
 endif
 have_grid = (n_elements(a) ge 2) and (n_elements(b) ge 2)
 if have_grid then begin
 !p.multi=[0,2,3,0,0]
 ymargin=[2,1]
 ; Right margin: vertical colorbar plus tick labels/title to its right.
 xmargin=[8,16]
 ; tvplot stretches the array so min(a)/max(a) are the image edges, not
 ; pixel centers. Offset the overlay stars only; leave the images as-is.
 na=n_elements(a)
 nb=n_elements(b)
 a_pix=(na gt 1)?(a[na-1]-a[0])/double(na):0d
 b_pix=(nb gt 1)?(b[nb-1]-b[0])/double(nb):0d
 a_res2_pix=a[0]+(idx_res2[0]+0.5d)*a_pix
 b_res2_pix=b[0]+(idx_res2[1]+0.5d)*b_pix
 a_chi2_pix=a[0]+(idx_chi2[0]+0.5d)*a_pix
 b_chi2_pix=b[0]+(idx_chi2[1]+0.5d)*b_pix
 tvplot,alog10(res2_best),a,b,charsize=charsize,xmargin=xmargin,ymargin=ymargin,title='LOG(RES!U2!N)',xtitle='a',ytitle='b',/sample,/iso
 plots,a_res2_pix,b_res2_pix,psym=2,color=250,symsize=symsize,thick=3
 gx_colorbar,minmax(alog10(res2_best)),cb_title='LOG(RES!U2!N)',cposition=[!x.window[1]+0.012,!y.window[0],!x.window[1]+0.032,!y.window[1]],charsize=1.4,/vertical

 tvplot,keyword_set(renorm_q)?alog10(q_res2_best):alog10(q_res2_best),a,b,charsize=charsize,xmargin=xmargin,ymargin=ymargin,title='LOG(Q!D0!N RES!U2!N)',xtitle='a',ytitle='b',/sample,/iso
 plots,a_res2_pix,b_res2_pix,psym=2,color=250,symsize=symsize,thick=3
 gx_colorbar,minmax(keyword_set(renorm_q)?alog10(q_res2_best):alog10(q_res2_best)),cb_title='LOG(Q!D0!N)',cposition=[!x.window[1]+0.012,!y.window[0],!x.window[1]+0.032,!y.window[1]],charsize=1.4,/vertical

 tvplot,alog10(chi2_best),a,b,charsize=charsize,xmargin=xmargin,ymargin=ymargin,title='LOG(CHI!U2!N)',xtitle='a',ytitle='b',/sample,/iso
 plots,a_chi2_pix,b_chi2_pix,psym=2,color=250,symsize=symsize,thick=3
 gx_colorbar,minmax(alog10(chi2_best)),cb_title='LOG(CHI!U2!N)',cposition=[!x.window[1]+0.012,!y.window[0],!x.window[1]+0.032,!y.window[1]],charsize=1.4,/vertical

 tvplot,keyword_set(renorm_q)?alog10(q_chi2_best):alog10(q_chi2_best),a,b,charsize=charsize,xmargin=xmargin,ymargin=ymargin,title='LOG(Q!D0!N CHI!U2!N)',xtitle='a',ytitle='b',/sample,/iso
 plots,a_chi2_pix,b_chi2_pix,psym=2,color=250,symsize=symsize,thick=3
 gx_colorbar,minmax(keyword_set(renorm_q)?alog10(q_chi2_best):alog10(q_chi2_best)),cb_title='LOG(Q!D0!N)',cposition=[!x.window[1]+0.012,!y.window[0],!x.window[1]+0.032,!y.window[1]],charsize=1.4,/vertical
  
 res2_best_b=a*0
 res2_min=a*0
 chi2_min=a*0
 chi2_best_b=a*0
 for k=0,n_elements(a)-1 do begin
 res2_min[k]= min(res2_best[k,*],/nan,imin)
 res2_best_b[k]=b[imin]
 chi2_min[k]= min(chi2_best[k,*],/nan,imin)
 chi2_best_b[k]=b[imin]
 endfor
 symsize=2
 plot,a,res2_best_b,/iso,xtitle='a',ytitle='b',charsize=charsize,ymargin=ymargin,/xsty,/ysty,xrange=minmax(a),yrange=minmax(b),/nodata,title='Best of Bests (a,b)'
 oplot,a,res2_best_b,color=50,thick=3,psym=-1
 oplot,a,chi2_best_b,color=250,thick=3,psym=-1
 plots,!x.crange,b[idx_res2[[1,1]]],linesty=1,color=50,thick=3
 plots,!x.crange,b[idx_chi2[[1,1]]],linesty=1,color=250,thick=3
 plots,a[idx_res2[[0,0]]],!y.crange,linesty=1,color=50,thick=3
 plots,a[idx_chi2[[0,0]]],!y.crange,linesty=1,color=250,thick=3
 plots,a[idx_res2[0]],b[idx_res2[1]],psym=2,color=50,symsize=symsize,thick=3
 plots,a[idx_chi2[0]],b[idx_chi2[1]],psym=2,color=250,symsize=symsize,thick=3
 gx_chmp_al_legend, [strcompress(string(best_res2,a[idx_res2[0]],b[idx_res2[1]],format="('RES!U2!N=',f0.3,'; a= ',f0.2,'; b= ',f0.2)")), $
   strcompress(string(best_chi2,a[idx_chi2[0]],b[idx_chi2[1]],format="('CHI!U2!N=',f0.3,'; a= ',f0.2,'; b= ',f0.2)"))], $
   /top, /left, charsize=1, textcolors=byte([50, 250]), box=1
 
 ymargin=[2,6]
 plot,a,res2_min,charsize=charsize,ymargin=ymargin,/xsty,title='Best of Bests (RES!U2!N, CHI!U2!N)',color=0,/noerase,ysty=9,xtitle='a',ytitle='RES!U2!N'
 oplot,a,res2_min,color=50,thick=3
 plot,a,chi2_min,charsize=charsize,ymargin=ymargin,/xsty,color=0,ysty=5
 oplot,a,chi2_min,color=250,thick=3
 axis,yaxis=1,ytitle='CHI!U2!N',/ysty,charsize=charsize,ymargin=ymargin
 gx_chmp_al_legend, [string(best_res2,best_res2_q,format="('RES!U2!N=',f0.3,' Q!D0!N=',g0)"), $
   string(best_chi2,best_chi2_q,format="('CHI!U2!N=',f0.3,' Q!D0!N=',g0)")], $
   /top, /left, charsize=1, textcolors=byte([50, 250]), box=1
 endif

 i_res=(where(a0 eq a[idx_res2[0]] and b0 eq b[idx_res2[1]],n_res))[0]
 i_chi=(where(a0 eq a[idx_chi2[0]] and b0 eq b[idx_chi2[1]],n_chi))[0]

 ; Winning cell pages (same layout as set_a*b*_final.ps). Same cell once.
 if n_res gt 0 then begin
   gx_plot_chmp_cell_pages, result[i_res], charsize=charsize, levels=levels, $
     header='Best of Bests RES!U2!N', refs_all=refs_all, _extra=user_extra
   if (n_chi gt 0) and (i_chi ne i_res) then $
     gx_plot_chmp_cell_pages, result[i_chi], charsize=charsize, levels=levels, $
       header='Best of Bests CHI!U2!N', refs_all=refs_all, _extra=user_extra
 endif

 maps_best=[]
 if keyword_set(return_best_maps) then begin
   obj_img=[obj_res2_best[idx_res2[0],idx_res2[1]],obj_chi2_best[idx_chi2[0],idx_chi2[1]]]
   abw=[[a[idx_res2[0]],b[idx_res2[1]]],[a[idx_chi2[0]],b[idx_chi2[1]]]]
   qw=[best_res2_q,best_chi2_q]
   for k=0,1 do begin
     obj_metrics=obj_img[k]
     if ~obj_valid(obj_metrics) then continue
     modI=obj_metrics->get(0,/map)
     R=modI.roi_metrics
     obsI=obj_metrics->get(1,/map)
     npix=obj_metrics->get(3,/roi_metrics)
     RES_NORM_MAP=obj_metrics->get(5,/map)
     res=res_norm_map.roi_metrics
     bad=where(RES_NORM_MAP.data eq 1,nbad)
     if nbad gt 0 then RES_NORM_MAP.data[bad]=0
     res2=obj_metrics->get(7,/roi_metrics)
     chi=obj_metrics->get(8,/roi_metrics)
     CHI2_MAP=obj_metrics->get(9,/map)
     chi2=CHI2_MAP.roi_metrics
     maps_best=[maps_best,{modI:modI,obsI:obsI,RES_NORM_MAP:RES_NORM_MAP,CHI2_MAP:CHI2_MAP,$
       a:abw[0,k],b:abw[1,k],q0:qw[k],npix:npix,R:R,chi:chi,chi2:chi2,res:res,res2:res2}]
   endfor
 endif

 device,/close
 !p.font=0 
 !p.multi=0  
 a=a_arr
 b=b_arr
 tvlct,rgb
 set_plot,thisDEvice
 if !version.os_family eq 'Windows' then cgPS2PDF,psDir+path_sep()+'Best of Bests.ps'; convert to pdf if on windows platform as GSVIEW stopped being supported
end

