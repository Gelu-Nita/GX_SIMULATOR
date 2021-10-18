function gx_processmwmodels_ebtel,ab=ab,ref=ref,$
                       modDir=modDir,modFiles=modFiles,psDir=psDir,$
                       lev=lev,resize=resize,$
                       file_arr=file_arr,q_arr=q_arr,$
                       apply2=apply2,_extra=_extra
 ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 ;This routine provides a means to search for the best EBTEL GX model to data match 
 ;in a set of GX microwave maps obtained using the gx_mwrender_ebtel macro, which are looked for 
 ;either in a modDir directory, or in an modFiles path array pointing to a subset of selected models
 ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      
 if ~isa(ref) or ~isa(modDir) or ~isa(psDir) then begin
  message,'Undefined reference data info structure and/or figure directories!',/cont
  return, !null
 endif

 loadct,39
 ;+++++++++++++++++++++++++++++++++++
 ref.maps=ref.maps
 a_beam=ref.a_beam
 b_beam=ref.b_beam
 phi=ref.phi_beam
 _obsI=ref.maps[0]
 _obsIsdev=ref.maps[1]
 ;++++++++++++++++++++++++++++++++++++++++++++++++
 if ~isa(lev) then lev=[12,20,30,50,80]
 
 if moddir ne '' then modFiles=find_files('*.map',modDir)

 ;----------------------------------------------------------------------------
 nmod=n_elements(modFiles)
 a0=(b0=(q0=fltarr(nmod)))
 formula0=(id0=(setfiles0=strarr(nmod)))
 dx0=(dy0=(width0=0))
 set_plot,'ps'
 for i=0,nmod-1 do begin
  restore,modFiles[i]
  keys=gx_getEBTELparms(map->get(/gx_key),aa,bb,qq,f=f)
  id0[i]=map->get(0,/id)
  a0[i]=aa
  b0[i]=bb
  q0[i]=qq
  formula0[i]=f
 endfor
 set=1
 result=[]
 repeat begin
 if n_elements(ab) eq 2 then begin
  good=where(a0 eq ab[0] and b0 eq -ab[1],count)
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
   if (apply2 ne 3) then Filename=psDir+strcompress(string(a[0],b[0],format="('\set_a',g0,'b',g0,'.ps')"),/rem) else Filename=psDir+strcompress(string(a[0],b[0],format="('\set_a',g0,'b',g0,'_final.ps')"),/rem)
   charsize=1.2
   psObject = Obj_New("FSC_PSConfig", /Color, /Times, /Bold, Filename=Filename,xoffset=0.5,yoffset=0.25,xsize=6.4,ysize=9.5,landscape=0,bits=8)
   Device, _Extra=psObject->GetKeywords()
   !p.multi=[0,2,3,0,1]
   print,string(set,count,format="('Processing SET ',i2, ' file count=',i2)")
   print,string(a[0],b[0],format="('a= ',g0, ' b=',g0)")
   
   Iobs=dblarr(count)
   Imod=dblarr(count)
   res=iobs
   chi=res
   res2_best=res
   chi2_best=res
   obj_metrics_arr=objarr(count)
   obj_metrics=obj_new()   
   for i=0,count-1 do begin
    print,'restoring{ ',modFiles[good[i]]
    restore,modFiles[good[i]]
    
    if n_elements(resize) ne 0 then begin
      if n_elements(resize) eq 1 then resize=[resize,resize]
      for k=0, map->get(/count)-1 do begin
        rmap=gx_rebin_map(map->get(k,/map),resize[0],resize[1])
        rmap.id='rebinned_'+rmap.id
        map->setmap,k,rmap
      endfor
    endif

    dx=map->get(/dx)
    dy=map->get(/dy)
    width=size(map->get(/data),/dimensions)
    ;ensure that width is odd
    if width[0] mod 2 eq 0 then width[0]+=1
    if width[1] mod 2 eq 0 then width[1]+=1

    obsBeam=gx_psf([a_beam,b_beam]/[dx,dy],phi,width)
    
    freq=map->get(/freq)
    for k=1,map->get(/count)-1 do freq=[freq,map->get(k,/freq)]
    m=min(abs(freq-ref.freq),modidx)
    modI=map->get(modidx,/map)
    modI.data=convol_fft(modI.data, ObsBeam)  
    modI0=modI
    _obsI0=_obsI
    _obsIsdev0=_obsIsdev
    obj_destroy,obj_metrics
    obj_metrics=gx_metrics_map(modI, _obsI,_obsIsdev,mask=lev[0],metrics=metrics,apply2=apply2)
    obj_metrics_arr[i]=gx_metrics_map(modI0, _obsI0,_obsIsdev0,mask=lev[0],metrics=metrics3,apply2=3)
    obsI=obj_metrics->get(1,/map)
    dx=obsI.xc-obsI.orig_xc
    dy=obsI.yc-obsI.orig_yc
    obsIsdev=obj_metrics->get(2,/map)
    ; :Return value:
    ;     The routine returns a structure with the following fields:
    ;       R - Pearson correlation coefficient
    ;       res_img= data_model - data_obs
    ;       res= total(res_img[mask_pix])
    ;       res_img_norm=res_img/data_obs
    ;       res_norm=total(res_img_norm[mask_pix])/n_mask_pix
    ;       res2_img=res_img^2
    ;       res2=total(res2_img[mask_pix])-res^2/n_mask_pix
    ;       res2_img_norm=res_img_norm^2
    ;       res2_norm=total(res2_img_norm[mask_pix])-res_norm^2
    ;
    ;
    ;       chi_img=res_img/data_sdev
    ;       chi=total(chi_img[mask_pix])/n_mask_pix
    ;       chi2_img=chi_img^2
    ;       chi2=total(chi2_img[mask_pix])/(n_mask_pix-n_free)-chi^2
    
      goodPix=where(metrics.mask_img,n_mask_pix)
      
      default,obs_dS,1
      default,mod_dS,1
      default,metrics_renorm,1
      
      obs_dS=obsI.dx*obsI.dy
      mod_dS=modI.dx*modI.dy
      metrics_renorm=n_mask_pix*mod_dS
      
      Iobs[i]=total(obsI.data[goodPix])*obs_dS
      Imod[i]=total(modI.data[goodPix])*mod_dS
      Res[i]=metrics.res_norm*metrics_renorm
      Chi[i]=metrics.chi*metrics_renorm
      res2_best[i]=metrics3.res2_norm
      chi2_best[i]=metrics3.chi2

    plot_map,modI,charsize=charsize,title=modI.id
    plot_map,modI,/over,lev=lev,/perc,color=0,thick=3
    plot_map,obsI,/over,lev=lev,/perc,color=200,thick=3
    get_map_coord,modI,x,y
    sz=size(modI.data)
    xyouts,x[10.*sz[1]/100,90.*sz[2]/100],y[10.*sz[1]/100,90.*sz[2]/100],string(dx,dy,format="('!4D!3x=',f7.3,' !4D!3y=',f7.3)"),charsize=charsize,color=255
    xyouts,x[10.*sz[1]/100,90.*sz[2]/100],y[10.*sz[1]/100,80.*sz[2]/100],string(q[i],format="('q=',g0)"),charsize=charsize,color=255
   endfor
   !p.multi=[0,1,2]
   charsize=1.2
   ;=================RES=========================
   Qm=exp(alog(min(Q))+(alog(max(Q))-alog(min(Q)))*dindgen(1000)/999)
   d=(n_elements(Q)-1)<3   
   
   !p.font=2
   plot, Q, Res, psym=4, /xlog,  xstyle=0, ystyle=1, xticks=4,$
     xrange=[min(Q)*0.95, max(Q)*1.05], yrange=[-max(abs(Res))*1.2, max(abs(Res))*1.2], $
     xtitle='!18Q!3', ytitle='!17 Residual!3', thick=2,charsize=1.2*charsize,title=_obsI.ID
   
   res_arr=res
   
   afit=(apply2 eq 1)?poly_fit(alog(Q), Res, d):linfit(alog(Q), Res)
   Res2=poly(alog(Qm), afit)
   oplot, Qm, Res2, thick=2
   
   oplot, 10^!x.crange, [0,0], linestyle=2, thick=2,color=250  
   
   res_thresh=sqrt(n_elements(goodPix))
   res_in_between=where(abs(Res) le res_thresh, res_count,comp=res_out)
   if res_count gt 0 then begin
    min_res=min(abs(Res[res_in_between]),min_res_idx)
    res2_best=double(res2_best[res_in_between[min_res_idx]])
    q_res2_best=double(q[res_in_between[min_res_idx]])
    res2_best_file=setfiles[res_in_between[min_res_idx]]
    res_best_metrics=obj_clone(obj_metrics_arr[res_in_between[min_res_idx]])
    res_done=1
   endif else begin
    res2_best=-1d
    q_res2_best=-1d
    res2_best_file=''
    res_done=0
    res_best_metrics=obj_new()
   endelse

   
   oplot, 10^!x.crange, res_thresh*[1,1], linestyle=1, thick=2
   oplot, 10^!x.crange, -res_thresh*[1,1], linestyle=1, thick=2  
   
   
   amin=min(abs(Res2), k)
   
   if (Res2[k] ne Res2[0]) and (Res2[k] ne Res2[-1]) then begin
     q_res_best=Qm[k]
     res_best=Res2[k]
     u=where(abs(Res2) le res_thresh)
     q_res_range=Qm[[min(u),max(u)]]
     res_range=Res2[[min(u),max(u)]]
   endif else begin
     idx=(amin eq Res2[0])?[0,1]:[Res2[n_elements(Res2)-2:*]]
     lfit=linfit(Qm[idx],Res2[idx])
     q_res_best=-lfit[0]/lfit[1]
     q_res_range=[(-res_thresh-lfit[0])/lfit[1],(res_thresh-lfit[0])/lfit[1]]
     q_res_range=q_res_range[sort(q_res_range)]
     res_best=lfit[0]
     res_range=[-res_thresh,res_thresh]
   endelse
   
   !p.font=2
   gx_plot_label,0.01,0.9,/xlog, string(a[0],-b[0],format="('a=',f5.2,'; ','b=',f5.2)"),charsize=charsize
   gx_plot_label,0.01,0.8, 'PROJECTED SOLUTION:',/xlog,charsize=charsize
   gx_plot_label,0.01,0.7, string([q_res_best,q_res_range-q_res_best], format="('Q!Dres_best!N = ',g0,'!S!D',g0,'!R!U+',g0)") ,/xlog,charsize=charsize
   if res_done then begin
     gx_plot_label,0.01,0.3, 'FINAL SOLUTION:',/xlog,charsize=charsize
     gx_plot_label,0.01,0.2, string(res2_best, format="('RES!S!U2!N!R!Dnorm!N = ',g0)") ,/xlog,charsize=charsize
     gx_plot_label,0.01,0.1, string([q_res2_best,q_res_range-q_res2_best], format="('Q = ',g0,'!S!D',g0,'!R!U+',g0)") ,/xlog,charsize=charsize
   end
   ;=================Chi=========================
   !p.font=2
   plot, Q, Chi, psym=4, /xlog,  xstyle=0, ystyle=1, xticks=4,$
     xrange=[min(Q)*0.95, max(Q)*1.05], yrange=[-max(abs(Chi))*1.2, max(abs(Chi))*1.2], $
     xtitle='!18Q!3', ytitle='!17 Chi!3', thick=2,charsize=1.2*charsize,title=_obsI.ID
   chi_arr=chi

   afit=(apply2 eq 1)?poly_fit(alog(Q), Chi, d):linfit(alog(Q), Chi)
   Chi2=poly(alog(Qm), afit)
   oplot, Qm, Chi2, thick=2
   oplot, 10^!x.crange, [0,0], linestyle=2, thick=2,color=250

   chi_thresh=sqrt(total((obsI.data[goodPix]/obsIsdev.data[goodPix])^2))
   chi_in_between=where(abs(Chi) le chi_thresh, chi_count,comp=chi_out)
   if chi_count gt 0 then begin
     min_chi=min(abs(chi[chi_in_between]),min_chi_idx)
     chi2_best=double(chi2_best[chi_in_between[min_chi_idx]])
     q_chi2_best=double(q[chi_in_between[min_chi_idx]])
     chi2_best_file=setfiles[chi_in_between[min_chi_idx]]
     chi_best_metrics=obj_clone(obj_metrics_arr[chi_in_between[min_chi_idx]])
     chi_done=1
   endif else begin
     chi2_best=-1d
     q_chi2_best=-1d
     chi2_best_file=''
     chi_best_metrics=obj_new()
     chi_done=0
   endelse

   oplot, 10^!x.crange, chi_thresh*[1,1], linestyle=1, thick=2
   oplot, 10^!x.crange, -chi_thresh*[1,1], linestyle=1, thick=2
   amin=min(abs(Chi2), k)
   if (Chi2[k] ne Chi2[0]) and (Chi2[k] ne Chi2[-1]) then begin
     q_chi_best=Qm[k]
     chi_best=Chi2[k]
     u=where(abs(Chi2) le chi_thresh)
     q_chi_range=Qm[[min(u),max(u)]]
     chi_range=Chi2[[min(u),max(u)]]
   endif else begin
     idx=(amin eq Chi2[0])?[0,1]:[Chi2[n_elements(Chi2)-2:*]]
     lfit=linfit(Qm[idx],Chi2[idx])
     q_chi_best=-lfit[0]/lfit[1]
     q_chi_range=[(-chi_thresh-lfit[0])/lfit[1],(chi_thresh-lfit[0])/lfit[1]]
     q_chi_range=q_chi_range[sort(q_chi_range)]
     chi_best=lfit[0]
     chi_range=[-chi_thresh,chi_thresh]
   endelse
   
   !p.font=2
   gx_plot_label,0.01,0.9,/xlog, string(a[0],-b[0],format="('a=',f5.2,'; ','b=',f5.2)"),charsize=charsize
   gx_plot_label,0.01,0.8, 'PROJECTED SOLUTION:',/xlog,charsize=charsize
   gx_plot_label,0.01,0.7, string([q_chi_best,q_chi_range-q_chi_best], format="('Q!Dchi_best!N = ',g0,'!S!D',g0,'!R!U+',g0)") ,/xlog,charsize=charsize
   if chi_done then begin
     gx_plot_label,0.01,0.3, 'FINAL SOLUTION:',/xlog,charsize=charsize
     gx_plot_label,0.01,0.2, string(chi2_best, format="('CHI!S!U2!N = ',g0)") ,/xlog,charsize=charsize
     gx_plot_label,0.01,0.1, string([q_chi2_best,q_chi_range-q_chi2_best], format="('Q = ',g0,'!S!D',g0,'!R!U+',g0)") ,/xlog,charsize=charsize
   end
  !p.font=-1
 endif
 
 device,/close
 res_idx=sort([abs(res)])
 chi_idx=sort([abs(chi)])
 case n_elements(res_idx) of
  1:res_idx=replicate(res_idx[0],3)
  2:res_idx=[res_idx,res_idx[0:1]]
  else:res_idx=res_idx[0:2]
 endcase
 case n_elements(chi_idx) of
   1:chi_idx=replicate(chi_idx[0],3)
   2:res_idx=[chi_idx,chi_idx[0:1]]
   else:chi_idx=chi_idx[0:2]
 endcase
 file_arr=setfiles[[res_idx,chi_idx]]
 q_arr=q[[res_idx,chi_idx]]
 idx=uniq(file_arr,sort(file_arr))
 file_arr=file_arr[idx]
 q_arr=q_arr[idx]
 
 result=[result,{a:double(a[0]),b:double(-b[0]),$
  q_res_best:double(q_res_best),q_res_range:double(q_res_range), res_best:double(res_best),res_range:double(res_range),$
  q_chi_best:double(q_chi_best),q_chi_range:double(q_chi_range), chi_best:double(chi_best),chi_range:double(chi_range),$
  ref:ref,use_mean:keyword_set(use_mean),modDir:modDir,psfile:filename,$
  res_threshold:res_thresh,chi_threshold:chi_thresh,chi2_best:chi2_best,res2_best:res2_best,q_chi2_best:q_chi2_best,$
  q_res2_best:q_res2_best,chi_done:chi_done,res_done:res_done,$
  res2_best_file:res2_best_file,chi2_best_file:chi2_best_file,res_best_metrics:res_best_metrics,chi_best_metrics:chi_best_metrics,mask:lev[0]}]

 obj_destroy,[obj_metrics_arr,obj_metrics]
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
 set_plot,'win'
 return,result
end

