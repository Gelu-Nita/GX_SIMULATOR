pro gx_plotbestmwmodels_ebtel, result,res2_best=res2_best,chi2_best=chi2_best,$
q_res2_best=q_res2_best,q_chi2_best=q_chi2_best, a=a,b=b, psDir,levels=levels,$
renorm_q=renorm_q
if ~isa(result) then begin
  message,'No input structure provided!',/cont
  return
endif
compile_opt idl2
default,psDir,curdir()+'\psDir'
if not file_test(psDir) then file_mkdir,psDir
 
 ;----------------------------------------------------------------------------
 objMetricsArr=[[result.RES_BEST_METRICS],[result.CHI_BEST_METRICS]]
 psFilesArr=[psDir+'\BestRes.ps',psDir+'\BestChi.ps']
 modFilesArr=[[result.res2_best_file],[result.chi2_best_file]]
 a0=result.a
 b0=result.b
 q_best= [[result.Q_RES2_BEST],[result.Q_CHI2_BEST]]
 
 a=a0[uniq(a0,sort(a0))]
 b=b0[uniq(b0,sort(b0))]
 chi2_img=(res2_img=(res_img=(chi_img=(q0_img=dblarr(n_elements(a),n_elements(b),2)*!values.d_nan))))
 obj_img=objarr(n_elements(a),n_elements(b),2)
 thisDevice = !D.Name
 set_plot,'ps'
 for k=0,1 do begin
   nmod=n_elements(result)
   R=(npix=(q0=(res=(res2=fltarr(nmod)))))
   chi2=res2
   chi=chi2
   psObject = Obj_New("FSC_PSConfig", /Color, /Times, /Bold, Filename=psFilesArr[k],yoffset=0.5,xoffset=0.25,ysize=6.4,xsize=9.5,landscape=1,bits=8)
   psKeys=psObject->GetKeywords()
   psKeys.filename=psFilesArr[k]
   Device, _Extra= psKeys
    
   default,pmulti,[0,3,2,0,0]
   !p.multi=pmulti
   !p.font=2
   charsize=1.2
   loadct,39
   modidx=0  
   smaxm=0
   d_res=1 
   default,levels,[12,20,30,50,80]
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
        dx=obsI.xc-obsI.orig_xc
        dy=obsI.yc-obsI.orig_yc
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
      plot_map,modI,charsize=charsize,title=filnam
      plot_map,modI,/over,levels=levels,/perc,color=0,thick=3
      plot_map,obsI,/over,levels=levels,/perc,color=200,thick=3
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
     endfor

     
     charsize=1.5
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
  
   if min(a0) lt max(a0) and min(b0) lt max(b0) then begin
    !p.multi=[0,3,4,0,0]
    ymargin=[3,1]
    plot,a,res2_img[*,0,k],psym=-1,charsize=charsize,xtitle='a',ytitle='RES!U2!N',yrange=minmax(res2_img),/nodata,/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(b)-1 do oplot,a,res2_img[*,l,k],psym=-1,color=50+l*30,thick=2
    min_res2=min(res2_img[*,*,k],imin_res2)
    idx_res2=array_indices(res2_img[*,*,k],imin_res2)
    plots,a[idx_res2[0]],min_res2,psym=2,color=250,symsize=symsize,thick=3
    minres2=min(res2,imin)
    gx_plot_label,0.1,1.5,string(minres2,format="('RES!U2!N=',g0)"),charsize=charsize
    gx_plot_label,0.1,1.3,string(a[idx_res2[0]],b[idx_res2[1]],format="('a=',f5.2,'; b=',f5.2)"),charsize=charsize
    gx_plot_label,0.1,1.1,string(q0[imin],format="('q=',g0)"),charsize=charsize
  
    plot,a,chi2_img[*,0,k],psym=-1,charsize=charsize,xtitle='a',ytitle='Chi!U2!N',yrange=minmax(chi2_img),/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(b)-1 do oplot,a,chi2_img[*,l,k],psym=-1,color=50+l*30,thick=2
    min_chi2=min(chi2_img[*,*,k],imin_chi2)
    idx_chi2=array_indices(chi2_img[*,*,k],imin_chi2)
    plots,a[idx_chi2[0]],min_chi2,psym=2,color=250,symsize=symsize,thick=3
    minchi2=min(chi2,imin)
    gx_plot_label,0.1,1.5,string(minchi2,format="('Chi!U2!N=',g0)"),charsize=charsize
    gx_plot_label,0.1,1.3,string(a[idx_chi2[0]],b[idx_chi2[1]],format="('a=',f5.2,'; b=',f5.2)"),charsize=charsize
    gx_plot_label,0.1,1.1,string(q0[imin],format="('q=',g0)"),charsize=charsize
  
    plot,a,res_img[*,0,k],psym=-1,charsize=charsize,xtitle='a',ytitle='RES',yrange=max(abs(minmax(res_img)))*[-1,1],/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(b)-1 do oplot,a,res_img[*,l,k],psym=-1,color=50+l*30,thick=2
     oplot,a,result.res_threshold/npix/mod_dS,color=250, thick=2
     oplot,a,-result.res_threshold/npix/mod_dS, color=250, thick=2

    plot,b,res2_img[0,*,k],psym=-1,charsize=charsize,xtitle='b',ytitle='RES!U2!N',yrange=minmax(res2_img),/nodata,/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(a)-1 do oplot,b,res2_img[l,*,k],psym=-1,color=50+l*30,thick=2
    plots,b[idx_res2[1]],min_res2,psym=2,color=250,symsize=symsize,thick=3
  
    plot,b,chi2_img[0,*,k],psym=-1,charsize=charsize,xtitle='b',ytitle='Chi!U2!N',yrange=minmax(chi2_img),/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(a)-1 do oplot,b,chi2_img[l,*,k],psym=-1,color=50+l*30,thick=2
    plots,b[idx_chi2[1]],min_chi2,psym=2,color=250,symsize=symsize,thick=3
  
    plot,b,res_img[0,*,k],psym=-1,charsize=charsize,xtitle='b',ytitle='RES',yrange=max(abs(minmax(res_img)))*[-1,1],/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(a)-1 do oplot,b,res_img[l,*,k],psym=-1,color=50+l*30,thick=2
    oplot,b,result.res_threshold/npix/mod_dS, color=250, thick=2
    oplot,b,-result.res_threshold/npix/mod_dS, color=250, thick=2
  
  
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
    oplot,a,result.chi_threshold/npix/mod_dS, color=250,thick=2
    oplot,a,-result.chi_threshold/npix/mod_dS, color=250,thick=2
    
    !p.multi=[1,3,4,0,0]
    plot,b,chi_img[0,*,k],psym=-1,charsize=charsize,xtitle='b',ytitle='CHI',yrange=(max(abs(minmax(chi_img)))>0.25)*[-1,1],/xsty, xmargin=xmargin,ymargin=ymargin
    for l=0,n_elements(a)-1 do oplot,b,chi_img[l,*,k],psym=-1,color=50+l*30,thick=2
    oplot,b,result.chi_threshold/npix/mod_dS, color=250,thick=2
    oplot,b,-result.chi_threshold/npix/mod_dS, color=250,thick=2
  end
   device,/close
 end
 a_arr=a
 b_arr=b
 if n_elements(a) lt 2 or n_elements(b) lt 2 then return
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
 filename=psDir+'\Best of Bests.ps'
 psObject = Obj_New("FSC_PSConfig", /Color, /Times, /Bold, Filename=Filename,yoffset=0.5,xoffset=0.25,ysize=6.4,xsize=9.5,landscape=1,bits=8)
 _Extra=psObject->GetKeywords()
 _Extra.filename=psDir+'\Best of Bests.ps'
 Device, _Extra=_Extra
 !p.multi=[0,3,2,0,1]
 
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
 ymargin=[2,1]
 cposition=[0.07,0.98,0.31,1.0]
 tvplot,res2_best,a,b,charsize=charsize,ymargin=ymargin,title='RES!U2!N',xtitle='a',ytitle='b',/sample,/iso
 plots,a[idx_res2[0]],b[idx_res2[1]],psym=2,color=250,symsize=symsize,thick=3
 gx_colorbar,minmax(res2_best),cposition=cposition-[0,0.02,0,0.02],charsize=2,font=!p.font

 tvplot,chi2_best,a,b,charsize=charsize,ymargin=ymargin,title='CHI!U2!N',xtitle='a',ytitle='b',/sample,/iso
 plots,a[idx_chi2[0]],b[idx_chi2[1]],psym=2,color=250,symsize=symsize,thick=3
 gx_colorbar,minmax(chi2_best),cposition=cposition-[0,0.52,0,0.52],charsize=2,font=!p.font
 
 cposition=[0.07,0.98,0.31,1.0]+[1,0,1,0]*0.335
 tvplot,keyword_set(renorm_q)?alog10(q_res2_best):q_res2_best,a,b,charsize=charsize,ymargin=ymargin,title='Q!D0!N RES!U2!N',xtitle='a',ytitle='b',/sample,/iso
 plots,a[idx_res2[0]],b[idx_res2[1]],psym=2,color=250,symsize=symsize,thick=3
 gx_colorbar,minmax(keyword_set(renorm_q)?alog10(q_res2_best):q_res2_best),cposition=cposition-[0,0.02,0,0.02],charsize=2,font=!p.font
 
 tvplot,keyword_set(renorm_q)?alog10(q_chi2_best):q_chi2_best,a,b,charsize=charsize,ymargin=ymargin,title='Q!D0!N CHI!U2!N',xtitle='a',ytitle='b',/sample,/iso
 plots,a[idx_chi2[0]],b[idx_chi2[1]],psym=2,color=250,symsize=symsize,thick=3
 gx_colorbar,minmax(keyword_set(renorm_q)?alog10(q_chi2_best):q_chi2_best),cposition=cposition-[0,0.52,0,0.52],charsize=2,font=!p.font
  
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
 gx_plot_label,0.05,0.9,strcompress(string(best_res2,a[idx_res2[0]],b[idx_res2[1]],format="('RES!U2!N=',f0.3,'; a= ',f0.2,'; b= ',f0.2)")),charsize=1,color=50
 gx_plot_label,0.05,0.8,strcompress(string(best_chi2,a[idx_chi2[0]],b[idx_chi2[1]],format="('CHI!U2!N=',f0.3,'; a= ',f0.2,'; b= ',f0.2)")),charsize=1,color=250
 
 ymargin=[2,6]
 plot,a,res2_min,charsize=charsize,ymargin=ymargin,/xsty,title='Best of Bests (RES!U2!N, CHI!U2!N)',color=0,/noerase,ysty=9,xtitle='a',ytitle='RES!U2!N'
 oplot,a,res2_min,color=50,thick=3
 plot,a,chi2_min,charsize=charsize,ymargin=ymargin,/xsty,color=0,ysty=5
 oplot,a,chi2_min,color=250,thick=3
 axis,yaxis=1,ytitle='CHI!U2!N',/ysty,charsize=charsize,ymargin=ymargin
 gx_plot_label,0.05,0.9,string(best_res2,best_res2_q,format="('RES!U2!N=',f0.3,' Q!D0!N=',g0)"),charsize=1,color=50
 gx_plot_label,0.05,0.8,string(best_chi2,best_chi2_q,format="('CHI!U2!N=',f0.3,' Q!D0!N=',g0)"),charsize=1,color=250
 
 obj_img=[obj_res2_best[idx_res2[0],idx_res2[1]],obj_chi2_best[idx_chi2[0],idx_chi2[1]]]
 !p.multi=[0,3,2,0,0]
 plots=['Best RES solution: ','Best CHI solution: ']
 ab=[[a[idx_res2[0]],b[idx_res2[1]]],[a[idx_chi2[0]],b[idx_chi2[1]]]]
 q=[q_res2_best,q_chi2_best]
 for k=0,1 do begin
   filnam=plots[k]
   a=ab[0,k]
   b=ab[1,k]
   q0=q[k]
   obj_metrics=obj_img[k]
   modI=obj_metrics->get(0,/map)
   R=modI.roi_metrics
   obsI=obj_metrics->get(1,/map)
   dx=obsI.xc-obsI.orig_xc
   dy=obsI.yc-obsI.orig_yc
   obsIsdev=obj_metrics->get(2,/map)
   mod_dS=modI.dx*modI.dy
   npix=obj_metrics->get(3,/roi_metrics)
   RES_NORM_MAP=obj_metrics->get(5,/map)
   res=res_norm_map.roi_metrics
   bad=where(RES_NORM_MAP.data eq 1,nbad,ncomp=ncomp)
   if nbad gt 0 then RES_NORM_MAP.data[bad]=0
   RES2_MAP=obj_metrics->get(7,/map)
   res2=RES2_MAP.roi_metrics
   CHI_MAP=obj_metrics->get(8,/map)
   chi=CHI_MAP.roi_metrics
   CHI2_MAP=obj_metrics->get(9,/map)
   chi2=CHI2_MAP.roi_metrics
   plot_map,modI,charsize=charsize,title=filnam+'Model2Data'
   plot_map,modI,/over,levels=levels,/perc,color=0,thick=3
   plot_map,obsI,/over,levels=levels,/perc,color=200,thick=3
   get_map_coord,modI,x,y
   sz=size(modI.data)
   sx=sz[1]/100.
   sy=sz[2]/100.
   !p.font=-1
   xyouts,x[10*sx,90*sy],y[10*sx,90*sy],strcompress(string(dx,dy,format="('!4D!3x=',f7.2,'; !4D!3y=',f7.2)"),/rem),charsize=1.1*charsize,color=255,charthick=3
   !p.font=2
   xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(R,format="(' R=',g0)"),charsize=charsize,color=255
   xyouts,x[10*sx,90*sy],y[10*sx,70*sy],string(Q0,format="(' Q0=',g0)"),charsize=charsize,color=255
   xyouts,x[10*sx,90*sy],y[10*sx,60*sy],string(a,b,format="(' (a; b)=(',g0,'; ',g0,')')"),charsize=charsize,color=255
   xyouts,x[10*sx,90*sy],y[10*sx,10*sy],string(total(npix),format="(' Mask_Npix=',I0)"),charsize=charsize,color=255
   plot_map,RES_NORM_MAP,charsize=charsize,title=filnam+'RES Map',dmax=d_res, dmin=-d_res
   xyouts,x[10*sx,90*sy],y[10*sx,90*sy],string(res,format="(' Res=',g0)"),charsize=charsize,color=25
   xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(res2,format="(' Res!U2!N=',g0)"),charsize=charsize,color=25
   plot_map,CHI2_MAP,charsize=charsize,title=filnam+'CHI!U2!N Map' ,dmax=d_res*20, dmin=0 ;-d_res*10
   xyouts,x[10*sx,90*sy],y[10*sx,90*sy],string(chi,format="(' Chi=',g0)"),charsize=charsize,color=200
   xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(chi2,format="(' Chi!U2!N=',g0)"),charsize=charsize,color=200
 endfor
 device,/close
 !p.font=0 
 !p.multi=0  
 a=a_arr
 b=b_arr
 set_plot,thisDEvice
end

