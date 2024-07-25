pro gx_plot_mwrender_ebtel_solution,result,levels=levels,wsize=wsize,$
    charsize=charsize,labelsize=labelsize,dcolor=dcolor,mcolor=mcolor,thick=thick,ct=ct
objMetricsID=['RES','CHI']
default,levels,[12,20,30,50,80]
default,wsize,1000
default,charsize,2.5
default,labelsize,charsize*1.5
default,dcolor,200
default,mcolor,100
default,thick,3
default,charthick,thick
default,ct,39
objMetricsArr=[result.RES_BEST_METRICS,result.CHI_BEST_METRICS]
modFilesArr=[result.res2_best_file,result.chi2_best_file]
qArr=[result.q_res2_best,result.q_chi2_best]
a=result.a
b=result.b
thisDevice=!d.name
thisPlot=!p
tvlct,rgb,/get
loadct,ct
!p.multi=0
!p.font=1
!p.charsize=charsize
set_plot,(!version.os_family eq 'Windows')?'win':'x'
window,0,xsize=wsize,ysize=wsize
for k=0,1 do begin
  q=qarr[k]
  obj_metrics=objMetricsArr[k]
  modI=obj_metrics->get(0,/map)
  ;modI.data*=1e-6
  r=modI.roi_metrics
  obsI=obj_metrics->get(1,/map)
  dx=tag_exist(obsI,'orig_xc')?(obsI.xc-obsI.orig_xc):0.0
  dy=tag_exist(obsI,'orig_yc')?(obsI.yc-obsI.orig_yc):0.0
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
   d_res=1
  erase,255
  !p.font=1
  plot_map,modI,charsize=charsize,title='Brightness Temperature (K)',color=0,/noerase,$
    thick=thick,/cbar
  plot_map,modI,/over,levels=levels,/perc,color=mcolor,thick=thick
  plot_map,obsI,/over,levels=levels,/perc,color=dcolor,thick=thick
  gx_plot_label,0.6,0.1,'___ model',color=mcolor,charthick=charthick,charsize=labelsize
  gx_plot_label,0.6,0.05,'___ data',color=dcolor,charthick=charthick,charsize=labelsize
  gx_plot_label,0.1,0.9,string(a,b,format="('a=',g0,'; b=',g0)"),charsize=labelsize,color=255,charthick=charthick
  gx_plot_label,0.1,0.8,string(q,format="('Q!D0!N=',g0.3)"),charsize=labelsize,color=255,charthick=charthick
  gx_plot_label,0.1,0.7,string(total(npix),format="('N!DROI!N=',I0)"),charsize=labelsize,color=255,charthick=charthick
  gx_plot_label,0.1,0.6,string(R,format="('R=',g0.2)"),charsize=labelsize,color=255,charthick=charthick
  write_png,strcompress(string(a,b,k,format="('a',g0,'b',g0,'_Model2data_',i1,'.png')"),/rem),TVRD(/TRUE)
  erase,255
  !p.font=1
  plot_map,RES_NORM_MAP,charsize=charsize,title='Normalized Residual',color=0,/noerase,thick=thick,/cbar,dmax=d_res, dmin=-d_res
  ;!p.font=-1
  gx_plot_label,0.1,0.7,string(res,format="('!12<!9r!12>!3=',g0.3)"),charsize=labelsize,color=50,charthick=charthick
  gx_plot_label,0.1,0.6,string((res2),format="('  !9s!S!3!U2!R!D!9r!N!3=',g0.3)"),charsize=labelsize,color=50,charthick=charthick
  gx_plot_label,0.1,0.9,string(a,b,format="('a=',g0,'; b=',g0)"),charsize=labelsize,color=50,charthick=charthick
  gx_plot_label,0.1,0.8,string(q,format="('Q!D0!N=',g0.3)"),charsize=labelsize,color=50,charthick=charthick
  ;gx_plot_label,0.1,0.5,string(total(npix),format="('N!DROI!N=',I0)"),charsize=labelsize,color=255,charthick=charthick

  write_png,strcompress(string(a,b,k,format="('a',g0,'b',g0,'_res_',i1,'.png')"),/rem),TVRD(/TRUE)
  erase,255
  !p.font=1
  plot_map,CHI_MAP,charsize=charsize,title='CHI',color=0,/noerase,thick=thick,/cbar
  !p.font=-1
  gx_plot_label,0.1,0.7,string(chi,format="('!12<!7v!12>!3=',g0.3)"),charsize=labelsize,color=50,charthick=charthick
  gx_plot_label,0.1,0.6,string(chi2,format="(' !7v!U2!N!3=',g0.3)"),charsize=labelsize,color=50,charthick=charthick
  gx_plot_label,0.1,0.9,string(a,b,format="('a=',g0,'; b=',g0)"),charsize=labelsize,color=50,charthick=charthick
  gx_plot_label,0.1,0.8,string(q,format="('Q!D0!N=',g0.3)"),charsize=labelsize,color=50,charthick=charthick
  ;gx_plot_label,0.1,0.5,string(total(npix),format="('N!DROI!N=',I0)"),charsize=labelsize,color=255,charthick=charthick

  write_png,strcompress(string(a,b,k,format="('a',g0,'b',g0,'_chi_',i1,'.png')"),/rem),TVRD(/TRUE)


endfor
set_plot,thisDevice
!p=thisPlot
tvlct,rgb
end