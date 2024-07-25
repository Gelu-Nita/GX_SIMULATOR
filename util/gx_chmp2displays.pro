pro gx_chmp2displays,obj_metrics,win=win,levels=levels,$
res_min=res_min,res_max=res_max,chi2_min=chi2_min,$
chi2_max=chi2_max,xsize=xsize,ysize=ysize,metrics_log=metrics_log,_extra=_extra
 
 if n_elements(xsize) eq 1 and n_elements(ysize) eq 1 then begin
  win=[1,0,2]
  for k=0,2 do begin
    window,win[k],xsize=xsize,ysize=ysize
    wshow,win[k]
  endfor
 endif
 thisP=!p
 !p.background=255
 !p.color=0
 !p.charthick=2
 if ~obj_valid(obj_metrics) then begin
   for k=0,2 do begin
     wset,win[k]
     plot,findgen(10),/nodata
     gx_plot_label,0.2,0.5,'No solution here!'
   endfor
   goto,exit_point
 endif
 
 maps=gx_chmp2maps(obj_metrics)
 if n_elements(win) gt 0 then wset,win[0]
 default,levels,[12,20,30,50,80]
 if tag_exist(maps.modI,'freq') then $
 filnam=strcompress(string(fix(100*maps.modI.freq)/100d,format="('Best @',g0,' GHz: ')")) $
 else $
 filnam=strcompress(string(fix(100*maps.modI.chan)/100d,format="('Best @',g0,' A: ')")) 
 plot_map,maps.modI,_extra=_extra,title=filnam+'Data2Model',/cbar,cb_title='Tb (K)'
 plot_map,maps.modI,/over,levels=levels,/perc,color=0,thick=3
 plot_map,maps.obsI,/over,levels=levels,/perc,color=200,thick=3
 get_map_coord,maps.modI,x,y
 sz=size(maps.modI.data)
 dx=tag_exist(maps.obsI,'orig_xc')?(maps.obsI.xc-maps.obsI.orig_xc):0.0
 dy=tag_exist(maps.obsI,'orig_yc')?(maps.obsI.yc-maps.obsI.orig_yc):0.0
 sx=sz[1]/100.
 sy=sz[2]/100. 
 xyouts,x[10*sx,90*sy],y[10*sx,70*sy],string(maps.R,format="(' R=',g0)"),_extra=_extra,color=255
 xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(maps.Q0,format="(' q=',g0)"),_extra=_extra,color=255
 xyouts,x[10*sx,90*sy],y[10*sx,90*sy],string(maps.a,maps.b,format="(' (a; b)=(',g0,'; ',g0,')')"),_extra=_extra,color=255
 xyouts,x[10*sx,90*sy],y[10*sx,15*sy],string(total(maps.npix),format="(' Mask_Npix=',I0)"),_extra=_extra,color=255
 xyouts,x[10*sx,90*sy],y[10*sx,5*sy],strcompress(string(dx,dy,format="('!4D!3x=',f7.2,'; !4D!3y=',f7.2)"),/rem),_extra=_extra,color=255
 if n_elements(win) ge 2 then wset,win[1]
 plot_map,maps.RES_NORM_MAP,_extra=_extra,title=filnam+'RES Map',dmax=res_min, dmin=res_max,/cbar
 xyouts,x[10*sx,90*sy],y[10*sx,90*sy],string(maps.res,format="(' Res=',g0)"),_extra=_extra,color=25
 xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(maps.res2,format="(' Res!U2!N=',g0)"),_extra=_extra,color=25
 if n_elements(win) ge 3 then wset,win[2]
 plot_map,maps.CHI2_MAP,_extra=_extra,title=filnam+'CHI!U2!N Map' ,dmax=chi2_min, dmin=chi2_max,/cbar,log=metrics_log
 xyouts,x[10*sx,90*sy],y[10*sx,90*sy],string(maps.chi,format="(' Chi=',g0)"),_extra=_extra,color=200
 xyouts,x[10*sx,90*sy],y[10*sx,80*sy],string(maps.chi2,format="(' Chi!U2!N=',g0)"),_extra=_extra,color=200
 exit_point:
 !p=thisP
end