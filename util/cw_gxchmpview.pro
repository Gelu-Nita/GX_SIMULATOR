function gxchmpview::INIT,wBase,_extra=_extra
  compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return, 0
  end
  self.WinOS=!version.os_family eq 'Windows'
  void=self->gxWidget::Init(wBase,self,KILL_NOTIFY='objgxchmpviewKill',_extra=_extra)
  return,1
end  


pro objgxchmpviewKill,wBase
  widget_control,wBase,get_uvalue=obj
  obj_destroy,obj
end


pro gxchmpview__define
  struct_hide,{gxchmpview, inherits gxwidget,$
               ResultsDir:'',pfiles:ptr_new(),summary:ptr_new(),maps:ptr_new(),$
               WinOS:0l,wmetrics:0l,wmap:0l,wmetrics_spectrum:0l,wmetrics_solution:0l,$
               wmap_spectrum:0l,wmetrics_select:0l,wmap_select:0l,$
               wa:0l,wb:0l,wfreq:0l,wx:0l,wy:0l,wmaps_spectrum_extra:0l,wmetrics_spectrum_extra:0l,wmetrics_solution_extra:0l}
end

pro gxchmpview_lock_set, wid_id, lock
 widget_control,wid_id,pro_set_value=''
 widget_control,wid_id,set_value=lock?gx_bitmap(gx_findfile('lock.bmp')):gx_bitmap(gx_findfile('unlock.bmp')),/bitmap,set_button=lock
 widget_control,wid_id,pro_set_value='gxchmpview_lock_set'
end

pro gxchmpview::CreatePanel,xsize=xsize,ysize=ysize

  subdirectory=['resource', 'bitmaps']
  colors=['black','maroon','red','pink','orange','yellow','olive','green','dark green','cyan','blue','dark_blue','magenta','purple']
  styles=['Solid', 'Dotted', 'Dashed', 'Dash-Dot']+ ' Line'
  symbols = ['No Symbols','Plus', 'Asterisk', 'Period', 'Diamond' , 'Triangle','Square']
  metrics=['Eta','CHI','Rho','bestQ','CC','SHIFTX','SHIFTY']
  maps=['Data', 'Convolved Model','Model','Eta','Rho','Chi']

  toolbar= widget_base(self.wBase, /row,/toolbar)
  wAbout=widget_button( toolbar, $
    value=gx_bitmap(filepath('help.bmp', subdirectory=subdirectory)), /bitmap,uname='help_about')
  settings_base=widget_base(self.wBase,/column,uname='settings_base',/frame)

  main_Base=widget_base(self.wBase,/row)
  
  left_tab=widget_tab(main_Base,uname='left_tab')
  right_tab=widget_tab(main_Base,uname='right_tab')
   
  
  metrics_base=widget_base(left_tab,/column,title='Metrics Display',/frame,uname='metrics_page')
  map_base=widget_base(right_tab,/column,title='Map Display',/frame,uname='map_base')
  
  metrics_base_selectors=widget_base(metrics_base,/row,/frame)
  map_base_selectors    =widget_base(map_base,/row,/frame)

  metrics_display_base=widget_base(metrics_base,/frame,/column)
  map_display_base    =widget_base(map_base,/frame,/column)
  
  
  contours_base=widget_base(metrics_base,/frame,/column)
  metrics_contours_base=widget_base(contours_base,/frame,/row)
  cc_contours_base=widget_base(contours_base,/frame,/row)
  
  
  map_contours_base=widget_base(map_base,/frame,/column)
  data_contours_base=widget_base(map_contours_base,/frame,/row)
  model_contours_base=widget_base(map_contours_base,/frame,/row)
  
  
  metrics_spectrum_base=widget_base(right_tab,/column,title='Metrics Spectrum',/frame,uname='metrics_spectrum_base')
  metrics_solution_base=widget_base(right_tab,/column,title='Metrics Solution',/frame,uname='metrics_solution_base')
  maps_spectrum_base=widget_base(left_tab,/column,title='Map Spectrum',/frame,uname='map_spectrum_page')

  metrics_spectrum_base_selectors=widget_base(metrics_spectrum_base,/row,/frame)
  metrics_solution_base_selectors=widget_base(metrics_solution_base,/row,/frame)
  maps_spectrum_base_selectors    =widget_base(maps_spectrum_base,/row,/frame)

  metrics_spectrum_display_base=widget_base(metrics_spectrum_base,/frame,/column)
  metrics_solution_display_base=widget_base(metrics_solution_base,/frame,/column)
  maps_spectrum_display_base    =widget_base(maps_spectrum_base,/frame,/column)
 


  levels='[50,70,90]'
  metrics_colors=widget_combobox(metrics_contours_base,value=colors,uname='metrics_colors')
  widget_control,metrics_colors,set_combobox_select=5
  wThick=cw_objfield(metrics_contours_base,label='thick:',value=1.0,format='(f0.1)',inc=1,min=1,uname='metrics_thick')
  wLevels=widget_text(metrics_contours_base,value=levels,/editable,uname='metrics_levels',uvalue=levels,xsize=12)
  wpercent=cw_bgroup(metrics_contours_base,/exclusive,/row,['value','%'],uname='metrics_percent',set_value=[1])
  wContours=cw_bgroup(metrics_contours_base,/nonexclusive,/row,['Metrics Contours'],label_left='',uname='metrics_contours',set_value=[0])

  cc_colors=widget_combobox(cc_contours_base,value=colors,uname='cc_colors')
  widget_control,cc_colors,set_combobox_select=10
  wThick=cw_objfield(cc_contours_base,label='thick:',value=1.0,format='(f0.1)',inc=1,min=1,uname='cc_thick')
  wLevels=widget_text(cc_contours_base,value='0.9',/editable,uname='cc_levels',uvalue=levels,xsize=12)
  wpercent=cw_bgroup(cc_contours_base,/exclusive,/row,['value','%'],uname='cc_percent',set_value=[0])
  wContours=cw_bgroup(cc_contours_base,/nonexclusive,/row,['CC Contours'],label_left='',uname='cc_contours',set_value=[0])

  
  g = widget_info (metrics_contours_base, /geom) 
  scr_xsize=g.scr_xsize
  scr_ysize=scr_xsize

    
  metrics_base_selectors=widget_base(metrics_base_selectors,scr_xsize=scr_xsize,/row)
  self.wmetrics_select=widget_combobox(metrics_base_selectors,value=metrics,/dynamic)

  wLogmetrics=widget_combobox(metrics_base_selectors,value=['Linear Scale','Log Scale'],uname='log_metrics')
  metrics_toolbar=widget_base(metrics_base_selectors,/toolbar,/row)  
   
  
  
  self.wmetrics=WIDGET_DRAW(metrics_display_base,scr_xsize=scr_xsize,scr_ysize=scr_ysize,UNAME='metrics_display',$
    /button_events, $
    ;/motion_events, $
    retain=1,$
    /expose_events, $
    graphics_level=1 $
    )
  
  map_base_selectors=widget_base(map_base_selectors,scr_xsize=scr_xsize,/row)  
  self.wmap_select=widget_combobox(map_base_selectors,value=maps ,/dynamic)
  
  wLogMap=widget_combobox(map_base_selectors,value=['Linear Scale','Log Scale'],uname='log_map')
  map_toolbar=widget_base(map_base_selectors,/toolbar,/row)
  

  self.wmap=WIDGET_DRAW(map_display_base ,scr_xsize=scr_xsize,scr_ysize=scr_ysize,UNAME='map_display',$
    /button_events, $
    ;/motion_events, $
    retain=1,$
    /expose_events, $
    graphics_level=1 $
    )


  levels='[15,30,60,90]'
  data_colors=widget_combobox(data_contours_base,value=colors,uname='data_colors')
  widget_control,data_colors,set_combobox_select=5
  wThick=cw_objfield(data_contours_base,label='thick:',value=1.0,format='(f0.1)',inc=1,min=1,uname='data_thick')
  wLevels=widget_text(data_contours_base,value=levels,/editable,uname='data_levels',uvalue=levels,xsize=14)
  wpercent=cw_bgroup(data_contours_base,/exclusive,/row,['value','%'],uname='data_percent',set_value=[1])
  wContours=cw_bgroup(data_contours_base,/nonexclusive,/row,['Data Contours'],label_left='',uname='data_contours',set_value=[0])

  model_colors=widget_combobox(model_contours_base,value=colors,uname='model_colors')
  widget_control,model_colors,set_combobox_select=10
  wThick=cw_objfield(model_contours_base,label='thick:',value=1.0,format='(f0.1)',inc=1,min=1,uname='model_thick')
  wLevels=widget_text(model_contours_base,value=levels,/editable,uname='model_levels',uvalue=levels,xsize=14)
  wpercent=cw_bgroup(model_contours_base,/exclusive,/row,['value','%'],uname='model_percent',set_value=[1])
  wContours=cw_bgroup(model_contours_base,/nonexclusive,/row,['Model Contours'],label_left='',uname='model_contours',set_value=[0])

  
  
  tvlct,rgb_curr,/get
  loadct,39,/silent
  gx_rgb_white2black
  tvlct,rgb,/get
  tvlct,rgb_curr
  wPalette = widget_button(metrics_toolbar, $
    value=gx_bitmap(filepath('palette.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Change Metrics Color Table',uname='metrics_lct',uvalue=rgb)
 
  wmetrics2PNG=widget_button(metrics_toolbar, $
    value=gx_bitmap(filepath('export.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Export plot as PNG',uname='plot2png',uvalue=self.wmetrics)
  
  wMetrics2Movie=widget_button(widget_base(metrics_toolbar,/nonexclusive), $
    value=gx_bitmap(filepath('eba_prop_ex_nocm.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Create movie over frequencies',uname='plot2movie',uvalue=self.wmetrics)
  
  
  wPalette = widget_button(map_toolbar, $
    value=gx_bitmap(filepath('palette.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Change Map Color Table',uname='map_lct',uvalue=rgb) 
  
  wMap2PNG=widget_button(map_toolbar, $
    value=gx_bitmap(filepath('export.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Export plot as PNG',uname='plot2png',uvalue=self.wmap)  
  
  wMap2Movie=widget_button(map_toolbar, $
    value=gx_bitmap(filepath('eba_prop_ex_nocm.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Create movie over frequencies',uname='plot2movie',uvalue=self.wmap)   

  wlabel=widget_label(map_toolbar,value='     x: ')
  self.wx=widget_combobox(map_toolbar,value=['None'],/dynamic)
  wlabel=widget_label(map_toolbar,value='y: ')
  self.wy=widget_combobox(map_toolbar,value=['None'],/dynamic)
    
    
  g = widget_info (main_base, /geom)
  wResultsDirBase=widget_base(settings_base,/row,scr_xsize=g.scr_xsize,/frame)
  wResultsBase=widget_base(wResultsDirBase,/row)
  wlabel=widget_label(wResultsBase,value='CHMP Results Directory',scr_xsize=label_scr_xsize)
  wSelectResultsDir= widget_button(wResultsBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Select the directory where the CHMP results are stored',uname='resultsdir_select')
  wUpdateResultsDir= widget_button(wResultsBase, $
    value=gx_bitmap(filepath('redo.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Update the list of flies located in the CHMP results directory',uname='resultsdir_update')
  wDeleteSolution=widget_button(wResultsBase,value=gx_bitmap(filepath('delete.bmp', subdirectory=subdirectory)),tooltip='Discard the current results',/bitmap,uname='remove')
  geom = widget_info (wResultsBase, /geom)
  wResultsDirPath=widget_text(wResultsDirBase,scr_xsize=2*scr_xsize-geom.scr_xsize,uname='resultsdir',/editable,$
    value=self.resultsdir)
  options_base=widget_base(settings_base,/row,/frame)
  charsize_base=widget_base(options_base,/frame,/row)
  wcharsize=cw_objfield(charsize_base,value=!version.os_family eq 'Windows'?1.2:1,inc=1,min=0.1,max=5,label='Plot Charsize: ',font=!defaults.font,xtextsize=6,uname='charsize')
  wLabels=cw_bgroup(widget_base(options_base,/frame),label_left='Plot Labels: ',$
          ['[a,b] Crosshair','Metrics @ [a,b]','Selected [a,b,freq.]','Data to Model Allignment Shift',$
           '{x,y} Crosshair','Map @ [x,y]'],/nonexclusive,set_value=[1,1,1,1,1,1],/row,uname='plot_legends')
  selectors_base=metrics_toolbar
  wlabel=widget_label(selectors_base,value='a: ')
  self.wa=widget_combobox(selectors_base,value=['None'],/dynamic)
  wlabel=widget_label(selectors_base,value='b: ')
  self.wb=widget_combobox(selectors_base,value=['None'],/dynamic)
  wBestMetrics= widget_button(font=font, widget_base(metrics_base_selectors,/nonexclusive,/row), $
    value=gx_bitmap(gx_findfile('unlock.bmp')), /bitmap,tooltip='Lock/Unlock Best Metrics',uname='metrics_best',pro_set_value='gxchmpview_lock_set')

  frequency_base=widget_base(settings_base,/frame,/row)
  wleft=widget_base(frequency_base,/row)
  wmiddle=widget_base(frequency_base,/row,/toolbar)
  wright=widget_base(frequency_base,/row)
  wlabel=widget_label(wmiddle,value='freq: ')
  self.wfreq=widget_combobox(wmiddle,value=['00.00 GHz'],/dynamic)
  wFreqCycle=widget_button(font=font, widget_base(wmiddle,/nonexclusive,/row), $
    value=gx_bitmap(gx_findfile('redo.bmp')), /bitmap,tooltip='Cycle through all frquencies',uname='freq_cycle')
  g1=widget_info(settings_base,/geometry)
  g2=widget_info(wmiddle,/geometry)
  dummy=widget_base(wleft,scr_xsize=(g1.scr_xsize-g2.scr_xsize)/2)
  
  
  self.wmetrics_spectrum=WIDGET_DRAW(metrics_spectrum_display_base,scr_xsize=scr_xsize,scr_ysize=scr_ysize,UNAME='metrics_spectrum_display',$
    ;/button_events, $
    ;/motion_events, $
    retain=1,$
    /expose_events, $
    graphics_level=1 $
    )
    
  self.wmap_spectrum=WIDGET_DRAW(maps_spectrum_display_base,scr_xsize=scr_xsize,scr_ysize=scr_ysize,UNAME='map_spectrum_display',$
    ;/button_events, $
    ;/motion_events, $
    retain=1,$
    /expose_events, $
    graphics_level=1 $
    )
    
  self.wmetrics_solution=WIDGET_DRAW(metrics_solution_display_base,scr_xsize=scr_xsize,scr_ysize=scr_ysize,UNAME='metrics_solution_display',$
    ;/button_events, $
    ;/motion_events, $
    retain=1,$
    /expose_events, $
    graphics_level=1 $
    )  
 
  g0=widget_info(metrics_base_selectors,/geometry)
  metrics_spectrum_toolbar=widget_base(metrics_spectrum_base_selectors,/toolbar,/row,scr_ysize=g0.scr_ysize) 
  metrics_solution_toolbar=widget_base(metrics_solution_base_selectors,/toolbar,/row,scr_ysize=g0.scr_ysize) 
  maps_spectrum_toolbar=widget_base(maps_spectrum_base_selectors,/toolbar,/row,scr_ysize=g0.scr_ysize)  

  wMapSpectrum2PNG=widget_button(maps_spectrum_toolbar, $
    value=gx_bitmap(filepath('export.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Export plot as PNG',uname='plot2png',uvalue=self.wmap_spectrum) 
 
  wMetricsSpectrum2PNG=widget_button(metrics_spectrum_toolbar, $
    value=gx_bitmap(filepath('export.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Export plot as PNG',uname='plot2png',uvalue=self.wmetrics_spectrum)  
  
  wMetricsSolution2PNG=widget_button(metrics_solution_toolbar, $
    value=gx_bitmap(filepath('export.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Export plot as PNG',uname='plot2png',uvalue=self.wmetrics_solution)
    
  wMetricsSolution2Movie=widget_button(metrics_solution_toolbar, $
    value=gx_bitmap(filepath('eba_prop_ex_nocm.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Create movie over frequencies',uname='plot2movie',uvalue=self.wmetrics_solution)   
    
  metrics_spectrum_extra_base=widget_base(metrics_spectrum_base_selectors,/frame,/row)
  wlabel=widget_label(metrics_spectrum_extra_base,value='plot_extra: ')
  g1=widget_info(metrics_spectrum_toolbar,/geometry)
  g2=widget_info(wlabel,/geometry)
  self.wmetrics_spectrum_extra=widget_text(metrics_spectrum_extra_base,scr_xsize=scr_xsize-(g1.scr_xsize+g2.scr_xsize+12),/editable, value='/xlog, /xsty, xmargin=[10,3]')     
  
  
  metrics_solution_extra_base=widget_base(metrics_solution_base_selectors,/frame,/row)
  wlabel=widget_label(metrics_solution_extra_base,value='plot_extra: ')
  g2=widget_info(wlabel,/geometry)
  self.wmetrics_solution_extra=widget_text(metrics_solution_extra_base,scr_xsize=scr_xsize-(g1.scr_xsize+g2.scr_xsize+12),/editable, value='/xsty, psym=-1, xmargin=[10,3]')

  
  metrics_spectrum_overplot=widget_base(metrics_spectrum_base,/column,uname='metrics_spectrum_overplot')
  metrics_spectrum_overplot1=widget_base(metrics_spectrum_overplot,/frame,/row,uname='metrics_spectrum_overplot1')
  metrics_spectrum_overplot2=widget_base(metrics_spectrum_overplot,/frame,/row,uname='metrics_spectrum_overplot2')
  metrics_spectrum_overplot3=widget_base(metrics_spectrum_overplot,/frame,/row,uname='metrics_spectrum_overplot3')
  
  wlabel=widget_label(metrics_spectrum_overplot1,value='Over: ')
  wlabel=widget_label(metrics_spectrum_overplot2,value='Over: ')
  wlabel=widget_label(metrics_spectrum_overplot3,value='Over: ')
  
  overplot1=widget_combobox(metrics_spectrum_overplot1,value=['None',metrics],uname='overplot_select')
  overplot2=widget_combobox(metrics_spectrum_overplot2,value=['None',metrics],uname='overplot_select')
  overplot3=widget_combobox(metrics_spectrum_overplot3,value=['None',metrics],uname='overplot_select')

  overplot1_color=widget_combobox(metrics_spectrum_overplot1,value=colors,uname='overplot_colors')
  overplot2_color=widget_combobox(metrics_spectrum_overplot2,value=colors,uname='overplot_colors')
  overplot3_color=widget_combobox(metrics_spectrum_overplot3,value=colors,uname='overplot_colors')
  
  overplot1_symbol=widget_combobox(metrics_spectrum_overplot1,value=symbols,uname='overplot_symbols')
  overplot2_symbol=widget_combobox(metrics_spectrum_overplot2,value=symbols,uname='overplot_symbols')
  overplot3_symbol=widget_combobox(metrics_spectrum_overplot3,value=symbols,uname='overplot_symbols')
  
  overplot1_style=widget_combobox(metrics_spectrum_overplot1,value=styles,uname='overplot_styles')
  overplot2_style=widget_combobox(metrics_spectrum_overplot2,value=styles,uname='overplot_styles')
  overplot3_style=widget_combobox(metrics_spectrum_overplot3,value=styles,uname='overplot_styles')

  overplot1__thick=cw_objfield(metrics_spectrum_overplot1,label='thick:',value=1.0,min=0,format='(f0.1)',inc=1,uname='overplot_thick')
  overplot2__thick=cw_objfield(metrics_spectrum_overplot2,label='thick:',value=1.0,min=0,format='(f0.1)',inc=1,uname='overplot_thick')
  overplot3__thick=cw_objfield(metrics_spectrum_overplot3,label='thick:',value=1.0,min=0,format='(f0.1)',inc=1,uname='overplot_thick')
  
 
  maps_spectrum_extra_base=widget_base(maps_spectrum_base_selectors,/frame,/row)
  wlabel=widget_label(maps_spectrum_extra_base,value='plot_extra: ')
  g1=widget_info(maps_spectrum_toolbar,/geometry)
  g2=widget_info(wlabel,/geometry)
  self.wmaps_spectrum_extra=widget_text(maps_spectrum_extra_base,scr_xsize=scr_xsize-(g1.scr_xsize+g2.scr_xsize+12),/editable, value='/xlog,/xsty,/ylog, xmargin=[10,3]')

  maps_spectrum_overplot=widget_base(maps_spectrum_base,/column,uname='maps_spectrum_overplot')
  maps_spectrum_overplot1=widget_base(maps_spectrum_overplot,/frame,/row,uname='maps_spectrum_overplot1')
  maps_spectrum_overplot2=widget_base(maps_spectrum_overplot,/frame,/row,uname='maps_spectrum_overplot2')
  maps_spectrum_overplot3=widget_base(maps_spectrum_overplot,/frame,/row,uname='maps_spectrum_overplot3')

  wlabel=widget_label(maps_spectrum_overplot1,value='Over: ')
  wlabel=widget_label(maps_spectrum_overplot2,value='Over: ')
  wlabel=widget_label(maps_spectrum_overplot3,value='Over: ')

  overplot1=widget_combobox(maps_spectrum_overplot1,value=['None',maps],uname='overplot_select')
  overplot2=widget_combobox(maps_spectrum_overplot2,value=['None',maps],uname='overplot_select')
  overplot3=widget_combobox(maps_spectrum_overplot3,value=['None',maps],uname='overplot_select')

  overplot1_color=widget_combobox(maps_spectrum_overplot1,value=colors,uname='overplot_colors')
  overplot2_color=widget_combobox(maps_spectrum_overplot2,value=colors,uname='overplot_colors')
  overplot3_color=widget_combobox(maps_spectrum_overplot3,value=colors,uname='overplot_colors')
  
  
  overplot1_symbol=widget_combobox(maps_spectrum_overplot1,value=symbols,uname='overplot_symbols')
  overplot2_symbol=widget_combobox(maps_spectrum_overplot2,value=symbols,uname='overplot_symbols')
  overplot3_symbol=widget_combobox(maps_spectrum_overplot3,value=symbols,uname='overplot_symbols')

  overplot1_style=widget_combobox(maps_spectrum_overplot1,value=styles,uname='overplot_styles')
  overplot2_style=widget_combobox(maps_spectrum_overplot2,value=styles,uname='overplot_styles')
  overplot3_style=widget_combobox(maps_spectrum_overplot3,value=styles,uname='overplot_styles')

  overplot1__thick=cw_objfield(maps_spectrum_overplot1,label='thick:',value=1.0,min=0,format='(f0.1)',inc=1,uname='overplot_thick')
  overplot2__thick=cw_objfield(maps_spectrum_overplot2,label='thick:',value=1.0,min=0,format='(f0.1)',inc=1,uname='overplot_thick')
  overplot3__thick=cw_objfield(maps_spectrum_overplot3,label='thick:',value=1.0,min=0,format='(f0.1)',inc=1,uname='overplot_thick')

end   


function gxchmpview::valid_repository,path
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    answ=dialog_message(['"'+path+'"',error],/INFO)
    return, !null
  end
  if ~(file_info(path)).exists then begin
    error='does not exists!'
    dummy=does_not_exists
  endif
  files = FILE_SEARCH(path + path_sep() + 'fit*.sav')
  error='is not a valid CHMP results repository!'
  if n_elements(files) gt 0 then begin
    restore,files[0],/relaxed
    if n_elements(BESTQARR) eq 0 then dummy=invalid_repository
  endif else dummy=invalid_repository
  return,files
end

pro gxchmpview::UpdateSummary
 if ~ptr_valid(self.pfiles) then begin
  ptr_free,self.summary
  return
 endif
 widget_control,/hourglass
 files=*(self.pfiles)
 a_arr=[]
 b_arr=[]
 n_ab=n_elements(files)
 for k=0,n_ab-1 do begin
  o=obj_new('IDL_Savefile', /relaxed, files[k])
  if k eq 0 then begin
    o->restore,'INSTRUMENT'
    o->restore,'freqlist'
    o->restore, 'EBTELFILENAME'
    o->restore, 'MODELFILENAME'
    o->restore, 'MODIMAGEARR'
    get_map_coord,modimagearr->get(0,/map),xp,yp
  endif
  o->restore, 'a'
  o->restore, 'b'
  a_arr=[a_arr,a]
  b_arr=[b_arr,b]
  obj_destroy,o
 endfor
 a=a_arr[uniq(a_arr,sort(a_arr))]
 b=b_arr[uniq(b_arr,sort(b_arr))]
 index_a=[]
 index_b=[]
 
 freq=temporary(freqlist)
 N_a=n_elements(a)
 N_b=n_elements(b)
 N_freq=n_elements(freq)

 bestQ=dblarr(N_a, N_b, N_freq)
 chi=dblarr(N_a, N_b, N_freq)
 rho=dblarr(N_a, N_b, N_freq)
 eta=dblarr(N_a, N_b, N_freq)
 CC=dblarr(N_a, N_b, N_freq)
 shiftx=dblarr(N_a, N_b, N_freq)
 shifty=dblarr(N_a, N_b, N_freq)
 filenames=strarr(N_a,N_b)
 for k=0,n_ab-1 do begin
   o=obj_new('IDL_Savefile', /relaxed, files[k])
   index_a=WHERE(a EQ a_arr[k])
   index_b=WHERE(b EQ b_arr[k])
   filenames[index_a,index_b]=files[k]
   dummy=where(o->Names() eq 'CHIARR',count)
   if count then begin
     o->restore, 'chiArr'
     chi[index_a,index_b,*]=chiArr
   end
   dummy=where(o->Names() eq 'RHOARR',count)
   if count then begin
     o->restore, 'rhoArr'
     rho[index_a,index_b,*]=rhoArr
   end
   dummy=where(o->Names() eq 'ETAARR',count)
   if count then begin
     o->restore, 'etaArr'
     eta[index_a,index_b,*]=etaArr
   end
   o->restore, 'bestQarr'
   bestQ[index_a,index_b,*]=bestQArr
   o->restore, 'CCarr
   CC[index_a,index_b,*]=CCArr
   o->restore, 'obsImageArr'
     for index_freq=0, n_elements(freq)-1 do begin
       m=obj_isa(obsImageArr,'map')?obsimagearr->get(index_freq,/map):obsimagearr(index_freq)
       if tag_exist(m, 'shiftX') then shiftX[index_a,index_b,index_freq]=m.shiftX
       if tag_exist(m, 'shiftY') then shiftY[index_a, index_b, index_freq]=m.shiftY
     endfor
   obj_destroy,o
 endfor
 
 ptr_free,self.summary
 self.summary=ptr_new({instrument:instrument,ebtel:EBTELFILENAME,model:MODELFILENAME,$
                      a:a,b:b,freq:freq,x:reform(xp[*,0]),y:reform(yp[0,*]),files:filenames,$
                      data:{eta:eta,chi:chi,rho:rho,$
                      bestQ:bestQ,CC:CC,shiftx:shiftx,shifty:shifty}})  
 widget_control,self.wa,set_value=string((*self.summary).a,format="(g0)")  
 widget_control,self.wb,set_value=string((*self.summary).b,format="(g0)")  
 widget_control,self.wfreq,set_value=string((*self.summary).freq,format="(f5.2, ' GHz')")
 widget_control,self.wx,set_value=string((*self.summary).x,format="(g0)")+'"'
 widget_control,self.wy,set_value=string((*self.summary).y,format="(g0)")+'"'
 self->UpdateMaps                            
end

function gxchmpview::combobox_index,w
  widget_control,w,get_value=value
  selected_value=widget_info(w,/COMBOBOX_GETTEXT)
  return,(where(value eq selected_value))[0]
end

function gxchmpview::List2Map,maps
 names=tag_names(maps)
 modI=maps.MODIMAGEARR
 xp=reform((modI->get(/xp))[*,0])
 yp=reform((modI->get(/yp))[0,*])
 xc=modI->get(/xc)
 yc=modI->get(/yc)
 tags2remove=[]
 for k=0, n_elements(names)-1 do begin
  if size(maps.(k),/tname) eq 'OBJREF' and names[k] ne 'MODIMAGEARR' then begin
    if obj_isa(maps.(k),'list') then begin
      obj_map=obj_clone(modI)
      obj_list=maps.(k)
      if n_elements(obj_list(0)) ne 0 then begin
        for kk=0,obj_map->get(/count)-1 do begin
          amap=obj_map->get(kk,/map)
          amap.xc=mean(obj_list(kk).x)
          amap.data=array_replicate(obj_list(kk).flux,n_elements(xp))
          add_prop,amap,scan=1
          if tag_exist(obj_list(kk),'shiftx') then add_prop,amap,shiftx=obj_list(kk).shiftx
          obj_map->setmap,kk,amap
        endfor
        maps=rep_tag_value(maps,obj_map,names[k])
      endif else tags2remove=[tags2remove,names[k]]
     endif 
  endif
 endfor
 if n_elements(tags2remove) ne 0 then maps=rem_tag(maps,tags2remove)
 return,maps
end

pro gxchmpview::UpdateMaps
  if ~ptr_valid(self.summary) then return
  index_a=self.combobox_index(self.wa)
  index_b=self.combobox_index(self.wb)
  filename=(*self.summary).files[index_a,index_b]
  if filename eq '' then begin
    good=where((*self.summary).files ne '')
    filename=(*self.summary).files[good[0]]
    good=array_indices((*self.summary).files,good)
    index_a=good[0]
    index_a=good[1]
    widget_control,self.wa,set_combobox_select=index_a
    widget_control,self.wb,set_combobox_select=index_b
  endif
  maps=self->List2Map(gx_filevars2struct(filename))
  ptr_free,self.maps
  self.maps=ptr_new(maps)
end

pro gxchmpview::UpdateDisplays
 if ~ptr_valid(self.summary) then return 
 widget_control,/hourglass
 thisP=!p
 thisD=!d.name
 thisX=!x
 thisY=!y
 !p.font=-1
 !p.multi=0
 !x.margin=[6,6]
 !y.margin=[6,6]
 selected_metrics=widget_info(self.wmetrics_select,/COMBOBOX_GETTEXT)
 if strupcase(selected_metrics) eq 'ETA' or $
    strupcase(selected_metrics) eq 'CHI' or $
    strupcase(selected_metrics) eq 'RHO' then $
    selected_metrics='<'+selected_metrics+'!U2!N>'
 best=widget_info(widget_info(self.wBase,find_by_uname='metrics_best'),/button_set)
 selected_map=widget_info(self.wmap_select,/COMBOBOX_GETTEXT)
 index_freq=self.combobox_index(self.wfreq)
 a=(*self.summary).a
 b=(*self.summary).b
 freq=(*self.summary).freq
 widget_control,widget_info(self.wBase,find_by_uname='charsize'), get_value=charsize
 !p.charsize=charsize
 set_plot,self.WinOS?'win':'x'
 widget_control,widget_info(self.wBase,find_by_uname='plot_legends'), get_value=legends

  dummy=execute('data=(*self.summary).data.'+widget_info(self.wmetrics_select,/COMBOBOX_GETTEXT))
  data=data[*,*,index_freq]
  if keyword_set(best) then begin
   minimized_metrics=tag_exist((*self.maps),'metric')?(*self.maps).metric:'Eta'
   dummy=execute('adata=(*self.summary).data.'+minimized_metrics)
   adata=adata[*,*,index_freq]
   zero_index=where(adata le 0, zero_count)
   if zero_count gt 0 then adata[zero_index]=!values.f_nan
   best_metrics=min(adata,imin,/nan)
   index=array_indices(data,imin)
   index_a=index[0]
   index_b=index[1]
   widget_control,self.wa,set_combobox_select=index_a
   widget_control,self.wb,set_combobox_select=index_b
   self->UpdateMaps
  endif
  index_a=self.combobox_index(self.wa)
  index_b=self.combobox_index(self.wb)
  widget_control,widget_info(self.wBase,find_by_uname='metrics_lct'),get_uvalue=rgb
  tvlct,rgb
  widget_control,self.wmetrics,get_value=win
  wset,win
  log_metrics=(widget_info(widget_info(self.wBase,find_by_uname='log_metrics'),/COMBOBOX_GETTEXT) eq 'Log Scale')
  gx_data2grid, a, b, data, gridded_data=gdata, x_grid=xg, y_grid=yg, dx=dx, dy=dy
  map_metrics=make_map(gdata,xc=mean(xg),yc=mean(yg),dx=dx,dy=dy,time=(*self.maps).MODIMAGEARR->get(/time))
  map_metrics.id=selected_metrics
  odmin=min(map_metrics.data,max=odmax,/nan)
  if (odmin eq 0) && (odmax eq 0) then begin
    map_metrics.data=1e-15
    cbar=0
  endif else cbar=1
  bad=where(map_metrics.data le 0,nbad)
  plot_map,map_metrics,cbar=cbar,log=log_metrics, top=254,bottom=(nbad eq 0)?1:0,cb_title=selected_metrics,xtitle='a',ytitle='b'
  if legends[0] then begin
    oplot,a[index_a[[1,1]]],!y.crange,color=0,thick=3,linesty=2
    oplot,!x.crange,b[index_b[[1,1]]],color=0,thick=3,linesty=2
  end  
  metrics_legend=[string(selected_metrics+': ',data[index_a,index_b],format="(a0,g0)"),str2arr(string(a[index_a],b[index_b],freq[index_freq],format="('a=',g0,'; b=',g0,',freq=',f0.2, 'GHz')"))]
  if legends[1] then begin
    al_legend,metrics_legend,position=[a[index_a],b[index_b]],back='grey',right=a[index_a] gt mean(a),top=b[index_b] gt mean(b)
  endif
  widget_control,widget_info(self.wBase,find_by_uname='metrics_contours'),get_value=metrics_contours
  widget_control,widget_info(self.wBase,find_by_uname='cc_contours'),get_value=cc_contours
  widget_control,widget_info(self.wBase,find_by_uname='metrics_levels'),get_value=value
  metrics_levels=self->list2flt(value)
  widget_control,widget_info(self.wBase,find_by_uname='cc_levels'),get_value=value
  cc_levels=self->list2flt(value)
  widget_control,widget_info(self.wBase,find_by_uname='metrics_percent'),get_value=metrics_percent
  widget_control,widget_info(self.wBase,find_by_uname='cc_percent'),get_value=cc_percent
  metrics_color=self->combobox_index(widget_info(self.wBase,find_by_uname='metrics_colors'))
  cc_color=self->combobox_index(widget_info(self.wBase,find_by_uname='cc_colors'))
  widget_control,widget_info(self.wBase,find_by_uname='metrics_thick'),get_value=metrics_thick
  widget_control,widget_info(self.wBase,find_by_uname='cc_thick'),get_value=cc_thick

  linecolors
  if metrics_contours[0] eq 1 then begin
    plot_map,map_metrics,/over,levels=metrics_levels,percent=metrics_percent[0],$
             color=metrics_color,thick=metrics_thick,c_labels=replicate(1,n_elements(metrics_levels)),$
             c_ann=string(metrics_levels,format="(g0)")+(metrics_percent[0]?"%":""), $
             c_charsize=charsize,c_charthick=metrics_thick/2>1
  end
             
  if cc_contours[0] eq 1 then begin
    dummy=execute('cc=(*self.summary).data.CC')
    cc=cc[*,*,index_freq]
    gx_data2grid, a, b, cc, gridded_data=gcc, x_grid=xg, y_grid=yg, dx=dx, dy=dy
    map_cc=make_map(gcc,xc=mean(xg),yc=mean(yg),dx=dx,dy=dy)
    plot_map,map_cc,/over,levels=cc_levels,percent=cc_percent[0],$
             color=cc_color,thick=cc_thick,c_labels=replicate(1,n_elements(cc_levels)),$
             c_ann=string(cc_levels,format="('CC: ',g0)")+(cc_percent[0]?"%":""), $
             c_charsize=charsize,c_charthick=cc_thick/2>1
  endif
  widget_control,self.wmetrics,set_uvalue={x:!x,y:!y,z:!z,p:!p}
    loadct,0,/silent
    linecolors
    gx_rgb_white2black
    widget_control,self.wmetrics_spectrum,get_value=win
    widget_control,self.wmetrics_spectrum_extra,get_value=new_value,get_uvalue=old_value
    _extra=strcompress(new_value[0],/rem) eq ''?'':', '+new_value[0]
    wset,win
    code=execute('data=(*self.summary).data.'+widget_info(self.wmetrics_select,/COMBOBOX_GETTEXT))
    code=execute("plot,(*self.summary).freq,data[index_a,index_b,*],xtitle='Frequency (GHz)',ytitle=selected_metrics"+_extra)
    if code eq 0 then begin
      value=exist(old_value)?old_value:""
    endif else begin
      value=new_value
      oplot,(*self.summary).freq[[index_freq,index_freq]],gx_vline(),linesty=2
      if legends[1] then begin
          metrics_legend=string(selected_metrics+': ',data[index_a,index_b],format="(a0,g0)")
          al_legend,metrics_legend,position=[freq[index_freq],data[index_a,index_b,index_freq]],back='grey',right=freq[index_freq] gt mean(freq)
      endif
      overplot_base=widget_info(self.wBase,find_by_uname='metrics_spectrum_overplot')
      overplot_legend=[]
      tvlct,rgb,/get
      loadct,0,/silent
      linecolors
      overplot_all=widget_info(overplot_base,/all)
      for k=0,n_elements(overplot_all)-1 do begin
        wSelect=widget_info(overplot_all[k],find_by_uname='overplot_select')
        if self->combobox_index(wSelect) gt 0 then begin
          widget_control,wSelect,get_value=metrics
          color=self->combobox_index(widget_info(overplot_all[k],find_by_uname='overplot_colors'))
          widget_control,widget_info(overplot_all[k],find_by_uname='overplot_thick'),get_value=thick
          linestyle=self->combobox_index(widget_info(overplot_all[k],find_by_uname='overplot_styles'))
          psym=self->combobox_index(widget_info(overplot_all[k],find_by_uname='overplot_symbols'))
          if thick eq 0 then linestyle=!null else psym*=-1
          code=execute('odata=(*self.summary).data.'+widget_info(wSelect,/COMBOBOX_GETTEXT))
          oplot,(*self.summary).freq,odata[index_a,index_b,*],thick=thick,color=color,linestyle=linestyle,psym=psym
          overplot_legend=[overplot_legend,{items:metrics[self->combobox_index(wSelect)],line_thick:thick,textcolors:color,linestyle:exist(linestyle)?linestyle:0L,psym:psym}]
        end    
      endfor
      if n_elements(overplot_legend) ne 0 then begin
        al_legend,overplot_legend.items,textcolors=overplot_legend.textcolors,colors=overplot_legend.textcolors,line_thick=overplot_legend.line_thick,linestyle=overplot_legend.linestyle,psym=overplot_legend.psym,/top,/right,back='grey'
      endif
      tvlct,rgb
    endelse
    al_legend,string(a[index_a],b[index_b],format="('a=',g0,'; b=',g0)"),back='grey',/top,/left
    widget_control,self.wmetrics_spectrum_extra,set_value=value,set_uvalue=value

  index_a=self.combobox_index(self.wa)
  index_b=self.combobox_index(self.wb)
  index_x=self.combobox_index(self.wx)
  index_y=self.combobox_index(self.wy)
  filename=(*self.summary).files[index_a,index_b]
  if filename eq '' then begin
    answ=dialog_message('No solution has been yet computed for this grid point',/info)
    return
  endif
  obsI=(*self.Maps).OBSIMAGEARR->get(index_freq,/map)
  modI=(*self.Maps).MODIMAGEARR->get(index_freq,/map)
  cmodI=(*self.Maps).MODIMAGECONVARR->get(index_freq,/map)
  has_sigma=tag_exist((*self.Maps),'OBSIMAGESIGMAARR')
  obsSigma=has_sigma?(*self.Maps).OBSIMAGESIGMAARR->get(index_freq,/map):obsI
  THRESHOLD=tag_exist((*self.Maps),'THRESHOLD_IMG')?(*self.Maps).THRESHOLD_IMG:float(strmid(file_basename(filename),11,5))
  u=where((obsI.data lt max(obsI.data)*threshold) and (modI.data lt max(modI.data)*threshold), count)
  if legends[2] then $
    map_legend=string(a[index_a],b[index_b],freq[index_freq],format="('a=',g0,'; b=',g0,'; freq=',f0.2, 'GHz')") $
    else map_legend=[]
  case selected_map of
    'Data':map=obsI
    'Model':map=modI
    'Convolved Model':map=cmodI
    'Eta':begin
           map=cModI
           map.ID='Eta!U2!N'
           map.data=((cmodI.data-obsI.data)/mean(obsI.data))^2
           map.data[u]=0
           if tag_exist((*self.maps),'threshold_img') then $
             map_legend=[map_legend,string((*self.maps).threshold_img*100,format="('Iobs or Imod >',g0,'%')")]
           map_legend=[map_legend,string(map.id,(*self.summary).data.eta[index_a,index_b,index_freq],format="('<',a0,'>=',f0.2)")]
          end
     'Chi':begin
            map=cModI
            map.ID='Chi!U2!N'
            map.data=((cmodI.data-obsI.data)/obsSigma.data)^2
            map.data[u]=0
            if tag_exist((*self.maps),'threshold_img') then $
              map_legend=[map_legend,string((*self.maps).threshold_img*100,format="('Iobs or Imod >',g0,'%')")]
            map_legend=[map_legend,string((*self.summary).data.chi[index_a,index_b,index_freq],format="('Chi=',f0.2)")]
          end    
      'Rho':begin
            map=cModI
            map.ID='Rho!U2!N'
            map.data=(cmodI.data/obsI.data-1)^2
            map.data[u]=0
            if tag_exist((*self.maps),'threshold_img') then $
              map_legend=[map_legend,string((*self.maps).threshold_img*100,format="('Iobs or Imod >',g0,'%')")]
            map_legend=[map_legend,string((*self.summary).data.rho[index_a,index_b,index_freq],format="('Rho=',f0.2)")]
          end       
    else: begin
          end
  endcase
  widget_control,widget_info(self.wBase,find_by_uname='map_lct'),get_uvalue=rgb
  tvlct,rgb
  widget_control,self.wmap,get_value=win
  wset,win
  log_map=(widget_info(widget_info(self.wBase,find_by_uname='log_map'),/COMBOBOX_GETTEXT) eq 'Log Scale') 
  if tag_exist(map,'scan') then begin
    get_map_coord,map,xp,yp
    x=reform(xp[*,0])
    y=map.data[*,0]
    plot,x,y,ylog=log_map,ytitle=map.id,xtitle='X (arcsec)',xmargin=[12,6]
  endif else plot_map,map,/cbar,log=log_map,bottom=1, top=254
  if selected_map eq 'Data' and legends[3] then begin
    if tag_exist(map,'shiftx') and tag_exist(map,'shifty') then if ~(map.SHIFTX eq 0 and map.SHIFTY eq 0) then $
     map_legend=[map_legend,string(map.SHIFTX,'"',map.SHIFTY,'"',format="('xshift= ',f0.2,a0,'; yshift= ',f0.2,a0)")]
  endif
    
  
  if legends[5] then al_legend,string(map.data[index_x,index_y],format="(g0)"),back='grey',$
                position=[(*self.summary).x[index_x],(*self.summary).y[index_y]],$
                          right=(*self.summary).x[index_x] gt mean((*self.summary).x),top=(*self.summary).y[index_y] gt mean((*self.summary).y)
  if exist(map_legend) then al_legend,map_legend,back='grey'
  widget_control,widget_info(self.wBase,find_by_uname='model_contours'),get_value=model_contours
  widget_control,widget_info(self.wBase,find_by_uname='data_contours'),get_value=data_contours
  widget_control,widget_info(self.wBase,find_by_uname='data_levels'),get_value=value
  data_levels=self->list2flt(value)
  widget_control,widget_info(self.wBase,find_by_uname='model_levels'),get_value=value
  model_levels=self->list2flt(value)
  widget_control,widget_info(self.wBase,find_by_uname='data_percent'),get_value=data_percent
  widget_control,widget_info(self.wBase,find_by_uname='model_percent'),get_value=model_percent
  data_color=self->combobox_index(widget_info(self.wBase,find_by_uname='data_colors'))
  model_color=self->combobox_index(widget_info(self.wBase,find_by_uname='model_colors'))
  widget_control,widget_info(self.wBase,find_by_uname='data_thick'),get_value=data_thick
  widget_control,widget_info(self.wBase,find_by_uname='model_thick'),get_value=model_thick
  
  linecolors
  if data_contours[0] eq 1 then begin
    if tag_exist(obsI,'scan') then begin
      get_map_coord,obsI,xp,yp
      x=reform(xp[*,0])
      y=obsI.data[*,0]
      oplot,x,y,color=data_color,thick=data_thick
    endif else plot_map,obsI,/over,levels=data_levels,percent=data_percent,color=data_color,thick=data_thick
  endif
  if model_contours[0] eq 1 then begin
    if tag_exist(cmodI,'scan') then begin
      get_map_coord,cmodI,xp,yp
      x=reform(xp[*,0])
      y=cmodI.data[*,0]
      oplot,x,y,color=model_color,thick=model_thick
    endif else plot_map,cmodI,/over,levels=model_levels,percent=data_percent[0],color=model_color,thick=model_thick
  endif
  if legends[4] then begin
    cross_color=(strlowcase(selected_map) eq 'eta' or strlowcase(selected_map) eq 'chi' or strlowcase(selected_map) eq 'rho')?0:255
    oplot,(*self.summary).x[index_x[[1,1]]],!y.crange,color=cross_color,thick=3,linesty=2
    oplot,!x.crange,(*self.summary).y[index_y[[1,1]]],color=cross_color,thick=3,linesty=2
  endif 
  widget_control,self.wmap,set_uvalue={x:!x,y:!y,z:!z,p:!p} 

    loadct,0,/silent
    linecolors
    gx_rgb_white2black
    widget_control,self.wmap_spectrum,get_value=win
    widget_control,self.wmaps_spectrum_extra,get_value=new_value,get_uvalue=old_value
     _extra=strcompress(new_value[0],/rem) eq ''?'':', '+new_value[0]
    wset,win
    obs_spec=[]
    mod_spec=[]
    cmod_spec=[]
    sigma_spec=[]
 
    for k=0,n_elements((*self.summary).freq)-1 do begin
      obs_spec=[obs_spec,((*self.maps).OBSIMAGEARR->get(k,/data))[index_x,index_y]]
      mod_spec=[mod_spec,((*self.maps).MODIMAGEARR->get(k,/data))[index_x,index_y]]
      cmod_spec=[cmod_spec,((*self.maps).MODIMAGECONVARR->get(k,/data))[index_x,index_y]]
      sigma_spec=has_sigma?[sigma_spec,((*self.maps).OBSIMAGESIGMAARR->get(k,/data))[index_x,index_y]]:obs_spec 
    endfor
    case strlowcase(selected_map) of
      'data':data=obs_spec
      'model':data=mod_spec
      'convolved model':data=cmod_spec
      'eta':data=((cmod_spec-obs_spec)/mean(obs_spec))^2
      'chi':data=((cmod_spec-obs_spec)/mean(sigma_spec))^2
      'rho':data=(cmod_spec/obs_spec-1)^2
      else:
    endcase
    code=execute("plot,(*self.summary).freq,data,xtitle='Frequency (GHz)',ytitle=selected_map"+_extra)
    if code eq 0 then begin
      value=exist(old_value)?old_value:""
    endif else begin
      value=new_value
      oplot,(*self.summary).freq[[index_freq,index_freq]],gx_vline(),linesty=2
      if legends[5] then begin
        map_legend=string(selected_map+': ',data[index_freq],format="(a0,g0)")
        al_legend,map_legend,position=[freq[index_freq],data[index_freq]],back='grey',$
                  right=freq[index_freq] gt mean(freq)
      endif
      overplot_base=widget_info(self.wBase,find_by_uname='maps_spectrum_overplot')
      overplot_legend=[]
      tvlct,rgb,/get
      loadct,0,/silent
      linecolors
      overplot_all=widget_info(overplot_base,/all)
      for k=0,n_elements(overplot_all)-1 do begin
        wSelect=widget_info(overplot_all[k],find_by_uname='overplot_select')
        if self->combobox_index(wSelect) gt 0 then begin
          widget_control,wSelect,get_value=maps
          color=self->combobox_index(widget_info(overplot_all[k],find_by_uname='overplot_colors'))
          widget_control,widget_info(overplot_all[k],find_by_uname='overplot_thick'),get_value=thick
          linestyle=self->combobox_index(widget_info(overplot_all[k],find_by_uname='overplot_styles'))
          psym=self->combobox_index(widget_info(overplot_all[k],find_by_uname='overplot_symbols'))
          if thick eq 0 then linestyle=!null else psym*=-1
            case strlowcase(maps[self->combobox_index(wSelect)]) of
              'data':odata=obs_spec
              'model':odata=mod_spec
              'convolved model':odata=cmod_spec
              'eta':odata=((cmod_spec-obs_spec)/mean(obs_spec))^2
              'chi':odata=((cmod_spec-obs_spec)/mean(sigma_spec))^2
              'rho':odata=(cmod_spec/obs_spec-1)^2
              else:
          endcase
          oplot,(*self.summary).freq,odata,thick=thick,color=color,linestyle=linestyle,psym=psym
          overplot_legend=[overplot_legend,{items:+maps[self->combobox_index(wSelect)],line_thick:thick,textcolors:color,linestyle:exist(linestyle)?linestyle:0L,psym:psym}]
        end
      endfor
      if n_elements(overplot_legend) ne 0 then begin
        al_legend,overplot_legend.items,textcolors=overplot_legend.textcolors,colors=overplot_legend.textcolors,line_thick=overplot_legend.line_thick,linestyle=overplot_legend.linestyle,psym=overplot_legend.psym,/top,/right,back='grey'
      endif
      tvlct,rgb 
     al_legend,string((*self.summary).x[index_x],'"',(*self.summary).y[index_y],'"',format="('x=',g0,a0,'; y=',g0,a0)"),back='grey',/top,/left
    endelse
    widget_control,self.wmaps_spectrum_extra,set_value=value,set_uvalue=value
    
 o=obj_new('IDL_Savefile', /relaxed, filename)   
 o->Restore,'ALLMETRICS'
 o->Restore,'ALLQ'
 obj_destroy,o
 loadct,0,/silent
 linecolors
 gx_rgb_white2black
 widget_control,self.wmetrics_solution,get_value=win
 widget_control,self.wmetrics_solution_extra,get_value=new_value,get_uvalue=old_value
 _extra=strcompress(new_value[0],/rem) eq ''?'':', '+new_value[0]
 wset,win
 erase,0
 code=execute("plot,allq,allmetrics[*,index_freq],xtitle='Heating Rate Q',ytitle='METRICS SOLUTION'"+_extra)
 if code eq 0 then begin
   value=exist(old_value)?old_value:""
   widget_control,self.wmetrics_solution_extra,set_value=value,set_uvalue=value
   al_legend,['Plot Extra Keywords Error(s) Detected!','   ',_extra],textcolors=2
 endif else begin
   value=new_value
   best=min(allmetrics[*,index_freq],index_best)
   oplot,allq[[index_best,index_best]],gx_vline(),linesty=2
   alegend=string(a[index_a],b[index_b],freq[index_freq],format="('a=',g0,'; b=',g0,'; freq=',f0.2, 'GHz')")
   alegend=[alegend,[string(allq[index_best],format="('Best Q=',g0)"),$
                      string(tag_exist((*self.maps),'metric')?(*self.maps).metric:'Eta', best, format="('Best <',a0,'!U2!N>=',g0)")]]  
   flags=reform((*self.maps).modflagarr[index_freq,*])
   alegend=[alegend,'',$
   string(flags[0],format="('Coronal Voxels: ',i0)"),$
   string(flags[2],flags[2] eq 0?0:(100d*flags[2])/flags[0],format="('L-B Voxels: ',g0,' (',i0,'%)' )"),$
   string(flags[4],flags[4] eq 0?0:(100d*flags[4])/flags[0],format="('L Out Of Table Voxels: ',i0,' (',i0,'%)' )"),$
   string(flags[5],flags[5] eq 0?0:(100d*flags[5])/flags[0],format="('Q Out Of Table Voxels: ',i0,' (',i0,'%)' )")]
   al_legend,alegend,back='grey',/top,/left 
 end

 !p=thisp
 !x=thisX
 !y=thisY
 set_plot,thisD
end 

pro gxchmpview::OnPallete,metrics=metrics,map=map
  if keyword_set(metrics) then map=0 else map=1
  tvlct,rgb_curr,/get
  xloadct,/silent,/block
  gx_rgb_white2black
  tvlct,rgb,/get
  tvlct,rgb_curr
  widget_control,widget_info(self.wBase,find_by_uname=keyword_set(metrics)?'metrics_lct':'map_lct'),set_uvalue=rgb
end

function gxchmpview::list2flt,strlist
  catch, error_status
  IF error_status NE 0 THEN BEGIN
    CATCH, /CANCEL
    RETURN, !NULL  ; Return a null object if there's an error
  ENDIF
  s='['+arr2str(str_replace(str_replace(str_replace('['+arr2str(strlist)+']','[',''),']','')))+']'
  code=execute('arr='+s)
  if code eq 0 then begin
    !ERR=0
    return,!null
  endif
  return,arr
end

function gxchmpview::HandleEvent, event
compile_opt hidden
catch, error_status
if error_status ne 0 then begin
  catch, /cancel
  void = dialog_message( $
    dialog_parent=self.wIDBase, $
    title='Error', $
    /error, $
    !error_state.msg + ' ' + !error_state.sys_msg $
    )
  return, self->Rewrite(event)
endif
IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_DRAW' THEN BEGIN
  if ptr_valid(self.summary) then begin
    if  event.type eq 1 then begin
      widget_control,event.id, get_value=win,get_uvalue=g
      wset,win
      !x=g.x
      !y=g.y
      !z=g.z
      !p=g.p
      case event.id of
        self.wmetrics: begin
                        cursor,a,b,/nowait,/data
                        minda=min((*self.summary).a-a,index_a,/abs)
                        mindb=min((*self.summary).b-b,index_b,/abs)
                        filename=(*self.summary).files[index_a,index_b]
                        if filename eq '' then begin
                          answ=dialog_message('No solution has been yet computed for this grid point',/info)
                          goto,skip_plot
                        endif
                        widget_control,self.wa,set_combobox_select=index_a
                        widget_control,self.wb,set_combobox_select=index_b
                        self->UpdateMaps
                       end
        self.wmap: begin
                        cursor,x,y,/nowait,/data
                        mindx=min((*self.summary).x-x,index_x,/abs)
                        mindy=min((*self.summary).y-y,index_y,/abs)
                        widget_control,self.wx,set_combobox_select=index_x
                        widget_control,self.wy,set_combobox_select=index_y
                  end
        else:
      endcase
    endif
  end
ENDIF
case event.id of
  self.wa:Self->UpdateMaps
  self.wb:Self->UpdateMaps
  else:
endcase
case widget_info(event.id,/uname) of
  'metrics_lct':begin 
              self->OnPallete,/metrics
             end 
  'map_lct':begin
             self->OnPallete,/map
            end   
  
  'maps_levels':begin
    widget_control,event.id,get_value=value,get_uvalue=uvalue
    levels=self->list2flt(value)
    if n_elements(levels) eq 0 then begin
      answ=dialog_message('Invalid levels syntax!')
      widget_control,event.id,set_value=uvalue
    endif else begin
      widget_control,event.id,set_uvalue=uvalue
    endelse
  end 
  'cc_levels':begin
    widget_control,event.id,get_value=value,get_uvalue=uvalue
    levels=self->list2flt(value)
    if n_elements(levels) eq 0 then begin
      answ=dialog_message('Invalid levels syntax!')
      widget_control,event.id,set_value=uvalue
    endif else begin
      widget_control,event.id,set_uvalue=uvalue
    endelse
  end  
  'data_levels':begin
    widget_control,event.id,get_value=value,get_uvalue=uvalue
    levels=self->list2flt(value)
    if n_elements(levels) eq 0 then begin
      answ=dialog_message('Invalid levels syntax!')
      widget_control,event.id,set_value=uvalue
    endif else begin
      widget_control,event.id,set_uvalue=uvalue
    endelse
  end
  'model_levels':begin
    widget_control,event.id,get_value=value,get_uvalue=uvalue
    levels=self->list2flt(value)
    if n_elements(levels) eq 0 then begin
      answ=dialog_message('Invalid levels syntax!')
      widget_control,event.id,set_value=uvalue
    endif else begin
      widget_control,event.id,set_uvalue=uvalue
    endelse
  end                      
  'resultsdir':begin
         widget_control,event.id,get_value=resultsdir
         goto,resultsdir_select
       end
  'resultsdir_update':begin
         widget_control,widget_info(event.top,find_by_uname='resultsdir'),get_value=resultsdir
         goto,resultsdir_select
       end     
  'resultsdir_select':begin
         resultsdir=dialog_pickfile(TITLE='Please select a valid CHMP results repository',path=curdir(),/dir,/must)
         redo=1
         resultsdir_select:
         files=self.valid_repository(resultsdir)
         if n_elements(files) gt 0 then begin
          if ptr_valid(self.pfiles) then oldfiles=*self.pfiles else oldfiles=['']
            if ~array_equal(files,oldfiles)or keyword_set(redo) then begin
              self.resultsdir=resultsdir
              ptr_free,self.pfiles
              self.pfiles=ptr_new(files)
              widget_control,widget_info(self.wbase,find_by_uname='resultsdir_update'),sensitive=1
              self->UpdateSummary
              widget_control,event.top,tlb_set_title='CHMP Rresults Viewer [MODEL: '+file_basename((*self.summary).MODEL)+'; EBTEL Table: '+file_basename((*self.summary).ebtel)+']'
              widget_control,widget_info(self.wBase,find_by_uname='metrics_best'),set_value=1,/set_button
            endif else answ=dialog_message('No new files have been added to the CHMP results directory, no updates necessary!',/info)
          endif
         widget_control,widget_info(self.wBase,find_by_uname='resultsdir'),set_value=self.resultsdir
       end  
 
     'plot2png':begin
                 filename=dialog_pickfile(filter='*.png',default='*.png',title='Save plot to PNG')
                 if filename ne '' then begin
                   widget_control,event.id,get_uvalue=wdraw
                   widget_control,wdraw,get_value=win
                   wset,win
                   crop=wdraw eq self.wmap_spectrum or wdraw eq self.wmetrics_spectrum
                   write_png,filename,crop?gx_remove_border(tvrd(/true),/exact):tvrd(/true)
                 endif
                end 
      'plot2movie':begin
                    if ptr_valid(self.summary) then begin
                      if float(!version.release) ge 8.1 then begin
                        widget_control,event.id,get_uvalue=wdraw
                        widget_control,wdraw,get_value=win
                        wset,win
                        crop=wdraw eq self.wmap_spectrum or wdraw eq self.wmetrics_spectrum
                        movie_frame=crop?gx_remove_border(tvrd(/true),/exact):tvrd(/true)
                        dimensions=(size(movie_frame,/dim))[1:*]
                        oVid = gxVideo(dimensions,stream=stream)
                        for k=0,n_elements((*self.summary).freq)-1 do begin
                          widget_control,self.wfreq,set_combobox_select=k
                          self->UpdateDisplays
                          wset,win
                          crop=wdraw eq self.wmap_spectrum or wdraw eq self.wmetrics_spectrum
                          movie_frame=crop?gx_remove_border(tvrd(/true),/exact):tvrd(/true)
                          result=oVid->Put(stream,movie_frame)
                        end
                        obj_destroy, oVid
                      endif else answ=dialog_message('Sorry, this feature is supported only for IDL versions 8.1 or higher!',/info)
                    endif
                    widget_control,event.id,set_button=0
                   end  
         'freq_cycle':begin
                     if ptr_valid(self.summary) then begin
                         index_freq=self.combobox_index(self.wfreq)
                         freq_indexes=shift(lindgen(n_elements((*self.summary).freq)),-index_freq)
                         for k=0,n_elements(freq_indexes) do begin
                           widget_control,self.wfreq,set_combobox_select=freq_indexes[0]
                           self->UpdateDisplays
                           freq_indexes=shift(freq_indexes,-1)
                         end
                       endif
                     widget_control,event.id,set_button=0
                   end  
      'metrics_best':begin 
                      help,event.select
                      widget_control,event.id,set_value=event.select    
                     end                         
      'help_about':answ=dialog_message('This application may be used to visualize the results created by the code located in the GX Simulator /external/chmp submodule contributed by Alexey Kuznetsov',/info)                    
     else:
 endcase
 self->UpdateDisplays
 skip_plot:
 end      

function cw_gxchmpview,Base,_extra=_extra
  obj=obj_new('gxchmpview',Base,_extra=_extra)
  obj->GetProperty,widget_id=widget_id
  return,widget_id
end