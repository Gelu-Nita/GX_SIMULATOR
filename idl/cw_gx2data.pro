function gx2data::INIT,wBase,uname=uname,frame=frame,plotman_obj=plotman_obj,_extra=_extra
 compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
      catch, /cancel
      MESSAGE, /INFO, !ERROR_STATE.MSG
      return, 0
  end
  if obj_valid(plotman_obj) then self.plotman=plotman_obj
  void=self->gxWidget::Init(wBase,self,frame=frame,KILL_NOTIFY='objGx2DataKill',_extra=_extra)
  return,1
end

pro gx2data__define
 struct_hide,{gx2data, inherits gxwidget, plotman:obj_new(),$
  beam:obj_new(),gxmap:obj_new(),refmap:obj_new(),sdevmap:obj_new(),metrics:obj_new()}
end

pro objGx2DataKill,wBase
  widget_control,wBase,get_uvalue=obj
  obj_destroy,obj
end

pro gx2data::CreatePanel,xsize=xsize,ysize=ysize
  device, get_screen_size=scr
  xlabelsize=27
  if not exist(xsize) then xsize = fix (scr[0] * .4)
  if not exist(ysize) then ysize = xsize * 1.1
  if ~obj_valid(self.plotman) then begin
    wPanelBase=widget_base(self.wBase,/row)
    wLeftBase=widget_base(wPanelBase,/column)
    wRightBase=widget_base(wPanelBase,/column)
    tbar= widget_base(wLeftBase, /row,/toolbar)
    w_file=WIDGET_BUTTON(tbar, VALUE='File', /MENU)
    wNewFits=widget_button(w_file,value='Import FITS map from file',uname='GXMAPCONTAINER:IMPORTFITS',event_func='IDLexWidget__HandleEvent')
    wNewMap=widget_button(w_file,value='Import MAP structure from file',uname='GXMAPCONTAINER:IMPORTMAP',event_func='IDLexWidget__HandleEvent')
    
    wPlotmanBase=widget_base(wLeftBase,/column,uvalue={mbar:widget_base(tbar,/row),w_file:w_file})
    self.plotman = obj_new ('plotman', mainbase=wPlotmanBase, /multi_panel, $
      wxsize=xsize, wysize=ysize, $
      colortable=colortable, $
      widgets=widgets, error=error)
    geom = widget_info (widgets.w_maindrawbase, /geom)

    w_splashdraw = widget_draw ( widgets.w_maindrawbase, xsize=geom.scr_xsize, ysize=geom.scr_ysize)
    state = { $
      w_file: w_file, $
      w_splashdraw: w_splashdraw, $
      widgets: widgets, $
      plotman_obj: self.plotman}
      
    parent=widget_info(self.wBase,/parent)
    if parent ne 0 then begin
      repeat begin
        tlb=parent
        parent=widget_info(tlb,/parent)
      endrep until parent eq 0l
    end

    widget_control,tlb,set_uvalue=state  
    wMapContainer=cw_objMapContainer(tbar,name='MAP_CONTAINER',plotman_obj=self.plotman)
    widget_control,wMapContainer,get_uvalue=objMapContainer
    widget_control,wNewFits,set_uvalue=objMapContainer
    widget_control,wNewMap,set_uvalue=objMapContainer
    wControlTab=WIDGET_TAB(wRightBase,/Align_top,UNAME='ControlTab',UVALUE='ControlTab',LOCATION=2)
    wGX2DataControl=Widget_Base(wControlTab,Title='Data To Model Image Comparison',UNAME='gx2data')
  endif else begin
   wGX2DataControl=self.wBase
  endelse
   ;font=!defaults.font
   ;wControlPanel=widget_base(wGX2DataControl,/column)
   wControlPanel=widget_base(wGX2DataControl,/column,xsize=scr[0],ysize=scr[1],$
     x_scroll_size=xsize,y_scroll_size=ysize*1.1,/scroll)
   wControlBase=widget_base(wControlPanel,/column,xsize=xsize)
   toolbar= widget_base(wControlBase, /row,/toolbar)
   geom = widget_info (wControlPanel, /geom)
   wBeam=Widget_Button(toolbar,/menu,value='PSF')
   wNORH=WIDGET_BUTTON(wBeam, VALUE='Import reference map and PSF parameters from NORH Fits', uname='BEAM:NORH',font=font)
   wSSRT=WIDGET_BUTTON(wBeam, VALUE='Generate SSRT PSF Parameters', uname='BEAM:SSRT',font=font)
   wConvolve=WIDGET_BUTTON(wBeam, VALUE='Convolve Selected Map', uname='CONVOLVE',font=font)
   wMetrics=Widget_Button(toolbar,/menu,value='Metrics')
   wCompute=WIDGET_BUTTON(wMetrics, VALUE='Compute Metrics', uname='COMPUTE',font=font)
   
   wlabel=widget_label(wControlBase,value=' ',font=font,/align_left)
   wlabel=widget_label(wControlBase,value='INPUT DATA SELECTION:',font=font,/align_left)
   wSelectBase=widget_base(wControlBase,/row,/frame)
   wButtonBase=widget_base(wSelectBase,/column,/frame)
   button=widget_button(wButtonBase,VALUE='SELECT MODEL MAP',uname='MODEL:SELECT',font=font,/align_left,/frame)
   geom_button = widget_info(button, /geom)
   scr_xsize=geom_button.scr_xsize
   scr_ysize=geom_button.scr_ysize
   button=widget_button(wButtonBase,VALUE='SELECT REF MAP',uname='REF:SELECT',font=font,/align_left,xsize=scr_xsize,/frame)
   button=widget_button(wButtonBase,VALUE='SELECT SDEV MAP',uname='SDEV:SELECT',font=font,/align_left,xsize=scr_xsize,/frame)
   button=widget_button(wButtonBase,VALUE='SELECT BEAM MAP',uname='BEAM:SELECT',font=font,/align_left,xsize=scr_xsize,/frame)
   geom_button = widget_info (wButtonBase, /geom)
   scr_xsize=geom.scr_xsize-geom_button.scr_xsize

   wBase=widget_base(wSelectBase,/column,/frame)
   wlabel=widget_label(wbase,value='no model map selected',font=font,scr_xsize=scr_xsize,scr_ysize=scr_ysize,uname='MODEL:ID',/dynamic,/frame)
   wlabel=widget_label(wbase,value='no reference map selected',font=font,scr_xsize=scr_xsize,scr_ysize=scr_ysize,uname='REF:ID',/dynamic,/frame)
   wlabel=widget_label(wbase,value='no sdev map selected',font=font,scr_xsize=scr_xsize,scr_ysize=scr_ysize,uname='SDEV:ID',/dynamic,/frame)
   wlabel=widget_label(wbase,value='no beam map selected',font=font,scr_xsize=scr_xsize,scr_ysize=scr_ysize,uname='BEAM:ID',/dynamic,/frame)
   wBeamBase=widget_base(wControlBase,/row,/frame)
   wBeamSubBase=widget_base(wBeamBase,/column,/frame)
   button=widget_button(wBeamSubBase,VALUE='GENERATE BEAM',uname='BEAM:GENERATE',font=font,/align_left,xsize=geom_button.scr_xsize,/frame)
   map=make_map(findgen(100,100))
   label=widget_text(wBeamSubBase,uname='BEAM:TIME',value=map.time,font=font,/editable)
   wBeamParmsBase=widget_base(wBeamBase,/column)
   wBeamCorr=cw_objArray(wBeamParmsBase,value=[1,1,0,2,2],units=['"','"',STRING(176b),'"','"'],names=['a','b','phi','dx','dy'],font=font,/frame,label='Synthetic Beam',inc=0.1,xtextsize=4,xlabelsize=4,uname='BEAM:PARMS',/static)
   wBeamCorr=cw_objArray(wBeamParmsBase,value=[100,1],units=['pix',''],names=['width','correction'],font=font,/frame,label='Synthetic Beam Corr',inc=0.1,xtextsize=4,uname='BEAM:CORR',/static)
   wMaskBase=widget_base(wControlBase,/frame,/row)
   wMask=cw_objfield(wMaskBase,value=12.00,unit='%',font=font,/frame,label='ROI MASK THRESHOLD:',inc=1,xtextsize=6,xlabelsize=xlabelsize,uname='MASK:LEVEL')
   wMaskOptions=cw_bgroup(wMaskBase,['No Mask','Data','Model','OR', 'AND'],/exclusive,font=font,uname='MASK:OPTIONS',/frame,/row)
   widget_control,wMaskOptions,set_value=3
   wbase=widget_base(wControlBase,/frame,/row)
   label=widget_label(wBase,value='Model to Data Shift ',font=font,xsize=geom_button.scr_xsize)  
   wShift=cw_objArray(wBase,value=[0,0],units=['"','"'],names=['X:','Y:'],font=font,/frame,label='Model to Data Shift',inc=0.1,xtextsize=8,xlabelsize=4,uname='MAP:SHIFT',/static)
   widget_control,wshift,sensitive=0
   wCheck=cw_bgroup(wBase,['auto','manual'],/exclusive,font=font,uname='MAP:USESHIFT',/frame,/row)
   widget_control,wCHECK,set_value=0
   wlabel=widget_label(wControlBase,value=' ',font=font,/align_left)
   wlabel=widget_label(wControlBase,value='COMPUTED METRICS:',font=font,/align_left)
   wMetrics=cw_objfield(wControlBase,value=0.0,font=font,/frame,label='NPIX [ROI]:',xtextsize=16,xlabelsize=xlabelsize,uname='ROI:NPIX',/indicator)
   wMetrics=cw_objfield(wControlBase,value=0.0,font=font,/frame,label='Data [ROI]:',xtextsize=16,xlabelsize=xlabelsize,uname='ROI:DATATOTAL',/indicator)
   wMetrics=cw_objfield(wControlBase,value=0.0,font=font,/frame,label='Model[ROI]:',xtextsize=16,xlabelsize=xlabelsize,uname='ROI:MODELTOTAL',/indicator)
   wMetrics=cw_objfield(wControlBase,value=0.0,font=font,/frame,label='R:',xtextsize=16,xlabelsize=xlabelsize,uname='MAP:R',/indicator)
   wMetrics=cw_objfield(wControlBase,value=0.0,font=font,/frame,label='RES:',xtextsize=16,xlabelsize=xlabelsize,uname='ROI:RES',/indicator)
   wMetrics=cw_objfield(wControlBase,value=0.0,font=font,/frame,label='RES_NORM:',xtextsize=16,xlabelsize=xlabelsize,uname='ROI:RES_NORM',/indicator)
   wMetrics=cw_objfield(wControlBase,value=0.0,font=font,/frame,label='RES2:',xtextsize=16,xlabelsize=xlabelsize,uname='ROI:RES2',/indicator)
   wMetrics=cw_objfield(wControlBase,value=0.0,font=font,/frame,label='RES2_NORM:',xtextsize=16,xlabelsize=xlabelsize,uname='ROI:RES2_NORM',/indicator)
   wMetrics=cw_objfield(wControlBase,value=0.0,font=font,/frame,label='CHI:',xtextsize=16,xlabelsize=xlabelsize,uname='ROI:CHI',/indicator)
   wMetrics=cw_objfield(wControlBase,value=0.0,font=font,/frame,label='CHI2:',xtextsize=16,xlabelsize=xlabelsize,uname='ROI:CHI2',/indicator)
   info=[ '',$
     'METRICS DEFINITIONS:',$
     ;'',$
     'R=Pearson correlation coefficient',$
     ;'',$
     'res_img= data_model - data_obs',$
     ;'',$
     'res= total(res_img[mask_pix])/n_mask_pix',$
     ;'',$
     'res_img_norm=res_img/data_obs',$
     ;'',$
     'res_norm=total(res_img_norm[mask_pix])/n_mask_pix',$
     ;'',$
     'res2_img=res_img^2',$
     ;'',$
     'res2=total(res2_img[mask_pix])-res^2',$
     ;'',$
     'res2_img_norm=res_img_norm^2',$
     ;'',$
     'res2_norm=total(res2_img_norm[mask_pix])-res_norm^2',$
     ;'',$
     'chi_img=res_img/data_sdev',$
     ;'',$
     'chi=total(chi_img[mask_pix])/n_mask_pix',$
     ;'',$
     'chi2_img=chi_img^2',$
     'chi2=total(chi2_img[mask_pix])/(n_mask_pix-n_free)-chi^2']
     ;'',$
   wMetrics=widget_text(wControlBase,scr_xsize=scr_xsize,value=info,/scroll,$
   ysize=16)
end

function gx2data::select_map
  self.plotman->select
  panel_struct=self.plotman->get(/current_panel_struct)
  if size(panel_struct,/tname) eq 'STRUCT' then return, (*panel_struct.saved_data.data) else return, obj_new()
end
function gx2data::HandleEvent, event
  compile_opt hidden
  catch, error_status
  if error_status ne 0 then begin
    catch, /cancel
    void = dialog_message( $
      dialog_parent=event.top, $
      title='Error', $
      /error, $
      !error_state.msg + ' ' + !error_state.sys_msg $
      )
    return, self->Rewrite(event)
  endif
  case strupcase(widget_info(event.id,/uname)) of
    'MODEL:SELECT': begin
                      obj_destroy,self.gxmap
                      self.gxmap=obj_clone(self.select_map())
                      if valid_map(self.gxmap) then begin
                        widget_control,widget_info(event.handler,find_by_uname='MODEL:ID'),set_value=self.gxmap->get(/id)
                        widget_control,widget_info(event.handler,find_by_uname='BEAM:TIME'),set_value=self.gxmap->get(/time)
                      endif
                    end
    'REF:SELECT': begin
                      obj_destroy,self.refmap
                      self.refmap=obj_clone(self.select_map())
                      if valid_map(self.refmap) then widget_control,widget_info(event.handler,find_by_uname='REF:ID'),set_value=self.refmap->get(/id)
                    end  
    'SDEV:SELECT': begin
                      obj_destroy,self.sdevmap
                      self.sdevmap=obj_clone(self.select_map())
                      if valid_map(self.sdevmap) then widget_control,widget_info(event.handler,find_by_uname='SDEV:ID'),set_value=self.sdevmap->get(/id)
                    end
    'BEAM:SELECT': begin
                      obj_destroy,self.beam
                      self.beam=obj_clone(self.select_map())
                      if valid_map(self.beam) then widget_control,widget_info(event.handler,find_by_uname='BEAM:ID'),set_value=self.beam->get(/id)
                    end   
    'BEAM:NORH': begin
                  file=dialog_pickfile(Title='Select a norh fits file to import observing beam parameters')
                  if file_exist(file) then begin
                    norh_rd_img,file, index, data
                    norh_index2map,index,data,map
                    omap=obj_new('map')
                    omap->setmap,0,map
                    beam=norh_beam(index,marx=marx)
                    spp=index.norh.sec_per_pix
                    x=(dindgen(marx)-long(marx)/2)*spp
                    y=(dindgen(marx)-long(marx)/2)*spp
                    FitBeam, x, y, beam, a, b, theta
                    widget_control,widget_info(event.handler,find_by_uname='BEAM:PARMS'),set_value=[a,b,-theta/!dtor,spp,spp]
                    widget_control,widget_info(event.handler,find_by_uname='BEAM:CORR'),set_value=[marx,1]
                    goto,generate_beam
                  endif
                 end  
    'BEAM:SSRT':begin
                  widget_control,widget_info(event.handler,find_by_uname='BEAM:TIME'),get_value=time
                  GetSSRTangles, time, dEW, dNS, pEW, pNS
                  marx=100
                  dx=2.
                  dy=2.
                  MakeSSRTbeam, dEW, dNS, pEW, pNS, marx, marx, dx, dy, x, y, beam
                  FitBeam, x, y, beam, a, b, theta
                  widget_control,widget_info(event.handler,find_by_uname='BEAM:PARMS'),set_value=[a,b,-theta/!dtor,dx,dx]
                  widget_control,widget_info(event.handler,find_by_uname='BEAM:CORR'),set_value=[marx,1]
                  goto,generate_beam
                end                        
    'BEAM:GENERATE': begin
                      generate_beam:
                      widget_control,widget_info(event.handler,find_by_uname='BEAM:PARMS'),get_value=parms
                      widget_control,widget_info(event.handler,find_by_uname='BEAM:CORR'),get_value=corr
                      a=parms[0]
                      b=parms[1]
                      phi=parms[2]
                      dx=parms[3]
                      dy=parms[4]
                      width=corr[0]
                      beam_corr=corr[1] 
                      beam=gx_psf(beam_corr*[a,b]/[dx,dy],phi,width)
                      if obj_valid(omap) then begin
                       time=omap->get(/time) 
                       id='Instrument Beam'
                      endif else begin
                        omap=obj_new('map')
                        widget_control,widget_info(event.handler,find_by_uname='BEAM:TIME'),get_value=time
                      end  
                      widget_control,widget_info(event.handler,find_by_uname='BEAM:TIME'),set_value=atime(time)
                      k=omap->get(/count)
                      omap->setmap,k,make_map(beam,dx=dx,dy=dy,time=time)
                      omap->set,k,id=isa(id)?id:'Synthetic Beam'
                      if k gt 0 then begin
                        widget_control,widget_info(widget_info(event.top,find_by_uname='GXMAPCONTAINER:MENU'),/parent),get_uvalue=oMapContainer
                        oMapContainer->add,omap,'NORH '+omap->get(/time)
                      endif else omap->plotman,k,plotman_obj=self.plotman,nodup=0
                    end  
    'CONVOLVE': begin
                      if valid_map(self.beam) then begin
                        map=self.select_map()
                        if valid_map(map) then begin
                          cmap=map->get(/map)
                          beam=self.beam->get(/data)
                          ;cmap.data=convol(cmap.data, beam, /edge_zero)
                          cmap.data=convol_fft(cmap.data, beam);to be consistent with gx_chmpp
                          cmap.id='Convolved '+cmap.id
                          omap=obj_new('map')
                          omap->setmap,0,cmap
                          omap->plotman,0,plotman_obj=self.plotman,nodup=0
                          obj_destroy,omap
                        endif
                      end
                    end   
     'MAP:USESHIFT':begin 
                     if event.select eq 1 then begin
                      widget_control,widget_info(event.handler,find_by_uname='MAP:SHIFT'),sensitive=event.value
                     endif
                    end
     'COMPUTE': begin
                      widget_control,widget_info(event.handler,find_by_uname='MAP:USESHIFT'),get_value=value
                      if value eq 1 then begin
                       widget_control,widget_info(event.handler,find_by_uname='MAP:SHIFT'),get_value=shift 
                      endif
                      if valid_map(self.refmap) and valid_map(self.gxmap) then begin
                       if valid_map(self.sdevmap) then sdev=self.sdevmap->get(/map)
                       widget_control,widget_info(event.handler,find_by_uname='MASK:LEVEL'),get_value=mask    
                       widget_control,widget_info(event.handler,find_by_uname='MASK:OPTIONS'),get_value=apply2                   
                       self.metrics=gx_metrics_map(self.gxmap->get(/map),self.refmap->get(/map),sdev,mask=mask,apply2=apply2,shift=shift)
                       widget_control,widget_info(widget_info(event.top,find_by_uname='GXMAPCONTAINER:MENU'),/parent),get_uvalue=oMapContainer
                       mod_total=total(self.metrics->get(0,/data)*self.metrics->get(3,/data)*self.metrics->get(0,/dx)*self.metrics->get(0,/dy))
                       obs_total=total(self.metrics->get(1,/data)*self.metrics->get(3,/data)*self.metrics->get(1,/dx)*self.metrics->get(1,/dy))
                       widget_control,widget_info(event.handler,find_by_uname='ROI:DATATOTAL'),set_value=obs_total
                       widget_control,widget_info(event.handler,find_by_uname='ROI:MODELTOTAL'),set_value=mod_total
                       for k=0,self.metrics->get(/count)-1 do begin
                        wid=widget_info(event.handler,find_by_uname=self.metrics->get(k,/uname))
                        if widget_valid(wid) then begin
                          widget_control,wid,set_value=self.metrics->get(k,/roi_metrics)
                          case self.metrics->get(k,/uname) of
                            'MAP:R': begin
                                      dx=self.metrics->get(k,/xc)-self.metrics->get(k,/orig_xc)
                                      dy=self.metrics->get(k,/yc)-self.metrics->get(k,/orig_yc)
                                      widget_control,widget_info(event.handler,find_by_uname='MAP:SHIFT'),set_value=[dx,dy]
                                     end
                            else:
                          endcase  
                        endif
                       endfor
                       oMapContainer->add,self.metrics,'GX METRICS '+atime(systime(/s))
                      end
                    end                                                        
    else:
  endcase
return,self->Rewrite(event)
end  


function cw_gx2data,Base,_extra=_extra
 obj=obj_new('gx2data',Base,_extra=_extra)
 obj->GetProperty,widget_id=widget_id
 return,widget_id
end
