function gxh2v::INIT,wBase,volume,_extra=_extra
  compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return, 0
  end
  default,volume,obj_new()
  if ~obj_isa(volume,'gxvolume') then return,0
  flags=volume->GetFlags()
  if ~flags.hasbl then return,0
  self.volume=volume
  void=self->gxWidget::Init(wBase,self,KILL_NOTIFY='h2vKill',_extra=_extra)
  return,1
end

pro gxh2v__define
  struct_hide,{gxh2v, inherits gxwidget, volume:obj_new()}
end

pro h2vKill,wBase
  widget_control,wBase,get_uvalue=obj
  obj_destroy,obj
end

pro gxh2v::CreatePanel,xsize=xsize,ysize=ysize
  device, get_screen_size=scr
  font=!defaults.font
  if not exist(xsize) then xsize = fix (scr[0] * .25)
  if not exist(ysize) then ysize = xsize
  h2v=self.volume->GetHeat2Volume()
  wTab0=WIDGET_TAB(self.wBase,font=font, /Align_Left,LOCATION=0,uname='GXVOLUME:TOPTAB') 
    
  self.volume->GetVertexAttributeData,'q0_coeff',q
  if n_elements(q) eq 0 then begin
    q=[0.415e-3,1e2,1e9,0,0]
    self.volume->SetVertexAttributeData,'q0_coeff',q
  end

  self.volume->GetVertexAttributeData,'q0_formula',q0_formula
  q0_formula=self.volume->SetQ0(q0_formula,q_formula=q_formula)
  
  wBase=widget_base(wTab0,title='EBTEL Heating Parameters')
  wParmBase=widget_base(wBase,/column,uname='GXVOLUME:q_formula_base')
  wEBTELToolbarBase = widget_base(wParmBase, /row, /frame,/TOOLBAR,map=1)
  wqBase=widget_base(wParmBase,/row,/frame)
  wq=cw_objArray( wqBase,uname='GXVOLUME:q',xtextsize=5,format='(g0)',units='',value=[q],label='q',/static)
  wqreset=widget_button(font=font, wqBase,value='Reset to default',uname='GXVOLUME:q_reset')

  wq0FormulaBase=widget_base(wParmBase,/row,/frame)
  label=widget_label(font=font,wq0FormulaBase,value=' q0 =  ')
  g=widget_info(wq,/geo)
  gl=widget_info(label,/geo)
  wq0f=widget_text(font=font,wq0FormulaBase,value=q0_formula,scr_xsize=g.scr_xsize-gl.scr_xsize,/edit,uname='GXVOLUME:q0_formula')
  wq0freset=widget_button(font=font,wq0FormulaBase,value='Reset to default',uname='GXVOLUME:q0_formula_reset')

  wqFormulaBase=widget_base(wParmBase,/row,/frame)
  label=widget_label(font=font,wqFormulaBase,value=' Q  =  ',scr_xsize=gl.scr_xsize)

  wqf=widget_text(font=font,wqFormulaBase,value=q_formula,scr_xsize=g.scr_xsize-gl.scr_xsize,/edit,uname='GXVOLUME:q_formula')
  wqfreset=widget_button(font=font,wqFormulaBase,value='Reset to default',uname='GXVOLUME:q_formula_reset')
  
  g=widget_info(wParmBase,/geo)

  wEBTELselect= widget_button( wEBTELToolbarBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Select EBTEL Table',uname='EBTELselect')
  wEBTELpath=widget_text(font=!defaults.font,wEBTELToolbarBase,value=gx_ebtel_path(),SCR_XSIZE=g.SCR_XSIZE,/wrap,uname='EBTELpath')
  
  wMask=widget_table(widget_base(wTab0,title='Selective Heating Mask'),value=(*h2v).mask,row_labels=(tag_names(*h2v))[0:6],$
    column_labels=(tag_names(*h2v))[0:6],uname='GXVOLUME:qmask',/editable)


  
  wControlBase=widget_base(self.wBase,/row,uname='GXVOLUME:control_base')
  w1=widget_base(wControlBase,/column,uname='GXVOLUME:w1')
  wDisplayOptionBase=widget_base(w1,title='Plot Options',/row)
  w2=widget_base(wControlBase,/row,uname='GXVOLUME:w2')   

  wDraw=WIDGET_DRAW(w2,xsize=xsize,ysize=ysize,uname='GXVOLUME:draw',$
                          /expose_events, $
                          retain=1,$
                          graphics_level=1 $
                          )

  wSlicer = WIDGET_SLIDER(w2, $
      MINIMUM = 0, $
      MAXIMUM = (self.Volume->Size())[1]-1, $
      /FRAME, uname='GXVOLUME:slicer',/vert)  
  value=[]
  for k=0,7 do value=[value,abs((*h2v).(k))]
  names=(tag_names(*h2v))[0:7]
  wTab1=WIDGET_TAB(w1,font=font, /Align_Left,LOCATION=0,uname='GXVOLUME:TAB1') 
  wQBase=widget_base(wTab1,title='Selective Seeds',/row)
  wQseedsBase=widget_base(wQBase)
  wQseeds=cw_objArray(wQSeedsBase,names=names, value= value,units='',/vert,/static,xlabelsize=7,uname='GXVOLUME:qseeds')                     
  wOpenBase=widget_base(wParmBase,/row,/frame)
  wOpenFormulaBase=widget_base(wOpenBase,uname='GXVOLUME:open_lines',map=(*h2v).olines,/row)
  wOpen=cw_bgroup(widget_base(wOpenBase),/nonexclusive,['Heat Open Lines'],set_value=(*h2v).olines,uname='GXVOLUME:olines',/row)
  wOpenFormulaReset=widget_button(wOpenFormulaBase,value='Reset to default',uname='GXVOLUME:olines_reset')
  g1=widget_info(wOpen,/geometry)
  g2=widget_info(wOpenFormulaReset,/geometry)
  wOpenFormula=widget_text(widget_base(wOpenFormulaBase),value=(*h2v).oscale,$
    uname='GXVOLUME:oscale',uvalue=(*h2v).oscale,/editable,scr_xsize=g.xsize-g1.xsize-g2.xsize)
   expert=1
   
  if keyword_set(expert) then begin
    for avgdem=0, 6 do begin
      gx_dem_interpolate,avgdem=avgdem, method=method,/expert,/info
      buttons=n_elements(buttons) eq 0?method:[buttons,method]
    endfor
  endif else begin
    for avgdem=0, 3 do begin
      gx_dem_interpolate,avgdem=avgdem, method=method,/info
      buttons=n_elements(buttons) eq 0?method:[buttons,method]
    endfor
  endelse
  prefix='GXVOLUME:'
  wDEMOptionBase=widget_base(wTab1,title='DEM/DDM Options',/column)
 
  if keyword_set(expert) then begin
    valid=gx_ebtel_path(has_ddm=has_ddm)
    wDEMDDM=cw_bgroup(font=font,wDEMOptionBase,$
      ['Use DEM','Use DDM'],$
      set_value=has_ddm ,$
      /exclusive,/return_index,/no_release,uname=prefix+'DEM/DDM',/row)
  end
  wLabel=widget_label(wDEMOptionBase,value='DEM/DDM Interpolation Method',font=font,/align_left)

  wDEMinterpolate=cw_bgroup(font=font,wDEMOptionBase,$
    buttons,$
    set_value=0 ,$
    /exclusive,/return_index,/no_release,uname=prefix+'DEMAVG')
  wLabel=widget_label(wDEMOptionBase,value='',font=font,/dynamic,uname=prefix+'DEMDT',/Align_left)
  wPlotOptions=cw_objPlotOptions(font=font,wDisplayOptionBase,title='Plot Options',uname=prefix+'PlotOptions',ylog=0,xlog=0,zlog=0,charsize=1,/frame)
  g4=widget_info(wDisplayOptionBase,/geometry)
  
  wOptionBase=widget_base(wDisplayOptionBase,/column,/frame)
  wSelectOptionBase=widget_base(wOptionBase,/frame,/column,uname=prefix+'slice_select')
  wLabel=widget_label(font=font,wSelectOptionBase,value='Slice Selection')
  wSelectionBase=widget_base(wSelectOptionBase,/row)
  wDisplay=cw_bgroup(font=font,wSelectionBase,/exclusive,['Mask','Q0', 'Q'],/column,$
    set_value=0,uvalue=[0,0,0],uname=prefix+'display')
  wDirection=cw_bgroup(font=font,wSelectionBase,/exclusive,['X','Y', 'Z'],/column,$
    set_value=0,uvalue=[0,0,0],uname=prefix+'slice')
  wEbtelGrid=cw_bgroup(font=font,wOptionBase,/non,'Display EBTEL Grid',uname=prefix+'ebtel_grid',/frame)
 end  

function gxh2v::HandleEvent, event
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
  case strupcase(widget_info(event.id,/uname)) of
    'EBTELSELECT': begin
              wScanbox=widget_info(event.top,find_by_uname='Scanbox')
              if widget_valid(wScanbox) then begin
                widget_control,wScanbox,get_uvalue=oScanbox
                if ~obj_isa(oScanbox,'gxscanbox') then goto,no_scanbox
                oScanbox->ReplaceEBTELtables
                
              endif else begin
                no_scanbox:
                path=dialog_pickfile(path=file_dirname(gx_findfile('ebtel.sav')),default='.sav')
                if gx_ebtel_valid_path(path) then begin
                  widget_control,widget_info(event.top,find_by_uname='EBTELpath'),set_value=gx_ebtel_path(path)
                  
                endif
              endelse
             end
    'GXVOLUME:SLICE':begin
             wslicer=widget_info(self.wBase,find_by_uname='GXVOLUME:slicer')
             widget_control,wslicer,get_value=current_idx
             if event.select then begin
               widget_control,event.id,get_uvalue=stored_idx
               widget_control,wslicer,set_slider_max=(self.Volume->Size(/volume))[event.value+1]-1,$
                                      set_value=stored_idx[event.value]
                                      
             endif else begin
               widget_control,event.id,get_uvalue=stored_idx
               stored_idx[event.value]=current_idx
               widget_control,event.id,set_uvalue=stored_idx
             endelse
            end      
     'GXVOLUME:EBTEL_GRID': begin
                widget_control,widget_info(self.wBase,find_by_uname='GXVOLUME:draw'),get_value=win
                wPlotOptions=widget_info(self.wBase,find_by_uname='GXVOLUME:PlotOptions')
                widget_control,wPlotOptions,get_value=objPlotOptions
                if event.select then begin
                  widget_control, widget_info(self.wBase,find_by_uname='GXVOLUME:slice_select'),map=0
                  objPlotOptions->SetProperty,scale='log-log',/auto,xrange=[0.0,0.0],yrange=[0.0,0.0]
                endif else begin
                  widget_control, widget_info(self.wBase,find_by_uname='GXVOLUME:slice_select'),map=1
                  objPlotOptions->SetProperty,scale='lin-lin',/auto,xrange=[0.0,0.0],yrange=[0.0,0.0]
                endelse
                 
               end     
     'GXVOLUME:QSEEDS':begin
                widget_control, event.id,get_value=seeds
                self.Volume->SetHeat2Volume,seeds=seeds
                h2v=self.Volume->GetHeat2Volume()
                widget_control, widget_info(self.wBase,find_by_uname='GXVOLUME:qmask'),set_value=(*h2v).mask
                compute=1
               end   
     'GXVOLUME:QMASK':begin
                 widget_control, event.id,get_value=mask
                 if event.x ne event.y then begin
                   mask[event.y,event.x]=mask[event.x,event.y]
                   widget_control, event.id,set_value=mask
                 end
                 self.Volume->SetHeat2Volume,mask=mask
                 compute=1
               end  
      'GXVOLUME:OLINES': begin
                  widget_control,widget_info(self.wBase,find_by_uname='GXVOLUME:open_lines'),map=event.select  
                  h2v=self.Volume->GetHeat2Volume()
                  (*h2v).olines=event.select
                  compute=1
                end  
      'GXVOLUME:OSCALE':begin
                widget_control,event.id,get_value=new_oscale,get_uvalue=old_oscale
                z=(zmax=(Bx=(By=(Bz=[1.0,1.0]))))
                test=execute('oscale='+new_oscale[0])
                if keyword_set(test) then begin
                  h2v=self.Volume->GetHeat2Volume()
                  (*h2v).oscale=new_oscale[0]
                  widget_control,event.id,set_value=new_oscale,set_uvalue=new_oscale
                  compute=1
                endif else widget_control,event.id,set_value=old_oscale
               end
      'GXVOLUME:OLINES_RESET': begin
                       h2v=self.Volume->GetHeat2Volume()
                       new_oscale='1.1-z/max(z)'
                       (*h2v).oscale=new_oscale
                       widget_control,widget_info(self.wBase,find_by_uname='GXVOLUME:oscale'),set_value=new_oscale,set_uvalue=new_oscale
                       compute=1
                      end 
      'GXVOLUME:Q_RESET':Begin
                        self.volume->GetVertexAttributeData,'q0_formula',q0_formula
                        q=[0.000415,1e2,1e9,0,0]
                        self.volume->SetVertexAttributeData,'q0_coeff',q
                        widget_control,widget_info(self.wBase,find_by_uname='GXVOLUME:q'),set_value=q
                        q0_formula=self.volume->SetQ0(q0_formula)
                        compute=1
                      End
      'GXVOLUME:Q':   Begin
                        self.volume->GetVertexAttributeData,'q0_formula',q0_formula
                        widget_control,event.id,get_value=q
                        self.volume->SetVertexAttributeData,'q0_coeff',q
                        q0_formula=self.volume->SetQ0(q0_formula)
                        compute=1
                      end
      'GXVOLUME:Q0_FORMULA': Begin
                        widget_control,event.id,get_value=q0_formula
                        q0_formula=q0_formula[0]
                        widget_control,event.id,set_value=self.volume->SetQ0(q0_formula)
                        compute=1
                      End
      'GXVOLUME:Q0_FORMULA_RESET': Begin
                        wq0f=widget_info(self.wBase,find_by_uname='GXVOLUME:q0_formula')
                        widget_control,wq0f,set_value=self.volume->SetQ0()
                        compute=1
                      End

      'GXVOLUME:Q_FORMULA': Begin
                        widget_control,event.id,get_value=q_formula
                        q_formula=q_formula[0]
                        widget_control,event.id,set_value=self.volume->SetQ(q_formula)
                        compute=1
                      End
      'GXVOLUME:Q_FORMULA_RESET': Begin
                        wqf=widget_info(self.wBase,find_by_uname='GXVOLUME:q_formula')
                        widget_control,wqf,set_value=self.volume->SetQ()
                        compute=1
                      End 
      'GXVOLUME:DEM/DDM': BEGIN
                          flags=self.volume->getflags()
                          path=gx_ebtel_path(has_ddm=has_ddm,/quiet)
                          if ~has_ddm and event.value eq 1 then begin
                            answ=dialog_message('No DDM provided by the selected  EBTEL table, reverting to DEM interpolation!',/info)
                            widget_control,event.id,set_value=0
                          endif else flags=self.volume->setflags(/newNT)
                        END
                      
       'GXVOLUME:DEMAVG': BEGIN
                        widget_control,event.id,get_value=demavg
                        flags=self.volume->setflags(/newNT)
                        wnparms=widget_info(event.top,FIND_BY_UNAME='renderer:nparms')
                        if widget_valid(wnparms) then begin
                        widget_control,widget_info(event.top,find_by_uname='Scanbox'),get_uvalue=scanbox
                         if obj_valid(scanbox) then scanbox->ReplaceParmValue,'DEMavg',demavg
                        end
                      END                                       
    else: begin
           if tag_exist(event,'select') then begin
            if ~event.select then goto,exit_point
           endif
          end
  endcase
  self->Display,compute=compute
  exit_point:
  return, self->Rewrite(event)
 end  
 
 pro gxh2v::DisplayEbtelGrid,_extra=_extra
   xx=self.volume->GetVertexData('length')*gx_rsun()
   xTitle='Loop length (cm)'
   yy=self.volume->GetVertexData('Q')
   yTitle='Loop heating rate (Q)'
   good=where(xx ne 0 and yy ne 0)
   xx=xx[good]
   yy=yy[good]
   if n_elements(xx) gt 5000 then begin
     dxrange=minmax(xx)
     dyrange=minmax(yy)
     idx=(n_elements(xx)-1)*randomu(seed,5000)
     xx=[xx[idx],dxrange]
     yy=[yy[idx],dyrange]
   endif
   s=sort(xx)
   plot,xx[s],yy[s],psym=3,xtitle=xTitle,ytitle=yTitle,_extra=_extra 
   ebtel_file=gx_ebtel_path()
   if gx_ebtel_valid_path(ebtel_file) then begin
     restore,ebtel_file
     psym=3
     oplot,2*lrun,qrun,psym=psym,color=250
   end
 end
 
 pro gxh2v::DisplaySlice,_extra=_extra
  h2v=self.volume->GetHeat2Volume() 
  widget_control, widget_info(self.wBase,find_by_uname='GXVOLUME:qmask'),foreground_color=(*h2v).seeded?[0,0,0]:[255,0,0]
  wslicer=widget_info(self.wBase,find_by_uname='GXVOLUME:slicer')
  widget_control,wSlicer,get_value=idx
  wslice=widget_info(self.wBase,find_by_uname='GXVOLUME:slice')
  widget_control,wSlice,get_value=axis
  widget_control,widget_info(self.wBase,find_by_uname='GXVOLUME:display'),get_value=display
  self.volume->GetProperty,parent=model
  data=self.volume->GetHeat2VolumeFactor(idx=vol_idx)
  data=model->box2volume(data,vol_idx,/corona)

  case display of
   1: begin 
        data=model->box2volume('Q0',/corona)
        title='Coronal Heating Factor q0'
      end   
   2: begin 
       data=model->box2volume('Q',/corona)
       title='Coronal Heating Rate Q'
     end  
   else: title='Coronal Selective Heating Mask'
  endcase
  if size(_extra,/tname) eq 'STRUCT' then begin
    if tag_exist(_extra,'zlog') then begin
      if _extra.zlog ne 0 then data=alog10(data)
    endif
  endif
  cb_range=minmax(data,/nan)
  case axis of
    1:begin
        data=reform(data[*,idx,*])
        xtitle='X (index)'
        ytitle='Z (index)'
      end
    2:begin 
        data=reform(data[*,*,idx])
        xtitle='X (index)'
        ytitle='Y (index)'
      end
    else:begin 
        data=reform(data[idx,*,*])
        xtitle='Y (index)'
        ytitle='Z (index)'
      end
  endcase


  dmin=cb_range[0]
  dmax=cb_range[1]

  map=make_map(data)

  get_map_coord,map,xp,yp
  map.xc=max(xp)
  map.yc=max(yp)
  if (min(data) eq 0 and max(data) eq 0) then begin
    map.data=-1
    dmin=cb_range[0]
    dmax=cb_range[1]
  endif
  plot_map,map,/cbar,xtitle=xtitle,ytitle=ytitle,title=title,dmin=dmin,dmax=dmax ,top=254,_extra=_extra
 end
 
 pro gxh2v::Display,compute=compute,reset_plot_options=reset_plot_options,_extra=_extra
   h2v=self.volume->GetHeat2VolumeFactor(compute=compute)
   widget_control,widget_info(self.wBase,find_by_uname='GXVOLUME:draw'),get_value=win
   wPlotOptions=widget_info(self.wBase,find_by_uname='GXVOLUME:PlotOptions')
   widget_control,wPlotOptions,get_value=objPlotOptions
   wset,win
   tvlct,rgb,/get
   loadct,39,/silent
   gx_rgb_white2black
   widget_control,widget_info(self.wBase,find_by_uname='GXVOLUME:ebtel_grid'),get_value=select
   case select[0] of
    1:begin 
       objPlotOptions->GetProperty,xrange=xrange,yrange=yrange,xlog=xlog,ylog=ylog,zlog=zlog,charsize=charsize
       self->DisplayEbtelGrid,xrange=xrange,yrange=yrange,xlog=xlog,ylog=ylog,zlog=zlog,charsize=charsize
      end
    else:begin
       objPlotOptions->GetProperty,xrange=xrange,yrange=yrange,xlog=xlog,ylog=ylog,zlog=zlog,charsize=charsize
       self->DisplaySlice,xrange=xrange,yrange=yrange,xlog=xlog,ylog=ylog,zlog=zlog,charsize=charsize
      end 
   endcase
   gx_rgb_white2black
   tvlct,rgb
 end
  

function cw_heat2volume,Base, volume,_extra=_extra
  obj=obj_new('gxh2v',Base,volume,_extra=_extra)
  obj->GetProperty,widget_id=widget_id
  return,widget_id
end