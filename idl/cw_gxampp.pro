function gxampp::INIT,wBase,uname=uname,_extra=_extra
 compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
      catch, /cancel
      MESSAGE, /INFO, !ERROR_STATE.MSG
      return, 0
  end
  void=self->gxWidget::Init(wBase,self,KILL_NOTIFY='objgxamppKill',_extra=_extra)
  return,1
end

pro gxampp__define
 struct_hide,{gxampp, inherits gxwidget}
end

pro objgxamppKill,wBase
  widget_control,wBase,get_uvalue=obj
  obj_destroy,obj
end

pro gxampp::CreatePanel,xsize=xsize,ysize=ysize
  device, get_screen_size=scr
  font=!defaults.font
  if not exist(xsize) then xsize = fix (scr[0] * .35)
  if not exist(ysize) then ysize = xsize *1.1
   
   wgxamppControl=self.wBase
   wControlPanel=widget_base(wgxamppControl,/column,xsize=xsize,ysize=ysize*1.1)
   geom = widget_info (wControlPanel, /geom)
   scr_xsize=geom.scr_xsize
   scr_ysize=geom.scr_ysize
   wControlBase=widget_base(wControlPanel,/column)
   wTmpDirBase=widget_base(wControlBase,/row,scr_xsize=scr_xsize)
   wTmpBase=widget_base(wTmpDirBase,/row)
   wlabel=widget_label(wTmpBase,font=font,value='SDO Data Repository    ')
   label=widget_info(wlabel,/geometry)
   label_scr_xsize=label.scr_xsize
   wSelectTmpDir= widget_button(font=font,wTmpBase, $
     value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
     /bitmap,tooltip='Select directory where HMI data will be downloaded or looked for',uname='tmp_dir_select')
   geom = widget_info (wTmpBase, /geom)
   wTmpDirPath=widget_text(font=font,wTmpDirBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='tmp_dir',/editable,$
                           value=!version.os_family eq 'Windows'?'C:\jsoc_cache':(getenv('HOME')+'\jsoc_cache'))
   
  
   wOutDirBase=widget_base(wControlBase,/row,scr_xsize=scr_xsize)
   wOutBase=widget_base(wOutDirBase,/row)
   wlabel=widget_label(wOutBase,font=font,scr_xsize=label.scr_xsize,value='GX Model Repository')
   wSelectOutDir= widget_button(font=font,wOutBase, $
     value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
     /bitmap,tooltip='Select directory where the output models will be stored',uname='out_dir_select')
     
   wOutDirPath=widget_text(wOutDirBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='out_dir',/editable,$
                           value=!version.os_family eq 'Windows'?'C:\gx_models':(getenv('HOME')+'\gx_models'),font=font)
   
   wTimeBase=widget_base(wControlBase,/row)
   wlabel=widget_label(wTimeBase,value='Model Time',scr_xsize=label.scr_xsize)
   wTime= widget_text(wTimeBase,scr_xsize=scr_xsize-label.scr_xsize,$
                      value='2016-02-20 17:00:00.000',uvalue='2016-02-20 17:00:00',font=font,/editable,uname='time')
   wCenterBase=widget_base(wControlBase,/row)
   wCenter=cw_objArray(wCenterBase,label='Model Coordinates ', value=[-15,185],scr_arraylabelsize=label.scr_xsize,$
    names=['Xc: ','Yc: '],units='"',xtextsize=10,/static,xlabelsize=4,font=font,/show,uname='center')
   wCarrington=cw_bgroup(wCenterbase,['Heliocentric','Carrington'],/exclusive,/row,uname='/carrington',set_value=0)
   
   wBox=widget_base(wControlBase,/row)
   wDimensions=cw_objArray(wBox, label='Model Gridpoints', value=[64,64,64],scr_arraylabelsize=label.scr_xsize,$
    names=['X: ','Y: ','Z: '],units='',xtextsize=6,/static,xlabelsize=4,font=font,/show,uname='size_pix',increment=1,type=1l)  
   
   wResolution=cw_objField(wBox,label=' Resolution ',value=1400.0,unit='km',xtextsize=10,uname='dx_km')
   wProjectionBase=widget_base(wControlBase,/row)
   wlabel=widget_label(wProjectionBase,value='Geometrical Projection',scr_xsize=label.scr_xsize)
   wProjection=cw_bgroup(wProjectionBase,['CEA','TOP'],/row,/exclusive,/frame,uname='projection',set_value=0)
   
   wSFQBase=widget_base(wControlBase,/row)
   wlabel=widget_label(wSFQBase,value='Pi-disambiguation',scr_xsize=label.scr_xsize)
   wSFQ=cw_bgroup(wSFQBase,['HMI','SFQ'],/row,/exclusive,/frame,uname='/sfq',set_value=0) 
   keywords1=['Download AIA/UV contextual maps','Download AIA/EUV contextual maps','Save Empty Box','Save Potential Box','Save Bounds Box']  
   wKeywordsBase=widget_base(wControlBase,/row,/frame)
   wbase1=widget_base(wKeywordsBase,/nonexclusive,/column)
   wbase2=widget_base(wKeywordsBase,/nonexclusive,/column)
   keys1=['/uv','/euv','/save_empty_box','/save_potential','/save_bounds']
   for i=0,n_elements(keywords1)-1 do begin
    wKeyword=widget_button(wbase1,value=keywords1[i],uname=keys1[i])
    if i eq 0 or i eq 1 then widget_control, wKeyword,set_button=1
   endfor
   keywords2=['Stop after the potential box is generated',$
         'Skip NLFFF extrapolation','Stop after the NLFFF box is generated',$
         'Center voxel magnetic field line tracing','Do not add Fontenla chromosphere model']
   keys2=['/potential_only','/use_potential','/nlfff_only','/center_vox','/generic_only']
   for i=0,n_elements(keywords2)-1 do begin
     wKeyword=widget_button(wbase2,value=keywords2[i],uname=keys2[i])
     case keys2[i] of
       '/use_potential': if !version.os_family ne 'Windows' then widget_control, wKeyword,set_button=1,sensitive=0
       '/nlfff_only': if !version.os_family ne 'Windows' then widget_control, wKeyword,set_button=0,sensitive=0
      else:
     endcase
        endfor                
   wScript=widget_text(wControlBase,scr_xsize=scr_xsize,ysize=3,$
    value='',uvalue=[keys1,keys2],/scroll,uname='script',font=font,/wrap)
   toolbar= widget_base(wControlBase, /row,/toolbar) 
   wExecute=widget_button(font=font,toolbar,value=gx_bitmap(gx_findfile('play.bmp')),tooltip='Execute Script',/bitmap,uname='execute')
   if widget_info(get_tlb(self.wBase),/uname) eq 'gx_simulator' then begin
    wImport=widget_button(font=font,toolbar,value=gx_bitmap(filepath('importf.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Import Model Data',uname='import',sensitive=0)
   endif
   geom = widget_info (wControlBase, /geom)                              
   wConsoleBase=widget_base(wControlPanel)  
   wConsole=widget_text(wConsoleBase,scr_xsize=scr_xsize,$
                 scr_ysize=scr_ysize-geom.scr_ysize,value='',/scroll,uname='console',font=font,/wrap)
end

function gxampp::HandleEvent, event
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
  case widget_info(event.id,/uname) of                                         
    'import':begin
              widget_control,event.id,get_uvalue=path
              file=dialog_pickfile(filter='*.sav',$
              DEFAULT_EXTENSION='sav',$
              /read,/must_exist,$
              title='Please select a file generated by AMPP',path=path)
              if file ne '' then begin
                model=gx_ImportModel(file)
                if obj_isa(model,'gxmodel') then widget_control,event.top,send_event={MODEL2GX,id:0l,top:0l,handler:0l,model:model,file:file} else obj_destroy,model
              end
             end
    'execute':begin
               widget_control,/hourglass
               widget_control,widget_info(self.wIDBase,find_by_uname='script'),get_value=script
               script=script[0]+string(widget_info(self.wIDBase,find_by_uname='console'),format="(', wConsole=',i6)")
               script+=', out_files=out_files, out_dir=out_dir'
               success=execute(script)
               wImport=widget_info(self.wIDBase,find_by_uname='import')
               if widget_valid(wImport) and file_exist(out_dir) then begin
                widget_control,wImport,sensitive=1,set_uvalue=out_dir
               endif
               return, self->Rewrite(event)
              end
    'time': begin
             widget_control,event.id,get_value=new_value,get_uvalue=old_value
             if ~valid_time(new_value,err) then begin
              answ=dialog_message(err)
              widget_control,event.id,set_value=old_value
              return,self->Rewrite(event)
             endif else widget_control,event.id,set_value=new_value
            end
    '/potential_only': begin
                        keys=['/use_potential','/nlfff_only','/center_vox','/generic_only']
                        for k=0, n_elements(keys)-1 do begin
                          wKey=widget_info(event.top,find_by_uname=keys[k])
                          widget_control,wKey,set_button=0,sensitive=~event.select
                        if !version.os_family ne 'Windows' then begin
                          case keys[k] of
                            '/use_potential': widget_control, wKey,set_button=~event.select,sensitive=0
                            '/nlfff_only': widget_control, wKey,set_button=0,sensitive=0
                            else:
                           endcase 
                        endif
                        endfor
      end
    '/use_potential': begin
                      keys=['/center_vox','/generic_only']
                      for k=0, n_elements(keys)-1 do begin
                        widget_control,widget_info(event.top,find_by_uname=keys[k]),set_button=0,sensitive=1
                      endfor
                        widget_control,widget_info(event.top,find_by_uname='/nlfff_only'),set_button=0,sensitive=~event.select
                      end
    '/nlfff_only':begin
                 keys=['/center_vox','/generic_only']
                 for k=0, n_elements(keys)-1 do begin
                   widget_control,widget_info(event.top,find_by_uname=keys[k]),set_button=0,sensitive=~event.select
                 endfor
                 end                  
    else:
  endcase
  self->GenerateScript
  return,self->Rewrite(event)
end  

pro gxampp::GenerateScript
 script='gx_fov2box'
 widget_control,widget_info(self.wBase,find_by_uname='time'),get_value=time
 time=strmid(atime(anytim(time)),0,18)
 script+=", '"+time+"'"
 widget_control,widget_info(self.wBase,find_by_uname='center'),get_value=center
 script+=', center_arcsec='+strcompress('['+arr2str(string(center,format='(g0)'),/trim,/compress,/rem)+']',/rem)
 widget_control,widget_info(self.wBase,find_by_uname='size_pix'),get_value=size
 script+=', size_pix='+strcompress('['+arr2str(string(size,format='(g0)'),/trim,/compress,/rem)+']',/rem)
 widget_control,widget_info(self.wBase,find_by_uname='dx_km'),get_value=dx_km
 script+=', dx_km='+string(dx_km,format='(g0)')
 widget_control,widget_info(self.wBase,find_by_uname='/carrington'),get_value=carrington
 script+=carrington?', /carrington':''
 widget_control,widget_info(self.wBase,find_by_uname='/sfq'),get_value=carrington
 script+=carrington?', /sfq':''
 widget_control,widget_info(self.wBase,find_by_uname='projection'),get_value=projection
 script+=projection?', /top':', /cea'

 wScript=widget_info(self.wIDBase,find_by_uname='script')
 widget_control,wScript,get_uvalue=keywords
 for k=0, n_elements(keywords)-1 do begin
  wKey=widget_info(self.wBase,find_by_uname=keywords[k])
  if widget_valid(wKey) then begin
    if widget_info(wKey,/button_set) then script+=', '+keywords[k]
  endif
 endfor
 widget_control,wScript,set_value=script
end

function cw_gxampp,Base,_extra=_extra
 obj=obj_new('gxampp',Base,_extra=_extra)
 obj->GenerateScript
 obj->GetProperty,widget_id=widget_id
 return,widget_id
end
