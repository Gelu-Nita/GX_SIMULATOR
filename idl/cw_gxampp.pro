function gxampp::INIT,wBase,uname=uname,_extra=_extra
 compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
      catch, /cancel
      MESSAGE, /INFO, !ERROR_STATE.MSG
      return, 0
  end
  self.WinOS=!version.os_family eq 'Windows'
  void=self->gxWidget::Init(wBase,self,KILL_NOTIFY='objgxamppKill',_extra=_extra)
  self->CheckOS
  return,1
end

pro gxampp__define
 struct_hide,{gxampp, inherits gxwidget,out_dir:'',tmp_dir:'',entry_box:'',jump2:'',WinOS:0l}
end

pro objgxamppKill,wBase
  widget_control,wBase,get_uvalue=obj
  obj_destroy,obj
end

pro gxampp::CreatePanel,xsize=xsize,ysize=ysize
  device, get_screen_size=scr
  if not exist(xsize) then xsize = fix (scr[0] * .45)
  if not exist(ysize) then ysize = xsize *1.1
   
   wgxamppControl=self.wBase
   wControlPanel=widget_base(wgxamppControl,/column,xsize=scr[0],ysize=scr[1],$
    x_scroll_size=xsize,y_scroll_size=ysize,/scroll)
   geom = widget_info (wControlPanel, /geom)
   scr_xsize=0.98*geom.scr_xsize
   scr_ysize=0.98*geom.scr_ysize
   wControlBase=widget_base(wControlPanel,/column,uname='control_base')
   
   wTmpDirBase=widget_base(wControlBase,/row,scr_xsize=scr_xsize,/frame)
   wTmpBase=widget_base(wTmpDirBase,/row)
   wlabel=widget_label(wTmpBase,value='SDO Data Repository    ')
   label=widget_info(wlabel,/geometry)
   label_scr_xsize=label.scr_xsize
   wSelectTmpDir= widget_button(wTmpBase, $
     value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
     /bitmap,tooltip='Select directory where HMI data will be downloaded or looked for',uname='tmp_dir_select')
   geom = widget_info (wTmpBase, /geom)
   self.tmp_dir=self.WinOS?'C:\jsoc_cache':(curdir()+'/jsoc_cache')
   wTmpDirPath=widget_text(wTmpDirBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='tmp_dir',/editable,$
                           value=self.tmp_dir)
   
  
   wOutDirBase=widget_base(wControlBase,/row,scr_xsize=scr_xsize,/frame)
   wOutBase=widget_base(wOutDirBase,/row)
   wlabel=widget_label(wOutBase,scr_xsize=label.scr_xsize,value='GX Model Repository')
   wSelectOutDir= widget_button(wOutBase, $
     value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
     /bitmap,tooltip='Select directory where the output models will be stored',uname='out_dir_select')
     
   self.out_dir=self.WinOS?'C:\gx_models':(curdir()+'/gx_models')
   wOutDirPath=widget_text(wOutDirBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='out_dir',/editable,$
                           value=self.out_dir)
   
   
   wEntryBoxBase=widget_base(wControlBase,/row,scr_xsize=scr_xsize,/frame)
   wEntryBase=widget_base(wEntryBoxBase,/row)
   wlabel=widget_label(wEntryBase,value='External Box path',scr_xsize=label.scr_xsize)
   wSelectEntryBox= widget_button(wEntryBase, $
     value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
     /bitmap,tooltip='Select an existg box structure to start the AMPP script with',uname='entrybox_select')
   wEntryBoxPath=widget_text(wEntryBoxBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='entrybox_path',/editable,$
     value='')
   
   wEntryBoxBlockBase=widget_base(wControlBase,/row,scr_xsize=scr_xsize,/frame)  
   wEntryBoxActionBase=widget_base(wEntryBoxBlockBase,/row,uname='jump2base',sensitive=0)
   wlabel=widget_label(wEntryBoxActionBase,value='Jump-to Action',scr_xsize=label.scr_xsize)
   wEntryBoxAction=cw_bgroup( wEntryBoxActionBase,['none','potential','nlff','lines','chromo'], $
                    /row,/exclusive,/frame,uname='jump2',set_value=0,$
                    button_uvalue=['','jump2potential','jump2nlff','jump2lines','jump2chromo'])  
   wCopyParms= widget_button(wEntryBoxActionBase, $
                    value=gx_bitmap(filepath('copy.bmp', subdirectory=['resource', 'bitmaps'])), $
                    /bitmap,tooltip='Copy external box parameters to AMPP',uname='entrybox_copy')               
   
   wInputBase=widget_base(wControlBase,/column,/frame,uname='box_base')
   wTimeBase=widget_base(wInputBase,/row)
   wlabel=widget_label(wTimeBase,value='Model Time',scr_xsize=label.scr_xsize)
   wTime= widget_text(wTimeBase,scr_xsize=scr_xsize-label.scr_xsize,$
                      value='2016-02-20 17:00:00.000',uvalue='2016-02-20 17:00:00',/editable,uname='time')
   wCenterBase=widget_base(wInputBase,/row)
   wCenter=cw_objArray(wCenterBase,label='Model Coordinates ', value=[-15,185],$
    scr_arraylabelsize=label.scr_xsize,$
    names=['Xc: ','Yc: '],units='"',xtextsize=10,/static,xlabelsize=4,/show,uname='center')
   wCarrington=cw_bgroup(wCenterbase,['Heliocentric','Carrington'],/exclusive,/row,uname='/carrington',set_value=0)
   
   wBox=widget_base(wInputBase,/row)
   wDimensions=cw_objArray(wBox, label='Model Gridpoints', value=[64,64,64],scr_arraylabelsize=label.scr_xsize,$
    names=['X: ','Y: ','Z: '],units='',xtextsize=6,/static,xlabelsize=4,/show,uname='size_pix',increment=1,type=1l)  
   
   wResolution=cw_objField(wBox,label=' Resolution ',value=1400.0,unit='km',xtextsize=10,uname='dx_km')

   wProjectionBase=widget_base(wInputBase,/row)
   wlabel=widget_label(wProjectionBase,value='Geometrical Projection',scr_xsize=label.scr_xsize)
   wProjection=cw_bgroup(wProjectionBase,['CEA','TOP'],/row,/exclusive,/frame,uname='projection',set_value=0)
   
   wSFQBase=widget_base(wInputBase,/row)
   wlabel=widget_label(wSFQBase,value='Pi-disambiguation',scr_xsize=label.scr_xsize)
   wSFQ=cw_bgroup(wSFQBase,['HMI','SFQ'],/row,/exclusive,/frame,uname='/sfq',set_value=0) 
  
   wBufferBase=widget_base(wControlBase,/row,/frame)
   wBufferZone=cw_objField(wBufferBase,label='Buffer Zone Size',value=10.0,unit='%',$
     scr_labelsize=label.scr_xsize,xtextsize=10,increment=1,$
     uname='weight_bound_size',tooltip='Blah',min=0,max=50)
   buffer_tip=widget_label(wBufferBase,value='(default, 10% of the box dimensions recommended)')  
   
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
   endfor                
   wScript=widget_text(wControlBase,scr_xsize=scr_xsize,ysize=3,$
    value='',uvalue=[keys1,keys2],/scroll,uname='script',/wrap)
   toolbar= widget_base(wControlBase, /row,/toolbar) 
   wGenerate=widget_button(toolbar,value=gx_bitmap(filepath('gears.bmp', subdirectory=['resource', 'bitmaps'])), $
     /bitmap,tooltip='Update Script',uname='update_script',sensitive=1)
   wImportScript= widget_button(toolbar, $
     value=gx_bitmap(filepath('importf.bmp', subdirectory=['resource', 'bitmaps'])), $
     /bitmap,tooltip='Import script to AMPP',uname='script_import')  
   wExecute=widget_button(toolbar,value=gx_bitmap(gx_findfile('play.bmp')),tooltip='Execute Script',/bitmap,uname='execute')
   wSave=widget_button(toolbar,value=gx_bitmap(filepath('export.bmp', subdirectory=['resource', 'bitmaps'])),tooltip='Save execution script',/bitmap,uname='savescript') 
   wModel2gx=widget_button(toolbar,value=gx_bitmap(filepath('dataspace.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Send Model Data to GX Simulator',uname='model2gx',sensitive=0)
   wSave=widget_button(toolbar,value=gx_bitmap(filepath('text.bmp', subdirectory=['resource', 'bitmaps'])),tooltip='Save execution log',/bitmap,uname='savelog') 
   wClearLog=widget_button(toolbar,value=gx_bitmap(filepath('delete.bmp', subdirectory=['resource', 'bitmaps'])),tooltip='Clear execution log',/bitmap,uname='clearlog')
   
   
   
   geom = widget_info (wControlBase, /geom)                              
   wConsoleBase=widget_base(wControlPanel)  
   wConsole=widget_text(wConsoleBase,scr_xsize=scr_xsize,$
                 scr_ysize=0.95*(scr[1]-geom.scr_ysize),$
                 value='',/scroll,uname='console',/wrap)
end

pro gxampp::message,msg,_extra=_extra
 wConsole=widget_info(self.wIDBase,find_by_uname='console')
 gx_message,msg,wConsole,_extra=_extra
end
       

function gxampp::HandleEvent, event
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
  case widget_info(event.id,/uname) of                                         
    'update_script':begin
    end
    'clearlog':begin
                widget_control,widget_info(self.wIDBase,find_by_uname='console'),set_value=''
               end
    'savelog':begin
               widget_control,widget_info(self.wIDBase,find_by_uname='console'),get_value=text
               file=dialog_pickfile(filter='*.txt',$
               DEFAULT_EXTENSION='txt',/write,$
                title='Please select a location to save the AMPP execution log')
               if file ne '' then begin
                openw,lun,file,/get
                printf,lun,text
                close,lun
                free_lun,lun
               endif
              end  
    'savescript':begin
                widget_control,widget_info(self.wIDBase,find_by_uname='script'),get_value=text
                file=dialog_pickfile(filter='*.txt',$
                  DEFAULT_EXTENSION='txt',/write,$
                  title='Please select a location to save the AMPP execution script')
                if file ne '' then begin
                  openw,lun,file,/get
                  printf,lun,text
                  close,lun
                  free_lun,lun
                endif
              end            
              
    'tmp_dir':begin
                widget_control,event.id,get_value=tmp_dir
                self.tmp_dir=tmp_dir
              end  
    'out_dir':begin
                widget_control,event.id,get_value=out_dir
                self.out_dir=out_dir
              end  
    'tmp_dir_select':begin
                       tmp_dir=dialog_pickfile(/dir,$
                       /must_exist,$
                       title='Please select an already existing JSOC repository',path=self.tmp_dir)
                        if file_exist(tmp_dir) then begin
                          self.tmp_dir=tmp_dir
                          widget_control,widget_info(self.wIDBase,find_by_uname='tmp_dir'),set_value=self.tmp_dir
                        endif
                      end
    'out_dir_select':begin
                       out_dir=dialog_pickfile(/dir,$
                       /must_exist,$
                       title='Please select an already existing GX models repository',path=self.out_dir)
                        if file_exist(out_dir) then begin
                          self.out_dir=out_dir
                          widget_control,widget_info(self.wIDBase,find_by_uname='out_dir'),set_value=self.out_dir
                        endif
                      end                        
    'entrybox_select': begin
                      file=dialog_pickfile(filter='*.sav',$
                      DEFAULT_EXTENSION='sav',$
                      /read,/must_exist,$
                      title='Please select a file generated by AMPP',path=self.out_dir)
                        if file_exist(file) then begin
                         widget_control,widget_info(self.wIDBase,find_by_uname='entrybox_path'),set_value=file
                         self.entry_box=file
                         widget_control,widget_info(self.wIDBase,find_by_uname='jump2base'),sensitive=1
                        end
                      end 
    'entrybox_copy':self->CopyBox
    'script_import':begin
                      file=dialog_pickfile(filter='*.txt',$
                      DEFAULT_EXTENSION='txt',$
                      /read,/must_exist,$
                      title='Please select script file generated by AMPP',path=curdir())
                      if file_exist(file) then begin
                       script=''
                       openr,lun,file,/get
                       readf,lun,script
                       free_lun,lun
                       self->CopyBox,script
                      end
                    end
    'jump2':begin
             if event.select then begin
               wScript=widget_info(self.wIDBase,find_by_uname='script')
               widget_control,wScript,get_uvalue=keywords
               for i=0,n_elements(keywords)-1 do widget_control,widget_info(self.wIDBase,find_by_uname=keywords[i]),set_button=0,sensitive=1
               jump2='/'+event.value
              case jump2 of
                '/':up2=-1 
                '/jump2potential':up2=2
                '/jump2nlff':up2=6
                '/jump2lines':up2=7
                '/jump2chromo':up2=9
                else:up2=-1
              endcase
              self->CopyBox,/nokeys
              for i=0,up2 do widget_control,widget_info(self.wIDBase,find_by_uname=keywords[i]),set_button=0,sensitive=0
              self.jump2=jump2 ne '/'?jump2:''
              widget_control,widget_info(self.wIDBase,find_by_uname='entrybox_copy'),sensitive=self.jump2 eq ''
              widget_control,widget_info(self.wIDBase,find_by_uname='box_base'),sensitive=self.jump2 eq ''
             endif
            end                                                
    'model2gx':begin    
              file=dialog_pickfile(filter='*.sav',$
              DEFAULT_EXTENSION='sav',$
              /read,/must_exist,$
              title='Please select a file generated by AMPP',path=self.out_dir)
              if file ne '' then begin
                self->message,'Importing '+file+'...'
                model=gx_ImportModel(file)
                if obj_isa(model,'gxmodel') then begin
                  if xalive('gx_simulator',/name,id=id) then begin
                    widget_control,id[0],send_event={MODEL2GX,id:0l,top:0l,handler:0l,model:model,file:file}
                    endif else begin
                      self->message,'Please wait until the GX Simulator GUI is launched'
                      gx_simulator,model,/expert
                    endelse
                endif
               end
             end
    'execute':begin
               self->CheckOS
               self->GenerateScript,script=test_script,/test
               widget_control,widget_info(self.wIDBase,find_by_uname='script'),get_value=script
               if script ne test_script then begin
                answ=dialog_message(['Selected seetings do not match the current script!', 'Use "Update Script" button to update the script and try again!'],/info)
                return, self->Rewrite(event)
               endif
               widget_control,widget_info(self.wIDBase,find_by_uname='control_base'),sensitive=0
               widget_control,/hourglass
               script=script[0]+string(widget_info(self.wIDBase,find_by_uname='console'),format="(', wConsole=',i6)")
               script+=', out_files=out_files'
               success=execute(script)
               default,out_files,''
               file=out_files[0]
               if file_exist(file) then begin
                 break_file,file, disk_log, dir, name, ext, fversion, node, /last_dot
                 self.out_dir=disk_log+dir
                 widget_control,widget_info(self.wIDBase,find_by_uname='model2gx'),sensitive=1
               endif
               widget_control,widget_info(self.wIDBase,find_by_uname='control_base'),sensitive=1
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
                          wKey=widget_info(self.wIDBase,find_by_uname=keys[k])
                          widget_control,wKey,set_button=0,sensitive=~event.select
                        endfor
      end
    '/use_potential': begin
                      keys=['/center_vox','/generic_only']
                      for k=0, n_elements(keys)-1 do begin
                        widget_control,widget_info(self.wIDBase,find_by_uname=keys[k]),set_button=0,sensitive=1
                      endfor
                        widget_control,widget_info(self.wIDBase,find_by_uname='/nlfff_only'),set_button=0,sensitive=~event.select
                      end
    '/nlfff_only':begin
                 keys=['/center_vox','/generic_only']
                 for k=0, n_elements(keys)-1 do begin
                   widget_control,widget_info(self.wIDBase,find_by_uname=keys[k]),set_button=0,sensitive=~event.select
                 endfor
                 end                  
    else:
  endcase
  self->CheckOS
  self->GenerateScript
  return,self->Rewrite(event)
end  

pro gxampp::CheckOS
       if ~keyword_set(self.WinOS) then begin
         ;widget_control, widget_info(self.wIDbase,find_by_uname='/use_potential'),set_button=1,sensitive=0
         ;widget_control,  widget_info(self.wIDbase,find_by_uname='/nlfff_only'),set_button=0,sensitive=0
       end
    end   

function gxampp::script2info,script
  default,script,''
  exec=strsplit(script,'&',/extract)
  n=n_elements(exec)
  if n gt 0 then begin
    case 1 of
      keyword_set(first): e=execute(exec[0]+',info=info_')
      else: e=execute(exec[n-1]+',info=info_')
    endcase
  endif else e=execute(exec[0]+',info=info_')
  if size(info_,/tname) eq 'STRUCT' then info=temporary(info_) else return,!null
  if ~tag_exist(info,'time') then info=add_tag(info,gx_id2time(box.id),'time',/no_copy,/duplicate)
  return,info
end    
    
pro gxampp::CopyBox,script,nokeys=nokeys
              if size(script,/tname) eq 'STRING' then begin
                info=self->script2info(script)
                if size(info,/tname) eq 'STRUCT' then goto,has_info
              endif
              box=gx_readbox(self.entry_box,info=info,/first)
              if size(box,/tname) eq 'STRUCT' then begin
                has_info:
                self.out_dir=tag_exist(info,'out_dir')?info.out_dir:self.WinOS?'C:\gx_models':(curdir()+'/gx_models')
                widget_control,widget_info(self.wIDBase,find_by_uname='out_dir'),set_value=self.out_dir
                self.tmp_dir=tag_exist(info,'tmp_dir')?info.tmp_dir:self.WinOS?'C:\jsoc_cache':(curdir()+'/jsoc_cache')
                widget_control,widget_info(self.wIDBase,find_by_uname='tmp_dir'),set_value=self.tmp_dir
                if tag_exist(info,'time') then if valid_time(info.time) then $
                  widget_control,widget_info(self.wIDBase,find_by_uname='time'),set_value=info.time
                if tag_exist(info,'center_arcsec') then  widget_control,widget_info(self.wIDBase,find_by_uname='center'),set_value=info.center_arcsec
                if tag_exist(info,'size_pix') then  widget_control,widget_info(self.wIDBase,find_by_uname='size_pix'),set_value=info.size_pix
                if tag_exist(info,'dx_km') then  widget_control,widget_info(self.wIDBase,find_by_uname='dx_km'),set_value=info.dx_km
                widget_control,widget_info(self.wIDBase,find_by_uname='weight_bound_size'),$
                  set_value=tag_exist(info,'weight_bound_size')?info.weight_bound_size:10
                widget_control,widget_info(self.wIDBase,find_by_uname='/carrington'),$
                  set_value=tag_exist(info,'carrington')?info.carrington:0
                if tag_exist(info,'cea') then  widget_control,widget_info(self.wIDBase,find_by_uname='projection'),set_value=1-info.cea
                if tag_exist(info,'top') then  widget_control,widget_info(self.wIDBase,find_by_uname='projection'),set_value=info.top
                widget_control,widget_info(self.wIDBase,find_by_uname='/sfq'),$
                  set_value=tag_exist(info,'sfq')?info.sfq:0
                if keyword_set(nokeys) then return
                wScript=widget_info(self.wIDBase,find_by_uname='script')
                widget_control,wScript,get_uvalue=keywords
                tag_names=tag_names(info)
                for k=0, n_elements(keywords)-1 do begin
                  wKey=widget_info(self.wBase,find_by_uname=keywords[k])
                  if widget_valid(wKey) then begin
                    tag=strmid(keywords[k],1)
                    idx=where(strcompress(strupcase(tag_names),/rem) eq strcompress(strupcase(tag),/rem))
                    set_button=(idx ge 0)?info.(idx):0
                    widget_control,wKey,set_button=set_button
                  endif
                endfor
              endif  
            end  

pro gxampp::GenerateScript,script=script,test=test
 script='gx_fov2box'
 if self.jump2 eq '' then begin
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
   widget_control,widget_info(self.wBase,find_by_uname='/sfq'),get_value=sfq
   script+=sfq?', /sfq':''
   widget_control,widget_info(self.wBase,find_by_uname='projection'),get_value=projection
   script+=projection?', /top':', /cea'
   widget_control,widget_info(self.wBase,find_by_uname='weight_bound_size'),get_value=buffer
   if buffer ne 10 then script+=string(buffer/100., format="(', weight_bound_size=',g0)")
 endif else begin
   script+=", entry_box= '"+self.entry_box+"'"
   widget_control,widget_info(self.wBase,find_by_uname='weight_bound_size'),get_value=buffer
   if buffer ne 10 then script+=string(buffer/100., format="(', weight_bound_size=',g0)")
   script+=', '+self.jump2
 endelse
   wScript=widget_info(self.wIDBase,find_by_uname='script')
   widget_control,wScript,get_uvalue=keywords
   for k=0, n_elements(keywords)-1 do begin
    wKey=widget_info(self.wBase,find_by_uname=keywords[k])
    if widget_valid(wKey) then begin
      if widget_info(wKey,/button_set) then script+=', '+keywords[k]
    endif
   endfor
 script+=", tmp_dir= '"+self.tmp_dir+"'"
 widget_control,widget_info(self.wIDBase,find_by_uname='out_dir'),get_value=out_dir
 self.out_dir=out_dir
 script+=", out_dir= '"+self.out_dir+"'"
 
 if ~keyword_set(test) then widget_control,wScript,set_value=script
end

function cw_gxampp,Base,_extra=_extra
 obj=obj_new('gxampp',Base,_extra=_extra)
 obj->GenerateScript
 obj->GetProperty,widget_id=widget_id
 return,widget_id
end
