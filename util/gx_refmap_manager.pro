pro gx_refmap_manager_update,event,refmaps
  default,refmaps,obj_new()
  wTable=widget_info(event.top,find_by_uname='TABLE')
  widget_control,wTable,get_uvalue=model
  if obj_isa(refmaps,'MAP') then model->SetProperty,refmaps=refmaps
  refmaps=*(model->RefMaps())
  count=refmaps->get(/count)
  if count gt 0 then begin
    data=replicate({id:'',time:''},count)
    for i=0,count-1 do begin
      data[i].id=refmaps->get(i,/id)
      data[i].time=refmaps->get(i,/time)
    endfor
    widget_control,wTable,get_value=old_data
    idx=lindgen(n_elements(old_data))
    count=n_elements(idx)
    idx=reform(transpose(array_replicate(idx,2)),2,count)
    idx[0,*]=0
    widget_control,wTable,set_table_select=idx
    widget_control,wTable,/delete_rows
    widget_control,wTable,insert_rows=n_elements(data)
    widget_control,wTable,set_value=data
  end
end

pro gx_refmap_manager_event,event
  CASE TAG_NAMES(event, /STRUCTURE_NAME) OF 
  'WIDGET_KILL_REQUEST':BEGIN
                          answer=dialog_message(/question,'Do you want to quit this application?')
                          if strupcase(answer) eq 'YES' then begin
                           widget_control,event.top,get_uvalue=modal
                           if ~keyword_set(modal) then begin
                             wTable=widget_info(event.top,find_by_uname='TABLE')
                             widget_control,wTable,get_uvalue=model
                             obj_destroy,model
                           end
                           wUndo=widget_info(event.top,find_by_uname='UNDO')
                           widget_control,wUndo,get_uvalue=uvalue
                           obj_destroy,uvalue
                           WIDGET_CONTROL, event.TOP, /DESTROY
                           return
                          end
                          END
   'WIDGET_TABLE_CELL_SEL':begin
                            wTable=widget_info(event.top,find_by_uname='TABLE')
                            widget_control,wTable,get_uvalue=model
                            idx=event.SEL_BOTTOM
                              if idx ge 0 then begin
                                map=(*(model->Refmaps()))->get(idx,/map)
                                widget_control,widget_info(event.top,find_by_uname='DRAW'),get_value=wid,get_uvalue=idx_ptr
                                if ptr_valid(idx_ptr) then *idx_ptr=idx
                                if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'
                                wset,wid
                                plot_map,map,grid=5
                              end
                           end                       
   else:
   ENDCASE                       
    case (strupcase(widget_info(event.id,/uname))) of
      'LOSMAP':Begin
        files=dialog_pickfile(title='Please select one more more files containg maps saved as IDL map structure, IDL objects, or fits',filter=['*.sav','*.map','*.f*s'],/must_exist,/multiple)
        files=files[sort(files)]
        FOR idx=0, n_elements(files)-1 DO BEGIN
          file=files[idx]
          if file ne '' then begin
            catch, error_stat
            if error_stat ne 0 then begin
              catch, /cancel
              gx_fits2map,file,map
              goto, got_a_map
            end
            osav=obj_new('idl_savefile',file)
            names=osav->names()
            valid=0
            for i=0,n_elements(names)-1 do begin
              osav->restore,names[i];,/RELAXED_STRUCTURE_ASSIGNMENT
              e=execute('result=size('+names[i]+',/tname)')
              if (result eq 'STRUCT') or (result eq 'OBJREF') then begin
                e=execute('m=temporary('+names[i]+')')
                if valid_map(m) then map=temporary(m)
              endif
            endfor
            got_a_map:
            if ~(size(map,/tname) eq 'STRUCT' or size(map,/tname) eq 'OBJREF') then begin
              answ=dialog_message('Unexpected file content!',/error)
            endif else begin
              wTable=widget_info(event.top,find_by_uname='TABLE')
              widget_control,wTable,get_uvalue=model
              edge=[-10,10]
              for i=0, n_elements(map)-1 do begin
                sub_map,map[i],amap,xrange=edge+((model->GetFovMap())->get(/xrange)),yrange=edge+((model->GetFovMap())->get(/yrange))
                model->AddMap,amap,id=id
              end
             gx_refmap_manager_update,event
            end
          end
        ENDFOR
      End
      'BASEMAP':Begin
        wTable=widget_info(event.top,find_by_uname='TABLE')
        widget_control,wTable,get_uvalue=model
        files=dialog_pickfile(title='Please select one or more map fits files',filter=['*.f*s'],/must_exist,/multiple)
        files=files[sort(files)]
        FOR idx=0, n_elements(files)-1 DO BEGIN
          file=files[idx]
          if file ne '' then begin
            gx_los2base,model->GetBaseIndex(),file,basemap,pixel=pixel
            if valid_map(basemap) then model->AddMap,basemap,id=id
            if n_elements(id) gt 0 then begin
              gx_refmap_manager_update,event 
            end
          end
        ENDFOR
      End
      'REMOVE': begin
         wTable=widget_info(event.top,find_by_uname='TABLE')
         widget_control,wTable,get_uvalue=model
         refmaps=*(model->Refmaps())
         selected=widget_info(wTable,/TABLE_SELECT)
         idx=reform(selected[1,*])
         good=where(idx ge 9,good_count)
         if good_count gt 0 then begin
           wUndo=widget_info(event.top,find_by_uname='UNDO')
           widget_control,wUndo,get_uvalue=uvalue
           obj_destroy,uvalue
           widget_control, wUndo,set_uvalue=obj_clone(refmaps),sensitive=1
           idx=idx[good]
           idx=idx[sort(idx)]
           undo=obj_new('map')
           k=0
           for i=n_elements(idx)-1,0,-1 do begin
            undo->set,k++,map=refmaps->get(idx[i],/map)
            refmaps->remove,idx[i]
           endfor
           count=n_elements(idx)
           idx=reform(transpose(array_replicate(idx,2)),2,count)
           idx[0,*]=0
           widget_control,wTable,set_table_select=idx
           widget_control,wTable,/delete_rows
         endif else answ=dialog_message('Selected maps cannot be removed!',/info)
      end
      
      'UNDO':begin
                 wUndo=widget_info(event.top,find_by_uname='UNDO')
                 widget_control,wUndo,get_uvalue=refmaps
                 if obj_isa(refmaps,'MAP') then begin
                   count=refmaps->get(/count)
                   if count gt 0 then begin
                     widget_control,wUndo,set_uvalue=obj_new(),sensitive=0
                     gx_refmap_manager_update,event,refmaps
                   end  
                 end 
             end
             
      'SAVE': begin
          wTable=widget_info(event.top,find_by_uname='TABLE')
          widget_control,wTable,get_uvalue=model
          file=dialog_pickfile(filter='*.gxm',$
          DEFAULT_EXTENSION='gxm',$
          /write,/OVERWRITE_PROMPT,$
          title='Please select a file to save current model configuration')
          if file ne '' then save,model,file=file,/compress
         end 
      'COLOR': begin
                  tvlct,rgb_curr,/get
                  xloadct,/silent,/block
                  wTable=widget_info(event.top,find_by_uname='TABLE')
                  widget_control,wTable,get_uvalue=model
                  selected=widget_info(wTable,/TABLE_SELECT)
                  idx=max(reform(selected[1,*]))
                  if idx ge 0 then begin
                    map=(*(model->Refmaps()))->get(idx,/map)
                    widget_control,widget_info(event.top,find_by_uname='DRAW'),get_value=wid
                    if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'
                    wset,wid
                    plot_map,map,grid=5
                  end
               end   
      else: begin
      end
    endcase
end  

; Widget creation routine.
PRO gx_refmap_manager,model,modal=modal,selection=selection
  if n_elements(model) eq 0 then model=gx_read()
  if ~obj_isa(model,'gxmodel') then model=gx_read()
  if ~obj_isa(model,'gxmodel') then begin
    message,'No valid model selected, no action performed!',/info
    return
  endif
  count=(*(model->Refmaps()))->get(/count)
  data=replicate({id:'',time:''},count)
  for i=0,count-1 do begin
    data[i].id=(*(model->Refmaps()))->get(i,/id)
    data[i].time=(*(model->Refmaps()))->get(i,/time)
  endfor

  labels = ['Map ID','Map Time']
  max_strlen=0
  max_id= max(strlen(data.id))
  max_id = max_id * !d.x_ch_size + 6   ; ... + 6 for padding
  max_time= max(strlen(data.time))
  max_time = max_time * !d.x_ch_size + 6   ; ... + 6 for padding
  
  device, get_screen_size=scr
  gx_setfonts,_extra=_extra
  if scr[0] lt 3200 then nb=16 else nb=32
  font=!defaults.font
  if not exist(xsize) then xsize = fix (scr[0] * .3)
  if not exist(ysize) then ysize = xsize
  subdirectory=['resource', 'bitmaps']
  prefix=''
  base = WIDGET_BASE(/COLUMN,/TLB_KILL_REQUEST_EVENTS,$
    title='Reference Maps Manager: ' +model->GetId(),uvalue=keyword_set(modal))
  wToolbarBase = widget_base(base, /row, /frame,/TOOLBAR)
  wPalette = widget_button(wToolbarBase, $
    value=gx_bitmap(filepath('palette.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Change Color Table',uname=prefix+'COLOR')
  wImportLOSMap= widget_button(font=font, wToolbarBase, $
    value=gx_bitmap(filepath('surface.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Import LOS reference map',uname=prefix+'LOSMAP')
  wImportBaseMap= widget_button(font=font, wToolbarBase, $
    value=gx_bitmap(gx_findfile('basemap.bmp')), $
    /bitmap,tooltip='Import BASE reference map',uname=prefix+'BASEMAP')
  wRemove= widget_button(font=font, wToolbarBase, $
    value=gx_bitmap(filepath('delete.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Remove selected maps',uname=prefix+'REMOVE')
  wUndo= widget_button(font=font, wToolbarBase, $
    value=gx_bitmap(filepath('undo.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Undo last action',uname=prefix+'UNDO',uvalue=obj_new(),sensitive=0)   
  wSaveModel= widget_button(font=font, wToolbarBase, $
    value=gx_bitmap(filepath('save.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Save this Model',uname=prefix+'SAVE') 
  wDisplayBase=widget_base(base,/row)      
  wTable = WIDGET_TABLE(wDisplayBase, VALUE=data, /ROW_MAJOR, $
    ROW_LABELS=string(lindgen(count)), COLUMN_LABELS=labels, /RESIZEABLE_COLUMNS,y_scroll_size=count<15,$
    column_width=[max_id,max_time],/DISJOINT_SELECTION,uname=prefix+'TABLE',uvalue=model,/ALL_EVENTS)
  wDrawBase=Widget_Base(wDisplayBase,/row)
  wDraw= widget_draw(wDrawBase, $
    xsize=XSIZE, $
    ysize=XSIZE, $
    uvalue=selection, $
    Uname=prefix+'DRAW')   
  ; Realize the widgets.
  WIDGET_CONTROL, base, /REALIZE
  map=(*(model->Refmaps()))->get(0,/map)
  widget_control,wDraw,get_value=wid
  if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'
  wset,wid
  plot_map,map,grid=5

  ; Call XMANAGER to manage the widgets.
  XMANAGER, 'gx_refmap_manager', base,no_block=~keyword_set(modal)

END
