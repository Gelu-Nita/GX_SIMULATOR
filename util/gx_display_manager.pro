pro display_freq,selected,tlb
  widget_control,/hourglass
  wsurface=widget_info(tlb,find_by_uname='select_surface')
  wopacity=widget_info(tlb,find_by_uname='select_opacity')
  wstyle=widget_info(tlb,find_by_uname='select_style')
  wshow=widget_info(tlb,find_by_uname='show_s')
  wpallete=widget_info(tlb,find_by_uname='pallete')
  widget_control,wsurface,get_value=surfaces
  widget_control,wopacity,get_value=opacities
  widget_control,wstyle,get_value=styles
  widget_control,wshow,get_value=show
  widget_control,wpallete,get_uvalue=ct
  selected_surface=widget_info(wsurface,/combobox_gettext)
  selected_style=widget_info(wstyle,/combobox_gettext)
  selected_opacity=widget_info(wopacity,/combobox_gettext)
  opacity=(where(opacities eq selected_opacity))[0]
  surface=(where(surfaces eq selected_surface))[0]
  case selected_style of
    'Points':style=0
    'Lines':style=1
    else:style=2
  end
  for k=0,selected->Count()-1 do begin
    selected[k]->SetProperty,style=style
    selected[k]->SetOpacity,opacity
    if show[k] then selected[k]->Display,surface,ct=ct else selected[k]->SetProperty,hide=1
  end
end

pro SortModelObjects, model,iso=iso,freq=freq,non_iso=non_iso
  if n_elements(iso) ne 0 then if obj_valid(iso) then obj_destroy,iso
  iso=[]
  non_iso=[]
  nlines=0
  niso=0
  freq=[]
  all=model->get(/all,count=count)
  for k=0,count-1 do begin
    all[k]->GetProperty,name=name
    if ~obj_isa(all[k],'gxisogauss') then begin
      if  ~obj_isa(all[k],'gxbline') and ~obj_isa(all[k],'gxCorona') then non_iso=[non_iso,name] $
      else if ~obj_isa(all[k],'gxcorona') then nlines+=1
    endif else begin
      iso=[iso,all[k]]
      freq=[freq,all[k]->GetFreq()]
      niso+=1
    endelse
  endfor
  if nlines gt 0 then non_iso=['Field Lines',non_iso]
  if niso gt 0 then begin
    idx=sort(freq)
    iso=list(iso[idx],/extract)
    freq=list(freq[uniq(freq,idx)],/extract)
  endif else begin
    iso=list()
    freq=list()
  endelse
  all=model->get(/all,isa='gxisogauss',count=count)
  if count gt 0 then begin
    model->Remove,all
    model->Add,all
  endif
end

function GetSelectedIso,iso,freq,info=info
  dummy=execute('selected=iso.filter(Lambda(x:x.getFreq() eq '+string(freq,format="(g0.10)")+'))')
  if selected->Count() eq 0 then return,!null
  s=[]
  sn=[]
  b=[]
  for k=0,selected->Count()-1 do begin
    selected[k]->GetProperty,hide=hide
    s=[s,~hide]
    b=[b,selected[k]->GetB()]
    sn=[sn,string(selected[k]->GetS(),b[k],format="('s',i0,'(',i0,'G)')")]
  endfor
  idx=reverse(sort(b))
  info={strings:sn[idx],values:s[idx]}
  return,list((selected->ToArray())[idx],/extract)
end


pro gx_display_manager_update_state,state,iso,freqs
  if iso->Count() gt 0 and freqs->Count() gt 0  then begin
    items=string(freqs->ToArray(),format="('iso ',g0,' Ghz')")
    obj_destroy,state.iso
    state.iso=iso
    state.freq=freqs
  endif
end

pro gx_display_manager_event,event
  widget_control,event.top,get_uvalue=state
  subdirectory=['resource', 'bitmaps']
  CASE TAG_NAMES(event, /STRUCTURE_NAME) OF
    'WIDGET_KILL_REQUEST':goto,exit
    else:
  ENDCASE
  case widget_info(event.id,/uname) of
   'exit':begin
                     exit:
                     widget_control,event.top,get_uvalue=modal
                     answer=keyword_set(modal)?'YES':dialog_message(/question,'Do you want to exit this application?')
                     if strupcase(answer) eq 'YES' then begin
                       if ~keyword_set(modal) then begin
                         ;          wTable=widget_info(event.top,find_by_uname='TABLE')
                         ;          widget_control,wTable,get_uvalue=model
                         ;          obj_destroy,model
                       end
                       ;        wUndo=widget_info(event.top,find_by_uname='UNDO')
                       ;        widget_control,wUndo,get_uvalue=uvalue
                       ;        obj_destroy,uvalue
                       WIDGET_CONTROL, event.TOP, /DESTROY
                       return
                    end
                    end
      'save': begin
                      model=state.model
                      file=dialog_pickfile(filter='*.gxm',$
                        DEFAULT_EXTENSION='gxm',$
                        /write,/OVERWRITE_PROMPT,$
                        title='Please select a file to save current model configuration')
                      if file ne '' then save,model,file=file,/compress
                    end              
      'select_freq':begin
                       if state.iso->count() gt 0 then begin
                          for k=0,state.iso->Count()-1 do state.iso[k]->SetProperty,hide=1
                          state.ODisplay->Draw, /hourglass
                          selected=GetSelectedIso(state.iso,state.freq[event.index],info=info)
                          widget_control,event.id,set_uvalue=selected
                          wshow_s=widget_info(event.top,find_by_uname='show_s')
                          if widget_valid(wshow_s) then widget_control,wshow_s,/destroy
                          wShowS=cw_bgroup(widget_info(event.top,find_by_uname='iso_s'),$
                           info.strings,/nonexclusive,/row,set_value=info.values,$
                           label_left='Show:',font=font,uname='show_s')   
                           display_freq,selected,event.top   
                           refresh=1
                       end
                      end   
      'select_surface':begin
                        widget_control,widget_info(event.top,find_by_uname='select_freq'),get_uvalue=selected
                        display_freq,selected,event.top
                        refresh=1
                      end                               
      'select_opacity':begin
                         widget_control,widget_info(event.top,find_by_uname='select_freq'),get_uvalue=selected
                         display_freq,selected,event.top   
                         refresh=1             
                       end 
      'select_style':begin
                         widget_control,widget_info(event.top,find_by_uname='select_freq'),get_uvalue=selected
                         display_freq,selected,event.top
                         refresh=1
                       end          
      'show_s':begin
                    widget_control,widget_info(event.top,find_by_uname='select_freq'),get_uvalue=selected
                    widget_control,event.id,get_value=show
                    for k=0,selected->count()-1 do selected[k]->SetProperty,hide=~show[k]
                    display_freq,selected,event.top
                    refresh=1
                  end  
       'pallete':begin
                 tvlct,rgb_curr,/get
                 xloadct,/silent,/block,/use_current
                 tvlct,rgb,/get
                 widget_control,event.id,set_uvalue=rgb
                 tvlct,rgb_curr
                 widget_control,widget_info(event.top,find_by_uname='select_freq'),get_uvalue=selected
                 display_freq,selected,event.top
                 refresh=1
                 end  
        'select_others':begin
                     selected_obj=widget_info(event.id,/combobox_gettext)
                     if selected_obj eq 'Field Lines' then $
                      obj=state.model->Get(/all,isa='gxbline') else obj=state.model->GetByName(selected_obj)
                      obj[0]->GetProperty,hide=hide
                      widget_control,widget_info(event.top,find_by_uname='show_obj'),set_value=~hide?gx_bitmap(filepath('image.bmp', subdirectory=subdirectory)):$
                        gx_bitmap(filepath('eye_closed.bmp', subdirectory=subdirectory)),set_uvalue=obj
                    end 
        'show_obj':begin
                    widget_control,widget_info(event.top,find_by_uname='show_obj'),set_value=~event.select?gx_bitmap(filepath('image.bmp', subdirectory=subdirectory)):$
                    gx_bitmap(filepath('eye_closed.bmp', subdirectory=subdirectory)),get_uvalue=obj
                    for k=0,n_elements(obj)-1 do obj[k]->SetProperty,hide=event.select
                    refresh=1
                   end      
         'iso_gen':widget_control,widget_info(event.top,find_by_uname='iso_parms'),map=1  
         'iso_cancel':widget_control,widget_info(event.top,find_by_uname='iso_parms'),map=0 
         'iso_ok':begin
                   widget_control,widget_info(event.top,find_by_uname='iso_freq'),get_value=freq
                   widget_control,widget_info(event.top,find_by_uname='iso_max_s'),get_value=s
                   widget_control,/hourglass
                   state.model->AddGyroLayers,freq,smax=s,hide=0
                   SortModelObjects, state.model,iso=iso,freq=freqs,non_iso=non_iso
                   gx_display_manager_update_state,state,iso,freqs
                   idx=(where(state.freq->ToArray() eq freq))[0]>0
                   w_select_freq=widget_info(event.top,find_by_uname='select_freq')
                   items=string(state.freq->ToArray(),format="('iso ',g0,' Ghz')")
                   widget_control,w_select_freq,set_value=items
                   widget_control,w_select_freq,send_event={id:0l,top:0l,handler:0l,index:idx}
                   widget_control,widget_info(event.top,find_by_uname='iso_parms'),map=0  
                  end  
         'iso_remove':begin
                        answ=dialog_message('Do you want to delete the selected isogauss surface?',/question)
                        if strupcase(answ) eq 'YES' then begin
                          w_select_freq=widget_info(event.top,find_by_uname='select_freq')
                          widget_control,widget_info(event.top,find_by_uname='show_s'),get_value=selected_idx
                          widget_control,w_select_freq,get_uvalue=selected
                          remove_idx=where(selected_idx eq 1,count)
                          if count gt 0 then begin
                          freq=selected[remove_idx[0]]->GetFreq()
                          for k=0,count-1 do begin
                           state.model->Remove,selected[remove_idx[k]]
                           obj_destroy,selected[remove_idx[k]]
                          endfor
                          SortModelObjects, state.model,iso=iso,freq=freqs,non_iso=non_iso
                          gx_display_manager_update_state,state,iso,freqs
                          idx=(where(state.freq->ToArray() eq freq))[0]>0
                          items=string(state.freq->ToArray(),format="('iso ',g0,' Ghz')")
                          widget_control,w_select_freq,set_value=items
                          widget_control,w_select_freq,send_event={id:0l,top:0l,handler:0l,index:idx}
                          widget_control,widget_info(event.top,find_by_uname='iso_parms'),map=0
                          end
                        end
                      end                                                                                             
    else:
  endcase
  widget_control,event.top,set_uvalue=state
  if keyword_set(refresh) then state.ODisplay->Draw, /hourglass
end  


PRO gx_display_manager,model,modal=modal,selection=selection,xsize=xsize,ysize=ysize,scale=scale
  if n_elements(model) eq 0 then model=gx_read() else  $
    if ~obj_isa(model,'gxmodel') then model=gx_read()
  if ~obj_isa(model,'gxmodel') then begin
    message,'No valid model selected, no action performed!',/info
    return
  endif
  all=model->get(/all,count=count)
  device, get_screen_size=scr
  gx_setfonts,_extra=_extra
  if scr[0] lt 3200 then nb=16 else nb=32
  font=!defaults.font
  if not exist(xsize) then xsize = fix (scr[0] * .3)
  if not exist(ysize) then ysize = xsize
  subdirectory=['resource', 'bitmaps']
  prefix=''
  tlb = WIDGET_BASE(/COLUMN,/TLB_KILL_REQUEST_EVENTS,$
    title='GX Display Manager: ' +model->GetId(),uvalue=keyword_set(modal))
  SortModelObjects, model,iso=iso,freq=freq,non_iso=non_iso
  if iso->Count() gt 0 and freq->Count() gt 0  then begin
    items=string(freq->ToArray(),format="('iso ',g0,' Ghz')")
    selected=GetSelectedIso(iso,freq[0],info=info)
  endif else begin
    items=['iso 0.00000 Ghz']
    selected=list()
    info={strings:'',values:0}
  endelse
  wDisplayBase=widget_base(tlb,/row)


wObjviewWidBase=widget_base(tlb,/column)
wMenuBase=widget_base(wObjviewWidBase,/row,uname='xobjview:menu',/toolbar)
wToolbarBase=widget_base(wObjviewWidBase,/row,uname='xobjview:toolbar',/toolbar)
wSaveModel= widget_button(wToolbarBase, font=font, $
    value=gx_bitmap(filepath('save.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Save this Model',uname='save')
wGenerateIsoSurface=widget_button(wToolbarBase,font=font,  $
      value=gx_bitmap(filepath('surface.bmp', subdirectory=subdirectory)), $
      /bitmap,tooltip='Generate isogauss surface',uname=prefix+'iso_gen')
   ;  wImportBaseMap= widget_button(font=font, wToolbarBase, $
  ;    value=gx_bitmap(gx_findfile('basemap.bmp')), $
  ;    /bitmap,tooltip='Import BASE reference map',uname=prefix+'BASEMAP')

  ;  wUndo= widget_button(font=font, wToolbarBase, $
  ;    value=gx_bitmap(filepath('undo.bmp', subdirectory=subdirectory)), $
  ;    /bitmap,tooltip='Undo last action',uname=prefix+'UNDO',uvalue=obj_new(),sensitive=0)   
    

wSelectBase=widget_base(wObjviewWidBase,/row,uname='select')
all[0]->GetProperty,hide=hide

default,scale,1.2
oObjviewWid = obj_new('IDLexObjviewWid', $
  wObjviewWidBase, $
  model, $
  menu_parent=wMenuBase, $
  toolbar_parent=wToolbarBase, $
  scale=scale, $
  draw_xsize=xsize, $
  draw_ysize=ysize, $
  background=background, $
  stationary=oStationary, $
  double_view=double_view, $
  renderer=renderer, $
  use_instancing=use_instancing, $
  /include_refresh_button, $
  /include_full_reset_button, $
  debug=debug $
  )
  
  widget_control,wToolbarBase,set_uvalue=oObjviewWid
  prefix='xobjview:'
  wQuit=widget_info(wMenuBase,find_by_uname=prefix + 'Quit')
  file_menu=widget_info(wQuit,/parent)
  widget_control,wQuit,/destroy
  wQuitButton = widget_button( $
    file_menu, $
    /separator, $
    uname='exit', $
    value='Exit' $
    )
  names=['reset','rotate','zoom','pan','select']
  for k=0,n_elements(names)-1 do begin
      button=widget_info(wToolbarBase,find_by_uname=prefix+names[k])
      if widget_valid(button) then widget_control,button, set_value=gx_bitmap(filepath(names[k]+'.bmp', subdirectory=subdirectory))
  endfor
  wOthersBase=widget_base(wMenuBase,/frame,/row)
  wObjSelect=widget_combobox(widget_base(wOthersBase),value=non_iso,font=font,uname='select_others')
  if non_iso[0] eq 'Field Lines' then $
     obj=model->Get(/all,isa='gxbline') else obj=model->GetByName(non_iso[0])
  obj[0]->GetProperty,hide=hide
  wOtherObj= widget_button(widget_base(wOthersBase,/nonexclusive), font=font,$
    value=~hide?gx_bitmap(filepath('image.bmp', subdirectory=subdirectory)):$
    gx_bitmap(filepath('eye_closed.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='hide/unhide this object class',uname='show_obj',uvalue=obj)
    
  wIsoParmsBase=widget_base(wToolbarBase,/row,uname='iso_parms',map=0)
  wIsoFreq=cw_ObjField(wIsoParmsBase,value=[1],unit='GHz',label='Freq',xtextsize=10,uname='iso_freq')
  wIsoFreq=cw_ObjField(wIsoParmsBase,value=3,unit='',label='max s',xtextsize=2,uname='iso_max_s',$
    inc=1,type=1L,min=2,max=10,tooltip='Maxmum garoresonance harmonics to geneate a isogauss surface for' )
  wIsoOK= widget_button(font=font, wIsoParmsBase, $
    value='OK',tooltip='Generate isosurface with this parms',uname='iso_ok')
  wIsoCancel= widget_button(font=font, wIsoParmsBase, $
      value=gx_bitmap(filepath('delete.bmp', subdirectory=subdirectory)), $
      /bitmap,tooltip='Cancel isosurface generation',uname='iso_cancel')

  
  
  wSurfaceBase=widget_base(wSelectBase,/column)
  wPropertiesBase=widget_base(wSurfaceBase,/frame,/row,uname='iso_prop')
  wSbase=widget_base(wSurfaceBase,/frame,/row,uname='iso_s')
  wPalette = widget_button(wSbase, $
    value=gx_bitmap(filepath('palette.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Change Color Table',uname='pallete', uvalue=39)
    
  wRemove= widget_button(wSbase, font=font,$
    value=gx_bitmap(filepath('delete.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Remove selected surfaces',uname='iso_remove')
  wShowS=cw_bgroup(wSbase,info.strings,/nonexclusive,/row,set_value=info.values,$
    label_left='Show:',font=font,uname='show_s')  
 
  widget_control,wSurfaceBase,map=(n_elements(items) gt 0)
  wObjSelect=widget_combobox(font=font, wPropertiesBase,value=items,uname='select_freq',uvalue=selected)
  wLabel=widget_label(wPropertiesBase, font=font,value='Surface:')          
  wSelect_Surface=widget_combobox(font=font,  wPropertiesBase,value=['Te','Tb_LCP','Tb_RCP'],uname='select_surface')
  wLabel=widget_label(wPropertiesBase, font=font,value='Opacity:')
  wSelect_Opacity=widget_combobox(wPropertiesBase, font=font,value=['Optical Depth','Opaque'],uname='select_opacity')
  wLabel=widget_label(wPropertiesBase, font=font,value='Style:')
  wSelect_style=widget_combobox(wPropertiesBase, font=font,value=['Points','Lines','Filled'],uname='select_style')
  widget_control,wSelect_style,set_combobox_select=2
  

  
  
  widget_control,tlb,set_uvalue={oDisplay:oObjviewWid,iso:iso,non_iso:non_iso,freq:freq,model:model}
  ; Realize the widgets.
  WIDGET_CONTROL, tlb, /REALIZE
  ; Call XMANAGER to manage the widgets.
  XMANAGER, 'gx_display_manager', tlb,no_block=~keyword_set(modal)

END
