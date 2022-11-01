function gxObjViewWid::Init,wParent, oSubjects, toolbar_parent=toolbar_parent,_extra=_extra
 
 Zoom2ViewBase=widget_base(toolbar_parent,/toolbar,/row)
 
 self.wZoom2View=widget_button(Zoom2ViewBase,$
              value=gx_bitmap(filepath('find.bmp', subdirectory=['resource', 'bitmaps'])), $
              /bitmap,tooltip='Zoom to FOV',sensitive=1)                  
 result=self->IDLexObjViewWid::Init(wParent,oSubjects, toolbar_parent=toolbar_parent,_extra=_extra)
 if result eq 1 then begin
   widget_control,self.wLabel,/destroy
   self.wExtraToolbarBase=widget_base(self.wToolbarBase,/toolbar,/row)
   self.wLabel = widget_label(self.wToolbarBase, value=' ', /dynamic_resize,font=!defaults.font)
   WIDGET_CONTROL,self.wReset,/bitmap,set_value=gx_bitmap(gx_findfile('sun.bmp')),tooltip='Full Sun view'
   ViewBase=self.wExtraToolbarBase
   self.wZoomIn=widget_button(ViewBase,$
     value=gx_bitmap(filepath('zoom_in.bmp', subdirectory=['resource', 'bitmaps'])), $
     /bitmap,tooltip='Zoomin',sensitive=1)
   self.wZoomOut=widget_button(ViewBase,$
     value=gx_bitmap(filepath('zoom_out.bmp', subdirectory=['resource', 'bitmaps'])), $
     /bitmap,tooltip='Zoomout',sensitive=1)
   self.wXY= widget_button( ViewBase, $
              value='XY',tooltip='Show XY view',font=!defaults.font $
              )
   self.wXZ= widget_button( ViewBase, $
              value='XZ',tooltip='Show XZ view',font=!defaults.font $
              )
   self.wZY= widget_button( ViewBase, $
              value='ZY',tooltip='Show ZY view',font=!defaults.font $
              )
   self.wMovie=widget_button( ViewBase, $
              value=gx_bitmap(filepath('eba_meth_ex_cm.bmp', subdirectory=*self.pBitmapPath)), $
              /bitmap,tooltip='Generate Rotating Model Video')        
   self.wFieldline=widget_button(widget_info(self.wRotate,/parent),$
              value=gx_bitmap(filepath('roi.bmp', subdirectory=*self.pBitmapPath)), $
              /bitmap,tooltip='Create Field Line',sensitive=0)
   self.wRemove= widget_button( widget_info(self.wRotate,/parent), $
              value=gx_bitmap(filepath('roi_active.bmp', subdirectory=*self.pBitmapPath)), $
              /bitmap,tooltip='Remove Field Line')
   self.wFluxtube=widget_button(widget_info(self.wRotate,/parent),$
              value=gx_bitmap(filepath('polar.bmp', subdirectory=*self.pBitmapPath)), $
              /bitmap,tooltip='Create Flux Tube')

  self.wBaseMapContextMenu= WIDGET_BASE(self.wDraw, /CONTEXT_MENU, UNAME="BaseMapContextMenu")
  wCreateButton=Widget_Button(self.wBaseMapContextMenu,Value='Create field line',uname='BASEMAP:CREATEFIELDLINE')
  wSeedButton=Widget_Button(self.wBaseMapContextMenu,Value='Seed field lines at this basemap location',uname='BASEMAP:CREATESEEDEDFIELDLINES')
  
  self.wBlineContextMenu= WIDGET_BASE(self.wBase, /CONTEXT_MENU, UNAME="BlineContextMenu")

  wLockButton=Widget_Button(self.wBlineContextMenu,Value='Lock',uname='BLINE:LOCK')
  wSeedButton=Widget_Button(self.wBlineContextMenu,Value='Seed field lines at top of this line',uname='BLINE:CREATESEEDEDFIELDLINES')
  wCreateButton=Widget_Button(self.wBlineContextMenu,Value='Create flux tube',uname='BLINE:CREATEFLUXTUBE')
  wDeleteButton=Widget_Button(self.wBlineContextMenu,Value='Delete field line',uname='BLINE:DELETE')


  self.wFluxTubeContextMenu= WIDGET_BASE(self.wBase, /CONTEXT_MENU, UNAME="FluxTubeContextMenu")
  wDeleteButton=Widget_Button(self.wFluxTubeContextMenu,Value='Delete flux tube',uname='FLUXTUBE:DELETE')
  wLockButton=Widget_Button(self.wFluxTubeContextMenu,Value='Lock',uname='FLUXTUBE:LOCK')

   if not self.own_toolbar then begin
     widget_control, $
          self.wZoom2View, $
          event_func='IDLexWidget__HandleEvent', $
          set_uvalue=self
      widget_control, $
          self.wXY, $
          event_func='IDLexWidget__HandleEvent', $
          set_uvalue=self
      widget_control, $
          self.wXZ, $
          event_func='IDLexWidget__HandleEvent', $
          set_uvalue=self
      widget_control, $
          self.wZY, $
          event_func='IDLexWidget__HandleEvent', $
          set_uvalue=self
      widget_control, $
          self.wMovie, $
          event_func='IDLexWidget__HandleEvent', $
          set_uvalue=self    
      widget_control, $
          self.wFieldline, $
          event_func='IDLexWidget__HandleEvent', $
          set_uvalue=self
      widget_control, $
          self.wFluxtube, $
          event_func='IDLexWidget__HandleEvent', $
          set_uvalue=self
      widget_control, $
          self.wRemove, $
          event_func='IDLexWidget__HandleEvent', $
          set_uvalue=self
      widget_control, $
          self.wZoomIn, $
          event_func='IDLexWidget__HandleEvent', $
          set_uvalue=self  
      widget_control, $
          self.wZoomOut, $
          event_func='IDLexWidget__HandleEvent', $
          set_uvalue=self
      end
 endif

 return,result
end

pro gxObjViewWid::SetModelView,view
 self.ModelView=view
 WIDGET_CONTROL,self.wReset,/bitmap,set_value=self.ModelView?$
  gx_bitmap(filepath('dataspace.bmp', subdirectory=['resource', 'bitmaps'])):$
  gx_bitmap(gx_findfile('sun.bmp')),$
 tooltip=self.ModelView?'Perspective view':'Full Sun view'
end


pro gxObjViewWid::Remove,item
 self.oViewgroup->CallCurrent,'Remove',item
end

pro gxObjViewWid::Add,item
 self.oViewgroup->CallCurrent,'Add',item
 self->Reset,/full
end

pro gxObjViewWid::Reset,full=full
 self.oViewgroup->CallAll, 'Reset', self.oWindow, isa='IDLexObjview',full=full
end

pro gxObjViewWid::Rotate,axis,angle
 self.oViewgroup->CallAll, 'Rotate',axis,angle
end

pro gxObjViewWid::FieldLineButton,active=active
 widget_control,self.wFieldline,sensitive=keyword_set(active)
end

pro gxObjViewWid::Zoom2ViewButton,active=active
 widget_control,self.wZoom2View,sensitive=keyword_set(active)
end

function gxObjViewWid::HandleEvent, event
compile_opt hidden
on_error, 2 ; Return to caller on error.
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
 'BASEMAP:CREATEFIELDLINE':begin
      		                   wait,0.2
      		                   widget_control,self.wBaseMapContextMenu,get_uvalue=info
      		                   result=self.oWindow->PickData(self.oViewgroup->Get(/current),$
      		                   info.oSelected,[info.location.x,info.location.y],xyz)
      		                   info.oSelected->GetProperty,parent=oModel
      		                   oModel->CreateBline,xyz
      		                   self->Draw
                           end
 'BASEMAP:CREATESEEDEDFIELDLINES':begin
                             wait,0.2
                             widget_control,self.wBaseMapContextMenu,get_uvalue=info
                             result=self.oWindow->PickData(self.oViewgroup->Get(/current),$
                               info.oSelected,[info.location.x,info.location.y],xyz)
                             info.oSelected->GetProperty,parent=oModel
                             oModel->AddBLines,x0=xyz[0],y0=xyz[1],z0=xyz[2]
                             self->Draw
                           end  
                           
 'BLINE:CREATESEEDEDFIELDLINES':begin
                             wait,0.2
                             widget_control,self.wBlineContextMenu,get_uvalue=info
                             info.parent->AddBLines,x0=info.top[0],y0=info.top[1],z0=info.top[2]
                             self->Draw
                           end                                                  
 'BLINE:DELETE':begin
                    wait,0.2
                    widget_control,self.wBlineContextMenu,get_uvalue=oSelected
                    oSelected->GetProperty,Parent=model
                    if obj_isa(model,'GXMODEL') then begin
	                    model->Remove,oSelected
	                    obj_destroy,oSelected
                    end
                    self->Draw
                end
 'BLINE:LOCK':begin
                    wait,0.2
                    widget_control,event.id,get_uvalue=lock
                    widget_control,self.wBlineContextMenu,get_uvalue=oSelected
                    oSelected->SetProperty,lock=lock
                    self->Draw
                end

 'BLINE:CREATEFLUXTUBE':begin
		                    wait,0.2
		                    widget_control,self.wBlineContextMenu,get_uvalue=oSelected
		                    oSelected->GetProperty,Parent=model
		                    if obj_isa(model,'GXMODEL') then begin
		                        self->Draw
		                        widget_control,/hour
		                        model->CreateFluxtube,oSelected
		                    end
		                    self->Draw
                        end
 'FLUXTUBE:DELETE':begin
	                 if dialog_message('Do you want to destroy this fluxtube?',/question) eq 'Yes' then begin
	                    widget_control,self.wFluxTubeContextMenu,get_uvalue=oSelected
	                    oSelected->GetProperty,Parent=model,name=name
	                    if obj_isa(model,'GXMODEL') then begin
	                      model->GetProperty,wParent=wParent
	                      wid=widget_info(wParent,find_by_uname=name)
                        if widget_valid(wid) then begin
                          widget_control,wid,/destroy
                        endif else if obj_valid(oSelected) then obj_destroy,oSelected
                        model->SetRoi
	                      self->Draw
	                    end
		             end
                   end
  'FLUXTUBE:LOCK':begin
                    wait,0.2
                    widget_control,self.wFluxTubeContextMenu,get_uvalue=oSelected
                    widget_control,event.id,get_uvalue=lock
                    oSelected->SetProperty,lock=lock
                    oSelected->GetProperty,Parent=model,name=name
                    if obj_isa(model,'GXMODEL') then begin
	                  model->GetProperty,wParent=wParent
	                  if widget_valid(wParent) then begin
  	                  FluxTubeBase=widget_info(wParent,find_by_uname=name)
  	                  if widget_valid(FluxTubeBase) then begin
  	                   wlock=widget_info(FluxTubeBase,find_by_uname='GXFLUXTUBE:LOCK')
  	                   if widget_valid(wlock) then widget_control,wlock,set_button=lock,set_value=lock?$
  	                     gx_bitmap(gx_findfile('lock.bmp')):gx_bitmap(gx_findfile('unlock.bmp')),/bitmap
                       widget_control,widget_info(FluxTubeBase,find_by_uname='GXFLUXTUBE:REMOVE'),sensitive=~lock
                      end 
                    end
	                end
                    self->Draw
                end
else:
endcase
case event.id of
 self.wZoom2View:begin
           oCurrent = self.oViewgroup->Get(/current)
           oSun=oCurrent->Get(/all)
           scanbox=oSun->GetByname('scanboxobject')
           ;(oSun->GetByname('Solar Grid'))->SetProperty,hide=1
           r=scanbox->GetLocation()
           void=get_obj_range(osun,ocurrent,odestination,range)
           suncenter=total(range,2)/2
           r=r-suncenter
           self->Reset,/full
           oCurrent->translate,-r[0],-r[1],0
           fov=scanbox->getfov()
           if fov ne 0 then ocurrent->zoom,3/fov
           self->Draw
          end
 self.wZoomIn:begin   
              self->Zoom,2
              self->Draw
             end 
 self.wZoomOut:begin
               self->Zoom,0.5
               self->Draw
              end                  
 self.wXY:begin
           self->Reset,/full
           ;self->Zoom
           self->Draw
          end
 self.wXZ:begin
           self->Reset,/full
           self->Rotate,[1,0,0],-90
           ;self->Zoom
           self->Draw
          end
 self.wZY:begin
           self->Reset,/full
           self->Rotate,[1,0,0],-90
           self->Rotate,[0,1,0],-90
           ;self->Zoom
           self->Draw
          end
 self.wMovie:self->OnMovie         
 self.wFieldline: begin
         ; Set the button state if called manually.
        if (WIDGET_INFO(self.wFieldline, /BUTTON_SET) ne event.select) then $
            WIDGET_CONTROL, self.wFieldline, SET_BUTTON=event.select
        self.mode = 'fieldline'
        self.oViewgroup->CallAll, $
            'SetProperty', $
            mode='select', $
            isa='IDLexObjview'
        end
 self.wRemove: begin
         ; Set the button state if called manually.
        if (WIDGET_INFO(self.wRemove, /BUTTON_SET) ne event.select) then $
            WIDGET_CONTROL, self.wRemove, SET_BUTTON=event.select
        self.mode = 'remove'
        self.oViewgroup->CallAll, $
            'SetProperty', $
            mode='select', $
            isa='IDLexObjview'
        end
 self.wFluxtube: begin
         ; Set the button state if called manually.
        if (WIDGET_INFO(self.wFluxtube, /BUTTON_SET) ne event.select) then $
            WIDGET_CONTROL, self.wFluxtube, SET_BUTTON=event.select
        self.mode = 'fluxtube'
        self.oViewgroup->CallAll, $
            'SetProperty', $
            mode='select', $
            isa='IDLexObjview'
        end
else:
endcase
if event.id eq  self.wReset and self.ModelView eq 1 then begin
 ;overwrite the original behavioor of the Reset button
 self->Reset,/full
 self->Rotate, [0,0,1],  30
 self->Rotate, [1,0,0], -60
 self->Draw
 result=self->Rewrite(event)
 return,result
endif  
result=self->IDLexObjviewWid::HandleEvent(event)
return,result
end

;--------------------------------------------------------------------
pro gxObjViewWid::OnMouseDown, event
compile_opt hidden
case event.press of
    4: begin ; Right mouse-button.
         if (WIDGET_INFO(self.wSelect, /BUTTON_SET) ne 1) then $
            WIDGET_CONTROL, self.wSelect, SET_BUTTON=1
         self.mode = 'select'
         self.oViewgroup->CallAll, $
            'SetProperty', $
            mode=self.mode, $
            isa='IDLexObjview'
         if (self.mode eq 'select') or (self.mode eq 'fieldline') or (self.mode eq 'remove') or (self.mode eq 'fluxtube') then begin
           self.oViewgroup->SetCurrent, event
           oCurrent = self.oViewgroup->Get(/current)
           if obj_valid(oCurrent) then begin
            if obj_isa(oCurrent, 'IDLexObjview') then $
                void = oCurrent->Update(event)
            end
           self.oViewgroup->CallCurrent, $
                  'GetProperty', $
                  isa='IDLexObjview', $
                  selected=oSelected
           oSelected=oSelected[0]
           if obj_valid(oSelected) then begin
           case 1 of
            obj_isa(oSelected,'gxBline'): begin
              oSelected->GetProperty,lock=lock,center=center
              widget_control,widget_info(self.wBlineContextMenu,find_by_uname='BLINE:DELETE'),sensitive=(~lock and ~center)
              widget_control,widget_info(self.wBlineContextMenu,find_by_uname='BLINE:LOCK'),$
              set_value=lock?'Unlock':'Lock',set_uvalue=~lock
              widget_control,self.wBlineContextMenu,set_uvalue=oSelected
              WIDGET_DISPLAYCONTEXTMENU, event.id , event.X, event.Y, self.wBlineContextMenu
            end
            obj_isa(oSelected,'gxFluxtube'): begin
             oSelected->GetProperty,lock=lock
             widget_control,widget_info(self.wFluxTubeContextMenu,find_by_uname='FLUXTUBE:DELETE'),sensitive=~lock
             widget_control,widget_info(self.wFluxTubeContextMenu,find_by_uname='FLUXTUBE:LOCK'),$
             set_value=lock?'Unlock':'Lock',set_uvalue=~lock
             widget_control,self.wFluxTubeContextMenu,set_uvalue=oSelected
             WIDGET_DISPLAYCONTEXTMENU, event.id, event.X, event.Y, self.wFluxTubeContextMenu
            end
            obj_isa(oSelected,'IDLgrImage'): begin
              widget_control,self.wBaseMapContextMenu,set_uvalue={location:event,oSelected:oSelected}
              WIDGET_DISPLAYCONTEXTMENU, event.id, event.X, event.Y, self.wBaseMapContextMenu
            end
            else:
           endcase
           end
         end
       end
    2: begin ; Middle mouse-button.
        end
    1: begin ; Left mouse button.
        self.oViewgroup->SetCurrent, event
        oCurrent = self.oViewgroup->Get(/current)
        if obj_valid(oCurrent) then begin
            if obj_isa(oCurrent, 'IDLexObjview') then $
                void = oCurrent->Update(event)
            end
        if self.mode eq 'select' or self.mode eq 'fieldline' or self.mode eq 'fluxtube' or self.mode eq 'remove' then begin
            self.oViewgroup->CallCurrent, $
                'GetProperty', $
                isa='IDLexObjview', $
                selected=oSelected

            if (obj_valid(oSelected[0])) then begin
                oSelected=oSelected[0]
                oSelected->GetProperty, name=name
                if name eq '' then name = obj_class(oSelected)
                     case self.mode of
                     'fieldline': begin
                                    if obj_isa(oSelected,'IDLGRIMAGE') then begin
                                       self->Draw
                                       result=self.oWindow->PickData(oCurrent,oSelected,[event.x,event.y],xyz)
                                       oSelected->GetProperty,parent=oModel
                                       print,xyz
                                       oModel->CreateBline,xyz
                                       self->Draw
                                     end
                                  end
                     'remove': begin
                                    if obj_isa(oSelected,'GXBLINE') then begin
	                                    oSelected->GetProperty,Parent=model
	                                    if obj_isa(model,'GXMODEL') then begin
		                                    model->Remove,oSelected
		                                    obj_destroy,oSelected
		                                    name=''
	                                    end
	                                    self->Draw
                                   end
                                 end
                     'fluxtube': begin
                                    if obj_isa(oSelected,'GXBLINE') then begin
	                                    oSelected->GetProperty,Parent=model
	                                    if obj_isa(model,'GXMODEL') then begin
		                                    oSelected->SetProperty,color=[255,0,0]
		                                    self->Draw
		                                    widget_control,/hour
		                                    model->CreateFluxtube,oSelected
	                                    end
	                                    self->Draw
                                    end
                                 end
                     else:
                     endcase
                endif $
            else begin
                name = ' '
                endelse

            widget_control, $
                self.wLabel, $
                set_value=name
            endif $
        else begin
            self.oWindow->SetProperty, $
                quality=self.drag_quality
            self->Draw, /in_motion
            endelse
        end
    else:
    endcase
end

pro gxObjViewWid::Zoom,zoomfactor
  if n_elements(zoomfactor) eq 1 then self.zoomfactor=zoomfactor else  self.zoomfactor=1
  oCurrent = self.oViewgroup->Get(/current)
  oCurrent->Zoom,self.zoomfactor
end

function gxObjViewWid::GetSelected
 self.oViewgroup->CallCurrent, $
                'GetProperty', $
                isa='IDLexObjview', $
                selected=oSelected
 if (obj_valid(oSelected))[0] then return, oSelected[0] else return,obj_new()
end

pro gxObjViewWid::GetProperty,wExtraToolbarBase=wExtraToolbarBase,_extra=_extra
 wExtraToolbarBase=self.wExtraToolbarBase
end

;--------------------------------------------------------------------
pro gxObjViewWid::OnMovie
                  oCurrent = self.oViewgroup->Get(/current)
                  Model=oCurrent->Get(/all)
                  if obj_isa(model,'GXMODEL') then begin
                    model->Reset
                    model->Rotate,[1,0,0],15
                    self->Reset,/full
                    self->Rotate,[1,0,0],-90
                    self->Zoom,1.4
                    model->getproperty,transform=ctm
                    ez=gx_transform([0,0,1],ctm)-gx_transform([0,0,0],ctm)
                    model->GetProperty,xrange=xrange,yrange=yrange
                    model->Translate,xrange[0],yrange[0],0
                    oCurrent->Translate,-xrange[0],-yrange[0],0
                    self->Draw
                    self.oWindow->GetProperty, units=orig_units
                    self.oWindow->SetProperty, units=0
                    self.oWindow->GetProperty, dimensions=dimensions
                    self.oWindow->SetProperty, units=orig_units
                    oBuff = obj_new('IDLgrBuffer', dimensions=dimensions)
                    if float(!version.release) ge 8.1 then begin
                      oVid = gxVideo(dimensions,stream=stream,fps=fps,filename=filename)
                    endif else begin
                      desc = [ $
                        '0, LABEL, Movie Output Options, CENTER', $
                        '1, BASE,, ROW, FRAME', $
                        '0, DROPLIST,mpeg, LABEL_TOP=Movie Format,Row, TAG=format', $
                        '2, Float, 9, LABEL_TOP=Frames per second:, WIDTH=6, TAG=fps', $
                        '1, BASE,, ROW', $
                        '0, BUTTON, OK, QUIT,TAG=OK', $
                        '2, BUTTON, Cancel, QUIT, TAG=CANCEL']
                      opt=CW_FORM(desc,/Column,Title='Movie Options')
                      ext='mpg'
                      filename=dialog_pickfile(filter='*.'+ext,$
                        DEFAULT_EXTENSION=ext,$
                        /write,/OVERWRITE_PROMPT,$
                        title='Please choose a filename to save this video')
                      oVid= OBJ_NEW('IDLgrMPEG',frame_rate=2)
                    endelse
                    parm_list=['T_0','n_0','n_b']
                    vol=strcompress('None',/rem)
                    for i=0, n_elements(parm_list)-1 do vol=vol+'|'+strcompress(parm_list[i],/rem)
                    
                    desc = [ $
                      '0, LABEL, Movie Options, CENTER', $
                      '1, BASE,, ROW, FRAME', $
                      '0, DROPLIST,'+ vol+', LABEL_TOP= First Rotation,Row, TAG=first, Set_Value=1', $
                      '2, Float,0.5, LABEL_TOP=Volume Scaling Index:, WIDTH=6, TAG=first_scale', $
                      '1, BASE,, ROW, FRAME', $
                      '0, DROPLIST,'+ vol+', LABEL_TOP= Second Rotation,Row, TAG=second,Set_Value=2', $
                      '2, Float,0.5, LABEL_TOP=Volume Scaling Index:, WIDTH=6, TAG=second_scale', $
                      '1, BASE,, ROW, FRAME', $
                      '0, DROPLIST,'+ vol+', LABEL_TOP= Third Rotation,Row, TAG=third,Set_Value=3', $
                      '2, Float,0.2, LABEL_TOP=Volume Scaling Index:, WIDTH=6, TAG=third_scale', $
                      '1, BASE,, ROW', $
                      '2, BUTTON, OK, QUIT,TAG=OK']
                     opt=CW_FORM(desc,/Column,Title='Movie Options') 
                     rotations=[opt.first,opt.second,opt.third]
                     rotations_scale=[opt.first_scale,opt.second_scale,opt.third_scale]
                     good=where(rotations ne 0,count) 
                     parm_list=['None',parm_list]
                     if count ne 0 then begin
                      parm_list=parm_list[rotations[good]]
                      pwr_idx=rotations_scale[good]
                     endif else begin
                      count=1
                      parm_list='dR'
                      pwr_idx=1
                     endelse
                     if count gt 1 then begin
                      parm_list=['dR',parm_list]
                      count+=1
                      pwr_idx=[1,pwr_idx]
                     endif
                    for k=0,count-1 do begin
                      (model->GetVolume())->Update,parm_list[k],pwr_idx=pwr_idx[k],/update
                      print,parm_list[k],pwr_idx[k]
                    for i=0,36 do begin
                      self->Draw
                      oBuff->Draw, self.oViewgroup
                      oBuff->GetProperty, image_data=image_data
                      if obj_isa(oVid,'IDLgrMPEG') then begin
                        for l=0, 2 do image_data[l,*,*]=rotate(reform(image_data[l,*,*]),7)
                        for j=1, 24/opt.fps do oVid->Put, image_data
                      endif else begin
                        if obj_valid(oVid) then begin
                          for j=1, (fps/8.)>1 do result=oVid->Put(stream,image_data)
                        endif
                      endelse
                      ;if i eq 2 then write_png,parm_list[k]+'.png',image_data
                      wait,0.005
                      model->Rotate,ez,10
                    end
                  end  
                  endif
                  obj_destroy, oBuff
                  if obj_isa(oVid,'IDLgrMPEG') then  oVid->Save, FILENAME=filename
                  obj_destroy, oVid
                  self->Draw
                end
                
pro gxObjViewWid::WritePNG,pngfile
      default,pngfile,'gxsnapshot.png'
      self->Draw
      self.oWindow->GetProperty, units=orig_units
      self.oWindow->SetProperty, units=0
      self.oWindow->GetProperty, dimensions=dimensions
      self.oWindow->SetProperty, units=orig_units
      oBuff = obj_new('IDLgrBuffer', dimensions=dimensions)
      oBuff->Draw, self.oViewgroup
      oBuff->GetProperty, image_data=image_data
      write_png,pngfile,image_data
      obj_destroy, oBuff
    end
                
pro gxObjViewWid__define
 struct_hide,{gxObjViewWid, inherits IDLexObjViewWid, $
 wExtraToolbarBase:0l, $
 wFieldline:0l, $
 wRemove:0l,$
 wFluxtube:0l,$
 wXY:0l,$
 wXZ:0l,$
 wZY:0l,$
 wMovie:0l,$
 wZoomIn:0l,$
 wZoomOut:0l,$
 wZoom2View:0l,$
 wBlineContextMenu:0l,$
 wFluxTubeContextMenu:0l,$
 wBaseMapContextMenu:0l,$
 ModelView:0l,$
 zoomfactor:0d}
end