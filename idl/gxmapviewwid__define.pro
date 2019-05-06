;************************************************
function plotman_heir::init,_ref_extra=_extra
 result=self->plotman::init(_extra=_extra)
 if result eq 0 then return,result;failure for whatever reasons
 parent=widget_info(self.plot_base,/parent)
 if parent eq 0 then return,1; standard implementation
 ;if plot_base is not a tlb, find tlb
 repeat begin
 tlb=parent
 parent=widget_info(tlb,/parent) 
 endrep until parent eq 0l
 ;replace the original plot_base by the application's tlb not to break plotman's functionality
 self.plot_base=tlb
 return,1
end

pro plotman_heir::plot, no_notification=no_notification,_extra=_extra
 self->plotman::plot, _extra=_extra
 if ~keyword_set(no_notification) then begin
  tlb=self.plot_base
  widget_control,tlb,send_event={PLOTMANDRAWEVENT,id:0l,top:0l,handler:0l}
 end 
end

pro plotman_heir::delete_panel, _extra=_extra
 self->plotman::delete_panel,_extra=_extra
 tlb=self.plot_base
 widget_control,tlb,send_event={PLOTMANDELETEEVENT,id:0l,top:0l,handler:0l}
end

pro plotman_heir__define
 struct_hide,{plotman_heir,inherits plotman}
end


pro plotman_heir::cleanup
  r = self.orig_r
  g = self.orig_g
  b = self.orig_b

  ;GN: temporary? solution to avoid a conflict created by the
  ;<add_method,'free_var',self> call in the original plotman cleanup method
    
;  add_method,'free_var',self
;  ; Free pointer to data, but not data.  May be an object user is still using.
;  ptr_free, self.data
;  self->free_var, exclude='data'
  
  if xalive(self.plot_base) then widget_control, self.plot_base, /destroy
  print,'Cleanup of plotman_heir object complete.'
  tvlct,r,g,b
end


function gxMapViewWid::Init,wParent,mbar,frame=frame,wMapBase=wMapBase,wxsize=wxsize,wysize=wysize,_extra=_extra
font=!defaults.font
tbar= widget_base(wMapBase, /row,/toolbar)  
w_file=widget_button(font=font,widget_base(tbar,/row), VALUE='File', /MENU)
wModel =widget_button(font=font,w_file,event_func='IDLexWidget__HandleEvent',uvalue=self,value='Use magnetic field model to initiate project',uname='MODEL',sensitive=1)
wNewFitsMenu= widget_button(font=font,w_file,/sep,/menu,value='Import FITS map from file',uname='NewFitsMenu',event_func='IDLexWidget__HandleEvent',uvalue=self) 
wNewGxFits2Map=widget_button(font=font,wNewFitsMenu,uname='FITS',value='gx_fits2map',uvalue='gx_fits2map,file,map')
wNewFits2Map=widget_button(font=font,wNewFitsMenu,uname='FITS',value='fits2map',uvalue='fits2map,file,map')
wNewHMIFits2Map=widget_button(font=font,wNewFitsMenu,uname='FITS',value='hmi_rotfits2map',uvalue='hmi_rotfits2map,file,map')
wNewHSI2Map=widget_button(font=font,wNewFitsMenu,uname='FITS',value='hsi_fits2map',uvalue='hsi_fits2map,file,map,/sep')
wNewMap=widget_button(font=font,w_file,event_func='IDLexWidget__HandleEvent',uvalue=self,value='Import MAP structure from file',uname='MAP')
wNewFits=widget_button(font=font,w_file,/sep,value='Add FITS file to project',uname='ADDFITS',sensitive=1,event_func='IDLexWidget__HandleEvent',uvalue=self)
wNewMap =widget_button(font=font,w_file,event_func='IDLexWidget__HandleEvent',uvalue=self,value='Add MAP structure to project',uname='ADDMAP',sensitive=1)
wNewXY =widget_button(font=font,w_file,event_func='IDLexWidget__HandleEvent',uvalue=self,/sep,value='Overplot XY data from file',uname='XY')

self.wPlotmanBase=widget_base(wMapBase,/column,uvalue={mbar:widget_base(tbar,/row),w_file:w_file})
device, get_screen_size=scr
if not exist(wxsize) then wxsize = fix (scr[0] * .4)
if not exist(wysize) then wysize = wxsize * 1.1
self.plotman_obj = obj_new ('plotman_heir', mainbase=self.wPlotmanBase, /multi_panel, $
    wxsize=wxsize, wysize=wysize, $
    colortable=colortable, $
    widgets=widgets, error=error);,font=!defaults.font)
    
if error then return,0

geom = widget_info (widgets.w_maindrawbase, /geom)
scale=0.65
w_splashdraw = widget_draw ( widgets.w_maindrawbase, xsize=geom.scr_xsize, ysize=geom.scr_ysize)
state = { $
    w_file: w_file, $
    w_splashdraw: w_splashdraw, $
    widgets: widgets, $
    plotman_obj: self.plotman_obj}

parent=widget_info(wParent,/parent)   
if parent ne 0 then begin 
  repeat begin
   tlb=parent
   parent=widget_info(tlb,/parent) 
  endrep until parent eq 0l    
end

widget_control,tlb,set_uvalue=state 

wMapContainer=cw_objMapContainer(tbar,name='MAP_CONTAINER',plotman_obj=self.plotman_obj)
fov_base=widget_base(wMapBase,/row,event_func='IDLexWidget__HandleEvent',uvalue=self)
field=cw_objArray(fov_base,label='XRANGE',value=[-1000.0,1000.0],unit='"',inc=1,/static,uname='XRANGE') 
field=cw_objArray(fov_base,label='YRANGE',value=[-1000.0,1000.0],unit='"',inc=1,/static,uname='YRANGE')
 self.wControlBase=widget_base(wParent,/column,/frame,event_func='IDLexWidget__HandleEvent',uvalue=self)
 wWrapperBase=widget_base(self.wControlBase,/row,/toolbar)
 wSelecWrapper= widget_button(font=font,wWrapperBase, $
                 value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
                 /bitmap,tooltip='Extrapolation Engine Wrapper',uname='EXTSELECT')
 wWrapperPath=widget_text(font=font,wWrapperBase,xsize=100,ysize=1,value='',uname='EXTPATH')
 wTemplateBase=widget_base(self.wControlBase,/row)
 wResetTemplate= widget_button(font=font,wTemplateBase, $
                 value=gx_bitmap(filepath('reset.bmp', subdirectory=['resource', 'bitmaps'])), $
                 /bitmap,tooltip='Reset Template',uname='TEMPLATERESET')
 wTemplate=widget_text(font=font,wTemplateBase,xsize=100,uname='TEMPLATE',/editable)

 file=gx_findfile('gx_bz2lff.pro',folder='userslib'+path_sep()+'extrapolation'+path_sep()+'lff')
 wControlPanel=widget_base(self.wControlBase,/row)
 wFOVBase=widget_base(wControlPanel,/column,/frame)
 wFOV1=widget_base(wFOVBase,/column,/frame)
 button=widget_button(font=font,wFOVBase,value='GET FOV FROM CURRENT MAP',uname='GET_FROM_MAP',sensitive=1)
 button=widget_button(font=font,wFOVBase,value='CREATE REFERENCE MAP',uname='REFMAP',sensitive=1)
 button=widget_button(font=font,wFOVBase,value='ADD THIS MAP TO THE PROJECT',uname='ADD2EXT',SENSITIVE=1)

 wLabel=widget_label(font=font,wFOV1,value='REFERENCE MAP FOV: ',/align_left)
 wBase=widget_base(wFOV1,/row)
 wSubBase=widget_base(wBase,/column)
 field=cw_objArray(wSubBase,label='XRANGE',value=[10000,10000],unit='"',inc=1,/static,uname='EXTXRANGE')
 widget_control,field,set_value=[-64,64] 
 field=cw_objArray(wSubBase,label='YRANGE',value=[10000,10000],unit='"',inc=1,/static,uname='EXTYRANGE')
 widget_control,field,set_value=[-64,64] 
 wLabel=widget_label(font=font,wFOV1,value='REFERENCE MAP PIXELS: ',/align_left)
 wbase=widget_base(wFOV1,/row)
 field=cw_objArray(wBase,names=['Nx','Ny'],value=[10000,10000],inc=1,unit='vox',/static,uname='FOV_NxNy')
 widget_control,field,set_value=[64,64] 
 
 wLabel=widget_label(font=font,wFOV1,value='REFERENCE MAP RESOLUTION: ',/align_left)
 wbase=widget_base(wFOV1,/row)
 field=cw_objArray(wBase,names=['dx','dy'],value=[100,100],inc=1,unit='"',/static,uname='FOV_dxdy')
 widget_control,field,set_value=[2,2]
 
 wEXTBase=widget_base(wControlPanel,/column,/frame)
 wREFMAPList=widget_list(font=font,wEXTBase,xsize=65,YSIZE=11,uname='REFMAPLIST',scr_xsize=scale*geom.xsize)
 text='No B map selected yet'
 wBZInfo=widget_text(font=font,widget_base(wEXTBase),xsize=strlen(text),ysize=1,uname='BZINFO',$
         value=text)
 text='No White-Continuum map selected yet'      
 wBZInfo=widget_text(font=font,widget_base(wEXTBase),xsize=strlen(text),ysize=1,uname='WLINFO',$
   value=text)
 wBZ=widget_button(font=font,wEXTBase,value='SELECT B MAP FROM LIST',uname='BZMAP',sensitive=1)   
 wWL=widget_button(font=font,wEXTBase,value='SELECT WHITE-CONTINUUM MAP FROM LIST',uname='WLMAP',sensitive=1)
 wBoxDisplayBase=widget_base(wEXTBase,/row,/toolbar)
 tvlct,r0,g0,b0,/get
 loadct,39,/silent
 tvlct,r,g,b,/get
 self.wPalette = widget_button( wBoxDisplayBase, $
            value=gx_bitmap(filepath('palette.bmp', subdirectory=['resource', 'bitmaps'])), $
            /bitmap,tooltip='Change Color Table',uvalue=[[r],[g],[b]])
 tvlct,r0,g0,b0           
 wBoxSelect=widget_droplist(wBoxDisplayBase,font=font,/dynamic_resize,value=['Display Coronal Model','Display Chromo+Coronal Model'],uname='BoxSelect')
 wTagSelect=widget_droplist(wBoxDisplayBase,font=font,/dynamic_resize,value='Nothing to select',uname='TagSelect')

 wDisplayBase=wEXTbase
 wDrawBase=widget_base(wDisplayBase,/row,/frame)
 wDisplayDataBase=widget_base(wDisplayBase,/row,/frame)
 wLabel=widget_label(font=font,wDisplayDataBase,value='Displayed Data Range:')
 wSliceRange=widget_text(font=font,wDisplayDataBase,value='',uname='SliceRange',xsize=30)
 wProgressBase=wFOVbase

 wBzDraw=widget_draw(wDrawBase,xsize=scale*geom.xsize,ysize=scale*geom.xsize,uname='BZ_DRAW')
 wScroll=widget_slider(font=font,wDrawBase,/vertical,min=0,max=9,uname='BZ_SCROLL',uvalue=wBZDraw,/drag)
 wToolbarBase = widget_base(wProgressBase, /row, /frame,/TOOLBAR)
 self.wToolbar=widget_base(wToolbarBase,/exclusive,/row,uname='TOOLBAR',sensitive=0,/toolbar)
 self.wBridge=widget_button(font=font,self.wToolbar,value=gx_bitmap(gx_findfile('play.bmp')),tooltip='Perform Extrapolation',/bitmap,uname='EXT')
 self.wPause=widget_button(font=font,self.wToolbar,value=gx_bitmap(gx_findfile('pause.bmp')),tooltip='Pause Execution',/bitmap,uname='PAUSE_EXT',sensitive=0)
 self.wAbort=widget_button(font=font,self.wToolbar,value=gx_bitmap(gx_findfile('abort.bmp')),tooltip='Abort Execution',/bitmap,uname='ABORT_EXT',sensitive=0)
 self.wDebug=widget_button(font=font,self.wToolbar,value=gx_bitmap(gx_findfile('debug.bmp')),tooltip='Execute in UI thread (Debug)',/bitmap,uname='DEBUG_EXT',sensitive=1)
 wStatusbar=widget_text(font=font,wProgressBase,xsize=50,uname='STATUSBAR')
 wBase=widget_base(wProgressBase,/row,/frame)
 field=cw_objArray(wBase,names=['Nz'],value=[10000],inc=1,unit='vox',/static,uname='EXT_Nz') 
 widget_control,field,set_value=[64]
 field=cw_objArray(wBase,names=['dz'],value=[100],inc=1,unit='"',/static,uname='EXT_dz')
 widget_control,field,set_value=[2]
 field=cw_objArray(wBase,names=['Levels'],value=[100],inc=1,unit='',/static,uname='NLEVELS')
 self.levels=1
 widget_control,field,set_value=[self.levels]
 wBOX2GX=widget_button(font=font,wProgressBase,value='SEND B CUBE TO THE SIMULATOR',uname='BOX2GX',sensitive=1)
 wSAVEBOX=widget_button(font=font,wProgressBase,value='SAVE B CUBE TO FILE',uname='SAVEBOX',sensitive=1)

 wChromo=widget_button(font=font,wProgressBase,value='GENERATE CHROMO+CORONAL MODEL',uname='CHROMO',sensitive=1)
 wCHROMO2GX=widget_button(font=font,wProgressBase,value='SEND CHROMO+CORONAL MODEL TO THE SIMULATOR',uname='CHROMO2GX',sensitive=1)
 wSAVECHROMO=widget_button(font=font,wProgressBase,value='SAVE CHROMO+CORONAL MODEL TO FILE',uname='SAVECHROMO',sensitive=1)
 self.bridge=obj_new('gxBridge',userdata=self,out=GETENV('IDL_TMPDIR')+strcompress('ExtrapolationBridge.log',/rem))
; wADVANCED=widget_button(font=font,wProgressBase,value='ADVANCED',uname='ADVANCED',sensitive=0)
 widget_control,wScroll,set_slider_max=64*self.levels-1
 if self->ValidExtrapolator(file,template) then begin
  widget_control,wWrapperPath,set_value=file
  widget_control,wTemplate,set_value=template
 endif else answ=dialog_message('No valid extrapolation routine has been selected yet!',/info)
return,1
end

function gxMapViewWid::HandleEvent, event
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

  case TAG_NAMES(event, /STRUCTURE_NAME) OF
  'GX2PLOTMAN':begin
                 widget_control,widget_info(widget_info(event.top,find_by_uname='GXMAPCONTAINER:MENU'),/parent),get_uvalue=oMapContainer
                 oMapContainer->Add,event.omap,event.name,event.k
               end
  'GXSPEC2PLOTMAN':begin
                xdata=event.objxy->get_xdata()
                ydata=event.objxy->get_ydata()
                data=[[xdata],[ydata]]
                self.plotman_obj->new_panel,input=event.objxy,description=event.objxy->get(/id),plot_type='xyplot',/xlog,/ylog,nodup=0
                self.plotman_obj->select
                self.plotman_obj->plot,/no
               end             
  'PLOTMANDRAWEVENT':begin
           reset:
           self.plotman_obj ->select
           panel_struct=self.plotman_obj->get(/current_panel_struct)
           objxy=(*panel_struct.saved_data.data)
           xrange=self.plotman_obj->get(/xrange)
           yrange=self.plotman_obj->get(/yrange)
           if xrange[0] eq 0 and xrange[1] eq 0 then xrange=!x.crange;objxy->get(/xrange)
           if yrange[0] eq 0 and yrange[1] eq 0 then yrange=!y.crange;objxy->get(/yrange)
           widget_control,widget_info(event.handler,find_by_uname='XRANGE'),set_value=xrange
           widget_control,widget_info(event.handler,find_by_uname='YRANGE'),set_value=yrange
           widget_control,get_tlb(event.id),get_uvalue=state
           id=objxy->get(/id)
           widget_control,state.widgets.w_message,set_value=id
   end
  'PLOTMANDELETEEVENT':begin
    count=(self.plotman_obj->get(/panels))->get_count()
    if count eq 0 then begin
      widget_control,widget_info(event.top,find_by_uname='GET_FROM_MAP'),sensitive=0
      widget_control,widget_info(event.top,find_by_uname='REFMAP'),sensitive=0
    end
    if ptr_valid(self.REFMAPS) then begin
     valid=where(obj_valid(*self.REFMAPS),count,comp=comp,ncomp=ncomp)
     if count gt 0 then begin
       widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),get_uvalue=map_ids
       valid_omaps=(*self.REFMAPS)[valid]
       map_ids=map_ids[valid]
       ptr_free,self.REFMAPS
       self.REFMAPS=ptr_new(valid_omaps)
       widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),set_value=map_ids,set_uvalue=map_ids
     endif else begin
      delete_all:
      ptr_free,self.REFMAPS
      obj_destroy,self.BZMAP
      obj_destroy,self.WLMAP
      widget_control,widget_info(event.top,find_by_uname='EXTXRANGE'), sensitive=1
      widget_control,widget_info(event.top,find_by_uname='EXTYRANGE'), sensitive=1
      widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),set_value=['']
      widget_control,widget_info(event.top,find_by_uname='FOV_NxNy'),sensitive=1
      widget_control,widget_info(event.top,find_by_uname='FOV_dxdy'),sensitive=1
      widget_control,widget_info(event.top,find_by_uname='GET_FROM_MAP'),sensitive=1
      widget_control,widget_info(event.top,find_by_uname='REFMAP'),sensitive=1
      widget_control,widget_info(event.top,find_by_uname='BZMAP'),sensitive=1
      widget_control,widget_info(event.top,find_by_uname='ADD2EXT'),sensitive=0
      widget_control,widget_info(event.top,find_by_uname='ADDMAP'),sensitive=0
      widget_control,widget_info(event.top,find_by_uname='ADDFITS'),sensitive=0
     end
    end
    if ~obj_valid(self.BZMAP)then begin
     widget_control,widget_info(event.top,find_by_uname='BZINFO'),set_value='At least one map must be added to the list and confirmed as being a B map'
     widget_control,self.wToolbar,sensitive=0
    end
   end
  else:
  endcase

case strupcase(widget_info(event.id,/uname)) of
 'EXT_NZ':begin
           widget_control,event.id,get_value=Nz
           widget_control,widget_info(event.top,find_by_uname='BZ_SCROLL'),set_slider_max=Nz*self.levels-1
          end
 'NLEVELS':begin
          widget_control,event.id,get_value=levels
          self.levels=levels[0]
          widget_control,widget_info(event.top,find_by_uname='EXT_Nz'),get_value=Nz
          widget_control,widget_info(event.top,find_by_uname='BZ_SCROLL'),set_slider_max=Nz*self.levels-1
         end
 'FITS':begin
  file=dialog_pickfile(title='Please select a fits file',filter=['*.f*'],path=gx_findfile(folder='demo'),/must_exist)
  if file eq '' then return,self->Rewrite(event)
  widget_control,event.id,get_uvalue=proc
  result=execute(proc)
  if n_elements(map) eq 0 then begin
   answ=dialog_message('Invalid file content!',/error)
   return,self->Rewrite(event)
  end
  map=map[0]
  if map.id eq '' then begin
   break_file, file, dsk_log, dir, filename, ext
   map.id=filename
  end
  map=make_map(map.data,xc=map.xc,yc=map.yc,dx=map.dx,dy=map.dy,time=map.time,id=map.id)
  goto,getmap
 end
 'XY':begin
  self.plotman_obj->select
  panel_struct=self.plotman_obj->get(/current_panel_struct)
  objxy=(*panel_struct.saved_data.data)
  if is_class(objxy, 'xyplot', /quiet) then begin
    file=dialog_pickfile(title='Please select a file containing XY data',filter=['*.xy'],path=gx_findfile(folder='demo'),/must_exist) 
    if file eq '' then return,self->Rewrite(event)
    restore,file
    break_file, file, dsk_log, dir, filename, ext
    objxy->set,addplot_name='add2plotman',addplot_arg={x:data[*,0],y:data[*,1],id:filename}
    self.plotman_obj->plot,psym=0,/no_notification
  end 
 end
 'MODEL':begin
                 file=dialog_pickfile(filter=['*.sav','*.gxm'],$
                  /read,/must_exist,$
                  title='Please select a file containig a {Bx,By,Bz} datacube structure or GX model',path=gx_findfile(folder='demo'))
                  if file ne '' then begin
                    break_file, file, dsk_log, dir, filename, ext,/last
                    case strupcase(ext) of 
                    '.SAV': model=gx_ImportModel(file,box=fbox)
                    '.GXM': model=gx_read(file)
                    else:answ=dialog_message('SAV or GXM file extensions expected')
                    endcase
                  end
                  if obj_isa(model,'gxmodel') then begin
                       model->GetProperty,refmaps=refmaps,dr=dr,xrange=xrange,yrange=yrange
                       time=model->gettime()
                       void=model->GetB(Bx=Bx,by=By,Bz=Bz)
                       dim=size(Bx,/dim)
                       ref=*refmaps
                       ptr_free,refmaps
                       ptr_free,self.REFMAPS
                       obj_destroy,self.BZMAP
                       obj_destroy,self.WLMAP
                       widget_control,self.wBridge,set_button=0
                       widget_control,self.wDebug,set_button=0
                       widget_control,self.wPause,set_button=0
                       widget_control,self.wAbort,set_button=0
                       widget_control,self.wToolbar,sensitive=0
                       self.REFMAPS=ptr_new()
                       map_ids=['']
                       widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),set_value=map_ids,set_uvalue=map_ids                    
                          nref=ref->get(/count)
                          if nref le 3 then begin
                            answ=dialog_message('This is not a standard GX model. Action aborted!',/info)
                            return,event
                          endif
                          omap=obj_new('map')
                          map_ids=strarr(nref-3)
                          refmaps=objarr(nref-3)
                          for k=3,nref-1 do begin
                            refmaps[k-3]=obj_new('map')
                            map=ref->get(k,/map)
                            if k eq 3 then begin
                              if tag_exist(map,'index') then begin
                                index=map.index
                                id=strcompress(map.id,/rem)
                                id=strmid(id,4,strlen(id)-4)
                                base=create_struct(id,map.data)
                              endif
                            endif else begin
                              if tag_exist(map,'index') then begin
                                id=map.id
                                id=strcompress(strmid(id,4,strlen(id)-4),/rem)
                                base=create_struct(base,id,map.data)
                              endif
                            endelse   
                            ;map=rebin_map(map,dim[0],dim[1])
                            refmaps[k-3]->setmap,0,map
                            omap->setmap,k-3,map
                            map_ids[k-3]=ref->get(k,/id)+' ['+ref->get(k,/time)+']'
                          endfor
                          refmaps=ptr_new(refmaps)
                          self.REFMAPS=refmaps
                          rsun=(pb0r(time)*60)[2]
                          xrange*=rsun
                          yrange*=rsun
                          widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),set_value=map_ids,set_uvalue=map_ids
                          widget_control,event.top,send_event={GX2PLOTMAN,id:0l,top:0l,handler:0l,omap:omap,name:filename+' REFMAPS',k:0}
                          widget_control,widget_info(event.top,find_by_uname='XRANGE'), set_value=xrange
                          widget_control,widget_info(event.top,find_by_uname='YRANGE'), set_value=yrange
                          widget_control,widget_info(event.top,find_by_uname='EXTXRANGE'), set_value=xrange
                          widget_control,widget_info(event.top,find_by_uname='EXTYRANGE'), set_value=yrange
                          self.plotman_obj->set,xrange=xrange,yrange=yrange
                    
                     BzDraw=widget_info(self.wControlBase,find_by_uname='BZ_DRAW')
                     widget_control,BzDraw,get_value=win
                     widget_control,BzDraw,set_uvalue=bz
                     
                     bcube=fltarr(dim[0],dim[1],dim[2],3)
                     bcube[*,*,*,0]=temporary(bx)
                     bcube[*,*,*,1]=temporary(by)
                     bcube[*,*,*,2]=temporary(bz)
                     ptr_free,self.pbox
                     ptr_free,self.pchromobox
                     box={bcube:temporary(bcube),dr:dr,id:filename,refmaps:refmaps}
                     if n_elements(base) ne 0 and n_elements(index) ne 0 then box=create_struct(box,'base',base, 'index',index,'add_base_layer',0)
                     volume=model->GetVolume()
                     volume->GetVertexAttributeData,'dz',dz
                     if (n_elements(dz) ne 0) then box=create_struct(box,'dz',dz)
                     volume->GetVertexAttributeData,'idx',idx
                     volume->GetVertexAttributeData,'bmed',bmed
                     volume->GetVertexAttributeData,'length',length
                     volume->GetVertexAttributeData,'alpha',alpha
                     volume->GetVertexAttributeData,'curlb', curlb
                     if (n_elements(idx) ne 0 and (n_elements(idx)) eq n_elements(bmed)) and (n_elements(bmed) eq n_elements(length)) then $
                     box=create_struct(box,'idx',idx,'bmed',bmed,'length',length)
                     if (n_elements(idx) ne 0 and (n_elements(idx)) eq n_elements(alpha)) and (n_elements(alpha) eq n_elements(curlb)) then $
                     box=create_struct(box,'alpha',alpha,'curlb',curlb)
                     if n_elements(fbox) ne 0 then begin
                      names=tag_names(fbox)
                      for i=0, n_tags(fbox)-1 do begin
                        case strupcase(names[i]) of
                          'BX':
                          'BY':
                          'BZ':
                        else: if ~tag_exist(box,names[i]) then box=create_struct(box,names[i],fbox.(i)) 
                        endcase
                      endfor
                     endif
                     self.pbox=ptr_new(temporary(box))
                     self.levels=1
                     widget_control,widget_info(event.top,find_by_uname='FOV_NxNy'), set_value=dim[0:1]
                     widget_control,widget_info(event.top,find_by_uname='EXT_Nz'),set_value=dim[2]
                     widget_control,widget_info(self.wControlBase,find_by_uname='EXT_dz'),set_value=dr[2]*rsun
                     widget_control,widget_info(self.wControlBase,find_by_uname='FOV_dxdy'),set_value=dr[0:1]*rsun
                     widget_control,widget_info(event.top,find_by_uname='BZ_SCROLL'),set_slider_max=dim[2]-1,set_value=0
                     widget_control,widget_info(event.top,find_by_uname='BoxSelect'),SET_DROPLIST_SELECT=0
                     tags=['Bz','Bx','By']
                     names=tag_names(*self.pbox)
                     for i=0, n_tags(*self.pbox)-1 do begin
                      if (size(size((*self.pbox).(i),/dim),/dim))[0] eq 1 and n_elements((*self.pbox).(i)) gt dim[0] then begin
                        tags=[tags,names[i]]
                      endif
                     endfor
                     widget_control,widget_info(event.top,find_by_uname='TagSelect'),set_value=tags
                     idx=0
                     goto,bz_scroll  
                  endif
         end
 'BOXSELECT':begin 
              select=widget_info(event.id, /DROPLIST_SELECT)   
              if select eq 0 then pBox=self.pbox else pBox=self.pchromobox
              if ptr_valid(pBox) then begin
               TagSelect=widget_info(self.wControlBase,find_by_uname='TagSelect') 
               tags=['Bz','Bx','By']
               names=tag_names(*pbox)
               dim=size((*pBox).Bcube,/dim)
               for i=0, n_tags(*pbox)-1 do begin
                if size((*pbox).(i),/n_dim) eq 1 and n_elements((*pbox).(i)) gt dim[0] and $
                  names(i) ne 'IDX' and strmid(names(i),0,1) ne 'O' and strmid(names(i),0,1) ne 'F' then begin
                  tags=[tags,names[i]]
                endif
               endfor
               index=0
               if tag_exist(event,'z_index') then value=event.z_index else value=0
               widget_control,TagSelect, set_value=tags,set_droplist_select=0
               widget_control,widget_info(self.wControlBase,find_by_uname='BZ_SCROLL'),set_slider_max=dim[2]-1,set_value=value
               widget_control,widget_info(self.wControlBase,find_by_uname='TagSelect'),SET_DROPLIST_SELECT=index
               goto,tag_select
              endif else widget_control,event.id,set_droplist_select=1-select
             end    
 'TAGSELECT':begin
              widget_control,event.id, get_value=tags
              index=event.index
              tag_select:
              BoxSelect=widget_info(self.wControlBase,find_by_uname='BoxSelect')
              if widget_info(BoxSelect, /DROPLIST_SELECT) eq 0 then pBox=self.pbox else pBox=self.pchromobox
              if ptr_valid(pbox) then begin
                BzDraw=widget_info(self.wControlBase,find_by_uname='BZ_DRAW')
                case strupcase(tags[index]) of
                  'BX':widget_control,BzDraw,set_uvalue=(*pBox).bcube[*,*,*,0]
                  'BY':widget_control,BzDraw,set_uvalue=(*pBox).bcube[*,*,*,1]
                  'BZ':widget_control,BzDraw,set_uvalue=(*pBox).bcube[*,*,*,2]
                else: begin
                        tag=where(tags[index] eq tag_names(*pBox),count)
                        if count eq 1 then begin
                         data=(*pBox).(tag) 
                         if tag_exist((*pBox),'IDX') then begin
                          idx=(*pBox).idx
                         endif 
                        if n_elements(data) ne n_elements(idx) then begin 
                          if tag_exist((*pBox),'CHROMO_IDX') then idx=(*pBox).chromo_idx
                        endif
                        if n_elements(data) eq n_elements(idx) then begin
                          cube=(*pBox).bcube[*,*,*,0]*0
                          cube[idx]=data
                          widget_control,BzDraw,set_uvalue=cube
                         endif 
                        Endif 
                      end  
                endcase
                widget_control,widget_info(self.wControlBase,find_by_uname='BZ_SCROLL'),get_value=idx
                goto,bz_scroll
              end
             end        
 'MAP':begin
        file=dialog_pickfile(title='Please select a file containing an IDL map structure',filter=['*.sav','*.map'],path=gx_findfile(folder='demo'),/must_exist) 
        if file eq '' then return,self->Rewrite(event)
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
        ;restore,file,/RELAXED_STRUCTURE_ASSIGNMENT
        getmap:
        break_file, file, dsk_log, dir, filename, ext
        widget_control,get_tlb(event.id),get_uvalue=uvalue
        widget_control,uvalue.widgets.w_message,set_value='UPLOADING MAPS FROM FILE.......'
        widget_control,/hourglass
        if ~(size(map,/tname) eq 'STRUCT' or size(map,/tname) eq 'OBJREF') then begin
          answ=dialog_message('Unexpected file content!',/error)
          return,self->Rewrite(event)
        endif else begin
          if size(map,/tname) eq 'STRUCT' then begin
            omap=obj_new('map')
            for k=0,n_elements(map)-1 do begin
              omap->setmap,k,map[k]
            endfor
          endif else omap=map 
          if omap->get(/count) gt 1 then begin
           widget_control,widget_info(widget_info(event.top,find_by_uname='GXMAPCONTAINER:MENU'),/parent),get_uvalue=oMapContainer
           oMapContainer->Add,omap,filename
          endif else begin
           omap->plotman,0,plotman_obj=self.plotman_obj,nodup=0
           obj_destroy,omap
          end 
          widget_control,widget_info(event.top,find_by_uname='GET_FROM_MAP'),sensitive=1
          widget_control,widget_info(event.top,find_by_uname='REFMAP'),sensitive=1
       endelse   
    end
 'ADDFITS':begin
  file=dialog_pickfile(title='Please select a fits file',filter=['*.fits','*.fit'],path=gx_findfile(folder='demo'),/must_exist)
  if file eq '' then return,self->Rewrite(event)
  fits2map,file,map 
   if map.id eq '' then begin
   break_file, file, dsk_log, dir, filename, ext
   map.id=filename
  end
  map=make_map(map.data,xc=map.xc,yc=map.yc,dx=map.dx,dy=map.dy,time=map.time,id=map.id)
  omap=obj_new('map')
  omap->set,map=map
  if ptr_valid(self.REFMAPS) then begin
    newmap_id='*'+omap->get(/id)
    t0=omap->get(/time)
    ref_omap=(*self.REFMAPS)[0]
    newmap=omap->drotate(ref_map=ref_omap->get(/map),/keep_limb)
    t1=newmap->get(/time)
    if t1 ne t0 then t1=strmid(newmap->get(/time),strlen(newmap->get(/time))-12)
    newmap->set,id=newmap_id
    self.plotman_obj->new_panel,input=newmap,description=newmap_id,nodup=0
    panel_struct=self.plotman_obj->get(/current_panel_struct)
    omap=(*panel_struct.saved_data.data)
    obj_destroy,newmap
    REFMAPS=*self.REFMAPS
    ptr_free,self.REFMAPS
    widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),get_uvalue=map_ids
    self.REFMAPS=ptr_new([REFMAPS,omap])
    if ptr_valid(self.pbox) then (*self.pbox).refmaps=self.refmaps
    map_ids=[map_ids,t1 eq t0?omap->get(/id)+'['+t0+']':omap->get(/id)+'['+t0+'->'+t1+']']
    widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),set_value=map_ids,set_uvalue=map_ids
  endif else self.plotman_obj->new_panel,input=omap,description=map.id,nodup=0
  widget_control,widget_info(event.top,find_by_uname='GET_FROM_MAP'),sensitive=1
  widget_control,widget_info(event.top,find_by_uname='REFMAP'),sensitive=1
 end
 'ADDMAP':begin
  file=dialog_pickfile(title='Please select a file containing an IDL map structure',filter=['*.sav'],path=gx_findfile(folder='demo'),/must_exist) 
  if file eq '' then return,self->Rewrite(event)
  osav=obj_new('idl_savefile',file)
  names=osav->names()
  for i=0,n_elements(names)-1 do begin
    osav->restore,names[i]
    e=execute('result=size('+names[i]+',/tname)')
    if result eq 'STRUCT' then begin
      e=execute('map=temporary('+names[i]+')')
      if valid_map(map) then goto,validmap
    endif
  endfor
  return,self->Rewrite(event)
  validmap:
  obj_destroy,osav
  omap=obj_new('map')
  omap->set,map=map
  if ptr_valid(self.REFMAPS) then begin
    newmap_id='*'+omap->get(/id)
    t0=omap->get(/time)
    ref_omap=(*self.REFMAPS)[0]
    newmap=omap->drotate(ref_map=ref_omap->get(/map),/keep_limb)
    t1=newmap->get(/time)
    if t1 ne t0 then t1=strmid(newmap->get(/time),strlen(newmap->get(/time))-12)
    newmap->set,id=newmap_id
    self.plotman_obj->new_panel,input=newmap,description=newmap_id,nodup=0
    panel_struct=self.plotman_obj->get(/current_panel_struct)
    omap=(*panel_struct.saved_data.data)
    obj_destroy,newmap
    REFMAPS=*self.REFMAPS
    ptr_free,self.REFMAPS
    widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),get_uvalue=map_ids
    self.REFMAPS=ptr_new([REFMAPS,omap])
    if ptr_valid(self.pbox) then (*self.pbox).refmaps=self.refmaps
    map_ids=[map_ids,t1 eq t0?omap->get(/id)+'['+t0+']':omap->get(/id)+'['+t0+'->'+t1+']']
    widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),set_value=map_ids,set_uvalue=map_ids
  endif else self.plotman_obj->new_panel,input=omap,description=map.id,nodup=0
  widget_control,widget_info(event.top,find_by_uname='GET_FROM_MAP'),sensitive=1
  widget_control,widget_info(event.top,find_by_uname='REFMAP'),sensitive=1
 end
  'ADD2EXT':begin 
              widget_control,widget_info(event.top,find_by_uname='EXTXRANGE'), get_value=xrange
              widget_control,widget_info(event.top,find_by_uname='EXTYRANGE'), get_value=yrange
              widget_control,widget_info(event.top,find_by_uname='XRANGE'), set_value=xrange
              widget_control,widget_info(event.top,find_by_uname='YRANGE'), set_value=yrange
              self.plotman_obj->set,xrange=xrange,yrange=yrange
              self.plotman_obj -> select
              self.plotman_obj->plot,/no
              panel_struct=self.plotman_obj->get(/current_panel_struct)
              map=(*panel_struct.saved_data.data)
                    if size(map,/tname) eq 'STRUCT' then begin
                     if valid_map(map) then begin
                      omap=obj_new('MAP')
                      omap->Set,map=map
                      destroy=1
                     end 
                    endif else begin
                     if size(map,/tname) eq 'OBJREF' then begin
                      if obj_isa(map,'MAP') then omap=map
                     end
                    end
    
              match=where(*self.REFMAPS eq omap,count)
              if count gt 0 then begin
                 answ=dialog_message(omap->get(/id)+' already exists in the project!',/info)
                 return,self->Rewrite(event)
              end
              if obj_isa(omap,'MAP') then begin
                newmap_id='*'+omap->get(/id)
                t0=omap->get(/time)
                ref_omap=(*self.REFMAPS)[0]
                newmap=omap->drotate(ref_map=ref_omap->get(/map),/keep_limb)
                t1=newmap->get(/time)
                if t1 ne t0 then t1=strmid(newmap->get(/time),strlen(newmap->get(/time))-12)
                newmap->set,id=newmap_id
                self.plotman_obj->new_panel,input=newmap,description=newmap_id,nodup=0
                panel_struct=self.plotman_obj->get(/current_panel_struct)
                omap=(*panel_struct.saved_data.data)
                obj_destroy,newmap
                REFMAPS=*self.REFMAPS
                ptr_free,self.REFMAPS
                widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),get_uvalue=map_ids
                self.REFMAPS=ptr_new([REFMAPS,omap])
                if ptr_valid(self.pbox) then (*self.pbox).refmaps=self.refmaps
                map_ids=[map_ids,t1 eq t0?omap->get(/id)+'['+t0+']':omap->get(/id)+'['+t0+'->'+t1+']']
                widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),set_value=map_ids,set_uvalue=map_ids
               end 
           end   
 'XRANGE':begin
         widget_control,event.id, get_value=xrange
         widget_control,widget_info(event.handler,find_by_uname='YRANGE'), get_value=yrange
         self.plotman_obj->set,xrange=xrange,yrange=yrange
         self.plotman_obj -> select
         self.plotman_obj->plot,/no
        end
 'YRANGE':begin
         widget_control,event.id, get_value=yrange
         widget_control,widget_info(event.handler,find_by_uname='XRANGE'), get_value=xrange
         self.plotman_obj->set,xrange=xrange,yrange=yrange
         self.plotman_obj ->select
         self.plotman_obj->plot,/no
        end   
 'REFMAP':begin
            widget_control,widget_info(event.top,find_by_uname='EXTXRANGE'), get_value=xrange
            widget_control,widget_info(event.top,find_by_uname='EXTYRANGE'), get_value=yrange
            widget_control,widget_info(event.top,find_by_uname='XRANGE'), get_value=map_xrange
            widget_control,widget_info(event.top,find_by_uname='YRANGE'), get_value=map_yrange
            if (xrange[1] le map_xrange[0]) or (xrange[0] ge map_xrange[1]) or $
            (yrange[1] le map_yrange[0]) or (yrange[0] ge map_yrange[1]) then begin
             answ=dialog_message('Current map is outside the selected FOV range! You may get the FOV from the current map.',/info)
             return,self->Rewrite(event)
            end
            widget_control,widget_info(event.top,find_by_uname='XRANGE'), set_value=xrange
            widget_control,widget_info(event.top,find_by_uname='YRANGE'), set_value=yrange
            self.plotman_obj->set,xrange=xrange,yrange=yrange
            self.plotman_obj -> select
            self.plotman_obj->plot,/no
            panel_struct=self.plotman_obj->get(/current_panel_struct)
            omap=(*panel_struct.saved_data.data)
            if isa(omap,'MAP') then begin 
             widget_control,widget_info(event.top,find_by_uname='FOV_dxdy'), get_value=dr
             widget_control,widget_info(event.top,find_by_uname='FOV_NxNy'), get_value=N
             sub_map,omap->getmap(0),smap,xrange=xrange,yrange=yrange
             rmap=rebin_map(smap,n[0],n[1])
             newmap=obj_new('map')
             newmap_id='*'+omap->get(/id)
             t=omap->get(/time)
             newmap->set,map=rmap
             newmap->set,id=newmap_id
             self.plotman_obj->new_panel,input=newmap,description=newmap_id,nodup=0
             panel_struct=self.plotman_obj->get(/current_panel_struct)
             omap=(*panel_struct.saved_data.data)
             widget_control,widget_info(event.top,find_by_uname='EXTXRANGE'), set_value=omap->get(/xrange),sensitive=0
             widget_control,widget_info(event.top,find_by_uname='EXTYRANGE'), set_value=omap->get(/yrange),sensitive=0
             obj_destroy,newmap
             ptr_free,self.REFMAPS
             self.REFMAPS=ptr_new([omap])
             widget_control,widget_info(event.top,find_by_uname='REFMAPLIST'),set_value=[newmap_id+'['+t+']'],set_uvalue=[newmap_id+'['+t+']']
             widget_control,widget_info(event.top,find_by_uname='ADD2EXT'),sensitive=1
             widget_control,widget_info(event.top,find_by_uname='ADDMAP'),sensitive=1
             widget_control,widget_info(event.top,find_by_uname='ADDFITS'),sensitive=1
             widget_control,widget_info(event.top,find_by_uname='BZMAP'),sensitive=1
             widget_control,widget_info(event.top,find_by_uname='REFMAP'),sensitive=1
             widget_control,widget_info(event.top,find_by_uname='GET_FROM_MAP'),sensitive=1
             widget_control,widget_info(event.top,find_by_uname='FOV_NxNy'),sensitive=1
             widget_control,widget_info(event.top,find_by_uname='FOV_dxdy'),sensitive=1
            endif else begin
               answ=dialog_message('No valid map selected!',/error)
               return,self->Rewrite(event)
            endelse
           end
           
       
                    
 'GET_FROM_MAP':begin
                  panel_struct=self.plotman_obj->get(/current_panel_struct)
                  map=(*panel_struct.saved_data.data)
                  if size(map,/tname) eq 'STRUCT' then begin
                   if valid_map(map) then begin
                    omap=obj_new('MAP')
                    omap->Set,map=map
                    destroy=1
                   end 
                  endif else begin
                   if size(map,/tname) eq 'OBJREF' then begin
                    if obj_isa(map,'MAP') then omap=map
                   end
                  end
                  if n_elements(omap) ne 0 then begin
                   widget_control,widget_info(event.top,find_by_uname='XRANGE'), get_value=xrange
                   widget_control,widget_info(event.top,find_by_uname='YRANGE'), get_value=yrange
                   widget_control,widget_info(event.top,find_by_uname='EXTXRANGE'), set_value=xrange
                   widget_control,widget_info(event.top,find_by_uname='EXTYRANGE'), set_value=yrange
                   dx=omap->get(/dx)
                   dy=omap->get(/dy)
                   id=widget_info(event.top,find_by_uname='FOV_dxdy')
                   widget_control,id, set_value=[dx,dy]
                   if keyword_set(destroy) then obj_destroy,omap
                   widget_control,id,send_event={ID:ID,TOP:event.TOP,Handler:event.handler}
                  end            
                end
  
  'FOV_DXDY' :begin
             widget_control,event.id,get_value=dr
             widget_control,widget_info(event.top,find_by_uname='EXTXRANGE'), get_value=xrange
             widget_control,widget_info(event.top,find_by_uname='EXTYRANGE'), get_value=yrange
             widget_control,widget_info(event.top,find_by_uname='FOV_NxNy'), get_value=N
             nx=abs(xrange[1]-xrange[0])/dr[0]
             ny=abs(yrange[1]-yrange[0])/dr[1]
             n=fix(round([nx,ny]))
             dr[0]=abs(xrange[1]-xrange[0])/n[0]
             dr[1]=abs(yrange[1]-yrange[0])/n[1]
             widget_control,event.id,set_value=dr
             widget_control,widget_info(event.top,find_by_uname='FOV_NxNy'), set_value=N
            end   
   'FOV_NXNY' :begin
             widget_control,event.id,get_value=N
             n=fix(N)
             widget_control,widget_info(event.top,find_by_uname='EXTXRANGE'), get_value=xrange
             widget_control,widget_info(event.top,find_by_uname='EXTYRANGE'), get_value=yrange
             widget_control,widget_info(event.top,find_by_uname='FOV_dxdy'), get_value=dr
             dr[0]=abs(xrange[1]-xrange[0])/n[0]
             dr[1]=abs(yrange[1]-yrange[0])/n[1]
             widget_control,widget_info(event.top,find_by_uname='FOV_dxdy'),set_value=dr
             widget_control,event.id, set_value=N
            end   
    'EXTXRANGE':begin
                 widget_control,widget_info(event.top,find_by_uname='FOV_NxNy'),get_value=N
                 n=fix(N)
                 widget_control,widget_info(event.top,find_by_uname='EXTXRANGE'), get_value=xrange
                 widget_control,widget_info(event.top,find_by_uname='EXTYRANGE'), get_value=yrange
                 widget_control,widget_info(event.top,find_by_uname='FOV_dxdy'), get_value=dr
                 dr[0]=abs(xrange[1]-xrange[0])/n[0]
                 dr[1]=abs(yrange[1]-yrange[0])/n[1]
                 widget_control,widget_info(event.top,find_by_uname='FOV_dxdy'),set_value=dr
                 widget_control,widget_info(event.top,find_by_uname='FOV_NxNy'), set_value=N
                end     
     'EXTYRANGE':begin
                 widget_control,widget_info(event.top,find_by_uname='FOV_NxNy'),get_value=N
                 n=fix(N)
                 widget_control,widget_info(event.top,find_by_uname='EXTXRANGE'), get_value=xrange
                 widget_control,widget_info(event.top,find_by_uname='EXTYRANGE'), get_value=yrange
                 widget_control,widget_info(event.top,find_by_uname='FOV_dxdy'), get_value=dr
                 dr[0]=abs(xrange[1]-xrange[0])/n[0]
                 dr[1]=abs(yrange[1]-yrange[0])/n[1]
                 widget_control,widget_info(event.top,find_by_uname='FOV_dxdy'),set_value=dr
                 widget_control,widget_info(event.top,find_by_uname='FOV_NxNy'), set_value=N
                end   
      'BZMAP': begin
                wREFMAPLIST=widget_info(event.top,find_by_uname='REFMAPLIST')
                bzindex=widget_info(wREFMAPLIST,/list_select)
                widget_control,wREFMAPLIST,get_uvalue=map_ids
                if bzindex ge 0 then begin
                 self.BZMAP=(*self.REFMAPS)[bzindex]
                 widget_control,widget_info(event.top,find_by_uname='BZINFO'),set_value='B Map Selected: '+map_ids[bzindex]
                 widget_control,self.wToolbar,sensitive=1
                endif else answ=dialog_message('Click one item in the list to select the Bz map for this project!',/info)
               end 
      'WLMAP': begin
                  wREFMAPLIST=widget_info(event.top,find_by_uname='REFMAPLIST')
                  wlindex=widget_info(wREFMAPLIST,/list_select)
                  widget_control,wREFMAPLIST,get_uvalue=map_ids
                  if wlindex ge 0 then begin
                    self.WLMAP=(*self.REFMAPS)[wlindex]
                    widget_control,widget_info(event.top,find_by_uname='WLINFO'),set_value='WL Map Selected: '+map_ids[wlindex]
                    widget_control,self.wToolbar,sensitive=1
                  endif else answ=dialog_message('Click one item in the list to select the Whitelight-Continuum map for this project!',/info)
                end
                                  
 'BZ_SCROLL':begin
              widget_control,event.id,get_value=idx,get_uvalue=BzDraw
              bz_scroll:
              widget_control,BzDraw,get_value=wdraw,get_uvalue=cube
              if size(cube,/N_DIMENSIONS) eq 3 then begin
                geometry=widget_info(BzDraw,/geometry)
                wset,wdraw
                tvlct,r0,g0,b0,/get
                sz=size(cube)
                widget_control,self.wPalette,get_uvalue=rgb
                 tvlct,rgb[*,0],rgb[*,1],rgb[*,2]
                 if idx le sz[3]-1 then begin
                  tvscl,congrid(cube[*,*,idx],geometry.xsize,geometry.ysize)
                  widget_control,widget_info(self.wControlBase,find_by_uname='SliceRange'),set_value=arr2str((minmax(cube[*,*,idx])))
                 endif else begin
                   tvscl,congrid(cube[*,*,0]*0,geometry.xsize,geometry.ysize)
                 endelse
                tvlct,r0,g0,b0 
              end
             end    
 'SAVEBOX':begin
              if ptr_valid(self.pBox) then begin
              file=dialog_pickfile(default_extension='*.sav',filter='*.sav',/overwrite_promp,/write) 
              if file ne '' then begin
              box=*self.pBox
              save,box,file=file
              end
              end
            end  
 'BOX2GX':begin
              if ptr_valid(self.pBox) then begin
               box=*self.pbox
               if ptr_valid(self.pChromoBox) then begin
                box=create_struct(box,'tr',(*self.pChromoBox).tr,'chromo_layers',(*self.pChromoBox).chromo_layers)
               endif
               widget_control,event.top,send_event={BOX2GX,id:0l,top:0l,handler:0l,box:ptr_new(box)}
              end         
            end   
 'CHROMO':begin
             if obj_valid(self.BZMAP) and obj_valid(self.WLMAP) and ptr_valid(self.pbox) then begin
              widget_control,/hourglass
              widget_control,widget_info(self.wControlBase,find_by_uname='STATUSBAR'),set_value='Generating chromospheric model...'
              BzDraw=widget_info(event.top,find_by_uname='BZ_DRAW')
              mapsize=size((*self.pbox).bcube)
              chromo_mask=decompose(frebin(self.BZMAP->get(/data),mapsize[1],mapsize[2]), frebin(self.WLMAP->get(/data),mapsize[1],mapsize[2]))
              ptr_free,self.pchromobox
              self.pchromobox=ptr_new(combo_model(*self.pbox,chromo_mask))
              BoxSelect=widget_info(self.wControlBase,find_by_uname='BoxSelect')
              widget_control, BoxSelect, set_droplist_select=1,send_event={id:0l,top:0l,handler:0l,index:1l}
              widget_control,hourglass=0
              widget_control,widget_info(self.wControlBase,find_by_uname='STATUSBAR'),$
                set_value='Chromspheric model done!'
             endif 
            end   
 'CHROMO2GX':begin
                if ptr_valid(self.pchromobox) then begin
                  widget_control,event.top,send_event={BOX2GX,id:0l,top:0l,handler:0l,box:ptr_new(*self.pchromobox)}
                end
             end   
  'SAVECHROMO':begin
               if ptr_valid(self.pchromobox) then begin
                 file=dialog_pickfile(default_extension='*.sav',filter='*.sav',/overwrite_promp,/write)
                 if file ne '' then begin
                   box=*self.pchromobox
                   save,box,file=file
                 end
               end
             end                              
 'EXTSELECT':begin
               file=dialog_pickfile(filter='*.pro',TITLE='Please select an extrapolator IDL routine/wrapper',path=gx_findfile(folder='userslib'+path_sep()+'extrapolation'))
               if file ne '' then begin
                if self->ValidExtrapolator(file,template) then begin
                  widget_control,widget_info(event.top,find_by_uname='EXTPATH'),set_value=file
                  widget_control,widget_info(event.top,find_by_uname='TEMPLATE'),set_value=template
                end  
               end 
              end  
    'TEMPLATE':begin
            widget_control,event.id,get_value=template
            self.execute=template
            print,template
            end    
     'TEMPLATERESET':begin
            widget_control,widget_info(self.wControlBase,find_by_uname='EXTPATH'),get_value=file
            if file ne '' then begin
              if self->ValidExtrapolator(file,template) then begin
                widget_control,widget_info(self.wControlBase,find_by_uname='EXTPATH'),set_value=file
                widget_control,widget_info(self.wControlBase,find_by_uname='TEMPLATE'),set_value=template
              end  
            end 
            end                                                                                                                                 
 else:
 endcase
 case event.id of
  self.wPalette:begin
                tvlct,r0,g0,b0,/get
                xloadct,/block
                tvlct,r,g,b,/get
                tvlct,r0,g0,b0
                widget_control,event.id,set_uvalue=[[r],[g],[b]]
                BZ_SCROLL=widget_info(event.top,find_by_uname='BZ_SCROLL')
                widget_control,BZ_SCROLL,get_value=index
                widget_control,BZ_SCROLL,send_event={ID:0l, TOP:0l,Handler:0l,value:index,drag:0}
                end
  self.wBridge:begin
               self.OnBridge=event.select
               widget_control,self.wPause,sensitive=self.OnBridge or self.OnDebug
               widget_control,self.wAbort,sensitive=self.OnBridge or self.OnDebug
               if self.OnBridge then self->OnStart           
              end 
  self.wDebug:begin 
                self.OnDebug=event.select
                widget_control,self.wPause,sensitive=self.OnBridge or self.OnDebug
                widget_control,self.wAbort,sensitive=self.OnBridge or self.OnDebug
                if self.OnDebug then self->OnStart              
              end  
  self.wPause:begin 
               self.OnPause=event.select  
               self.OnTask=1-event.select  
               if event.select then begin
                widget_control,widget_info(self.wControlBase,find_by_uname='STATUSBAR'),$
                set_value='PAUSING: '+strcompress(string(self.level-1,self.levels,format=("('LEVELS COMPLETED: ',i3,' OUT OF',i3)"))) 
               end        
              end 
  self.wAbort:begin
               self.OnAbort=event.select 
               self.OnTask=0   
               if event.select then begin
                widget_control,widget_info(self.wControlBase,find_by_uname='STATUSBAR'),$
                set_value='ABORTING: '+strcompress(string(self.level-1,self.levels,format=("('LEVELS COMPLETED: ',i3,' OUT OF',i3)"))) 
               end   
              end 
  else:
 endcase
return,self->Rewrite(event)
end

function gxMapViewWid::ValidExtrapolator,file,template
                    cdir=curdir()
                     catch, error_stat
                     if error_stat ne 0 then begin
                        catch, /cancel
                        cd,cdir
                        void = dialog_message( /error, !error_state.msg + ' ' + !error_state.sys_msg+'Selected extrapolation routine has an invalid template!' )
                        return,0
                     end
                     dirpath=file_dirname(file,/mark)
                     cd,dirpath
                     break_file, file, dsk_log, dir, filename, ext
                     compile_test=execute('RESOLVE_ROUTINE, filename , /COMPILE_FULL_FILE ,/either')
                     cd,cdir
                     par=ROUTINE_INFO(filename,/par)
                     if par.num_args ge 4 then begin
                       template=filename+',Bin,nz,dr,Bout'
                       for i=4,par.num_args-1 do template+=','+strlowcase(par.args[i])
                       for i=0,par.num_kw_args-1 do begin
                        template+=','+strlowcase(par.kw_args[i])+'='+strlowcase(par.kw_args[i])
                       end
                       self.execute=template
                       return,1
                     endif else begin
                      answ=dialog_message('Selected extrapolation routine has an invalid template!',/error)
                      cd,cdir
                      return,0
                     end  
                  end 

pro gxMapViewWid::OnStart 
                      widget_control,widget_info(self.wControlBase,find_by_uname='TEMPLATE'),get_value=template
                      self.execute=template
                      if self.onPause then begin
                       bout=self.bridge->GetVar('Bout')
                       sz=size(bout)
                       self.bridge->SetVar,'Bin',bout[*,*,sz[3]-1,*]
                       level=self.bridge->GetVar('level')+1
                      endif else begin
                       if self.OnTask then begin
                        self.bridge->Execute,'Bin=Bout'
                        level=self.bridge->GetVar('level')+1
                       endif else begin
                        xrange=self.BZMAP->get(/xrange)
                        yrange=self.BZMAP->get(/yrange)
                        xc=self.BZMAP->get(/xc)
                        yc=self.BZMAP->get(/yc)
                        if tag_exist((self.BZMAP->get(/map)),'rsun') then rsun=(self.BZMAP->get(/map)).rsun else rsun=962.97562
                        Bin=float(self.BZMAP->get(/data))
                        widget_control,widget_info(self.wControlBase,find_by_uname='FOV_dxdy'),get_value=dxdy
                        widget_control,widget_info(self.wControlBase,find_by_uname='EXT_dz'),get_value=dz
                        dr=float([dxdy,dz])
                        widget_control,widget_info(self.wControlBase,find_by_uname='EXT_Nz'),get_value=Nz
                        self.bridge->SetVar,'Bin',Bin
                        self.bridge->SetVar,'nz',nz
                        self.bridge->SetVar,'dr',dr
                        self.bridge->SetVar,'rsun',rsun
                        self.bridge->SetVar,'Nlevels',self.levels
                        nz=fix(nz[0])
                        level=1
                       end
                       self.OnTask=1
                      end 
                      if level eq 1 then begin
                       ptr_free,self.pbox 
                      end 
                      self.level=level  
                      self.bridge->SetVar,'level',level   
                      widget_control,widget_info(self.wControlBase,find_by_uname='STATUSBAR'),$
                      set_value='COMPUTING: '+strcompress(string(self.level-1,self.levels,format=("('LEVELS COMPLETED: ',i3,' OUT OF',i3)")))
                      if self.OnBridge then self.bridge->Execute,self.execute,/no  
                      if self.OnDebug then begin
                       bin=self.bridge->GetVar('Bin')
                       nz=self.bridge->GetVar('nz')
                       dr=self.bridge->GetVar('dr')
                       rsun=self.bridge->GetVar('rsun')
                       result=Execute(self.execute)
                       if (result eq 1) and (size(BOUT,/n_dim) eq 4) then begin
                        self.bridge->SetVar,'Bout',Bout
                        self.bridge->Execute,'Wait,0.1',/no
                       endif else begin
                        answ=dialog_message(['Error executing ',self.execute,'Reseting extrapolation syntax'],/error)
                        
                        widget_control,self.wBridge,set_button=0
                        widget_control,self.wDebug,set_button=0
                        widget_control,self.wPause,set_button=0,sensitive=0
                        widget_control,self.wAbort,set_button=0,sensitive=0
                        self.OnBridge=0
                        self.OnPause=0
                        self.OnAbort=0
                        self.OnDebug=0
                        self.OnTask=0
                        widget_control,widget_info(self.wControlBase,find_by_uname='STATUSBAR'),$
                        set_value=strcompress(string(self.level,format=("('ALL ',i3,' LEVELS COMPLETED')")))
                        widget_control,widget_info(self.wControlBase,find_by_uname='EXTPATH'),get_value=file
                        if file ne '' then begin
                          if self->ValidExtrapolator(file,template) then begin
                            widget_control,widget_info(self.wControlBase,find_by_uname='EXTPATH'),set_value=file
                            widget_control,widget_info(self.wControlBase,find_by_uname='TEMPLATE'),set_value=template
                          end  
                        end 
                       end 
                      end         
              end               

pro gxMapViewWid::OnCallback,Status, Error,bridge
                    Bcube=self.bridge->GetVar('Bout')
                    self.bridge->SetVar,'Bout',Bcube*0
                    good=where(bcube,count)
                    if count eq 0 then answ=dialog_message(['Invalid input parameters',$
                      '','',$
                      self.execute,$
                      '','',$
                      'You may try one of the following:',$
                      '1. Reset the extrapolation routine template',$
                      '2. Upload the extrapolation routine again',$
                      '3. Run the extrapolation in debug mode'],/error)
                    Nlevels=self.bridge->GetVar('Nlevels')
                    level=self.bridge->GetVar('level')
                    nz=self.bridge->GetVar('nz')
                    dr=self.bridge->GetVar('dr')
                    bz=reform(bcube[*,*,*,2])
                    BzDraw=widget_info(self.wControlBase,find_by_uname='BZ_DRAW')
                    BoxSelect=widget_info(self.wControlBase,find_by_uname='BoxSelect')                    
                    widget_control,widget_info(self.wControlBase,find_by_uname='BOX2GX'),sensitive=1
                    widget_control,widget_info(self.wControlBase,find_by_uname='SAVEBOX'),sensitive=1
                    if ptr_valid(self.pbox) then begin
                     oldcube=*self.pbox
                     ptr_free,self.pbox
                     dr=oldcube.dr
                     id=oldcube.id
                     refmaps=oldcube.refmaps
                     bcube=[[[temporary(oldcube.bcube)]],[[bcube]]]
                    endif else begin
                    if tag_exist((self.BZMAP->get(/map)),'rsun') then rsun=(self.BZMAP->get(/map)).rsun else rsun=962.97562
                     time=self.BZMAP->get(/time)
                     id=self.BZMAP->get(/id)+':'+time
                     dr=dr/rsun
                     refmaps=self.REFMAPS
                    endelse
                     widget_control,BzDraw,set_uvalue=bcube[*,*,*,2]
                     self.pbox=ptr_new({bcube:temporary(bcube),dr:dr,id:id,refmaps:refmaps})
                     widget_control, BoxSelect, set_droplist_select=0,send_event={id:0l,top:0l,handler:0l,index:0l,z_index:long(level*nz-1)}
                     self.level=level
                     if self.level eq Nlevels then begin
                      widget_control,self.wBridge,set_button=0
                      widget_control,self.wDebug,set_button=0
                      widget_control,self.wPause,set_button=0,sensitive=0
                      widget_control,self.wAbort,set_button=0,sensitive=0
                      self.OnBridge=0
                      self.OnPause=0
                      self.OnAbort=0
                      self.OnDebug=0
                      self.OnTask=0
                      widget_control,widget_info(self.wControlBase,find_by_uname='STATUSBAR'),$
                      set_value=strcompress(string(self.level,format=("('ALL ',i3,' LEVELS COMPLETED')")))
                     endif else begin
                      prefix='' 
                      if self.OnPause then Prefix='PAUSED: '
                      if self.OnAbort then Prefix='ABORTED: '
                      widget_control,widget_info(self.wControlBase,find_by_uname='STATUSBAR'),$
                      set_value=prefix+strcompress(string(self.level,Nlevels,format=("('LEVELS COMPLETED: ',i3,' OUT OF',i3)")))
                      if self.OnTask then self->OnStart 
                     end                  
end



pro gxMapViewWid::Cleanup
 if ptr_valid(self.REFMAPS) then begin
  obj_destroy,*self.REFMAPS
  ptr_free,self.REFMAPS
 end
 obj_destroy,self.plotman_obj
 if obj_valid(self.bridge) then begin
  if self.bridge->Status() eq 1 then self.bridge->Abort
  obj_destroy,self.bridge
 end
end

function gxMapViewWid::Rewrite,event
 return,event
end

; Routine to call via addplot method  
pro add2plotman, x=x, y=y,id=id, _extra=_extra
oplot, x, y, psym=2, color=243
ssw_legend,id, /top, /left, box=0, charsize=1., linest=0, col=243
end

pro gxMapViewWid__define
 struct_hide,{gxMapViewWid, $
 plotman_obj:obj_new(),$
 REFMAPS:ptr_new(),$
 PBOX:ptr_new(),$
 PCHROMOBOX:ptr_new(),$
 BZMAP:obj_new(),$
 WLMAP:obj_new(),$
 bridge:obj_new(),$
 wPlotmanBase:0l,$
 wControlBase:0l,$
 wToolbar:0l,$
 wBridge:0l,$
 wPause:0l,$
 wAbort:0l,$
 wDebug:0l,$
 wPalette:0l,$
 OnBridge:0b,$
 OnPause:0b,$
 OnAbort:0b,$
 OnDebug:0b,$
 OnTask:0b,$
 execute:'',$
 level:0,$
 Levels:0}
end