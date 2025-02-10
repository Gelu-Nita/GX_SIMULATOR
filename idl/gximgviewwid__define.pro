function gxImgColorbarTICKFORMAT,direction, index, value,data=data
 default,data,[0,255]
 min=min(data,max=max,/nan)
 return,strcompress(string(min+value*(max-min)/256,format='(g12.2)'))
end

;------------------------------------------------------------------------------------------------
function gxImgViewWid::Init, wParent,info=info,renderer=renderer,nx=nx,ny=ny,wToolbarbase=wToolbarbase,draw_xsize=draw_xsize,draw_ysize=draw_ysize,$
                       xpad=xpad,ypad=ypad,frame=frame,wPlotBase=wPlotBase,$
                       xrange=xrange,yrange=yrange,uploadbttn=uploadbttn,savebttn=savebttn,fovmap=fovmap

if n_elements(renderer) eq 0 then return,0
self.renderer=renderer 
case size(info,/tname) of
  'POINTER':self.info=info
  'STRUCT':self.info=ptr_new(info)
  else: return,0
endcase
if obj_valid(fovmap) then self.fovmap=fovmap

default,xrange,[-1.5,1.5]
default,yrange,[-1.5,1.5]


default,nx,64
default,ny,64
default,xtitle,'X(Mm)'
default,ytitle,'Y(Mm)'

device, get_screen_size=scr
if not exist(draw_xsize) then draw_xsize = fix (scr[0] * .35)
if not exist(draw_ysize) then draw_ysize = draw_xsize * 1.1

default,xpad,draw_xsize*0.22
default,ypad,draw_ysize*0.13

self.xpad=xpad
self.ypad=ypad
self.xsize=draw_xsize-1.5*self.xpad
self.ysize=self.xsize

result=self->IDLexWidget::Init(wParent,frame=frame)
if result eq 0 then return,0

self.wBase = widget_base( self.wIDBase, $
    /column, $
    event_func='IDLexWidget__HandleEvent', $
    uvalue=self, $
    notify_realize='IDLexWidget__OnRealize', $
    uname=name,_extra=_extra)
prefix='GXIMAGE:'
self.wToolBarBase=widget_valid(wToolbarbase)?wToolbarbase:widget_base(self.wBase,/toolbar,/row,/frame)
wPSFBase=widget_base(self.wBase,/frame,/row,/toolbar)
self.wChannBase=widget_base(self.wBase,/row,/toolbar,/frame)
self.wChannels[0]=cw_field(self.wChannBase,/int,value=0,xsize=4,title='')
self.wChannels[1]=cw_field(self.wChannBase,/int,value=0,xsize=4,title='')
self.wPSF[0]=cw_objfield(wPSFBase,value=7.0,xtextsize=3,label='a=',unit='"')
self.wPSF[1]=cw_objfield(wPSFBase,value=7.0,xtextsize=3,label='b=',unit='"')
self.wPSF[2]=cw_objfield(wPSFBase,value=0.0,xtextsize=3,label='phi=',unit=STRING(176b))
self.wPSF[4]=cw_objfield(wPSFBase,value=0.0,xtextsize=3,label='min=',unit=STRING('"'))
self.wPSF[3]=cw_bgroup(wPSFBase,['Convolve'],set_value=0,/nonexclusive)
ExecBase=widget_base(self.wToolBarBase,/toolbar,/row)
subdirectory=['resource', 'bitmaps']
self.LockPalette=0
self.wLockPalette= widget_button( ExecBase, $
            value=self.LockPalette?gx_bitmap(gx_findfile('lock.bmp')):gx_bitmap(gx_findfile('unlock.bmp')), $
            /bitmap,tooltip='Lock/Unlock Color Table')
self.wPalette = widget_button( ExecBase, $
            value=gx_bitmap(filepath('palette.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Change Color Table')
if keyword_set(uploadbttn) then begin
          self.wImportGXlog=widget_button( ExecBase, $
            value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Upload GX Simulator log file')
endif 

if keyword_set(savebttn) then self.wLog2File=widget_button( ExecBase, $
  value=gx_bitmap(filepath('copy.bmp', subdirectory=subdirectory)), $
  /bitmap,tooltip='Save the GX Simulator log file')
            
self.wPrintImg=widget_button( ExecBase, $
            value=gx_bitmap(filepath('print1.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Print image')
self.wExportImg=widget_button( ExecBase, $
            value=gx_bitmap(filepath('print.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Export image')
self.wMovie= widget_button( ExecBase, $
            value=gx_bitmap(filepath('eba_meth_ex_cm.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Generate Video')            
self.wSave=widget_button( ExecBase, $
            value=gx_bitmap(filepath('save.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Save Maps to File')                      
self.wExportImgCube=widget_button( ExecBase, $
            value=gx_bitmap(filepath('export.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Export image cube' ) 
self.wImportImgCube=widget_button( ExecBase, $
            value=gx_bitmap(filepath('importf.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Import image cube')                                 
if ~keyword_set(uploadbttn) then self.wMap2Plotman=widget_button( ExecBase, $
            value=gx_bitmap(filepath('contour.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Send Map to Plotman')   
self.wSaveTb=widget_button( ExecBase, $
            value=gx_bitmap(filepath('bulb.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Save Tb Maps to File') 
widget_control, self.wSaveTb,sensitive=0                                                    

row_base=widget_base(self.wBase,/row)
self.wDrawImg = widget_draw( $
    row_base, $
    xsize=draw_xsize, $
    ysize=draw_ysize, $
    /button_events, $
    /motion_events, $
    retain=0, $
    ;renderer=renderer, $
    /expose_events, $
    uname=prefix + 'draw', $
    graphics_level=2 $
    )
self.wSlider=WIDGET_SLIDER(row_base,/drag,/vert,font=!defaults.font)
self.wContrast=WIDGET_SLIDER(self.wBase,minimum=0,maximum=1000,value=1000,/suppress,/drag,font=!defaults.font)

if ~widget_valid(wPlotBase) then wPlotBase=widget_base(row_base,/column)
widget_control, wPlotBase, EVENT_FUNC='gxImgViewHandleEvent',Set_UVALUE=self
wToolbar=widget_base(wPlotBase,/toolbar,/row,/frame)
self.wSpec2Plotman=widget_button( wToolbar, $
            value=gx_bitmap(filepath('plot.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Send Spectrum to Plotman')  
self.wSpec2File=widget_button( wToolbar, $
            value=gx_bitmap(filepath('save.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Save spectrum to file')              
self.wImportReference=widget_button(wToolbar,value=gx_bitmap(filepath('importd.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Import reference spectrum')
self.wDeleteReference=widget_button(wToolbar,value=gx_bitmap(filepath('delete.bmp', subdirectory=subdirectory)), $
            /bitmap,tooltip='Remove reference spectrum')            
self.wPlotSelection= cw_bgroup(wToolBar, ['Pixel','Image'],/row,/exclusive,/RETURN_NAME)
widget_control,self.wPlotSelection,Set_Value=0

wTimeReferenceBase=widget_base(wPlotBase,/row,map=0)
self.wTimeReference=WIDGET_SLIDER(wTimeReferenceBase,minimum=0,maximum=01,value=0,xsize=draw_xsize,/suppress,/drag,font=!defaults.font)
wTimeLabel=widget_label(wTimeReferenceBase,value='',/dynamic,font=!defaults.font)

wExtraPlotBase=widget_base(wPlotBase,/row)
self.wPlot = widget_draw(wExtraPlotBase, $
        xsize=draw_xsize, $
        ysize=draw_ysize, $
        graphics_level=1, $
        retain=0, $
        /expose_events, $
        Uname='IMAGE PROFILES')
window,/free,/pixmap,xsiz=draw_xsize,ysiz=draw_ysize
erase,255
self.wPixMap = !d.window
prefix='PlotOptions:'
PlotOptionBase = WIDGET_BASE(wExtraPlotBase,/Column,frame=1)
self.wCharSize=cw_objfield(PlotOptionBase,value=2.0,label='Plots Char Size',increment=0.1,xtextsize=4)
self.wSpectralPlotOptions=cw_objPlotOptions(PlotOptionBase,uname='Spectral Plot Options',/xlog,/ylog)
g=widget_info(self.wSpectralPlotOptions,/geometry)
self.wSpectralExtraOptions=cw_bgroup(PlotOptionBase,['Show Res. Xrange'],/column,/nonexclusive,xsize=g.scr_xsize,/frame)
self.wSDEV=widget_text(PlotOptionBase,uname='SDEV',scr_ysize=g.ysize/4,ysize=2)
self.wResidualsPlotOptions=cw_objPlotOptions(PlotOptionBase,uname='Residuals Plot Options',/xlog)
self.wRESCHI=cw_bgroup(PlotOptionBase,['RES','CHI'],/row,/exclusive,xsize=g.scr_xsize,/frame)
widget_control,self.wRESCHI,set_value=0,sensitive=0
self.wResidualsExtraOptions=cw_bgroup(PlotOptionBase,['Show Spec. Xrange','Limit to Yrange'],/column,/nonexclusive,xsize=g.scr_xsize,/frame)
self.wRDEV=widget_text(PlotOptionBase,uname='RDEV',scr_ysize=g.ysize/4,ysize=2)
self.wXProfilePlotOptions=cw_objPlotOptions(PlotOptionBase,uname='X Profile Plot Options',/ylog)
self.wYProfilePlotOptions=cw_objPlotOptions(PlotOptionBase,uname='Y Profile Plot Options',/ylog)


self.oViewgroup = obj_new('IDLexViewgroup')
self.oView=obj_new('IDLexInscribingView',location=[0,0],dimensions=[draw_xsize,draw_ysize],viewplane_rect=[0, 0, draw_xsize, draw_ysize])
self.oViewgroup->Add,self.oView
self.oModel=obj_new('IdlgrModel')
self.oView->Add,self.oModel

self.oPalette = OBJ_NEW('IDLgrPalette')
rgb_curr=bytarr(3,256)
tvlct,rgb_curr,/get
loadct,39
rgb=bytarr(3,256)
tvlct,rgb,/get
tvlct,rgb_curr
device, get_screen_size=scr
myfont = OBJ_NEW('IDLgrFont',!defaults.font,size=scr[0] lt 3200?8:12)
self.oPalette -> SetProperty, RED_VALUES = rgb[*,0], GREEN_VALUES = rgb[*,1], BLUE_VALUES = rgb[*,2]
self.oColorbar = OBJ_NEW( 'IDLgrColorbar',Palette = self.oPalette,SHOW_AXIS=1, /SHOW_OUTLINE,TICKFORMAT='gxImgColorbarTICKFORMAT',Major=3,dimensions=[self.xsize,self.xpad/5])
self.oColorbar->GetProperty, TICKTEXT=xticktext
xticktext->SetProperty,font=myfont
self.oColorbar->Translate,xpad,ypad/2,0
self.oModel->Add,self.oColorbar
self.oLabel = OBJ_NEW( 'IDLgrText','',locations=[self.xsize/2+self.xpad,self.ysize+2.5*self.ypad],alignment=0.5,font=myfont)
self.oModel->Add,self.oLabel
self.oLabel->GetProperty,char_dim=char_dim
pData=self->NewView(nx=nx,ny=ny,xrange=xrange,yrange=yrange)
return,1
end

;-----------------------------------------------------------------------------------------
function gxImgViewWid::NewView,info,renderer=renderer,nx=nx,ny=ny,xrange=xrange,yrange=yrange,data=data,fovmap=fovmap
 case size(info,/tname) of
  'POINTER':self.info=info
  'STRUCT':self.info=ptr_new(info)
  else: 
 endcase
 if size(renderer,/tname) eq 'STRING' then self.renderer=renderer
 if ~ptr_valid(self.info) then return,self.pData
 if n_elements(nx) ne 1 then return,self.pData
 if n_elements(ny) ne 1 then return,self.pData
 if n_elements(xrange) ne 2 then return,self.pData
 if n_elements(yrange) ne 2 then return,self.pData
 self.nx=nx
 self.ny=ny
 ptr_free,self.pData
 dim=[nx,ny,(*self.info).pixdim]
 if array_equal(size(data,/dim), dim) then self.pData=ptr_new(data) else self.pData=ptr_new(make_array(dim,/float))
 if obj_valid(fovmap) then self.fovmap=fovmap
 self.pConvolvedData=ptr_new(make_array(dim,/float))


 R=obj_valid(self.fovmap)?self.fovmap->get(/rsun):(pb0r()*60)[2]
 xtitle='X(arcsecs)'
 ytitle='Y(arcsecs)'
 xrange=xrange*R
 yrange=yrange*R
 deltax=max(xrange,min=min)-min
 deltay=max(yrange,min=min)-min

 self.oModel->Remove,self.oImage
 self.oModel->Remove,self.xAxis
 self.oModel->Remove,self.yAxis
 obj_destroy,[self.oImage,self.xAxis,self.yAxis,self.oXX,self.oYY]

 self.oImage=OBJ_NEW('IDLgrImage', bytscl((*self.pdata)[*,*]),$
             Palette=self.oPalette,DEPTH_TEST_DISABLE=2,$
             xcoord_conv=[self.xpad,self.xsize/self.nx],ycoord_conv=[2*self.ypad,self.ysize/self.ny])
          
 self.xAxis[0]=obj_new('IDLgrAxis',0,location=[self.xpad,2*self.ypad],range=xrange,xcoord_conv=[self.xpad-xrange[0]*self.xsize/deltax,self.xsize/deltax],/exact,$
               ticklen=0.05*self.xsize,TICKDIR=1,Title=obj_new('IdlgrText',xtitle))
 self.xAxis[1]=obj_new('IDLgrAxis',0,location=[self.xpad,2*self.ypad+self.ysize],range=xrange,xcoord_conv=[self.xpad-xrange[0]*self.xsize/deltax,self.xsize/deltax],/exact,$
               ticklen=0.05*self.xsize,TICKDIR=0,/notext)
 self.yAxis[0]=obj_new('IDLgrAxis',1,location=[self.xpad,2*self.ypad],range=yrange,ycoord_conv=[2*self.ypad-yrange[0]*self.ysize/deltay,self.ysize/deltay],/exact,$
               ticklen=0.05*self.ysize,TICKDIR=1,Title=obj_new('IdlgrText',ytitle))
 self.yAxis[1]=obj_new('IDLgrAxis',1,location=[self.xpad+self.xsize,2*self.ypad],range=yrange,ycoord_conv=[2*self.ypad-yrange[0]*self.ysize/deltay,self.ysize/deltay],/exact,$
               ticklen=0.05*self.ysize,TICKDIR=0,/notext)
 device, get_screen_size=scr              
 myfont = OBJ_NEW('IDLgrFont',!defaults.font,size=scr[0] lt 3200?8:12)
 self.xAxis[0]->GetProperty, TICKTEXT=xticktext,title=xtitle
 self.yAxis[0]->GetProperty, TICKTEXT=yticktext,title=ytitle
 xticktext->SetProperty,font=myfont  
 yticktext->SetProperty,font=myfont  
 xtitle->SetProperty,font=myfont
 ytitle->SetProperty,font=myfont            
 self.oColorbar->SetProperty,TICKFRMTDATA=(*self.pdata)[*,*]
 self.oModel->Add,self.oImage
 self.oModel->Add,self.xAxis
 self.oModel->Add,self.yAxis
 widget_control,self.wChannels[0],set_value=0
 widget_control,self.wChannels[1],set_value=0
 self.oXX=obj_new('IDLgrPolyline',[0,nx],[0,0],[1,1],linesty=1,color=[255,255,255],xcoord_conv=[self.xpad,self.xsize/self.nx],ycoord_conv=[2*self.ypad,self.ysize/self.ny])
 self.oYY=obj_new('IDLgrPolyline',[0,0],[0,ny],[1,1],linesty=1,color=[255,255,255],xcoord_conv=[self.xpad,self.xsize/self.nx],ycoord_conv=[2*self.ypad,self.ysize/self.ny])
 self.oModel->Add,self.oXX
 self.oModel->Add,self.oYY
 self->NewRenderer
 return,self.pData
end

pro gxImgViewWid::NewSpectrumSize
 nmaps=n_elements(((*self.info).spectrum).x.axis)
 widget_control,self.wSlider,SET_SLIDER_MIN=0,SET_SLIDER_MAX=nmaps-1,set_value=0,sensitive=nmaps-1
 value=((*self.info).spectrum).x.axis[0]
 self.oLabel->SetProperty,strings=string(value,((*self.info).spectrum).x.unit,format="(g0,' ',a)")
 if widget_valid(self.wChannels[2])  then widget_control,self.wChannels[2],/destroy
 if nmaps gt 0 then begin
 self.wChannels[2]=cw_objfield(self.wChannbase,label=((*self.info).spectrum).x.label,unit=((*self.info).spectrum).x.unit,$
                    value=((*self.info).spectrum).x.axis[0],/indicator,xtextsize=12)
 end
 if tag_exist((*self.info),'RGB') then begin
   self.LockPalette=0
   self->OnLockPalette
 endif else begin
   tvlct,rgb_curr,/get
   loadct,39,/silent
   tvlct,rgb,/get
   tvlct,rgb_curr
   self.oPalette->SetProperty, RED_VALUES = rgb[*,0], GREEN_VALUES = rgb[*,1], BLUE_VALUES = rgb[*,2]
 end                  
end

pro gxImgViewWid::MoveData,data
 ptr_free,self.pData
 self.pData=ptr_new(temporary(data))
end

pro gxImgViewWid::OnStartScan
 if isa(self.model,'gxmodel') then begin
 (self.model->GetVolume())->GetVertexAttributeData,'NTkey',gx_key
  gx_key=(n_elements(gx_key) ne 0) ? string(gx_key):''
  self.fovmap=(self.model->scanbox())->GetFOVMap()
  map=self.fovmap->get(/map)
  directions=self.model->GetDirections()
  add_prop,map,gx_key=gx_key,/replace
  add_prop,map,renderer=file_basename(self.renderer),/replace
  add_prop,map,directions=directions,/replace
  self.fovmap->setmap,0,map
 endif
 if widget_valid(self.wSave) then widget_control,self.wSave,sensitive=0
 if widget_valid(self.wLog2File) then widget_control,self.wLog2File,sensitive=0
 if widget_valid(self.wMap2Plotman) then widget_control,self.wMap2Plotman,sensitive=0
 if widget_valid(self.wPSF[3]) then widget_control,self.wPSF[3],sensitive=0,set_value=0
 dembase=widget_info(get_tlb(self.wBase),find_by_uname='DEMBASE')
 if widget_valid(dembase) then flagbase=widget_info(dembase,/parent) else flagbase=-1
 if widget_valid(flagbase) then widget_control,flagbase,sensitive=0
 self.newPSF=1
end

pro gxImgViewWid::OnEndScan
 if widget_valid(self.wSave) then widget_control,self.wSave,sensitive=1
 if widget_valid(self.wLog2File) then widget_control,self.wLog2File,sensitive=1
 if widget_valid(self.wMap2Plotman) then widget_control,self.wMap2Plotman,sensitive=1
 if widget_valid(self.wPSF[3]) then widget_control,self.wPSF[3],sensitive=1
 dembase=widget_info(get_tlb(self.wBase),find_by_uname='DEMBASE')
 if widget_valid(dembase) then flagbase=widget_info(dembase,/parent) else flagbase=-1
 if widget_valid(flagbase) then widget_control,flagbase,sensitive=1
end

function gxImgViewWid::SaveLog
 logfile=GETENV('IDL_TMPDIR')+'GX_Simulator.log'
 if ~file_exist(logfile) then begin
  answ=dialog_message('The GX_Simulator.log file has been already removed or renamed.'+string(10b)+'There is nothing to be saved!')
  if widget_valid(self.wLog2File) then widget_control,self.wLog2File,sensitive=0
  return,1
 end

 file=dialog_pickfile(filter='*.gxl',$
                   DEFAULT_EXTENSION='gxl',$
                   /write,/OVERWRITE_PROMPT,$
                   title='Please select a file to save the current GX simulator log file')
 if file ne '' then begin
  close,/all
  file_move,logfile,file,/OVERWRITE,/allow_same
  if widget_valid(self.wLog2File) then widget_control,self.wLog2File,sensitive=0
  saved=1
 endif else saved=0
 return,saved
end


function gxImgViewWid::SaveTbButton
  lookup=where(strcompress(strupcase(((*self.info).parms).name),/rem) eq strcompress(strupcase('f_min'),/rem),count)
  return, (count gt 0)
end

pro gxImgViewWid::NewRenderer
 self->NewSpectrumSize
 if widget_valid(self.wChannels[3])  then widget_control,self.wChannels[3],/destroy
 if n_elements(((*self.info).spectrum).y.label) gt 1 then begin
  self.wChannels[3]=widget_combobox(self.wChannbase,value=((*self.info).spectrum).y.label)
  sz=size(*self.pData,/dim)
  if n_elements(((*self.info).spectrum).y.label) gt sz[3] then widget_control,self.wChannels[3],SET_COMBOBOX_SELECT=sz[3]
 end
 if widget_valid(self.wChannels[4])  then widget_control,self.wChannels[4],/destroy
 if tag_exist((*self.info),'channels') then  self.wChannels[4]=widget_combobox(self.wChannbase,value=(*self.info).channels)
 widget_control,self.wSaveTb,sensitive=self->SaveTbButton()
 self.newPSF=1
 widget_control,self.wPSF[3],set_value=0
end

;-----------------------------------------------------------------
function gxImgViewWid::data_indices,chanid=chanid
 chanid=' '
 widget_control,self.wChannels[0],get_value=x
 widget_control,self.wChannels[1],get_value=y
 widget_control,self.wSlider,get_value=z
 idx=[x,y,z]
 for i=3,n_elements(self.wChannels)-1 do begin
  if widget_valid(self.wChannels[i]) then begin
   widget_control,self.wChannels[i],get_value=value
   item=widget_info(self.wChannels[i],/COMBOBOX_GETTEXT)
   if i le 3 then chanid=chanid+' '+item
   idx=[idx,where(value eq item)]
  end
 end
 return,idx
end

;--------------------------------------------------------------------
pro gxImgViewWid::Draw
compile_opt hidden
self.oWindow->Draw,self.oView
end

function gxImgViewWid::SFU2TB
    self.xaxis[0]->getproperty,xrange=xrange
    self.yaxis[0]->getproperty,yrange=yrange
    ds=delta(xrange)*delta(yrange)/self.nx/self.ny
    coeff=gx_sfu2tb(ds)
    return,coeff
end

pro gxImgViewWid::PlotProfile,objxy
compile_opt hidden
  if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'
  widget_control,self.wPlot,get_value=window
  geometry=widget_info(self.wPlot,/geometry)
  wset,self.wPixMap
  rgb_curr=bytarr(3,256)
  tvlct,rgb_curr,/get
  pmulti=!p.multi
  !p.multi=[0,1,3]
  self.xAxis[0]->GetProperty,range=xrange
  self.yAxis[0]->GetProperty,range=yrange
  idx=self->data_indices()
  if ptr_valid(self.refspectrum) then hasref=1 else hasref=0
  widget_control,self.wPSF[3],get_value=cv
  pData=(cv[0] eq 0)?self.pData:self.pConvolvedData
  sz=size(*pData)
;  if sz[0] eq 3 then if sz[3] eq 1 then goto,one_image
   case n_elements(idx) of
   2:begin
      ;provision for one image only
      one_image:
      xprofile=reform((*pData)[idx[0],*])
      yprofile=reform((*pData)[*,idx[1]])
      spectrum=reform((*pData)[*,*])
     end
   3:begin
      xprofile=reform((*pData)[idx[0],*,idx[2]])
      yprofile=reform((*pData)[*,idx[1],idx[2]])
      spectrum=reform((*pData)[*,*,*])
     end
   4:begin
      xprofile=reform((*pData)[idx[0],*,idx[2],*])
      yprofile=reform((*pData)[*,idx[1],idx[2],*])
      spectrum=reform((*pData)[*,*,*,*])
     end
   else: begin
          ;no more than 5 dimensions expected, but let's play safe
          xprofile=reform((*pData)[idx[0],*,idx[2],*,idx[4]])
          yprofile=reform((*pData)[*,idx[1],idx[2],*,idx[4]])
          spectrum=reform((*pData)[*,*,*,*,idx[4]])
         end
   endcase
   widget_control,self.wPlotSelection,get_value=PlotSelection
   case PlotSelection of
   1:begin
      spectrum=reform(total(total(spectrum,1,/nan),1,/nan))
      if hasref then begin
       xref=(*self.refspectrum).x
       widget_control,self.wTimeReference,get_value=t
       yref=((*self.refspectrum).y)[*,*,t]
       if tag_exist(*self.refspectrum,'yerr') then yerr=((*self.refspectrum).yerr)[*,*,t]
      endif
     end 
   else:begin
         spectrum=reform(spectrum[idx[0],idx[1],*,*])
         if hasref then begin
          xref=(*self.refspectrum).x
          widget_control,self.wTimeReference,get_value=t
          sz=size((*pData)[*,*,0,0,0,0,0,0]) 
          yref=((*self.refspectrum).y)[*,*,t]/sz[1]/sz[2]
          if tag_exist(*self.refspectrum,'yerr') then yerr=((*self.refspectrum).yerr)[*,*,t]/sz[1]/sz[2]
         endif
        end 
   endcase
   
   ;Handle Polarization and Tb
   
   coeff=self->SFU2TB()
   coeff=coeff/2;NORH CONVENTION
   axis=(((*self.info).spectrum).x.axis)
   f2=axis^2
   fidx=idx[2]
   if size(xprofile,/n_dim) eq 2 then begin
    spectrum=(size(spectrum))[0] eq 1?reform(spectrum,1,(size(spectrum))[1]):spectrum
    idx=idx[3]
    sz=size(*pData,/dim)
    case idx of
     sz[3]:begin
            xprofile=total(xprofile,2)
            yprofile=total(yprofile,2)
            spectrum=total(spectrum,2)
            if hasref then if size(yref,/n_dim) ge 2 then begin
              yref=total(yref,2)
              if isa(yerr) then yerr=sqrt(total(yerr^2,2))
            endif
           end
     (sz[3]+1):begin
            xprofile=(xprofile[*,1]-xprofile[*,0])
            yprofile=(yprofile[*,1]-yprofile[*,0])
            spectrum=(spectrum[*,1]-spectrum[*,0])
            if hasref then begin
              yref=yref[*,1]-yref[*,0]
              if isa(yerr) then yerr=sqrt(yerr[*,1]^2+yerr[*,0]^2)
            endif
           end
     (sz[3]+2):begin
            xprofile=100*(xprofile[*,1]-xprofile[*,0])/total(xprofile,2)
            yprofile=100*(yprofile[*,1]-yprofile[*,0])/total(yprofile,2)
            spectrum=100*(spectrum[*,1]-spectrum[*,0])/total(spectrum,2)
            if hasref then begin
              yref=100*(yref[*,1]-yref[*,0])/total(yref,2)
              if isa(yerr) then begin
                yerr=100*sqrt((yref[*,1]^2+yref[*,0]^2)/(yerr[*,1]^2+yerr[*,0]^2))
              endif
            endif
           end
     (sz[3]+3):begin
             xprofile=coeff*xprofile[*,0]/f2[fidx]
             yprofile=coeff*yprofile[*,0]/f2[fidx]
             spectrum=coeff*spectrum[*,0]/f2
            end
     (sz[3]+4):begin
             xprofile=coeff*xprofile[*,1]/f2[fidx]
             yprofile=coeff*yprofile[*,1]/f2[fidx]
             spectrum=coeff*spectrum[*,1]/f2
            end
     (sz[3]+5):begin
                xprofile=coeff*(xprofile[*,0]+xprofile[*,1])/f2[fidx]
                yprofile=coeff*(yprofile[*,0]+yprofile[*,1])/f2[fidx]
                spectrum=coeff*(spectrum[*,0]+spectrum[*,1])/f2
              end  
     (sz[3]+6):begin
               xprofile=coeff*(xprofile[*,1]-xprofile[*,0])/f2[fidx]
               yprofile=coeff*(yprofile[*,1]-yprofile[*,0])/f2[fidx]
               spectrum=coeff*(spectrum[*,1]-spectrum[*,0])/f2
      end              
    else:begin
           xprofile=xprofile[*,idx]
           yprofile=yprofile[*,idx]
           spectrum=(size(spectrum))[0] eq 1?spectrum[idx]:spectrum[*,idx]
           if hasref then if size(yref,/n_dim) ge 2 then yref=yref[*,idx]
         end
    endcase
   endif else idx=0
   if hasref then begin
    if size(yref,/n_dim) ge 2 then yref=yref[*,idx]
    if size(yerr,/n_dim) ge 2 then yerr=yerr[*,idx]
   endif

  ytitle=((*self.info).spectrum).y.label[idx]+' ('+((*self.info).spectrum).y.unit[idx]+')'
  axis=((*self.info).spectrum).x.axis
  good=where(finite(spectrum) eq 1,count)
  m=max(spectrum)
  if count gt 1 then begin
   if arg_present(objxy) then begin
    objxy= obj_new('xyplot',axis,spectrum)
    objxy->set, id='GX: '+ytitle, data_unit='Flux[sfu]', dim1_unit='Frequency[GHz]', dim1_ids=['GX:'+ytitle]  ;adds labeling information
   end
   widget_control,self.wSpectralPlotOptions,get_value=SpecPlotOptions
   SpecPlotOptions->GetProperty,range=spec_range,xrange=spec_pxrange,yrange=spec_pyrange,xlog=spec_xlog,ylog=spec_ylog
   widget_control,self.wResidualsPlotOptions,get_value=ResPlotOptions
   ResPlotOptions->GetProperty,range=res_range,xrange=res_pxrange,yrange=res_pyrange,xlog=res_xlog,ylog=res_ylog
   widget_control,self.wSpectralExtraOptions,get_value=SpecExtraOptions
   widget_control,self.wResidualsExtraOptions,get_value=ResidualsExtraOptions
   catch, error_stat
   if error_stat ne 0 then begin
     catch, /cancel
     goto,skip_plot
   end
   widget_control,self.wCharSize,get_value=charsize
   if total(spectrum,/nan) eq 0 and hasref then pyrange=minmax(yref)
   if SpecExtraOptions[0] eq 0 then begin 
   plot,axis,spectrum,charsize=charsize,color=0,back=255,xlog=spec_xlog,ylog=spec_ylog,xrange=spec_pxrange,yrange=spec_pyrange,$
                     xtitle=((*self.info).spectrum).x.unit,ytitle=ytitle,xsty=isa(spec_pxrange),ysty=isa(spec_pyrange)              
   endif else begin
   plot,axis,spectrum,charsize=charsize,color=0,back=255,xlog=spec_xlog,ylog=spec_ylog,xrange=res_pxrange,yrange=spec_pyrange,$
                     xtitle=((*self.info).spectrum).x.unit,ytitle=ytitle,xsty=isa(res_pxrange),ysty=isa(spec_pyrange)
   endelse
                     
   if spec_range eq 'Auto' then SpecPlotOptions->SetProperty,xrange=keyword_set(spec_xlog)?10^!x.crange:!x.crange, yrange=keyword_set(spec_ylog)?10^!y.crange:!y.crange
   if hasref then begin
    loadct,39
    oplot,xref,yref,psym=2,color=0
    if n_elements(yref) eq n_elements(yerr) then $
    oplot_err,xref,yref,yerr=yerr,psym=3,color=0,bcolor=250,thick=3
    
    data = SPLINE( axis, spectrum, xref )
    if n_elements(spec_pxrange) eq 2 then begin
      good=where(xref ge spec_pxrange[0] and xref le spec_pxrange[1],count)
      if count gt 0 then range_idx=good
    endif
    metrics=gx_metrics_spectrum(data,yref,yerr,range_idx=range_idx)
    rdev=metrics.res2_norm
    widget_control,self.wRESCHI,get_value=reschi
    
    if tag_exist(metrics,'chi2') then begin
     widget_control,self.wRESCHI,sensitive=1
     res=(keyword_set(reschi)?metrics.chi_spec:metrics.res_spec_norm)
     restitle=keyword_set(reschi)?'(ModelSpec-RefSpec)/RefErr':'(ModSpec-RefSpec)/RefSpec'
     chi2=metrics.chi2
    endif else begin
      widget_control,self.wRESCHI,sensitive=0
      res=metrics.res_spec_norm
      restitle='Data Normalized Residual'
    endelse
    info=string(rdev,format="('FULL RDEV=',g0)")
    if isa(chi2) then info =[info, string(chi2,format="('FULL CHI2=',g0)")]
    
    widget_control,self.wSDEV,set_value=info
    if ResidualsExtraOptions[0] eq 0 then begin 
      plot,xref,res,charsize=charsize,color=0,back=255,xlog=res_xlog,ylog=res_ylog,xrange=res_pxrange,yrange=res_pyrange,psym=-2,$
           xtitle=((*self.info).spectrum).x.unit,ytitle=restitle,xsty=isa(res_pxrange),ysty=isa(res_pyrange)
    endif else begin
      plot,xref,res,charsize=charsize,color=0,back=255,xlog=res_xlog,ylog=res_ylog,xrange=spec_pxrange,yrange=res_pyrange,psym=-2,$
           xtitle=((*self.info).spectrum).x.unit,ytitle=restitle,xsty=isa(spec_pxrange),ysty=isa(res_pyrange)
      if isa(res_pxrange) then begin
        oplot,res_pxrange[[0,0]],keyword_set(res_ylog)?10^!y.crange:!y.crange,thick=2,linesty=2,color=0
        oplot,res_pxrange[[1,1]],keyword_set(res_ylog)?10^!y.crange:!y.crange,thick=2,linesty=2,color=0
      endif
    endelse
    oplot,keyword_set(res_xlog)?10^!x.crange:!x.crange,[0,0],color=0
    if res_range eq 'Auto' then ResPlotOptions->SetProperty,xrange=keyword_set(res_xlog)?10^!x.crange:!x.crange, yrange=keyword_set(res_ylog)?10^!y.crange:!y.crange
    ResPlotOptions->GetProperty,range=res_range,xrange=res_pxrange,yrange=res_pyrange,xlog=res_xlog,ylog=res_ylog
   
    if isa(res_pxrange) then $
      good=where((xref ge min(res_pxrange)) and (xref le max(res_pxrange))) 
    if (ResidualsExtraOptions[1] eq 1) and isa(res_pyrange) then $
      good=where((res ge min(res_pyrange)) and (res le max(res_pyrange)) $
                  and (xref ge min(res_pxrange)) and (xref le max(res_pxrange)))    
    metrics=gx_metrics_spectrum(data,yref,yerr,range_idx=good)
    rdev=metrics.res2_norm
    if tag_exist(metrics,'chi2') then chi2=metrics.chi2
    info=string(rdev,format="('RDEV2=',g0)")
    if isa(chi2) then info =[info, string(chi2,format="('CHI2=',g0)")]
    widget_control,self.wRDEV,set_value=info
    !p.multi=[2,2,3]
   endif
  endif else begin
    skip_plot:
    !p.multi=[0,1,2]
  endelse
  
  npoints=n_elements(yprofile)
  xaxis=xrange[0]+findgen(npoints)*(max(xrange,min=min)-min)/(npoints-1)
  good=where(finite(yprofile) eq 1,count)
  if count gt 1 then begin
   widget_control,self.wXProfilePlotOptions,get_value=objPlotOptions
   objPlotOptions->GetProperty,range=range,xrange=pxrange,yrange=pyrange,xlog=xlog,ylog=ylog
   if min(xaxis,max=m) eq m then xlog=0
   if min(yprofile,max=m) eq m then ylog=0 
   plot,xaxis,yprofile,charsize=charsize,color=0,back=255,xtitle='X(Mm)',/xsty,ytitle=ytitle,ylog=ylog,xlog=xlog,xrange=pxrange,yrange=pyrange
   if range eq 'Auto' then objPlotOptions->SetProperty,xrange=keyword_set(xlog)?10^!x.crange:!x.crange, yrange=keyword_set(ylog)?10^!y.crange:!y.crange
  end

  npoints=n_elements(xprofile)
  yaxis=yrange[0]+findgen(npoints)*(max(yrange,min=min)-min)/(npoints-1)
  good=where(finite(xprofile) eq 1,count)
  if count gt 1 then begin
   widget_control,self.wYProfilePlotOptions,get_value=objPlotOptions
   objPlotOptions->GetProperty,range=range,xrange=pxrange,yrange=pyrange,xlog=xlog,ylog=ylog
   plot,yaxis,xprofile,charsize=charsize,color=0,back=255,xtitle='Y(Mm)',/xsty,ytitle=ytitle,ylog=ylog,xlog=xlog,xrange=pxrange,yrange=pyrange
   if range eq 'Auto' then objPlotOptions->SetProperty,xrange=keyword_set(xlog)?10^!x.crange:!x.crange, yrange=keyword_set(ylog)?10^!y.crange:!y.crange
  end 
  wset,window
  device,copy=[0,0,geometry.draw_xsize,geometry.draw_ysize,0,0,self.wPixMap]
  tvlct,rgb_curr
  !p.multi=pmulti
end

;--------------------------------------------------------------------
pro gxImgViewWid::OnRealize
compile_opt hidden
widget_control, self.wDrawImg, get_value=oWindow
self.oWindow = oWindow
self->SelectImg
self->Draw
end
;--------------------------------------------------------------------
pro gxImgViewWid::OnExpose
compile_opt hidden
    self->Draw
end
;--------------------------------------------------------------------

;--------------------------------------------------------------------
function gxImgViewWid::ValidData
compile_opt hidden
    return,ptr_valid(self.pData) 
end
;---------------------------------



function gxImgViewWid::Cursor, event
  result={hit:0}
    if self->ValidData() then begin
      void=self.oWindow->PickData(self.oView,self.oImage,[event.x,event.y],xyz,pick_status=hit)
      sz=size(*self.pdata)
      x=xyz[0]<(sz[1]-1)>[0,0]
      y=xyz[1]<(sz[2]-1)>[0,0]
      if hit then begin
        result={hit:hit,x:x,y:y}
        if self.lock then begin
         widget_control,self.wChannels[0],set_value=x
         widget_control,self.wChannels[1],set_value=y
         self.oXX->GetProperty,data=oXX
         oXX[1,*]=y+0.5
         self.oXX->SetProperty,data=oXX
         self.oYY->GetProperty,data=oYY
         oYY[0,*]=x+0.5
         self.oYY->SetProperty,data=oYY
         self->Draw
        end
      end
  end
  return,result
end

;-------------------------------------------------------------------
pro gxImgViewWid::OnMouseDown, event
compile_opt hidden
    result=self->Cursor(event)
    if result.hit then begin
      self.lock=1-self.lock
      if self.lock then result=self->Cursor(event)
      if result.hit then self->PlotProfile
    end
end
;--------------------------------------------------------------------
pro gxImgViewWid::OnMouseMove, event
compile_opt hidden
    result=self->Cursor(event)
    if result.hit then begin
     self->PlotProfile
    end
end
;--------------------------------------------------------------------
pro gxImgViewWid::OnMouseUp, event
compile_opt hidden
end
;--------------------------------------------------------------------

pro gxImgViewWid::SelectChannel
  if self->ValidData() then begin
    idx=self->data_indices(chanid=chanid)
    row=idx[2]
    if n_elements(idx) gt 3 then idx=idx[3] else idx=0
    value=((*self.info).spectrum).x.axis[row]
    if strcompress(chanid,/rem) eq '' then chanid =((*self.info).spectrum).y.label[idx]
    if widget_valid(self.wChannels[2]) then widget_control,self.wChannels[2],set_value=value
    self.oLabel->SetProperty,strings=(*self.info).pixdim[0] gt 1?$
      strcompress(string(chanid, ((*self.info).spectrum).y.unit[idx],value,((*self.info).spectrum).x.unit,format="(a,'[',a,']',' ','@',' ',f0.2,' ',a)")):$
      strcompress(string(chanid, ((*self.info).spectrum).y.unit[idx],format="(a,'[',a,']')"))
    if ptr_valid(self.info) then begin
      if tag_exist((*self.info),'rgb') then self.oPalette->SetProperty, RED_VALUES = ((*self.info).rgb)[*,0,row], GREEN_VALUES = ((*self.info).rgb)[*,1,row], BLUE_VALUES = ((*self.info).rgb)[*,2,row]
    end
  end
end
;------------------------------------
pro gxImgViewWid::OnSlider, row
compile_opt hidden
    if self->ValidData() then begin
     widget_control,self.wSlider,set_value=row
     self->SelectImg
    end
end
pro gxImgViewWid::Convolve,compute=compute
  widget_control,self.wPSF[3],get_value=conv
  default,compute,(self.newPSF and conv)
  if ~compute then return
  self.oLabel->GetProperty,strings=strings
  self.oLabel->SetProperty,strings='Please wait while convolving raw images.....'
  self->Draw
  widget_control,/hourglass
  widget_control,self.wPSF[0],get_value=a
  widget_control,self.wPSF[1],get_value=b
  widget_control,self.wPSF[2],get_value=phi
  widget_control,self.wPSF[4],get_value=min_ab
  idx=self->data_indices()
  if (n_elements(idx) ge 4) and strmatch(strupcase((*self.info).spectrum.x.unit[0]),'*HZ*') then begin
  a=a/((*self.info).spectrum).x.axis>min_ab
  b=b/((*self.info).spectrum).x.axis>min_ab
  endif else begin
    a=replicate(a,n_elements(((*self.info).spectrum).x.axis))
    b=replicate(b,n_elements(((*self.info).spectrum).x.axis))
  endelse
  self->GetMapParms,xc=xc,yc=yc,time=time,dx=dx,dy=dy
  self.oImage->GetProperty,data=img
  width=size(img,/dimensions)
  ;ensure that width is odd
  if width[0] mod 2 eq 0 then width[0]+=1
  if width[1] mod 2 eq 0 then width[1]+=1
  ;;;
  ConvolvedData=(*self.pConvolvedData) 
  for k=0,n_elements(a)-1 do begin
     if self.newPSF eq 1 then begin
;      PSF=gaussian_function([a[k],b[k]]/[dx,dy],/normalize,width=width,/double)
;      if phi ne 0 then PSF=rot(PSF,phi)
      psf=gx_psf([a[k],b[k]]/[dx,dy],phi,width)
      if k eq 0 then begin
       dim=size(psf,/dim)
       kernel=dblarr(dim[0],dim[1],n_elements(a)) 
       ConvolvedData[*]=0
      endif
      kernel[*,*,k]=psf
     endif else psf=(*self.PSF )[*,*,k]
      img=self->getImg(k,/raw,psf=psf)
    case n_elements(idx) of
      2:begin
        ;provision for one image only
        ConvolvedData[*,*]=img
      end
      3:begin
        ConvolvedData[*,*,k]=img
      end
      4:begin
        ConvolvedData[*,*,k,*]=img
      end
      else: begin
        ;no more than 5 dimensions expected, but let's play safe
        ConvolvedData[*,*,k,*,idx[4]]=img
     end
    endcase 
  end
    if self.newPSF eq 1 then begin
     ptr_free,self.PSF
     self.PSF=ptr_new(temporary(kernel))
    end 
    *self.pConvolvedData=ConvolvedData
    self.newPSF=0
    self.oLabel->SetProperty,strings=strings
end
;--------------------------------------------------------------------
function gxImgViewWid::GetImg,k,idx,raw=raw,psf=psf
  compile_opt hidden
  default,idx, self->data_indices()
  default,k, idx[2]
  if ~keyword_set(raw) then begin
    widget_control,self.wPSF[3],get_value=cv
    pData=(cv[0] eq 0)?self.pData:self.pConvolvedData
  endif else pData=self.pData
  case n_elements(idx) of
    2:begin
        ;provision for one image only
        img=reform((*pData)[*,*])
      end
    3:begin
      img=reform((*pData)[*,*,k])
    end
    4:begin
    img=reform((*pData)[*,*,k,*])
  end
  else: begin
    ;no more than 5 dimensions expected, but let's play safe
    img=reform((*pData)[*,*,k,*,idx[4]])
  end
  endcase
  if n_elements(psf) ne 0 then begin
    case size(img,/n_dim) of
      2: begin
          img=convol_fft(img,PSF);convol(img,PSF,edge_truncate);
         end
      else: begin 
             sz=size(img)
             for i=0,sz[3]-1 do img[*,*,i]=convol_fft(img[*,*,i],PSF);convol(img[*,*,i],PSF,edge_truncate);
            end
    endcase
    return,img
  end
  
  ;Handle Polarization and Tb
  coeff=self->SFU2TB()
  coeff=coeff/2;NORH Convention
  axis=(((*self.info).spectrum).x.axis)
  f2=axis^2
  if size(img,/n_dim) eq 3 then begin
    sz=size(*self.pData,/dim)
    case idx[3] of
        sz[3]:begin
                img=total(img,3)
              end
        (sz[3]+1):begin
                    img=(img[*,*,1]-img[*,*,0])
                  end
        (sz[3]+2):begin
                    img=100*(img[*,*,1]-img[*,*,0])/total(img,3)
                  end
        (sz[3]+3):begin
                    img=coeff*img[*,*,0]/f2[k]
                  end
        (sz[3]+4):begin
                    img=coeff*img[*,*,1]/f2[k]
                  end
        (sz[3]+5):begin
                    img=coeff*((img[*,*,1]+img[*,*,0]))/f2[k]
                  end
        (sz[3]+6):begin
                    img=coeff*((img[*,*,1]-img[*,*,0]))/f2[k]
                  end
        else:begin
              img=img[*,*,idx[3]]
             end
    endcase
  end
  widget_control,self.wContrast,get_value=contrast
  minmax=widget_info(self.wContrast,/SLIDER_MIN_MAX)
  img=img<max(img,/nan)*contrast/minmax[1]
  return,img
end

pro gxImgViewWid::SelectImg
    if ptr_valid(self.pData) then begin
     self->Convolve
     img=self->GetImg()
     self.oImage->SetProperty,data=bytscl(img,/NAN)
     self.oColorbar->SetProperty,TICKFRMTDATA=img
     self->SelectChannel
     self->Draw
     self->PlotProfile
    end
end
;
pro gxImgViewWid::GetMapParms,xc=xc,yc=yc,time=time,dx=dx,dy=dy
  if isa(self.fovmap,'map') then begin
    xc=self.fovmap->get(/xc)
    yc=self.fovmap->get(/yc)
    dx=self.fovmap->get(/dx)
    dy=self.fovmap->get(/dy)
    time=self.fovmap->get(/time)
  end
end

pro gxImgViewWid::SaveTbMaps,tlb
  compile_opt hidden
  if self->ValidData() and obj_valid(self.fovmap) then begin
    idx=self->data_indices()
    if n_elements(idx) ge 5 then begin
      idx[4]=0
      sz=size(*self.pdata)
      map=self.fovmap->get(/map)
      add_prop, map, freq = 0.0
      add_prop, map, frequnit = 'GHz'
      add_prop, map, stokes = ''
      add_prop, map, dimensions = ['Freq','Pol']
      add_prop, map, dataunit = 'K'
      add_prop, map, datatype = 'Brightness Temperature'
      add_prop, map, rms = 0d
      add_prop, map, rmsunit = 'K'
      add_prop, map, comment='Generated by GX Simulator'
      maps=replicate(map,sz[3],2)
      pol_id=['LL','RR']
      for k=0,sz[3]-1 do begin
        for pol=0,1 do begin
          idx[3]=pol+5
          map.data=self->GetImg(k,idx)
          map_id=string(((*self.info).spectrum).x.axis[k],((*self.info).spectrum).x.unit,format="(g0,' ',a)")
          map.dataunit=((*self.info).spectrum).y.unit[pol+5]
          map.id = 'GX '+pol_id[pol]+' '+map_id
          map.freq = ((*self.info).spectrum).x.axis[k]
          map.stokes = pol_id[pol]
          maps[k,pol]=map
        end
      end
      file=''
      file=dialog_pickfile(filter='*.sav',$
        DEFAULT_EXTENSION='sav',$
        /write,/OVERWRITE_PROMPT,$
        file=file,$
        title='Please select a file to save this Tb map structure')
      if file ne '' then begin
        save,maps,file=file
      end
     endif else  answ=dialog_message(['This feature is dedicated only to saving GX bigtness temperature maps, if displayed on this page!',$
      'To save any other type of GX maps, send them first to Plotman and then use the "Map_Container/Save Grup to File" menu option implemented there.'])
  endif else answ=dialog_message('No valid image data has been created yet!')
end


;--------------------------------------------------------------------
 pro gxImgViewWid::SaveMaps,tlb
    compile_opt hidden
    if self->ValidData() then begin
     tvlct,rgb_curr,/get
     self.oPalette->GetProperty,red=red,green=green,blue=blue
     tvlct,red,green,blue
     idx=self->data_indices(chanid=chanid)
     self.oImage->GetProperty,data=data
     omap=obj_new('map')
     sz=size(*self.pdata)
     nimg=sz[0] gt 2?sz[3]:1
     for k=0,nimg-1 do begin
      map=self.fovmap->get(/map)
      map.data=self->GetImg(k)
      if n_elements(idx) ge 4 then begin
        add_prop,map,dataunit=((*self.info).spectrum).y.unit[idx[3]]
        case map.dataunit of
          'sfu':datatype='Flux'
          'K':datatype='Brightness Temperature'
          '%':datatype='Polarization Degree'
          else:
        endcase
        if isa(datatype) then begin
          add_prop,map,datatype=datatype
          add_prop,map,freq=((*self.info).spectrum).x.axis[k]
          add_prop,map,frequnit=((*self.info).spectrum).x.unit
          add_prop,map,STOKES=strcompress(chanid,/rem)
        endif
      endif
      
      id=string(((*self.info).spectrum).x.axis[k],((*self.info).spectrum).x.unit,format="(g0,' ',a)")
      map.id='GX '+id+chanid

      omap->setmap,k,map
      tvlct,red,green,blue,/get  
      if ptr_valid(self.info) then begin
       if tag_exist((*self.info),'rgb') then tvlct,((*self.info).rgb)[*,*,k]
      end  
      omap->save_ct,k
      tvlct,red,green,blue
     end
     name='GX'+chanid
     tvlct,rgb_curr
     file=dialog_pickfile(filter='*.map',$
         DEFAULT_EXTENSION='map',$
         /write,/OVERWRITE_PROMPT,$
         file=file,$
         title='Please select a file to save this MAP object') 
     if file ne '' then begin
      map=omap
      save,map,file=file
     end
    end 
   end  
;--------------------------------------------------------------------
pro gxImgViewWid::Map2Plotman,tlb
compile_opt hidden
    if self->ValidData() and obj_valid(self.fovmap) then begin
     tvlct,rgb_curr,/get
     self.oPalette->GetProperty,red=red,green=green,blue=blue
     tvlct,red,green,blue
     idx=self->data_indices(chanid=chanid)
     self.oImage->GetProperty,data=data
     omap=obj_new('map')
     sz=size(*self.pdata)
     nimg=sz[0] gt 2?sz[3]:1
     for k=0,nimg-1 do begin
      map=self.fovmap->get(/map)
      map.data=self->GetImg(k)
      id=string(((*self.info).spectrum).x.axis[k],((*self.info).spectrum).x.unit,format="(g0,' ',a)")
      map.id='GX '+id+chanid
      omap->setmap,k,map
      tvlct,red,green,blue,/get  
      if ptr_valid(self.info) then begin
       if tag_exist((*self.info),'rgb') then tvlct,((*self.info).rgb)[*,*,k]
      end  
      omap->save_ct,k
      tvlct,red,green,blue
     end 
     widget_control,tlb,send_event={GX2PLOTMAN,id:0l,top:0l,handler:0l,omap:omap,name:'GX'+chanid,k:idx[2]}
     tvlct,rgb_curr
    endif else answ=dialog_message('No valid image data has been created yet!')
end

pro gxImgViewWid::ImgCube2File,tlb
compile_opt hidden
  if self->ValidData() and obj_valid(self.fovmap) then begin
   file=dialog_pickfile(filter='*.gxc',$
                               DEFAULT_EXTENSION='*.gxc',$
                               /write,/OVERWRITE_PROMPT,$
                               title='Please select a filename to save the synthetized image cube')
   if file ne '' then begin
    (self.model->GetVolume())->GetVertexAttributeData,'dz',dz
    if n_elements(dz) eq 0 then begin
      self.model->getproperty,zcoord_conv=dz
      dz=dz[1]
    endif
    self.model->getproperty,xcoord_conv=dx,ycoord_conv=dy
    dx=dx[1]
    dy=dy[1]
    dim=self.model->Size()
    gxcube={info:*(self.info),data:*(self.pData),renderer:self.renderer,fovmap:self.fovmap,model:{dx:dx,dy:dy,dz:dz,dim:dim[1:3]},ebtel:gx_ebtel_path()}
    save,gxcube,file=file
   end
  end else answ=dialog_message('No valid image data has been created yet!')
end

pro gxImgViewWid::ImgCubeFile2Renderer,tlb
  compile_opt hidden
    file=dialog_pickfile(filter='*.gxc',$
      DEFAULT_EXTENSION='*.gxc',$
      /write,$
      title='Please select a filename to restore a synthetized image cube')
    if file ne '' then begin
      restore,file
      widget_control,widget_info(tlb,/child),get_uvalue=state
      if n_elements(state) ne 0 then begin
        state.Scanbox->ReplaceRenderer,gxcube
        obj_destroy,self.fovmap
        self.fovmap=gxcube.fovmap
      endif else begin
        if n_elements(gxcube) ne 0 then begin
          if ~tag_exist(gxcube,'renderer') or ~tag_exist(gxcube,'data') $
            or ~tag_exist(gxcube,'info') or ~tag_exist(gxcube,'fovmap') then return
          if ~obj_valid(gxcube.fovmap) then return
          data=gxcube.data
          sz=size(data)
          nx=sz[1]
          ny=sz[2]
          fovmap=gxcube.fovmap
          gx_fovmap2scanbox,fovmap,xrange=xrange,yrange=yrange
          info=gxcube.info
          self.pData=self->NewView(info,renderer=gxcube.renderer,nx=nx,ny=ny,xrange=xrange,yrange=yrange,data=data,fovmap=fovmap)
          if tag_exist(gxcube,'EBTEL') then set_value=gx_ebtel_path(gxcube.ebtel)
        end
      endelse
      self->SelectImg
    end
end

pro gxImgViewWid::MapArray2ImgCubeFile,tlb
  compile_opt hidden
  file=dialog_pickfile(filter='*.sav',$
    DEFAULT_EXTENSION='*.sav',$
    /write,$
    title='Please select a filename to import a reference map array')
  if file ne '' then begin
    restore,file
      if n_elements(eomap) ne 0 then begin
        eomap=reform(eomap)
        data=eomap.data
        sz=size(data)
        nx=sz[1]
        ny=sz[2]
        map=eomap[0]
        rsun=(pb0r(map.time)*60)[2]
        run=map.rsun
        arcsec=gx_rsun(unit='cm')/rsun
        ds=map.dx*map.dy*(arcsec^2)
        freqs=eomap.freq
        coeff=1.4568525e-026*ds*(freqs^2)*1e4;conversion from K to sfu assuming Tb is given in 10^4K
        
        for i=0,sz[3]-1 do eomap[i].data=eomap[i].data*coeff[i]
        data=eomap.data
        ;map=create_struct(map,'RSUN',rsun)
        fovmap=obj_new('map')
        fovmap->setmap,0,map
        xrange=fovmap->Get(/xrange)/rsun
        yrange=fovmap->Get(/yrange)/rsun
        info={parms:replicate({NAME:'dS',VALUE:2.0,UNIT:'',hint:''},1),$
              pixdim:[sz[3],2,3],$
              spectrum:{x:{axis:freqs,label:'Frequency',unit:'GHz'},$
              y:{label:['LCP','RCP','[RCP+LCP]','[RCP-LCP]','[R-L]/[R+L]','T_LCP','T_RCP','T_I','T_V'],unit:['sfu','sfu','sfu','sfu','%','K','K','K','K']}},$
              channels:['Exact Coupling', 'Weak Coupling', 'Strong Coupling']}
              fdata=fltarr(sz[1],sz[2],sz[3],2,3)
              for i=0,1 do for j=0,2 do fdata[*,*,*,i,j]=data/2
        self.pData=self->NewView(info,renderer='EOVSA',nx=nx,ny=ny,xrange=xrange,yrange=yrange,data=fdata,fovmap=fovmap)
        self->SelectImg
      end
  end
end

;--------------------------------------------------------------------
pro gxImgViewWid::Spec2Plotman,tlb
compile_opt hidden
    if self->ValidData() then begin
      self->PlotProfile,objxy
      widget_control,tlb,send_event={GXSPEC2PLOTMAN,id:0l,top:0l,handler:0l,objxy:objxy}
    end
end
;---------------------------------

;--------------------------------------------------------------------
pro gxImgViewWid::Spec2File,tlb
compile_opt hidden
    if self->ValidData() then begin
      
         file=dialog_pickfile(filter='*.sav',$
                               DEFAULT_EXTENSION='sav',$
                               /write,$
                               title='Please select a filename to save the spectrum')
        if file ne '' then begin
          self->PlotProfile,objxy
          save,objxy,file=file
        end 
    end
end
;---------------------------------
pro gxImgViewWid::OnPalette
 tvlct,rgb_curr,/get
 xloadct,/silent,/block
 tvlct,rgb,/get
 tvlct,rgb_curr
 self.oPalette->SetProperty, RED_VALUES = rgb[*,0], GREEN_VALUES = rgb[*,1], BLUE_VALUES = rgb[*,2]
 self->Draw
end
;---------------------------------------------------------------------------------------------------

pro gxImgViewWid::OnLockPalette
  self.LockPalette=1-self.LockPalette
  widget_control,self.wLockPalette,set_value=self.LockPalette?gx_bitmap(gx_findfile('lock.bmp')):$
    gx_bitmap(gx_findfile('unlock.bmp')),/bitmap
  widget_control,self.wPalette,sensitive=~self.LockPalette
end
;---------------------------------------------------------------------------------------------------

pro gxImgViewWid::OnMovie
 
  n=max(widget_info(self.wSlider,/slider))
  self.oWindow->GetProperty, units=orig_units
  self.oWindow->SetProperty, units=0
  self.oWindow->GetProperty, dimensions=dimensions
  self.oWindow->SetProperty, units=orig_units
  oBuff = obj_new('IDLgrBuffer', dimensions=dimensions)
  if float(!version.release) ge 8.1 then begin
    oVid = gxVideo(dimensions,stream=stream)
  endif else begin
    desc = [ $
      '0, LABEL, Movie Output Options, CENTER', $
      '1, BASE,, ROW, FRAME', $
      '0, DROPLIST,mpeg, LABEL_TOP=Movie Format,Row, TAG=format', $
      '2, Float, 24, LABEL_TOP=Frames per second:, WIDTH=6, TAG=fps', $
      '1, BASE,, ROW', $
      '0, BUTTON, OK, QUIT,TAG=OK', $
      '2, BUTTON, Cancel, QUIT, TAG=CANCEL']
    opt=CW_FORM(desc,/Column,Title='Moview Options')
    ext='mpg'
    filename=dialog_pickfile(filter='*.'+ext,$
      DEFAULT_EXTENSION=ext,$
      /write,/OVERWRITE_PROMPT,$
      title='Please choose a filename to save this video')
    oVid= OBJ_NEW('IDLgrMPEG',frame_rate=2)
  endelse
  if ~obj_valid(oVid) then begin
    answ=dialog_message('Error creating video stream',/error)
    return
  endif
    for k=0, n do begin
      widget_control,self.wSlider,set_value=k
      self->OnSlider, k
      oBuff->Draw, self.oViewgroup
      oBuff->GetProperty, image_data=image_data
      if obj_isa(oVid,'IDLgrMPEG') then begin
        for i=0, 2 do image_data[i,*,*]=rotate(reform(image_data[i,*,*]),7)
        for j=1, 24/opt.fps do oVid->Put, image_data
      endif else result=oVid->Put(stream,image_data)
    endfor
 
  obj_destroy, oBuff
  if obj_isa(oVid,'IDLgrMPEG') then  oVid->Save, FILENAME=filename
  obj_destroy, oVid
end
;-----------------------------------------------------------------------------------------------------------------
Function gxImgViewHandleEvent,event
 widget_control,event.handler,get_uvalue=obj
 return,obj->HandleEvent(event)
end
;---------------------------------------------------------------------
function gxImgViewWid::HandleEvent, event
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
case event.id of
 self.wDrawImg: begin
    case event.type of
        4: self->OnExpose
        0: self->OnMouseDown, event
        2: self->OnMouseMove, event
        1: self->OnMouseUp, event
        else:
        endcase
    end
  self.wSlider:self->OnSlider, event.value
  self.wPlot:begin
              case event.type of
               4: self->PlotProfile
               else:
              endcase
             end
  self.wPrintImg: begin
        self.oViewgroup->DialogPrint, $
            self.oWindow, $
            /hourglass, $
            dialog_parent=event.top
        end
  self.wExportImg: begin
        void = self.oViewgroup->DialogWriteImage( $
            self.oWindow, $
            dialog_parent=event.top $
            )
        end
  self.wImportReference:begin
    file=dialog_pickfile(filter='*.ref',$
                               DEFAULT_EXTENSION='ref',$
                               /read,/must_exist,$
                               title='Please select a file to upload a reference spectrum for this model')
   osav=obj_new('idl_savefile',file)
    names=osav->names()
    valid=0
    for i=0,n_elements(names)-1 do begin
      osav->restore,names[i]
      e=execute('result=size('+names[i]+',/tname)')
      if result eq 'STRUCT' then begin
       e=execute('ref=temporary('+names[i]+')')
       valid=(tag_exist(ref,'x') and tag_exist(ref,'y') and tag_exist(ref,'t'))
      endif 
     endfor   
    if ~valid then begin
     answ=dialog_message('Invalid reference data file:'+string(10b)+$
     'No structure having the required x and y tags has been found!')
    endif else begin
     ptr_free,self.refspectrum
     self.refspectrum=ptr_new(ref)
     if n_elements(ref.t) gt 0 then begin
      widget_control,self.wTimeReference,set_slider_max=n_elements(ref.t)-1,set_value=0
      widget_control,widget_info(self.wTimeReference,/parent),map=1
     end
    end 
    self->PlotProfile
  end      
  self.wDeleteReference :begin
                          ptr_free,self.refspectrum
                          widget_control,self.wTimeReference,set_value=0
                          widget_control,widget_info(self.wTimeReference,/parent),map=0
                          self->PlotProfile
                         end
  self.wTimeReference: begin
                         widget_control,event.id,get_value=tidx
                         widget_control,(widget_info(widget_info(event.id,/parent),/all))[1],set_value='      '+atime((*self.refspectrum).t[tidx])
                         self->PlotProfile 
                       end 
  self.wPSF[0]:begin
                  widget_control,self.wPSF[3],set_value=0
                  self.NewPSF=1
               end
  self.wPSF[1]:begin
                  widget_control,self.wPSF[3],set_value=0
                  self.NewPSF=1
               end
  self.wPSF[2]:begin
                  widget_control,self.wPSF[3],set_value=0
                  self.NewPSF=1
               end
  self.wPSF[4]:begin
     widget_control,self.wPSF[3],set_value=0
     self.NewPSF=1
   end             
  self.wPSF[3]:begin
                  self->SelectImg
               end                                                 
  self.wMovie:self->OnMovie                       
  self.wLockPalette:self->OnLockPalette                                      
  self.wPalette:self->OnPalette
  self.wChannels[3]:self->SelectImg
  self.wChannels[4]:begin
                     widget_control,self.wPSF[3],get_value=conv
                     if conv[0] then begin
                      idx=self->data_indices()
                      computed=total((*self.pConvolvedData)[*,*,*,*,idx(4)])
                      if computed eq 0 then self->Convolve,/compute
                     endif
                     self->SelectImg
                    end 
  self.wPlotSelection:self->PlotProfile
  self.wRESCHI:begin 
                 if event.select then self->PlotProfile
               end  
  self.wLog2File:result=self->SaveLog()
  self.wSave:self->SaveMaps,event.top
  self.wSaveTb:self->SaveTbMaps,event.top
  self.wMap2Plotman:self->Map2Plotman,event.top
  self.wExportImgCube:self->ImgCube2File,event.top
  self.wImportImgCube:self->ImgCubeFile2Renderer,event.top
  self.wSpec2Plotman:self->Spec2Plotman,event.top
  self.wSpec2File:self->Spec2File,event.top
  self.wContrast:self->SelectImg
  self.wImportGXlog:return,event
  self.wCharSize:self->PlotProfile
  self.wSpectralPlotOptions:self->PlotProfile
  self.wResidualsPlotOptions:self->PlotProfile
  self.wSpectralExtraOptions:self->PlotProfile
  self.wResidualsExtraOptions:self->PlotProfile
  self.wXProfilePlotOptions:self->PlotProfile
  self.wYProfilePlotOptions:self->PlotProfile
 else:
endcase
return,self->Rewrite(event)
end

pro gxImgViewWid::SetProperty,model=model,fovmap=fovmap,_extra=extra
  if isa(model,'gxmodel') then self.model=model
  if isa(fovmap,'map') then self.fovmap=fovmap
  self->IDLexWidget::SetProperty,_extra=extra
end

function gxImgViewWid::GetFovMap
 ;for convenience
 return,self.fovmap
end

function gxImgViewWid::GetModel
 ;for convenience
 return,self.model
end

pro gxImgViewWid::GetProperty,wUpload=wUpload,model=model,fovmap=fovmap,_ref_extra=extra
  wUpload=self.wImportGXlog
  model=self.model
  fovmap=self.fovmap
  self->IDLexWidget::GetProperty,_extra=extra
end

pro gxImgViewWid::Cleanup
  ptr_free,self.pdata
  ptr_free,self.info
  obj_destroy,self.oView
  self->IDLexWidget::Cleanup
end

pro gxImgViewWid__define
 struct_hide,{gxImgViewWid, inherits IDLexWidget,$
    wBase: 0L, $
    wToolbarbase:0L,$
    wPlot: 0L, $
    wStatusbar:0L,$
    wLockPalette:0L,$
    wPalette:0L,$
    wDrawImg: 0L, $
    wSlider:0L,$
    wChannBase:0L,$
    wChannels:lonarr(5),$
    wPlotSelection:0L,$
    wSave:0L,$
    wSaveTb:0L,$
    wLog2File:0L,$
    wMap2Plotman:0L,$
    wSpec2Plotman:0L,$
    wSpec2File:0L,$
    wImportGXlog:0L,$
    wContrast:0L,$
    wPrintImg:0L,$
    wExportImg:0L,$
    wExportImgCube:0L,$
    wImportImgCube:0L,$
    wMovie:0L,$
    wImportReference:0l,$
    wDeleteReference:0l,$
    wTimeReference:0l,$
    wSDEV:0L,$
    wRDEV:0L,$
    wRESCHI:0L,$
    wCharSize:0L,$
    wSpectralPlotOptions:0L,$
    wResidualsPlotOptions:0L,$
    wResidualsExtraOptions:0L,$
    wSpectralExtraOptions:0L,$
    wXProfilePlotOptions:0L,$
    wYProfilePlotOptions:0L,$
    wPSF:lonarr(5),$
    PSF: ptr_new(),$
    oWindow: obj_new(), $
    oViewgroup:obj_new(), $
    oView: obj_new(), $
    oModel: obj_new(),$
    oImage: obj_new(),$
    oPalette:obj_new(),$
    oColorbar:obj_new(),$
    oLabel:obj_new(),$
    oXX:obj_new(),$
    oYY:obj_new(),$
    newPSF: 0b,$
    lock:0b,$
    LockPalette:0b,$
    nx:0L,$
    ny:0L,$
    xsize:0.0,$
    ysize:0.0,$
    xpad:0.0,$
    ypad:0.0,$
    xAxis:objarr(2),$
    yAxis:objarr(2),$
    FOVMap:obj_new(),$
    pData:ptr_new(),$
    pConvolvedData:ptr_new(),$
    refspectrum:ptr_new(),$
    model:obj_new(),$
    info:ptr_new(info),$
    renderer:'',$
    wPixMap:0L}
end
