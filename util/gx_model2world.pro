;This is a utility program that allows ploting different model components (field lines, fluxtubes, model mox of FOV box, as they project on a LOS map.
;To plot all components on top of the model LOS Bz reference map one should call
;gx_model2world,model,/all
;To selectevely plot only some components, use the corresponding keywords
;To plot on a prexisting LOS map, use /over
;2/20/23 updates:
; -the fluxtubes keyword may be set to an array of integers indicating the 1-based idices of a set of selected fluxtubes to be displayed
; -the /centerlines keyword may be set to plot only the centerlines of the fluxtubes
pro gx_model2world,model,lines=lines,fluxtubes=tubes,centerlines=centerlines,fov=fov,box=box,over=over,$
                  scolor=scolor,lcolor=lcolor,ocolor=ocolor,bcolor=bcolor,fcolor=fcolor,$
                  sthick=sthick,lthick=lthick, lstyle=lstyle,refmap=ref,black2white=black2white,_extra=_extra,all=all
  default,model,obj_new()
  default,scolor,250
  default,lcolor,150
  default,ocolor,200
  default,bcolor,250
  default,fcolor,50
  default,sthick,3
  default,cthick,1
  default,lstyle,0
  default,lthick,1
  
  if keyword_set(all) then begin
    lines=1
    fluxtubes=1
    fov=1
    box=1
  end  
    
  if ~obj_isa(model,'gxmodel') then model=gx_read()
  if ~obj_isa(model,'gxmodel') then begin
    message,'No valid model in this file. Operation aborted!',/info
    return
  endif
  if ~valid_map(ref) then begin
    refmaps=*(model->Refmaps()) 
    for i=0, refmaps->get(/count)-1 do begin
      if refmaps->get(i,/id) eq 'Bz_reference' or refmaps->get(i,/id) eq 'LOS_magnetogram' then ref=refmaps->get(i,/map)
    endfor
    if ~valid_map(ref) then begin
      for i=0, refmaps->get(/count)-1 do begin
        if refmaps->get(i,/xunit) eq 'arcsecs' then ref=refmaps->get(i,/map)
      endfor
    endif 
    if ~valid_map(ref) then begin
      message,'No LOS reference map found in this model, please plot one and call this procedure using /over!',/info
      return
    endif
    ref.id= 'HMI LOS B'
  end
  if ~keyword_set(over) and ref.id eq 'HMI LOS B' then begin
    tvlct,r,g,b,/get
    loadct,0,/silent
    if keyword_set(black2white) then gx_rgb_white2black
    plot_map,ref,fov=get_map_fov(ref)/40,GRID=10,_extra=_extra
    tvlct,r,g,b
  endif
  if keyword_set(lines) then begin
    lines=model->get(/all,isa='gxbline')
    if obj_valid(lines[0]) then begin
      lcolor2=lcolor
      ocolor2=ocolor
      lthick2=lthick
      lstyle2=lstyle
      for k=0,n_elements(lines)-1 do begin
        if obj_isa(lines[k],'gxbline') then begin
            lines[k]->getproperty,data=ldata,open=open
            ldata=gx_transform(ldata,model->GetSTM(),/inv)*ref.rsun
            oplot,ldata[0,*],ldata[1,*],color=open?ocolor2[0]:lcolor2[0],thick=lthick2[0],linestyle=lstyle2[0],psym=0
            lcolor2=shift(lcolor2,1)
            ocolor2=shift(ocolor2,1)
            lthick2=shift(lthick2,1)
            lstyle2=shift(lstyle2,1)
        endif
      endfor
      endif else message,'No field lines found in this model!',/info
  endif
  if keyword_set(tubes) then begin
    fluxtubes=model->get(/all,isa='gxfluxtube')
    if size(tubes,/dim) gt 0 then begin
      fluxtubes=fluxtubes[(tubes-1)<(n_elements(fluxtubes)-1)]  
    endif
    if obj_valid(fluxtubes[0]) then begin
      for i=0,n_elements(fluxtubes)-1 do begin
        fluxtube=fluxtubes[i]
        if obj_isa(fluxtube,'gxfluxtube') then begin
          fluxtube->getproperty,centerline=centerline
          if ~keyword_set(centerlines) then tubelines=fluxtube->get(/all,isa='gxbline')
          centerline->GetProperty,data=data
          sdata=gx_transform(data,model->GetSTM(),/inv)*ref.rsun
          oplot,sdata[0,*],sdata[1,*],color=scolor,thick=sthick
          for k=0,n_elements(tubelines)-1 do begin
            tubelines[k]->getproperty,data=ldata,open=open
            ldata=gx_transform(ldata,model->GetSTM(),/inv)*ref.rsun
            oplot,ldata[0,*],ldata[1,*],color=open?ocolor:lcolor,thick=lthick,psym=0
          endfor
        end  
     end 
    endif else  message,'No fluxtubes found in this model!',/info  
  endif
  if keyword_set(fov) then begin
   fovdata=((model->GetRoi(/scanbox))->GetFOVData(/sun))*ref.rsun
   oplot,fovdata[0,[0,1,2,3,0]],fovdata[1,[0,1,2,3,0]],color=fcolor,_extra=_extra
  end
  if keyword_set(box) then begin
   (model->GetRoi())->GetProperty,data=data
   boxdata=gx_transform(data,model->GetSTM(),/inv)*ref.rsun
   oplot,boxdata[0,[indgen(16),0]],boxdata[1,[indgen(16),0]],color=bcolor,_extra=_extra
  end
end