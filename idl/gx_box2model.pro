function gx_box2model,box
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return,obj_new()
  end
 if (tag_exist(box,'Bx') and tag_exist(box,'By') and tag_exist(box,'Bz')) then size=(size(box.bx))[0:3]
 if tag_exist(box,'bcube') then size=(size(box.bcube))[0:3]
 if n_elements(size) ne 4 then return,obj_new()
 
 
 if tag_exist(box,'add_base_layer') then add_base_layer=box.add_base_layer else add_base_layer=1
 
 size[3]+=add_base_layer
 
 sx=size[1]
 sy=size[2]
 sz=size[3]
 
 osize=size(box.base.(0))
 osx=osize[1]
 osy=osize[2]
 

 
 wcs=fitshead2wcs(box.index)
 
 scale=[double(osx)/sx,double(osy)/sy]
 
 if ~tag_exist(box,'dr') then begin
 dx=wcs.cdelt[0]*!dtor*scale[0]
 dy=wcs.cdelt[1]*!dtor*scale[1]
 dz=mean([dx,dy])
 endif else begin
  dx=box.dr[0]
  dy=box.dr[1]
  dz=box.dr[2]
 endelse
 
 ;!!! changed by Sergey Anfinogentov!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ;creating Earth view WCS structure
 wcs_earth = wcs_2d_simulate(2048,2048, date_obs = wcs_get_time(wcs))

 ; "-1" is needed because in FITS standard left bottom pixel is [1,1], while in IDL it is [0,0]
 ctr_coord=wcs_get_coord(wcs,wcs.crpix - 1d)

 WCS_CONVERT_from_COORD, WCS, ctr_coord, 'hg', EW_c, NS_C, /carrington
 wcs_convert_to_coord, wcs_earth, coord_earth, 'HG', EW_c, NS_C, /carrington
 WCS_CONVERT_from_COORD, WCS_earth, coord_earth, 'hg', EW, NS

; ;!!!! end of changes !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 XCOORD_CONV=[0,dx]
 YCOORD_CONV=[0,dy]
 ZCOORD_CONV=[0,dz]
 
 ; "-1" is needed because in FITS standard left bottom pixel is [1,1], while in IDL it is [0,0]
 delta_xy=[dx,dy]*size[1:2]*(wcs.crpix-1)/osize[1:2]
 
 xrange=[-delta_xy[0],-delta_xy[0]+sz*dx]
 yrange=[-delta_xy[1],-delta_xy[1]+sy*dy]
 zrange=[0,sz]*dz
 
 data=bytarr(sx,sy,sz)
 volume=obj_new('gxVolume',data,name='Volume',XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV,/interpolate,hints=2)
 
 validBxByBz=(tag_exist(box,'Bx') and tag_exist(box,'By') and tag_exist(box,'Bz'))
 validB=tag_exist(box,'bcube') 
 valid=(validBxByBz eq 1) or (validB eq 1)
 data=fltarr(sx,sy,sz)
 
 if add_base_layer eq 1 then begin
  data[*,*,0]=congrid(box.base.bx,sx,sy)
  if validBxByBz eq 1 then data[*,*,1:*]=box.Bx else data[*,*,1:*]=box.Bcube[*,*,*,0]
 endif else begin
  if validBxByBz eq 1 then data=box.Bx else data=box.Bcube[*,*,*,0]
 endelse
 volume->SetVertexAttributeData,'Bx',data
 BX=data[*,*,0]
 
 if add_base_layer eq 1 then begin
   data[*]=0
   data[*,*,0]=congrid(box.base.by,sx,sy)
   if validBxByBz eq 1 then data[*,*,1:*]=box.By else data[*,*,1:*]=box.Bcube[*,*,*,1]
 endif else begin
  if validBxByBz eq 1 then data=box.By else data=box.Bcube[*,*,*,1]
 endelse
 volume->SetVertexAttributeData,'By',data
 BY=data[*,*,0]
 
 if add_base_layer eq 1 then begin
   data[*]=0
   data[*,*,0]=congrid(box.base.bz,sx,sy)
   if validBxByBz eq 1 then data[*,*,1:*]=box.Bz else data[*,*,1:*]=box.Bcube[*,*,*,2]
 endif else begin
  if validBxByBz eq 1 then data=box.Bz else data=box.Bcube[*,*,*,2]
 endelse
 volume->SetVertexAttributeData,'Bz',data
 Bz=data[*,*,0]
 
 wcs.simple=1
 refmaps=obj_new('map')
 base=box.base
 if ~tag_exist(base,'Chromo_Mask') and tag_exist(base,'BZ') and tag_exist(base,'IC') then begin
  chromo_mask=decompose(base.bz,base.ic)
  base=add_tag(base, chromo_mask,'Chromo_Mask')
 endif
 names=tag_names(base)
 ntags=n_tags(base)
 for i=0, ntags-1 do begin
  wcs2map,base.(i),wcs,ceamap,id='BASE '+names[i]
  ceamap=add_tag(ceamap,box.index,'index')
  if i eq 0 then begin
    amap=rebin_map(ceamap,sx,sy)
    amap.id='Bx'
    amap.data=Bx
    refmaps->setmap,0,amap
    amap.id='By'
    amap.data=By
    refmaps->setmap,1,amap
    amap.id='Bz'
    amap.data=Bz
    refmaps->setmap,2,amap
  endif
  refmaps->setmap,i+3,ceamap
 endfor

if tag_exist(box,'refmaps') then begin
  nref=n_elements(*box.refmaps)
    for i=0, nref-1 do begin
      rmap=(*box.refmaps)[i]
      if valid_map(rmap) then begin
       rcount=rmap->get(/count)
       for k=0,rcount-1 do begin
        match=0
        count=refmaps->get(/count)
        for l=0,count-1 do begin
          if refmaps->get(l,/id) eq rmap->get(k,/id) then match=1
        endfor
        if ~match then refmaps->setmap,count,rmap->get(k,/map)
       endfor
      endif
    endfor    
endif
tags2remove=['Bx','By','Bz','Bcube','Base','index','refmaps','ADD_BASE_LAYER','ID','EXECUTE']
if tag_exist(box,'idx') and $
   tag_exist(box,'length') and $
   tag_exist(box,'bmed') and $
   tag_exist(box,'foot1') and $
   tag_exist(box,'foot2') then begin
  physLength=(avField=dblarr(sx,sy,sz))
  startidx=(endidx=lonarr(sx,sy,sz))
  status=bytarr(sx,sy,sz)
  status[box.idx]=(status[box.idx] or 4L) or 2l
  physLength[box.idx]=box.length
  avField[box.idx]=box.bmed
  startidx[box.idx]=box.foot1>0
  endidx[box.idx]=box.foot2>0
  if tag_exist(box,'oidx') and $
   tag_exist(box,'olength') and $
   tag_exist(box,'obmed') and $
   tag_exist(box,'ofoot1') and $
   tag_exist(box,'ofoot2') then begin  
   ofoot1=array_indices([sx,sy,sz]+[0,0,10],/dim,box.ofoot1)
   ofoot2=array_indices([sx,sy,sz]+[0,0,10],/dim,box.ofoot2)
   omin=min([min(ofoot1[2,*]),min(ofoot2[2,*])])
   good=where(((ofoot2[2,*] eq omin) and (ofoot1[2,*] gt omin)) or ((ofoot1[2,*] eq omin) and (ofoot2[2,*] gt omin)),count)
   if count ne 0 then begin
   ofoot1=ofoot1[*,good]
   ofoot2=ofoot2[*,good]
   oidx=box.oidx[good]
   status[oidx]=(status[oidx] or 16L) or 2l
   physLength[oidx]=box.olength[good]
   avField[oidx]=box.obmed[good]
   startidx[oidx]=box.ofoot1[good]>0
   endidx[oidx]=box.ofoot2[good]>0
  end  
  endif
  volume->Add2ROM,status=status,physLength=physLength,avField=avField,startidx=startidx,endidx=endidx,$
    q0_coeff=[0.1e-3,1e2,1e9,0,0],q0_formula='q[0]',q_formula='q0*(B/q[1])/(L/q[2])'
endif  

tags2remove=[tags2remove,'length','bmed','foot1','foot2','oidx','olength','obmed','ofoot1','ofoot2']

if  ~(tag_exist(box,'idx') and $
    tag_exist(box,'N') and $
    tag_exist(box,'T')) then tags2remove=[tags2remove,'idx']

volume->Add2ROM,_extra=rem_tag(box,tags2remove)

 model=obj_new('gxmodel')
 model->SetProperty,NS=NS,EW=EW,$
   XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV,$
   XRANGE=XRANGE,YRANGE=YRANGE,ZRANGE=ZRANGE,size=size,volume=volume,refmaps=refmaps
 if tag_exist(box,'id') then model->SetName,box.id
 if tag_exist(box,'execute') then model->SetId,str_replace(str_replace(box.execute,'/','\'),string(1b),'1');some bug fix
 return,model
end