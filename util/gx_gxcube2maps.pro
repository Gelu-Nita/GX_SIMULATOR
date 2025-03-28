function gx_gxcube2maps,gxcube,map,idx=idx,npol=npol
  ;map input should be provided to add the gxcube maps to an existing map object
  default,map,obj_new('map')
  if ~obj_valid(map) then begin
    message,'Invalid mapcontainer provided!',/info
    goto,invalid_input
  endif
  
  if size(gxcube,/tname) eq 'STRUCT'  then begin
    if ~tag_exist(gxcube,'renderer') or ~tag_exist(gxcube,'data') $
      or ~tag_exist(gxcube,'info') or ~tag_exist(gxcube,'fovmap') then begin
      message,'Invalid gxcube input structure!',/info
      goto,invalid_input
    end
  endif else begin
    message,'None or invalid gxcube input structure provided!',/info
    goto,invalid_input
  endelse

  if ~valid_map(gxcube.fovmap) then begin
    message,'Invalid FOV map GXCUBE tag!'
    goto,invalid_input
  endif
  
 info=gxcube.info
 fovmap=gxcube.fovmap
 chan=(((info).spectrum).x.axis)
 nchan=n_elements(chan)
 dx=fovmap->Get(/dx)
 dy=fovmap->Get(/dy)
 amap=fovmap->get(/map)
 add_prop,amap,chan=chan[0]
 add_prop,amap,datatype=(((info).spectrum).x.label)
 add_prop,amap,dataunit=(((info).spectrum).x.unit)
 add_prop,amap,renderer=file_basename(gxcube.renderer),/replace
 if tag_exist(info,'rparms') then begin
    idx=where(strcompress(strupcase((info.rparms).name),/rem) eq strcompress(strupcase('relative_abundance'),/rem) $
      or strcompress(strupcase((info.rparms).name),/rem) eq strcompress(strupcase('relative'),/rem) ,count)
    if count eq 1 then begin
      rparms=info.rparms
      amap.gx_key=amap.gx_key + string(rparms[idx].value,format="('& relative_abundance=',g0)")
    endif
 end
 i=map->get(/count) 
 sz=size(gxcube.data)
 if sz[0] gt 3 then begin
  default,npol,2
  pol=gxcube.info.spectrum.y.label[0:npol-1]
 endif else begin
  pol=''
  npol=1
 endelse
 if tag_exist((info),'rgb') then begin
  rgb=info.rgb
  add_prop,amap, red=rgb[*,0,0]
  add_prop,amap, green=rgb[*,1,0]
  add_prop,amap, blue=rgb[*,2,0]
 endif
 for p=0,npol-1 do begin
 for k=0,nchan-1 do begin
  map_idx=i++
  amap.chan=chan[k]
  amap.id=string(pol[p],amap.chan,amap.dataunit,format="('GX ',a,' ',g0,' ',a)")
  amap.data=gxcube.data[*,*,k]
  if n_elements(rgb) gt 0  then begin
   amap.red=rgb[*,0,k]
   amap.green=rgb[*,1,k]
   amap.blue=rgb[*,2,k]
  endif
   map->setmap,map_idx,amap
 endfor
 end
 return,map
 invalid_input:
 return,map
end