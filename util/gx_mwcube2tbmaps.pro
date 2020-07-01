function gx_mwcube2tbmaps,gxcube,map
  ;map input should be provaded to add the TB maps to an existing map object
  default,map,obj_new('map')
  if ~obj_valid(map) then begin
    message,'Invalid mapcontainer provided!',/cont
    goto,invalid_input
  endif
  
  if size(gxcube,/tname) eq 'STRUCT'  then begin
    if ~tag_exist(gxcube,'renderer') or ~tag_exist(gxcube,'data') $
      or ~tag_exist(gxcube,'info') or ~tag_exist(gxcube,'fovmap') then begin
      message,'Invalid gxcube input structure!',/cont
      goto,invalid_input
    end
  endif else begin
    message,'None or invalid gxcube input structure provided!',/cont
    goto,invalid_input
  endelse
    
  if ~(gxcube.info.spectrum.x.unit eq 'GHz') then begin
    message,'Not a MW GXCUBE provided as input!'
    goto,invalid_input
  endif
  if ~valid_map(gxcube.fovmap) then begin
    message,'Invalid FOV map GXCUBE tag!'
    goto,invalid_input
  endif
 info=gxcube.info
 fovmap=gxcube.fovmap
 freq=(((info).spectrum).x.axis)
 freq2=freq^2
 nfreq=n_elements(freq)
 dx=fovmap->Get(/dx)
 dy=fovmap->Get(/dy)
 coeff=gx_sfu2tb(dx*dy)
 amap=fovmap->get(/map)
 add_prop,amap,freq=0d
 add_prop,amap,frequnits='GHz'
 add_prop,amap,Stokes=''
 add_prop,amap,datatype='Brightness Temperature'
 add_prop,amap,dataunit='K'
 keys=gx_getEBTELparms(amap.gx_key,a=a,b=b,q0=q0,parms=parms,formula=formula)
 formula=str_replace(formula,'q0',string(parms[0],format='(g0)'))
 i=map->get(/count)
 for k=0,nfreq-1 do begin
  amap.freq=freq[k]
  amap.id=string(freq[k],formula,format="('GX Tb_I ',g0,' GHz',a0)")
  amap.Stokes='I'
  amap.data=coeff*(gxcube.data[*,*,k,1,0]+gxcube.data[*,*,k,0,0])/freq2[k]/2
  map->setmap,i++,amap
  amap.id=string(freq[k],formula,format="('GX Tb_V ',g0,' GHz',a0)")
  amap.Stokes='V'
  amap.data=coeff*(gxcube.data[*,*,k,1,0]-gxcube.data[*,*,k,0,0])/freq2[k]/2
  map->setmap,i++,amap
 endfor
 return,map
 invalid_input:
 return,map
end