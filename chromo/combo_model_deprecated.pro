function combo_model_deprecated,pbox,chromo_mask
setenv, 'WCS_RSUN=6.96d8'
populate_chromo, chromo_mask,chromo
csize=size(chromo.nh)
msize=size(reform((pbox).bcube[*,*,*,0]))
dx=(pbox).dr[0]
dy=(pbox).dr[1]
if ~tag_exist(pbox,'dz') then begin
 dz=dblarr(msize[1],msize[2],msize[3])
 dz[*]=(pbox).dr[2]
endif else dz=(pbox).dz

;Assume the heights to be 0,dz,2*dz,.....
z=dblarr(msize[1],msize[2],msize[3])
cumz=total(dz,3,/cum)
z[*,*,1:msize[3]-1]=cumz[*,*,0:msize[3]-2]


bad=where(chromo.dh eq 1,nbad,comp=chromo_idx)
if nbad gt 0 then chromo.dh[bad]=0
tr_h=total(chromo.dh,3,/double)/gx_rsun(unit='km')
max_tr_h=max(tr_h)
corona_base_idx=min(where(z[0,0,*] ge max_tr_h))
corona_base_height=z[0,0,corona_base_idx]
dh=chromo.dh/gx_rsun(unit='km')
tr_idx=lonarr(csize[1],csize[2])

for i=0, csize[1]-1 do begin
  for j=0,csize[2]-1 do begin
    tr_idx[i,j]=max(where(chromo.dh[i,j,*] ne 0))+1
    if tr_idx[i,j] lt csize[3] then begin
      dh[i,j,tr_idx[i,j]:*]=(corona_base_height-tr_h[i,j])/n_elements(dh[i,j,tr_idx[i,j]:*])
    endif else dz[i,j,corona_base_idx]=dz[i,j,corona_base_idx]+corona_base_height-tr_h[i,j]
  endfor
endfor

dz=dz[*,*,corona_base_idx:*]

size_dz=size(dz)
big_size=csize[3]+size_dz[3]
big_dh=dblarr(csize[1],csize[2],big_size)
big_dh[*,*,0:csize[3]-1]=dh[*,*,0:csize[3]-1]
big_dh[*,*,csize[3]:*]=dz
big_h=dblarr(csize[1],csize[2],big_size)
cum_big_h=total(big_dh,3,/cum,/double)
big_h[*,*,1:big_size-1]=cum_big_h[*,*,0:big_size-2]

max_chromo_idx=max(tr_idx)-1
h=big_h[*,*,0:max_chromo_idx]


bcube=fltarr(csize[1],csize[2],max_chromo_idx+1,3)


for i=0,csize[1]-1 do begin
  for j=0,csize[2]-1 do begin    
      bcube[i,j,*,0]=interpol((pbox).bcube[i,j,*,0],z[i,j,*],h[i,j,*])
      bcube[i,j,*,1]=interpol((pbox).bcube[i,j,*,1],z[i,j,*],h[i,j,*])
      bcube[i,j,*,2]=interpol((pbox).bcube[i,j,*,2],z[i,j,*],h[i,j,*])
  end
end


t=chromo.temp[chromo_idx]

n=chromo.nne[chromo_idx]

nh=chromo.nh[chromo_idx]

nhi=chromo.nhi[chromo_idx]

np=chromo.np[chromo_idx]


bx=bcube[*,*,*,0]
by=bcube[*,*,*,1]
bz=bcube[*,*,*,2]
csize=size(bx)



min_corona_idx=corona_base_idx;(array_indices(z,min(where(z gt max(h)))))[2]+1
bcube=fltarr(csize[1],csize[2],max_chromo_idx+msize[3]-min_corona_idx+1,3)

bsize=size(bcube)
bcube[*,*,0:max_chromo_idx,0]=bx
bcube[*,*,max_chromo_idx+1:*,0]=(pbox).bcube[*,*,min_corona_idx:*,0]
bcube[*,*,0:max_chromo_idx,1]=by
bcube[*,*,max_chromo_idx+1:*,1]=(pbox).bcube[*,*,min_corona_idx:*,1]
bcube[*,*,0:max_chromo_idx,2]=bz
bcube[*,*,max_chromo_idx+1:*,2]=(pbox).bcube[*,*,min_corona_idx:*,2]


deltaz=max(total(big_dh,3))/bsize[3]

box={bcube:temporary(bcube),dr:[dx,dy,deltaz],$
  chromo_idx:chromo_idx, n_htot:nh,n_hi:nhi,n_p:np,dz:temporary(big_dh),$
  chromo_n:n,chromo_t:t,chromo_layers:max_chromo_idx+1,tr:tr_idx,tr_h:tr_h}

if tag_exist((pbox),'refmaps') then box=add_tag(box,ptr_new(*((pbox).refmaps)),'refmaps',/no_copy,/duplicate)

if tag_exist((pbox),'base') ne 0 and tag_exist((pbox),'index') ne 0 then begin
  base=(pbox).base
  base=rem_tag(base,'Chromo_Mask')
  base=add_tag(base,chromo_mask,'Chromo_Mask',/no_copy,/duplicate)
  box=rem_tag(box,'base')
  box=add_tag(box,base,'base',/no_copy,/duplicate)
  box=add_tag(box,(pbox).index,'index',/no_copy,/duplicate)
end

    
if tag_exist((pbox),'lon') ne 0 and tag_exist((pbox),'lat') ne 0  then begin
  box=create_struct(box,'lon',(pbox).lon, 'lat',(pbox).lat)  
endif

 
 if tag_exist((pbox),'add_base_layer') ne 0 then box=create_struct(box,'add_base_layer',(pbox).add_base_layer)
 
 
 if tag_exist(pbox,'idx')  then begin   
    good=where((pbox).idx ge msize[1]*msize[2]*min_corona_idx)
    idx=(pbox).idx[good]-msize[1]*msize[2]*(min_corona_idx-1)
    idx=idx+csize[1]*csize[2]*(max_chromo_idx)
    c_idx=long(floor(h/(pbox).dr[2]))
    chromo_vol=float(c_idx*0)
    vol=fltarr(msize[1],msize[2],msize[3])
    vol[(pbox).idx]=(pbox).bmed
    for i=0,msize[1]-1 do  for j=0,msize[2]-1 do for k=tr_idx[i,j],csize[3]-1 do chromo_vol[i,j,k]=vol[i,j,c_idx[i,j,k]]
    chromo_full=where(chromo_vol ne 0,chromo_count)
    if chromo_count gt 0  then chromo_bmed=chromo_vol[chromo_full]
    if chromo_count gt 0 then begin
        vol[(pbox).idx]=(pbox).length
        chromo_vol[*]=0
        for i=0,msize[1]-1 do  for j=0,msize[2]-1 do for k=tr_idx[i,j],csize[3]-1 do chromo_vol[i,j,k]=vol[i,j,c_idx[i,j,k]]
        chromo_length=chromo_vol[chromo_full]
      if tag_exist(pbox,'alpha') then begin
        vol[(pbox).idx]=(pbox).alpha
        chromo_vol[*]=0
        for i=0,msize[1]-1 do  for j=0,msize[2]-1 do for k=tr_idx[i,j],csize[3]-1 do chromo_vol[i,j,k]=vol[i,j,c_idx[i,j,k]]
        chromo_alpha=chromo_vol[chromo_full]
      endif
      if tag_exist(pbox,'curlb') then begin
        vol[(pbox).idx]=(pbox).curlb
        chromo_vol[*]=0
        for i=0,msize[1]-1 do  for j=0,msize[2]-1 do for k=tr_idx[i,j],csize[3]-1 do chromo_vol[i,j,k]=vol[i,j,c_idx[i,j,k]]
        chromo_curlb=chromo_vol[chromo_full]
      endif
      if tag_exist(pbox,'foot1') then begin
        vol[(pbox).idx]=(pbox).foot1
        chromo_vol[*]=0
        for i=0,msize[1]-1 do  for j=0,msize[2]-1 do for k=tr_idx[i,j],csize[3]-1 do chromo_vol[i,j,k]=vol[i,j,c_idx[i,j,k]]
        chromo_foot1=chromo_vol[chromo_full]
      endif
      if tag_exist(pbox,'foot2') then begin
        vol[(pbox).idx]=(pbox).foot2
        chromo_vol[*]=0
        for i=0,msize[1]-1 do  for j=0,msize[2]-1 do for k=tr_idx[i,j],csize[3]-1 do chromo_vol[i,j,k]=vol[i,j,c_idx[i,j,k]]
        chromo_foot2=chromo_vol[chromo_full]
      endif
     end   
    idx=(chromo_count gt 0)?[chromo_full,idx]:idx 
    length=(chromo_count gt 0)?[temporary(chromo_length),(pbox).length[good]]:(pbox).length[good]
    bmed=  (chromo_count gt 0)?[temporary(chromo_bmed),(pbox).bmed[good]]:(pbox).bmed[good]
    if tag_exist(pbox,'alpha')  then $
    alpha=(chromo_count gt 0)?[temporary(chromo_alpha),(pbox).alpha[good]]:(pbox).alpha[good]
    if tag_exist(pbox,'curlb')  then $
    curlb=(chromo_count gt 0)?[temporary(chromo_curlb),(pbox).curlb[good]]:(pbox).curlb[good]
    if tag_exist(pbox,'foot1')  then $
    foot1=(chromo_count gt 0)?[temporary(chromo_foot1),(pbox).foot1[good]]:(pbox).foot1[good]
    if tag_exist(pbox,'foot2')  then $
    foot2=(chromo_count gt 0)?[temporary(chromo_foot2),(pbox).foot2[good]]:(pbox).foot2[good]
endif  

if tag_exist(pbox,'oidx')  then begin
    good=where((pbox).oidx ge msize[1]*msize[2]*min_corona_idx)
    oidx=(pbox).oidx[good]-msize[1]*msize[2]*(min_corona_idx-1)
    oidx=oidx+csize[1]*csize[2]*(max_chromo_idx)
    c_idx=long(floor(h/(pbox).dr[2]))
    chromo_vol=float(c_idx*0)
    vol=fltarr(msize[1],msize[2],msize[3])
    vol[(pbox).oidx]=(pbox).obmed
    for i=0,msize[1]-1 do  for j=0,msize[2]-1 do for k=tr_idx[i,j],csize[3]-1 do chromo_vol[i,j,k]=vol[i,j,c_idx[i,j,k]]
    chromo_full=where(chromo_vol ne 0,chromo_count)
    if chromo_count gt 0  then chromo_obmed=chromo_vol[chromo_full]
    if chromo_count gt 0 then begin
      vol[(pbox).oidx]=(pbox).olength
      chromo_vol[*]=0
      for i=0,msize[1]-1 do  for j=0,msize[2]-1 do for k=tr_idx[i,j],csize[3]-1 do chromo_vol[i,j,k]=vol[i,j,c_idx[i,j,k]]
      chromo_olength=chromo_vol[chromo_full]
      if tag_exist(pbox,'ofoot1') then begin
        vol[(pbox).oidx]=(pbox).ofoot1
        chromo_vol[*]=0
        for i=0,msize[1]-1 do  for j=0,msize[2]-1 do for k=tr_idx[i,j],csize[3]-1 do chromo_vol[i,j,k]=vol[i,j,c_idx[i,j,k]]
        chromo_ofoot1=chromo_vol[chromo_full]
      endif
      if tag_exist(pbox,'ofoot2') then begin
        vol[(pbox).oidx]=(pbox).ofoot2
        chromo_vol[*]=0
        for i=0,msize[1]-1 do  for j=0,msize[2]-1 do for k=tr_idx[i,j],csize[3]-1 do chromo_vol[i,j,k]=vol[i,j,c_idx[i,j,k]]
        chromo_ofoot2=chromo_vol[chromo_full]
      endif
    end
    oidx=(chromo_count gt 0)?[chromo_full,oidx]:oidx
    olength=(chromo_count gt 0)?[temporary(chromo_olength),(pbox).olength[good]]:(pbox).olength[good]
    obmed=  (chromo_count gt 0)?[temporary(chromo_obmed),(pbox).obmed[good]]:(pbox).obmed[good]
    if tag_exist(pbox,'ofoot1')  then $
      ofoot1=(chromo_count gt 0)?[temporary(chromo_ofoot1),(pbox).ofoot1[good]]:(pbox).ofoot1[good]
    if tag_exist(pbox,'ofoot2')  then $
      ofoot2=(chromo_count gt 0)?[temporary(chromo_ofoot2),(pbox).ofoot2[good]]:(pbox).ofoot2[good]
    endif
  
  if n_elements(alpha) ne 0 then box=add_tag(box,alpha,'alpha',/no_copy,/duplicate)
  if n_elements(curlb) ne 0 then box=add_tag(box,curlb,'curlb',/no_copy,/duplicate)
  if n_elements(idx) ne 0 then box=add_tag(box,idx,'idx',/no_copy,/duplicate)
  if n_elements(bmed) ne 0 then box=add_tag(box,bmed,'bmed',/no_copy,/duplicate)
  if n_elements(length) ne 0 then box=add_tag(box,length,'length',/no_copy,/duplicate)
  if n_elements(foot1) ne 0 then box=add_tag(box,foot1,'foot1',/no_copy,/duplicate)
  if n_elements(foot2) ne 0 then box=add_tag(box,foot2,'foot2',/no_copy,/duplicate)
  if n_elements(oidx) ne 0 then box=add_tag(box,oidx,'oidx',/no_copy,/duplicate)
  if n_elements(obmed) ne 0 then box=add_tag(box,obmed,'obmed',/no_copy,/duplicate)
  if n_elements(olength) ne 0 then box=add_tag(box,olength,'olength',/no_copy,/duplicate)
  if n_elements(ofoot1) ne 0 then box=add_tag(box,ofoot1,'ofoot1',/no_copy,/duplicate)
  if n_elements(ofoot2) ne 0 then box=add_tag(box,ofoot2,'ofoot2',/no_copy,/duplicate)

  if tag_exist((pbox),'id') then begin
    break_file,(pbox).id, disk_log, dir, name, ext, fversion, node, /last_dot
    box=add_tag(box,name+'.CHR','id',/no_copy,/duplicate)
  end
 if tag_exist((pbox),'execute') then  box=add_tag(box,pbox.execute,'execute',/no_copy,/duplicate)
return,box
end