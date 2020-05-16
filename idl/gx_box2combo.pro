function gx_box2combo,pbox,chromo_mask
  if n_elements(chromo_mask) eq 0 then begin
    if ~tag_exist(pbox,'base') then return, pbox
    if ~tag_exist(pbox.base,'bz') then return, pbox
    if ~tag_exist(pbox.base,'ic') then return, pbox
    chromo_mask=decompose(pbox.base.bz,pbox.base.ic)
  endif
  populate_chromo, chromo_mask,chromo
  csize=size(chromo.nh)
  if tag_exist(pbox,'bx') then begin
    bx=pbox.bx
    pbox=rem_tag(pbox,'bx')
    by=pbox.by
    pbox=rem_tag(pbox,'by')
    bz=pbox.bz
    pbox=rem_tag(pbox,'bz')
    dim=size(bx,/dim)
    bcube=fltarr(dim[0],dim[1],dim[2],3)
    bcube[*,*,*,0]=temporary(bx)
    bcube[*,*,*,1]=temporary(by)
    bcube[*,*,*,2]=temporary(bz)
    pbox=add_tag(pbox,temporary(bcube),'bcube',/no_copy,/duplicate)
  endif
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

  
  bsize=size(bcube)
  deltaz=max(total(big_dh,3))/bsize[3]

  chromobox={bcube:temporary(bcube),$
  chromo_idx:chromo_idx, n_htot:nh,n_hi:nhi,n_p:np,dz:temporary(dh),$
  chromo_n:n,chromo_t:t,chromo_layers:max_chromo_idx+1,tr:tr_idx,tr_h:tr_h}
    
  box=add_tag(pbox,temporary(chromobox),'chromobox',/no_copy,/duplicate)
  
  if tag_exist((box),'base') then begin
    base=(box).base
    base=rem_tag(base,'Chromo_Mask')
    base=add_tag(base,chromo_mask,'Chromo_Mask',/duplicate)
    box=rem_tag(box,'base')
    box=add_tag(box,base,'base',/no_copy,/duplicate)
  end

  
return,box
end