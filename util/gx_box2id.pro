;This function takes as input an AMPPP box structure and returns a voxel_id 3D array
;that replicates the voxel_id computed at the object level by the gx_volume::UpdateVoxeId method.
function gx_box2id,box,tr_mask=tr_mask
  if size(box,/tname) ne 'STRUCT' then return, !null
   validBxByBz=(tag_exist(box,'Bx') and tag_exist(box,'By') and tag_exist(box,'Bz'))
   validB=tag_exist(box,'bcube')
   if ~(validBxByBz or validB) then return, !null
  if ~tag_exist(box,'corona_base') then begin
    if tag_exist(box,'startidx') then begin
      corona_base=min((array_indices(box.startidx,box.startidx[where(box.startidx)]))[2,*])
    endif else corona_base=ceil(gx_tr_height()/box.dr[2])
  endif else corona_base=box.corona_base
  chromo_layers=tag_exist(box,'chromo_layers')?box.chromo_layers:corona_base
  sz=validBxByBz?size(box.Bx):size(box.bcube[*,*,*,0])
  if corona_base gt 0 then sz[3]=sz[3]-corona_base+chromo_layers
  id=ulonarr(sz[1:3])
  tr=ulonarr(sz[1],sz[2])
  if tag_exist(box,'chromo_idx') and tag_exist(box,'chromo_t') and tag_exist(box,'chromo_n') then begin
    vol_t=fltarr(sz[1],sz[2],sz[3])
    vol_t[box.chromo_idx]=box.chromo_t
    vol_n=fltarr(sz[1],sz[2],sz[3])
    vol_n[box.chromo_idx]=box.chromo_n
    for i=0,sz[1]-1 do for j=0, sz[2]-1 do begin
      tr[i,j]=max(where(vol_n[i,j,*] ne 0 and vol_t[i,j,*] ne 0 ))+1
    endfor
  endif else tr[*]=corona_base; if this is not a combo box, the height of TR is everywhere the height of the corona base
  id[*,*,0:max(tr)]=gx_voxelid(/chromo); Do not Interpolate! TR or Chromo
  for i=0,sz[1]-1 do begin
    for j=0, sz[2]-1 do begin
      id[i,j,tr[i,j]]+=gx_voxelid(/tr,/euv) ;Transition Region(TR voxels are by default EUV active TR voxels)
      id[i,j,tr[i,j]:*]+=gx_voxelid(/corona) ;Corona (TR is also part of corona)
    endfor
  endfor
  ;Here I am assigning EUV active TR IDs to those Coronal voxels that are direct neighbors of Chromo voxels
  coridx=where(id eq gx_voxelid(/chromo,/corona), count)
  if count gt 0 then edge=where(id[coridx+1] eq 1UL or id[coridx-1] eq 1,count)
  if count gt 0 then id[coridx[edge]]+=gx_voxelid(/tr,/euv)
  tr_sz=size(tr_mask)
  if tr_sz[1] eq sz[1] and tr_sz[2] eq sz[2] then begin
    mask=1ul-ulong(tr_mask)
    tr_idx=where((id and gx_voxelid(/tr,/euv)) ne 0, tr_count)
    if tr_count gt 0 then begin
      idx=array_indices(id,tr_idx)
      id[tr_idx]=id[tr_idx] -(mask[idx[0,*],idx[1,*]])*gx_voxelid(/euv)
    end
  endif
return,id
end