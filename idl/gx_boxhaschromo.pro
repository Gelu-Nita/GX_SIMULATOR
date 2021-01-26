function gx_boxhaschromo,box,tags=tags
 chromo_tags=[ 'chromo_idx','chromo_bcube','n_htot','n_hi','n_p','dz',$
               'chromo_n','chromo_t','chromo_layers','tr','tr_h','corona_base']
 tags=[]
 for i=0,n_elements(chromo_tags)-1 do begin
  tag=chromo_tags[i]
  if tag_exist(box,tag) then tags=[tags,tag]
 endfor
 return,n_elements(tags) ne 0
end