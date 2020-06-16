function gx_voxelid,test_id,chromo=chromo,tr=tr,corona=corona,euvtr=euvtr,in=in,$
                    nw=nw,enw=enw,fa=fa,pl=pl,pen=pen,umb=umb,tube=tube,layer=layer,mask=mask
 id=0UL
 if keyword_set(chromo) then id+=1UL
 if keyword_set(tr) then id+=2UL
 if keyword_set(corona) then id+=4UL
 if keyword_set(euvtr) then id+=8UL
 if keyword_set(tube) then id+=ishft(255l,8)
 if keyword_set(layer) then id+=ishft(255l,16)
 if keyword_set(mask) then id+=ishft(255l,24)
 if keyword_set(in) then  id+=ishft(1l,24)
 if keyword_set(nw) then  id+=ishft(2l,24)
 if keyword_set(enw) then id+=ishft(3l,24)
 if keyword_set(fa) then  id+=ishft(4l,24)
 if keyword_set(pl) then  id+=ishft(5l,24)
 if keyword_set(pen) then id+=ishft(6l,24)
 if keyword_set(umb) then id+=ishft(7l,24)
 return,n_elements(test_id) gt 0? id and ulong(test_id): id
end