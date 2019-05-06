function gx_voxelid,chromo=chromo,corona=corona,tr=tr,euvtr=euvtr,in=in,nw=nw,enw=enw,pl=pl,fa=fa,pen=pen,umb=umb
 id=0UL
 if keyword_set(chromo) then id+=1UL
 if keyword_set(tr) then id+=2UL
 if keyword_set(corona) then id+=4UL
 if keyword_set(euvtr) then id+=8UL
 if keyword_set(in) then  id+=ishft(1l,24)
 if keyword_set(nw) then  id+=ishft(2l,24)
 if keyword_set(enw) then id+=ishft(3l,24)
 if keyword_set(fa) then  id+=ishft(4l,24)
 if keyword_set(pl) then  id+=ishft(5l,24)
 if keyword_set(pen) then id+=ishft(6l,24)
 if keyword_set(umb) then id+=ishft(7l,24)
 return,id
end