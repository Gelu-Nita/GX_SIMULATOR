function gx_bitmap,file
 ;xy= GET_SCREEN_SIZE()
 device, get_screen_size=xy
 if xy[0] lt 3200 then nb=16 else nb=32
 bitmap=read_bmp(file,R,G,B,/rgb)
 if size(bitmap,/n_dim) eq 3 then $
  bitmap=rebin(transpose(bitmap,[1,2,0]),nb,nb,3) $
  else begin
    dim=size(bitmap,/dim)
    rgb=bytarr(dim[0],dim[1],3)
    rgb[*,*,0]=R[bitmap]
    rgb[*,*,1]=G[bitmap]
    rgb[*,*,2]=B[bitmap]
    bitmap=rebin(rgb,nb,nb,3)
  end  
 return,bitmap
end