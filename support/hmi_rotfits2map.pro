pro hmi_rotfits2map,file,map
  data=readfits(file,hdr)
  index=fitshead2struct(hdr)
  if tag_exist(index,'content') then begin
    if index.content eq 'CONTINUUM INTENSITY' then begin
      darklimb_correct, data, data, lambda=index.WAVELNTH,limbxyr=[index.crpix1,index.crpix2,index.rsun_obs/index.cdelt1]
    endif
  endif
  ;rotate HMI images by 180 degrees and update information in index (from HMI manual)
  data = rotate(temporary(data), 2)
  index.crpix1 = index.naxis1 - index.crpix1 + 1
  index.crpix2 = index.naxis2 - index.crpix2 + 1
  index.crota2 = index.crota2 - 180.
  ;update index and write to fits
  hdr=struct2fitshead(index, data)
  index2map,index, data, map
  map.id='SDO/HMI '+index.content
end