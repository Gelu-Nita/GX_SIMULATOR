function gx_name2idx,parms,name
  return,where(strcompress(strupcase(parms.name),/rem) eq strcompress(strupcase(name),/rem))
end