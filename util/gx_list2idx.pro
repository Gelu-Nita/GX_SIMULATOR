function gx_list2idx,list,name
  return,where(strcompress(strupcase(list),/rem) eq strcompress(strupcase(name),/rem))
end