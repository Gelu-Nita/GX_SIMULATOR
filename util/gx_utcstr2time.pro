function gx_utcstr2time,utcstr,seconds=seconds
 year=(strmid(strupcase(utcstr),0,4))
 month=(strmid(strupcase(utcstr),4,2))
 day=(strmid(strupcase(utcstr),6,2))
 hh=(strmid(strupcase(utcstr),9,2))
 mm=(strmid(strupcase(utcstr),11,2))
 ss=(strmid(strupcase(utcstr),13,2))
 time=atime(anytim(year+'/'+month+'/'+day+' '+hh+':'+mm+':'+ss))
 return,keyword_set(seconds)?anytim(time):time
end