function gx_get_trace_response,date
 default,date,RELTIME(/now)
 pb0r=pb0r(date,/arcsec,l0=l0)
 restore,gx_findfile('trace_response.sav')
 response=add_tag(response,AIA_BP_UTC2DATE_STRING(ANYTIM2UTC(date, /ccsds)),'date')
 response=add_tag(response,1d,'pix_arcsec')
 response=add_tag(response,pb0r[0],'p')
 response=add_tag(response,pb0r[1],'b0')
 response=add_tag(response,pb0r[2],'rsun_arcsec')
 response=add_tag(response,l0,'l0')
 return,response
end