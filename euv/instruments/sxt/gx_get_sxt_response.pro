function gx_get_sxt_response,date
 default,date,RELTIME(/now)
 pb0r=pb0r(date,/arcsec,l0=l0)
 restore,gx_findfile('sxt_response.sav')
 response=add_tag(response,AIA_BP_UTC2DATE_STRING(ANYTIM2UTC(date, /ccsds)),'date')
 response.channels= str_replace(strupcase(response.channels),'GAL','A')
 response=add_tag(response,2.45d,'pix_arcsec')
 response=add_tag(response,pb0r[0],'p')
 response=add_tag(response,pb0r[1],'b0')
 response=add_tag(response,pb0r[2],'rsun_arcsec')
 response=add_tag(response,l0,'l0')
 return,response
end