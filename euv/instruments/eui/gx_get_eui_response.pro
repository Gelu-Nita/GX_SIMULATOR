function gx_get_eui_response,date,FSI=FSI,HRI=HRI
;+
; Project     : GX Simulator
;
; Name        : get_eui_GXresponse
;
; Purpose     : Calculate SO/EUI temperature GXresponse in 174 A band. 
;
; Syntax      : resp=gx_get_eui_GXresponse(date,/FSI) or
;				resp=gx_get_eui_GXresponse(date,/HRI)
; Inputs:
;				date - date of observations
;
; Inputs keywords: 
;				FSI - Full Sun Imager (Default)
;				HRI - High Resolution Imager
;
; Outputs     :  structure containing GXresponse and related quantities. 
;				
;
; Calls: 	PB0R_SOLO
;
; History     : Written, 15-Nov-2024  T.A. Kucera
; 13-Dec-2024 : replaced the hardwired RSun_cm by the call to wcs_rsun function,
;              which in the GX Simulator enviroment is 6.96d10 cm, as per the HMI team convention
;            : added instrumental pix_arcsec and orbital prb0r and l0 values to the GXresponse function for later use 
;            : defaulted the date input to the current time
;            ; changed time format to the same format returned by the aia_get_GXresponse.pro
;+

;Must select either HRI or FSI. FSI is default
	if keyword_set(FSI) and keyword_set(HRI) then $
		message,'Must Select EITHER FSI or HRI, not both'
	
	if not (keyword_set(FSI) or keyword_set(HRI)) then begin
		message,/info,'Assuming EUI/FSI'
		FSI=1b
	endif
	
	if keyword_set(FSI) then begin
		respfile='EUIFSI_GXresponse.sav'
		pix_arcsec=4.4401245d       
	endif
	if keyword_set(HRI) then begin
		respfile='EUIHRI_GXresponse.sav'
		pix_arcsec=0.49200001d 
	endif
  default,date,RELTIME(/now)  
  pb0r=pb0r_solo(date,l0=l0,/arcsec)
	RSun_arcsec=(pb0r_solo(date,/arcsec))[2] ;(arcsec/RSun)
	RSun_cm=wcs_rsun(unit='cm') 
	
	
	restore,gx_findfile(respfile)
	;change units from DN cm^5 s^-1 cm^-2 to DN cm^5 s^-1 pix^-1 
	GXresponse.all=GXresponse.all/(RSun_arcsec/RSun_cm/pix_arcsec)^2
	GXresponse.Runits='DN cm^5 s^-1 pix^-1'
	GXresponse.date=AIA_BP_UTC2DATE_STRING(ANYTIM2UTC(date, /ccsds))
	GXresponse=add_tag(GXresponse,pix_arcsec,'pix_arcsec')	
	GXresponse=add_tag(GXresponse,pb0r[0],'p')
  GXresponse=add_tag(GXresponse,pb0r[1],'b0')
  GXresponse=add_tag(GXresponse,RSun_arcsec,'rsun_arcsec')
  GXresponse=add_tag(GXresponse,l0,'l0')
	return,GXresponse

end
