function gx_get_euvi_response,date,Ahead=Ahead,Behind=Behind
;+
; Project     : GX Simulator
;
; Name        : get_eui_GXresponse
;
; Purpose     : Calculate SO/EUI temperature GXresponse. 
;
; Syntax      : resp=gx_get_euvi_GXresponse(date,/A) or
;				resp=gx_get_euvi_GXresponse(date,/B)
; Inputs:
;				date - date of observations
;
; Inputs keywords: 
;				Ahead - EUVI on STEREO-Ahead
;				Behind - EUVI on STEREO-Behind
;
; Outputs     :  structure containing GXresponse and related quantities. 
;				
;
; Calls: 	PB0R_STEREO
;
; History     : Written, 15-Nov-2024  T.A. Kucera
; 13-Dec-2024 : replaced the hardwired RSun_cm by the call to wcs_rsun function,
;              which in the GX Simulator enviroment is 6.96d10 cm, as per the HMI team convention
;            : added instrumental pix_arcsec and orbital rsun_arcsec values to the GXresponse function for later use 
;            : defaulted the date input to the current time
;            ; changed time format to the same format returned by the aia_get_GXresponse.pro 
;+

	;Must select either Ahead or Behind. Ahead is default
	if keyword_set(Ahead) and keyword_set(Behind) then $
		message,'Must Select EITHER Ahead or Behind, not both'
	
	if not (keyword_set(Ahead) or keyword_set(Behind)) then begin
		message,/info,'Assuming STEREO-A/EUVI'
		Ahead=1b
	endif
	
	if keyword_set(Ahead) then begin
		respfile='EUVIA_GXresponse.sav'
		pix_arcsec = 1.58777d  ;arcsec/pixel
	endif
	if keyword_set(Behind) then begin
		respfile='EUVIB_GXresponse.sav'
		pix_arcsec  = 1.59000d		;arcsec/pixel
	endif
  default,date,RELTIME(/now)
  pb0r=pb0r_stereo(date,/arcsec,ahead=ahead,behind=behind,l0=l0)
	RSun_arcsec=pb0r[2] ;(arcsec/RSun)
	RSun_cm=wcs_rsun(unit='cm') ;cm/RSun
	
	
	restore,gx_findfile(respfile)
	;change units from DN cm^5 s^-1 cm^-2 to DN cm^5 s^-1 pix^-1 

	GXresponse.all=GXresponse.all/(RSun_arcsec/RSun_cm/pix_arcsec)^2
	GXresponse.Runits='DN cm^5 s^-1 pix^-1'
	GXresponse=rep_tag_value(GXresponse,AIA_BP_UTC2DATE_STRING(ANYTIM2UTC(date, /ccsds)),'date')
  GXresponse=add_tag(GXresponse,pix_arcsec,'pix_arcsec')
  GXresponse=add_tag(GXresponse,pb0r[0],'p')
  GXresponse=add_tag(GXresponse,pb0r[1],'b0')
  GXresponse=add_tag(GXresponse,RSun_arcsec,'rsun_arcsec')
  GXresponse=add_tag(GXresponse,l0,'l0')
	return,GXresponse

end
