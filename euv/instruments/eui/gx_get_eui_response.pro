function gx_get_eui_response,date,FSI=FSI,HRI=HRI
;+
; Project     : GX Simulator
;
; Name        : get_eui_response
;
; Purpose     : Calculate SO/EUI temperature response in 174 A band. 
;
; Syntax      : resp=gx_get_eui_response(date,/FSI) or
;				resp=gx_get_eui_response(date,/HRI)
; Inputs:
;				date - date of observations
;
; Inputs keywords: 
;				FSI - Full Sun Imager (Default)
;				HRI - High Resolution Imager
;
; Outputs     :  structure containing response and related quantities. 
;				
;
; Calls: 	PB0R_SOLO
;
; History     : Written, 15-Nov-2024  T.A. Kucera
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
		pix_arcsec=4.4401245       
	endif
	if keyword_set(HRI) then begin
		respfile='EUIHRI_GXresponse.sav'
		pix_arcsec=0.49200001 
	endif

	RSun_arcsec=(pb0r_solo(date,/arcsec))[2] ;(arcsec/RSun)
	RSun_cm=6.957d+10  
	
	
	restore,gx_findfile(respfile),/ver
	;change units from DN cm^5 s^-1 cm^-2 to DN cm^5 s^-1 pix^-1 
	GXResponse.all=GXResponse.all/(RSun_arcsec/RSun_cm/pix_arcsec)^2
	GXResponse.Runits='DN cm^5 s^-1 pix^-1'
	GXResponse.date=date
		
	return,GXResponse

end
