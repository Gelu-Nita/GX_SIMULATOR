function gx_get_euvi_response,date,Ahead=Ahead,Behind=Behind
;+
; Project     : GX Simulator
;
; Name        : get_eui_response
;
; Purpose     : Calculate SO/EUI temperature response. 
;
; Syntax      : resp=gx_get_euvi_response(date,/A) or
;				resp=gx_get_euvi_response(date,/B)
; Inputs:
;				date - date of observations
;
; Inputs keywords: 
;				Ahead - EUVI on STEREO-Ahead
;				Behind - EUVI on STEREO-Behind
;
; Outputs     :  structure containing response and related quantities. 
;				
;
; Calls: 	PB0R_STEREO
;
; History     : Written, 15-Nov-2024  T.A. Kucera
;+

	;Must select either Ahead or Behind. Ahead is default
	if keyword_set(Ahead) and keyword_set(Behind) then $
		message,'Must Select EITHER Ahead or Behind, not both'
	
	if not (keyword_set(Ahead) or keyword_set(Behind)) then begin
		message,/info,'Assuming STEREO-A/EUVI'
		Ahead=1b
	endif
	
	if keyword_set(Ahead) then begin
		respfile='EUVIA_GXResponse.sav'
		pix_arcsec = 1.58777  ;arcsec/pixel
	endif
	if keyword_set(Behind) then begin
		respfile='EUVIB_GXResponse.sav'
		pix_arcsec  = 1.59000		;arcsec/pixel
	endif

	RSun_arcsec=(pb0r_stereo(date,/arcsec,ahead=ahead,behind=behind))[2] ;(arcsec/RSun)
	RSun_cm=6.957d+10  ;cm/RSun
	
	
	restore,gx_findfile(respfile)
	;change units from DN cm^5 s^-1 cm^-2 to DN cm^5 s^-1 pix^-1 

	GXResponse.all=GXResponse.all/(RSun_arcsec/RSun_cm/pix_arcsec)^2
	GXResponse.Runits='DN cm^5 s^-1 pix^-1'
	GXResponse.date=date

	
	return,GXResponse

end
