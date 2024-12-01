function gx_get_aia_response,date,TResp=TResp
;+
; Project     : GX Simulator
;
; Name        : get_aia_response
;
; Purpose     : Calculate SDO/AIA temperature response. 
;				This program is not needed - but is an example of 
;				what is being done for the other EUV instruments
;
; Syntax      : resp=gx_get_aia_response(date)
;
; Inputs:
;				date - date of observations
;
; Inputs  keywords: 
;				TResp - use AIA temperature response function
;				Otherwise use response derived from pre-calculated spectra 
;				convolved with instrment response as a function of wavelength
;
; Outputs     :  structure containing response and related quantities. 
;				
;
; Calls: 	PB0R, AIA_GET_RESPONSE
; 			REQUIRES AIA SSW PACKAGE
;
; History     : Written, 15-Nov-2024  T.A. Kucera
;+

if keyword_set(TResp) then begin
	GXResponse= aia_get_response(/temp,/dn,/evenorm,/chiantifix,timedepend=date) 
	
endif else begin		;based calculation on precaculated response based on 
					;model spectra and instrument wavelength response.
	respfile='AIA_GXresponse.sav'

	RSun_arcsec=(pb0r(date,/arcsec))[2]
	RSun_cm=6.957d+10    						
	pix_arcsec=0.6

	restore,gx_findfile(respfile)

	sz=size(GXResponse.all)
	;change units from DN cm^5 s^-1 cm^-2 to DN cm^5 s^-1 pix^-1 
	GXResponse.all=GXResponse.all/(RSun_arcsec/RSun_cm/pix_arcsec)^2
	GXResponse.Runits='DN cm^5 s^-1 pix^-1'
	GXResponse.date=date
endelse
return,GXResponse

end
