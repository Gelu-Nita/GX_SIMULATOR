function gx_get_aia_response,date,TResp=TResp,evenorm=evenorm,chiantifix=chiantifix
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
; 13-Dec-2024: exposed the evenorm and chiantifix keywors, for compatibility with GX Simulator
;            : replaced the hardwired RSun_cm by the call to wcs_rsun function, 
;              which in the GX Simulator enviroment is 6.96d10 cm, as per the HMI team convention
;            : added instrumental pix_arcsec and orbital rsun_arcsec values to the response function for later use  
;            : defaulted the date input to the current time, if not provided unless the /Tresp is used   
;            ; changed time format to the same format returned by the aia_get_response.pro
;+

pix_arcsec=0.6d
pb0r=pb0r(date,/arcsec,l0=l0)
RSun_arcsec=(pb0r(date,/arcsec))[2]
RSun_cm=wcs_rsun(unit='cm')

if keyword_set(TResp) then begin
	if n_elements(date) ne 0  then begin
	 GXResponse= aia_get_response(/temp,/dn,evenorm=evenorm,chiantifix=chiantifix,timedepend=date)
	endif else begin
	 restore,gx_findfile('AIA_response.sav')
	 GXResponse=response
	endelse
	
endif else begin		;based calculation on precaculated response based on 
					;model spectra and instrument wavelength response.
	default,date,RELTIME(/now)					
	restore,gx_findfile('AIA_GXresponse.sav')

	sz=size(GXResponse.all)
	;change units from DN cm^5 s^-1 cm^-2 to DN cm^5 s^-1 pix^-1 
	GXResponse.all=GXResponse.all/(RSun_arcsec/RSun_cm/pix_arcsec)^2
	GXResponse.Runits='DN cm^5 s^-1 pix^-1'
	GXResponse.date=AIA_BP_UTC2DATE_STRING(ANYTIM2UTC(date, /ccsds))
endelse
  GXResponse=add_tag(GXResponse,pix_arcsec,'pix_arcsec')
  GXresponse=add_tag(GXresponse,pb0r[0],'p')
  GXresponse=add_tag(GXresponse,pb0r[1],'b0')
  GXresponse=add_tag(GXresponse,RSun_arcsec,'rsun_arcsec')
  GXresponse=add_tag(GXresponse,l0,'l0')
  return,GXResponse
end
