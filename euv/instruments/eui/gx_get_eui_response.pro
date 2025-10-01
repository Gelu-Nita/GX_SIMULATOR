function gx_get_eui_response,date,FSI=FSI,HRI=HRI,respdir=respdir
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
;				respdir - directory containing the instrument response file. 
;						Default is '$ssw/packages/gx_simulator/euv/instruments/eui/'
;
; Outputs     :  structure containing GXresponse and related quantities. 
;				
;
; Calls: 	PB0R_SOLO,  AIA_GET_RESPONSE, AIA_BP_MAKE_TRESP
;
; History     : Written, 15-Nov-2024  T.A. Kucera
; 13-Dec-2024 : replaced the hardwired RSun_cm by the call to wcs_rsun function,
;              which in the GX Simulator enviroment is 6.96d10 cm, as per the HMI team convention
;            : added instrumental pix_arcsec and orbital prb0r and l0 values 
;			 :	to the GXresponse function for later use 
;            : defaulted the date input to the current time
;            ; changed time format to the same format returned by the aia_get_GXresponse.pro
;14-Jul-2025 :  Rewrite to tie in calculation to AIA routines. TAK
;23-Sep-2025 : Fixed bug that EAUnits was not defined, and added respdir keyword 
;+

default,date,RELTIME(/now)  
  
;Must select either HRI or FSI. FSI is default
	if keyword_set(FSI) and keyword_set(HRI) then $
		message,'Must Select EITHER FSI or HRI, not both'
	
	if not (keyword_set(FSI) or keyword_set(HRI)) then begin
		message,/info,'Assuming EUI/FSI'
		FSI=1b
	endif
	
	;Restore Wavelength response for SO/EUI
	default,respdir,file_dirname(gx_findfile('EUIFSI_GXResponse.sav'),/mark)
	
;;;;
    ;restgen,file=respfile,EUI_FSI_resp,EUI_HRI_resp
  	if keyword_set(FSI) then begin
  		restore,concat_dir(respdir,'EUIFSI_GXResponse.sav')
		EffArea=EUI_FSI_resp
		pix_arcsec=4.4401245d 
		Name='EUVI/FSI'      
	endif
	if keyword_set(HRI) then begin
  		restore,concat_dir(respdir,'EUIHRI_GXResponse.sav')
		EffArea=EUI_HRI_resp
		pix_arcsec=0.49200001d 
		Name='EUVI/HRI'      
	endif  
    
    RA2ArcSec=!RADeg*3600d 
	platescale = (pix_arcsec/RA2ArcSec)^2
	countunits='DN'
    
    ;Want effective area in 'cm^2 DN phot^-1' to parallel the output of aia_get_response(/dn)
    EAChan='A174'
	NChan=1
	EAlam=10.*EffArea.wav	;converting from nm to Angstroms
	;Frederic Auchere says the units are 'DN ph^-1 cm^2 sr', 
	;but the input for  AIA_BP_MAKE_TRESP should be 'DN ph^-1 cm^2'	so I am dividing by the platescale in sr.
	EAAll=EffArea.resp/platescale ;converting from cm^2 DN phot^-1 sr to cm^2 DN phot^-1 
	EAUnits='cm^2 DN phot^-1'	
	
  ;Emissivity being used by AIA
	emiss= AIA_GET_RESPONSE(/emiss, /full) ;in  'erg cm+3 sr-1 s-1'
	Emiss_LogTe=emiss.total.LogTe
	Emiss_Wave=emiss.total.wave  ; 'Angstroms'
	NTemp=n_elements(Emiss_LogTe) 
	
	GXResponse={Name:Name}
	GXResponse=add_tag(GXResponse,emiss.total.logte,'LogTe')
	GXResponse=add_tag(GXResponse,dblarr(NTemp,NChan),'All')
	GXResponse=add_tag(GXResponse,EAChan,'Channels')
	GXResponse=add_tag(GXResponse,'DN cm^5 s^-1 pix^-1','Units')
	GXResponse=add_tag(GXResponse,EffArea.history,'EffArea_History')
	GXResponse=add_tag(GXResponse,emiss.general,'Emissinfo')
	

	
	;calculate the TResp  and put is in the same format as for AIA Temperature response
	trespstr={name:EAChan,wave:EALam,EA:EAAll, Platescale:platescale,units:EAunits}
	TResp1chan=  AIA_BP_MAKE_TRESP(emiss_wave, Emiss_LogTe, emiss.total.emissivity,$
				EAlam, EAall,EAChan, platescale, trespstr , countunits)
	GXResponse.all=TResp1chan.TResp
	GXResponse=add_tag(GXResponse,TResp1chan,GXResponse.Channels)

	
	;Add information about spacecraft POV for future use.
  	pb0r=pb0r_solo(date,l0=l0,/arcsec)
	RSun_arcsec=(pb0r_solo(date,/arcsec))[2] ;(arcsec/RSun)
	RSun_cm=wcs_rsun(unit='cm') 
	
	GXresponse=add_tag(GXresponse,AIA_BP_UTC2DATE_STRING(ANYTIM2UTC(date, /ccsds)),'date')
	GXresponse=add_tag(GXresponse,pix_arcsec,'pix_arcsec')	
	GXresponse=add_tag(GXresponse,pb0r[0],'p')
  	GXresponse=add_tag(GXresponse,pb0r[1],'b0')
 	GXresponse=add_tag(GXresponse,RSun_arcsec,'rsun_arcsec')
 	GXresponse=add_tag(GXresponse,l0,'l0')
 	
	return,GXresponse

end
