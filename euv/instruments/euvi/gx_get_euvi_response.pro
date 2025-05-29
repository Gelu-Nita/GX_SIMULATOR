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
;            : added instrumental pix_arcsec and orbital rsun_arcsec values to the 
;			   GXresponse function for later use 
;            : defaulted the date input to the current time
;            ; changed time format to the same format returned by the aia_get_GXresponse.pro 
; 25-Feb-2025:  Rewrite to tie in calculation to AIA routines. TAK. I AM NOT SURE IF I HAVE THIS RIGHT
;+

 	default,date,RELTIME(/now)

;Must select either Ahead or Behind. Ahead is default
	if keyword_set(Ahead) and keyword_set(Behind) then $
		message,'Must Select EITHER Ahead or Behind, not both'
	
	if not (keyword_set(Ahead) or keyword_set(Behind)) then begin
		message,/info,'Assuming STEREO-A/EUVI'
		Ahead=1b
	endif
						
					;This is the STEREO/SECCHI: $SSW/stereo/secchi/calibration/euvi_response
	EUVI_REPONSE_DIR=get_environ('EUVI_RESPONSE')
	if keyword_set(Ahead) then begin
		usefile=concat_dir(EUVI_REPONSE_DIR,'ahead_sra_001.geny')
		pix_arcsec = 1.58777d
		Name='EUVIA'
		FullName='STEREO-A/EUVI'
	endif else begin
		usefile=concat_dir(EUVI_REPONSE_DIR,'behind_sra_001.geny')
		pix_arcsec  = 1.59000d
		Name='EUVIB'
		FullName='STEREO-B/EUVI'
	endelse
	
	;Effective area for EUVI
	restgenx,file=usefile,EffArea
	filter=1		;most commonly used filter
	EAChan='A'+trim(reform(EffArea.wavelnth[0,*]))
	NChan=n_elements(EAChan)
	EAlam=EffArea.lambda	
	
		
		;  the photon-to-photoelectron conversion factor, and the camera gain:
	h_c = 6.6262d-34 * 2.9979d8   ; h * c in Joules
	esi = 3.65 * 1.6022d-19       ; bandgap of Si = 3.65 eV * e in Coulombs
	el_dn = 15.0                  ; camera gain = 15 electrons/DN
						;in the range between 150 to 350 A confac is
						;between 1.51 and 0.64 so this is a small change 
	confac = (h_c / (EAlam*1d-10*esi*el_dn))
	
	EAAll = rearrange(reform(EffArea.area[*,filter,*]),[2,1]) * (replicate(1d0,4)#confac)
	EAUnits='cm^2 DN phot^-1'

	RA2ArcSec=!RADeg*3600d 
	platescale = (pix_arcsec/RA2ArcSec)^2
	countunits='DN'
  	
;Emissivity being used by AIA
	emiss= AIA_GET_RESPONSE(/emiss, /full) 
	Emiss_LogTe=emiss.total.LogTe
	Emiss_Wave=emiss.total.wave
	NTemp=n_elements(Emiss_LogTe)
	
;Set up output structure	
	GXResponse=str_subset(EffArea,['Buildfile','version','date'])
	GXResponse=add_tag(GXResponse,name,'Name',index=-1)
	GXResponse=add_tag(GXResponse,emiss.total.logte,'LogTe')
	GXResponse=add_tag(GXResponse,dblarr(NTemp,NChan),'All')
	GXResponse=add_tag(GXResponse,EAChan,'Channels')
	;GXResponse=add_tag(GXResponse,platescale,'platescale') ;redundant with pix_arcsec
	GXResponse=add_tag(GXResponse,'DN cm^5 s^-1 pix^-1','Units')
	GXResponse=add_tag(GXResponse,EffArea.source_data,'EffArea_Source_Data')
	GXResponse=add_tag(GXResponse,emiss.general,'Emissinfo')


	for i=0,nChan-1 do begin
	;calculate the TResp  and put is in the same formate as for AIA Temperature response
	 	trespstr={name:EAChan[i],wave:EALam,EA:EAAll[*,i], Platescale:platescale,units:EAunits}
	 	TResp1chan=  AIA_BP_MAKE_TRESP(emiss_wave, Emiss_LogTe, emiss.total.emissivity,$
	 				EAlam, EAall[i,*],EAChan[i], platescale, trespstr , countunits)
		GXResponse.all[*,i]=TResp1chan.TResp
		GXResponse=add_tag(GXResponse,TResp1chan,GXResponse.Channels[i])
	endfor

	;Add information about spacecraft POV
  pb0r=pb0r_stereo(date,/arcsec,ahead=ahead,behind=behind,l0=l0)
	RSun_arcsec=pb0r[2] ;(arcsec/RSun)
	RSun_cm=wcs_rsun(unit='cm') ;cm/RSun
	
	GXresponse=rep_tag_value(GXresponse,AIA_BP_UTC2DATE_STRING(ANYTIM2UTC(date, /ccsds)),'date')
	GXresponse=add_tag(GXresponse,pix_arcsec,'pix_arcsec')
	GXresponse=add_tag(GXresponse,pb0r[0],'p')
	GXresponse=add_tag(GXresponse,pb0r[1],'b0')
	GXresponse=add_tag(GXresponse,RSun_arcsec,'rsun_arcsec')
	GXresponse=add_tag(GXresponse,l0,'l0')
	return,GXresponse

end
