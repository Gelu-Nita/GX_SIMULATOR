function gx_get_aia_response,date,TResp=TResp,evenorm=evenorm,chiantifix=chiantifix,$
		responsefile=responsefile
;+
; Project     : GX Simulator
;
; Name        : get_aia_response
;
; Purpose     : Calculate SDO/AIA temperature response.
;
; Syntax      : resp=gx_get_aia_response(date)
;
; Inputs:
;				date - date of observations
;
; Inputs  keywords:
;				TResp - use AIA temperature response function. This is the default.
;					In the future it may be possible to use response derived from pre-calculated spectra
;					convolved with instrument response as a function of wavelength
;				responsefile - name of an IDL .sav file containing an AIA temperature response.
;					The file must contain a structure named GXResponse or tresp, in the format
;					returned by aia_get_response(/temp,/dn). If date is supplied and the file
;					has TIME_APPLIED=YES together with a DATE, that DATE is the epoch already
;					in the curves and the time factor is EA(date)/EA(file DATE). If the file
;					has no DATE, or time is not yet applied, the curves are the undegraded
;					response and the factor is EA(date)/EA(24 Mar 2010). If /evenorm is set,
;					the SSW EVE normalization AIA_OVER_EVE is applied as well. Both factors
;					are one scale per channel. /chiantifix is not applied to an external file:
;					that correction is an empirical patch for the SSW emissivity, which
;					the file replaces.
;				chiantifix, evenorm - inputs to aia_get_response. See that routine for documentation.
;
; Outputs     :  structure containing response and related quantities.
;
;
; Calls: 	PB0R, AIA_GET_RESPONSE
; 			REQUIRES AIA SSW PACKAGE
;
; History     : Written, 15-Nov-2024  T.A. Kucera
; 13-Dec-2024: exposed the evenorm and chiantifix keywords, for compatibility with GX Simulator
;            : replaced the hardwired RSun_cm by the call to wcs_rsun function,
;              which in the GX Simulator enviroment is 6.96d10 cm, as per the HMI team convention
;            : added instrumental pix_arcsec and orbital rsun_arcsec values to the response
;				function for later use
;            : defaulted the date input to the current time, if not provided unless the /Tresp is used
;              changed time format to the same format returned by the aia_get_response.pro
; 24-Feb-2025: effective area- based calculation.
; 11-Jul-2025: small changes
; 13-Oct-2026: added option of using external temperature response file.
; 14-Oct-2026: apply SSW time degradation and EVE normalization to an external response when requested.
; 16-Oct-2026: when an external file already records its effective-area date, measure the
;              time factor from that date. The 24 Mar 2010 baseline remains for a file with no date.
;+

pix_arcsec=0.6d
pb0r=pb0r(date,/arcsec,l0=l0)
RSun_arcsec=(pb0r(date,/arcsec))[2]

default,TResp,1  ;this is currently being assumed so the actual variable is not being used.

use_file=n_elements(responsefile) gt 0 && strtrim(responsefile[0],2) ne ''
if use_file then begin
	use_eve=keyword_set(evenorm)
	use_chianti=keyword_set(chiantifix)
	use_time=n_elements(date) gt 0
	obs_date=use_time ? date : ''
	resp_path=strtrim(responsefile[0],2)
	if ~file_test(resp_path) then message,'AIA response file not found: '+resp_path
	restore,resp_path
	if size(gxresponse,/type) ne 8 then begin
		if size(tresp,/type) eq 8 then GXResponse=tresp else $
			message,'AIA response file must contain a structure named GXResponse or tresp: '+resp_path
	endif
	if use_chianti then message,/info,'CHIANTIfix is not applied to an external AIA response. The file replaces the SSW emissivity that this correction adjusts.'
	; TIME_APPLIED=YES and a non-blank DATE mean the curves already include the
	; effective area at that date. A missing date, or time not yet applied, means
	; the undegraded response, whose baseline is 24 Mar 2010.
	file_epoch=''
	time_in_file=0
	if tag_exist(GXResponse,'CORRECTIONS') then begin
		if tag_exist(GXResponse.corrections,'TIME_APPLIED') then $
			time_in_file=strupcase(strtrim(GXResponse.corrections.time_applied,2)) eq 'YES'
	endif
	if time_in_file && tag_exist(GXResponse,'DATE') then file_epoch=strtrim(GXResponse.date[0],2)
	if time_in_file && file_epoch eq '' then message,/info,'External AIA response marks time as applied but has no DATE. The time factor is measured from 24 Mar 2010.'
	if file_epoch ne '' && strlen(file_epoch) ge 15 && strmid(file_epoch,8,1) eq '_' then $
		file_epoch=strmid(file_epoch,0,4)+'-'+strmid(file_epoch,4,2)+'-'+strmid(file_epoch,6,2)+' '+$
			strmid(file_epoch,9,2)+':'+strmid(file_epoch,11,2)+':'+strmid(file_epoch,13,2)
	if use_eve || use_time then begin
		; evenorm=0 is explicit so a timedepend request does not stop for the SSW EVE prompt.
		; The correction ratios are returned whether or not they are applied to the reference area.
		if use_time then ref=aia_get_response(/area,/dn,evenorm=0,timedepend_date=obs_date,/silent) $
			else ref=aia_get_response(/area,/dn,evenorm=0,/silent)
		file_ref=ref
		if use_time && file_epoch ne '' then $
			file_ref=aia_get_response(/area,/dn,evenorm=0,timedepend_date=file_epoch,/silent)
		resp_chan=strupcase(strtrim(GXResponse.channels,2))
		time_chan=strupcase(strtrim(ref.corrections.time.channels,2))
		file_chan=strupcase(strtrim(file_ref.corrections.time.channels,2))
		eve_chan=strupcase(strtrim(ref.corrections.evenorm.channels,2))
		nch=n_elements(resp_chan)
		scale=dblarr(nch)+1d
		applied_time=dblarr(n_elements(time_chan))+1d
		if use_time then applied_time=ref.corrections.time.ea_sample_over_t0
		for i=0,nch-1 do begin
			it=where(time_chan eq resp_chan[i],n_time)
			ie=where(eve_chan eq resp_chan[i],n_eve)
			if n_time ne 1 || n_eve ne 1 then message,'No SSW correction factor for AIA channel '+resp_chan[i]
			if use_time then begin
				t_scale=ref.corrections.time.ea_sample_over_t0[it[0]]
				if file_epoch ne '' then begin
					itf=where(file_chan eq resp_chan[i],n_file)
					if n_file ne 1 then message,'No SSW time factor at the file date for AIA channel '+resp_chan[i]
					t_file=file_ref.corrections.time.ea_sample_over_t0[itf[0]]
					if t_file eq 0 then message,'SSW time factor at the file date is zero for AIA channel '+resp_chan[i]
					t_scale=t_scale/t_file
				endif
				applied_time[it[0]]=t_scale
				scale[i]*=t_scale
			endif
			if use_eve then scale[i]*=ref.corrections.evenorm.aia_over_eve[ie[0]]
			GXResponse.all[*,i]=GXResponse.all[*,i]*scale[i]
			chan_id=where(tag_names(GXResponse) eq resp_chan[i],nchan_tag)
			if nchan_tag eq 1 then begin
				chan=GXResponse.(chan_id[0])
				if tag_exist(chan,'TRESP') then begin
					chan.tresp=chan.tresp*scale[i]
					GXResponse.(chan_id[0])=chan
				endif
			endif
		endfor
		corr=ref.corrections
		if use_time && file_epoch ne '' then corr.time.ea_sample_over_t0=applied_time
		corr.time_applied=use_time ? 'YES' : 'NO'
		corr.evenorm_applied=use_eve ? 'YES' : 'NO'
		; The restored corrections structure can differ in string length from the
		; current SSW structure, so replace the tag instead of assigning it.
		if tag_exist(GXResponse,'CORRECTIONS') then GXResponse=rem_tag(GXResponse,'corrections')
		GXResponse=add_tag(GXResponse,corr,'corrections')
		if tag_exist(GXResponse,'DATE') then GXResponse.date=ref.date else $
			GXResponse=add_tag(GXResponse,ref.date,'date')
	endif
endif else GXResponse=aia_get_response(/temp,/dn,evenorm=evenorm,chiantifix=chiantifix,timedepend=date)

GXResponse=add_tag(GXResponse,pix_arcsec,'pix_arcsec')
GXresponse=add_tag(GXresponse,pb0r[0],'p')
GXresponse=add_tag(GXresponse,pb0r[1],'b0')
GXresponse=add_tag(GXresponse,RSun_arcsec,'rsun_arcsec')
GXresponse=add_tag(GXresponse,l0,'l0')
return,GXResponse
end
