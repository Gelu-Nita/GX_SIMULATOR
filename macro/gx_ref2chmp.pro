;this functions attempts to interpret various reference data formats and return a map object 
;containing three map structures: Data, SDEV, and BEAM, where at least the DATA map has the restoring beam parameters included as map properties 
function gx_ref2chmp,refdata,freq=freq,chan=chan,$
                                 a_beam=a_beam,b_beam=b_beam,phi_beam=phi_beam,corr_beam=corr_beam,$
                                 data=data,sdev=sdev,err_msg=err_msg,help=help,quiet=quiet
  ;WARNING: if any of the a_beam,b_beam,phi_beam or corr_beam keywords are provided as input parameters, 
  ;they are used to replace any such parameters that the reference data might contain                               
  if keyword_set(help) or n_elements(refdata) eq 0 then begin
    err_msg=['Expected reference data formats:',$
             '1) A map object containing up to 2 relevant map structures:',$
             '   a) data=map->get(0,/map)',$
             '   b) data=map->get(0,/map)',$
             '      sdev=map->get(1,/map)',$
             '2) An array of up to 2 relevant map structures:',$
             '   a) maps[0]:  data map',$
             '   b) maps[0]:  data map',$
             '      maps[1]:  sdev map',$
             '   The DATA map must also contain the following tags:',$
             '      FREQ or CHAN    ; Frequency (GHz) or Wavelength (A)',$
             '',$
             '      BMAJ or A_BEAM  ; FWHM or Gauss Sigma major beam axis, arcsecs',$
             '      BMIN or B_BEAM  ; FWHM or Gauss Sigma minor beam axis, arcsecs',$
             '      BPA or PHI_BEAM ; Major axis polar angle in degrees,',$
             'OR',$
             '      BMAJ_BMIN_BPA   ; comma separated string list of the above beam parameters',$
             '3) An IDL structure having the following tags:',$
             '      A_BEAM          Gauss Sigma A, arcsecs',$
             '      B_BEAM          Gauss Sigma B, arcsecs',$
             '      PHI_BEAM        A-axis rotation angle, degrees',$
             '      CORR_BEAM       correction factor for A and B (optional)',$
             '      MAPS            [Data] or [Data, SDEV] array of map structures',$
             '      FREQ or CHAN    Frequency (GHz) or Wavelength (A)',$
             '      NOTE: Alternatively, FREQ or CHAN  may be tags of the Data map'$
              ]         
    goto,exit_point
  endif
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
    err_msg=!ERROR_STATE.MSG
    goto,exit_point
  END
  case size(refdata,/tname) of
    'STRUCT': ref=refdata
    'OBJREF': ref=obj_clone(refdata)  
    'STRING': begin
                if file_exist(refdata) then begin
                  sObj = OBJ_NEW('IDL_Savefile', refdata)
                  sNames = sObj->Names()
                  for k=0,n_elements(sNames)-1 do begin
                    sObj->Restore,sNames[k]
                    void=execute('ref=temporary('+sNames[k]+')')
                  endfor
                endif else error=1
               end                  
     else:      
  endcase
    case size(ref,/tname) of
      'OBJREF':begin
                if valid_map(ref) then begin
                  data=ref->get(0,/map)
                  if ref->get(/count) ge 2 then sdev=ref->get(1,/map)
                endif else error=1
               end
      'STRUCT':begin
                if valid_map(ref) then begin
                  data=ref[0]
                  if n_elements(ref) gt 1  then sdev=ref[1]
                endif else begin
                  if tag_exist(ref,'maps') then begin
                    if valid_map(ref.maps) then begin
                      data=ref.maps[0]
                      if n_elements(ref.maps) ge 1 then sdev=ref.maps[1]
                    endif else error=1
                  endif  
                  if tag_exist(ref,'a_beam') and tag_exist(ref,'b_beam') then begin
                    a_beam=n_elements(a_beam) ne 0?a_beam:ref.a_beam
                    b_beam=n_elements(a_beam) ne 0?b_beam:ref.b_beam
                    phi_beam=n_elements(phi_beam) ne 0?phi_beam:tag_exist(ref,'phi_beam')?ref.phi_beam:0.0
                    corr_beam=n_elements(corr_beam) ne 0?corr_beam:tag_exist(ref,'corr_beam')?ref.corr_beam:1
                  endif
                  if tag_exist(ref,'BMAJ') and tag_exist(ref,'BMIN')then begin
                    a_beam=n_elements(a_beam) ne 0?a_beam:gx_fwhm2sigma(ref.bmaj)
                    b_beam=n_elements(a_beam) ne 0?b_beam:gx_fwhm2sigma(ref.bmin)
                    phi_beam=n_elements(phi_beam) ne 0?phi_beam:tag_exist(ref,'BPA')?ref.bpa:0.0
                    corr_beam=n_elements(corr_beam) ne 0?corr_beam:tag_exist(ref,'corr_beam')?ref.corr_beam:1
                  endif
                  if tag_exist(ref,'freq') then freq=ref.freq
                  if tag_exist(ref,'chan') then chan=ref.chan
                endelse
              end
      else:  error=1
    endcase
 
  if keyword_set(error) then begin
    err_msg=['Unexpected reference data format:',$
      'You may use',$
      'IDL> void=gx_ref2chmp(/help)',$
      'to list all acceptable reference data formats!']
    exit_point:
    if ~keyword_set(quiet) then begin
      message,'',/info
      box_message,err_msg
    end
    return, !null   
  endif
;check existance of all required information  
if n_elements(freq) eq 0 and n_elements(chan) eq 0 then begin
    if ~tag_exist(data,'freq') then begin
      if ~tag_exist(data,'chan') then begin
        strarr=str2arr(data.id,del='\')
        if n_elements(strarr) gt 1 then chan=fix(strarr[1])
        strarr=str2arr(data.id,del='_')
        if n_elements(strarr) gt 1 then chan=fix(strarr[1])
      endif else chan=data.chan 
    endif else freq=data.freq
    if n_elements(freq) eq 0 and n_elements(chan) eq 0 then begin
       err_msg=['Required FREQ or CHAN parameters have not been found',$
                'in the provided reference data structure!',$
                'You may use',$
                'IDL> void=gx_ref2chmp(/help)',$
                'to list all acceptable reference data formats!']
       goto,exit_point         
    endif
endif
if n_elements(a_beam) eq 0 then if tag_exist(data,'a_beam') then a_beam=data.a_beam
if n_elements(a_beam) eq 0 and tag_exist(data,'BMAJ') then a_beam=gx_fwhm2sigma(ref.bmaj)
if n_elements(b_beam) eq 0 then if tag_exist(data,'b_beam') then b_beam=data.b_beam
if n_elements(b_beam) eq 0 and tag_exist(data,'BMIN') then _beaam=gx_fwhm2sigma(ref.bmin)
if n_elements(phi_beam) eq 0 then if tag_exist(data,'phi_beam') then phi_beam=data.phi_beam
if n_elements(phi_beam) eq 0 and tag_exist(data,'BPA') then phi_beam=data.bpa
if n_elements(corr_beam) eq 0 then if tag_exist(data,'corr_beam') then corr_beam=data.corr_beam else corr_beam=1
if n_elements(a_beam) eq 0 or n_elements(b_beam) eq 0 then begin
  if tag_exist(data,'BMAJ_BMIN_BPA') then begin
      bparms=float(str2arr(data.BMAJ_BMIN_BPA))
      a_beam=bparms[0]
      b_beam=bparms[1]
      phi_beam=bparms[2]
  endif
end  
if n_elements(a_beam) eq 0 or n_elements(b_beam) eq 0 then begin
  err_msg=['Required beam parameters have not been found',$
    'in the provided reference data structure!',$
    'You may use',$
    'IDL> void=gx_ref2chmp(/help)',$
    'to list all acceptable reference data formats!']
  goto,exit_point
end 
;we have what needed to generate a beam, so let's add it to the data map for future use!
add_prop,data,a_beam=a_beam,b_beam=b_beam,phi_beam=phi_beam,corr_beam=corr_beam,freq=freq,chan=chan,/replace
if ~valid_map(sdev) then begin
    sdev=data
    if tag_exist(data,'rms') then begin
      sdev.data[*,*]=data.rms
      sdev.id='SDEV '+sdev.id
    endif else err_msg=['WARNING!','No SDEV information found in the reference data,',$
                       'the reference data map was used as a SDEV placeholder!'] 
endif
;let's also generate a beam map, at least for display purposes
width=size(data.data,/dimensions)<10*max([a_beam,b_beam])
;ensure that width is odd
if width[0] mod 2 eq 0 then width[0]+=1
if width[1] mod 2 eq 0 then width[1]+=1
beam=data
add_prop,beam,data=gx_psf(corr_beam*[a_beam,b_beam]/[data.dx,data.dy],phi_beam,width),id='Restoring beam '+data.id,/rep
ref=obj_new('map')
if valid_map(data) then ref->setmap,0,data
if valid_map(sdev) then ref->setmap,1,sdev
if valid_map(beam) then ref->setmap,2,beam
if n_elements(err_msg) ne 0 and ~keyword_set(quiet) then begin
  message,'',/info
  box_message,err_msg
end
return, ref  
end
  