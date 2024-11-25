pro gx_setparm,info, name,value,found=found
  found=0
  if strlowcase(name) eq 'freqlist' then begin
    if size(value,/tname) eq 'STRING' then value=double(str2arr(value))
    spectrum=info.spectrum
    x=spectrum.x
    x=rep_tag_value(x,value,'axis')
    spectrum=rep_tag_value(spectrum,x,'x')
    info=rep_tag_value(info,spectrum,'spectrum')
    gx_setparm,info, 'N_freq',n_elements(value)
    gx_setparm,info, 'f_min',0
  endif 
  
  if ~((size(info,/tname) eq 'STRUCT') $
   and (size(name,/tname) eq 'STRING') $ 
   and isa(value,/number)) then begin
    message,'No valid input parameters provided, no action performed!',/info
    return
  endif
  
  if size(info,/tname) eq 'STRUCT' then begin
    idx=where(strupcase((info.parms).name) eq strupcase(name),count)
    if count eq 1 then begin
      parms=info.parms
      parms[idx].value=value
      info.parms=parms
      found=1
    endif
    if tag_exist(info,'nparms') then begin
        idx=where(strcompress(strupcase((info.nparms).name),/rem) eq strcompress(strupcase(name),/rem),count)
        if count eq 1 then begin
          nparms=info.nparms
          nparms[idx].value=value
          info.nparms=nparms
          found=1
        endif
      end
    if tag_exist(info,'rparms') then begin
        idx=where(strcompress(strupcase((info.rparms).name),/rem) eq strcompress(strupcase(name),/rem),count)
        if count eq 1 then begin
          rparms=info.rparms
          rparms[idx].value=value
          if name eq 'response_date' then rparms[idx].hint=atime(value)
          info.rparms=rparms
          found=1
        endif
    end
  end
end