pro gx_setfonts,font=font
  defsysv,'!DEFAULTS',EXISTS=exists
  if not exists then gx_defparms
  if n_elements(font) eq 0 then begin
    device, get_screen_size=scr
    if !version.os_family eq 'Windows' then begin
      if scr[0] lt 3200 then !defaults.font='lucida console*12' else !defaults.font='lucida console*24'
    endif else begin
      if scr[0] lt 3200 then !defaults.font='' else !defaults.font=''
    endelse
  endif else !defaults.font=font
  font=!defaults.font
  Widget_Control, DEFAULT_FONT=!defaults.font
end  