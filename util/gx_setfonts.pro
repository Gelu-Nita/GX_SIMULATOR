pro gx_setfonts,font=font
  defsysv,'!DEFAULTS',EXISTS=exists
  if not exists then gx_defparms
  if n_elements(font) eq 0 then begin
    if !version.os_family eq 'Windows' then begin
      dname=!d.name
      set_plot,'win'
      device, get_screen_size=scr
      if scr[0] lt 3200 then !defaults.font='lucida console*12' else !defaults.font='lucida console*24'
      set_plot,dname
    endif else !defaults.font=''
  endif else !defaults.font=font
  font=!defaults.font
  Widget_Control, DEFAULT_FONT=!defaults.font
end  