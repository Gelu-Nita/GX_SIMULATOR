;+
; NAME:
;   gx_open_browser
;
; PURPOSE:
;   Open a local file or URL in a web browser (not the OS file-type handler).
;   On macOS, plain `open` / `open -u` for a .md file can launch a markdown
;   editor (e.g. MacDown); local files are therefore opened with Safari.
;
; CALLING SEQUENCE:
;   gx_open_browser, path_or_url
;
; INPUTS:
;   path_or_url - Existing local file path, or http(s)/file URL string.
;-
pro gx_open_browser, path_or_url
  compile_opt idl2
  if n_elements(path_or_url) eq 0 then return
  target = strtrim(path_or_url[0], 2)
  if strlen(target) eq 0 then return

  local_file = ''
  if file_test(target, /regular) then begin
    local_file = file_expand_path(target)
    path = local_file
    if !version.os_family eq 'Windows' then begin
      path = strjoin(strsplit(path, '\', /extract, /preserve_null), '/')
      if stregex(path, '^[A-Za-z]:', /boolean) then path = '/' + path
    endif
    path = strjoin(strsplit(path, ' ', /extract, /preserve_null), '%20')
    target = 'file://' + path
  endif

  case 1 of
    !version.os_family eq 'Windows': spawn, 'cmd /c start "" "' + target + '"', /hide
    !version.os eq 'darwin': begin
      if strlen(local_file) gt 0 then begin
        ; Safari is always present; avoids Launch Services sending .md to MacDown.
        spawn, 'open -a Safari "' + local_file + '"'
      endif else begin
        spawn, 'open -u "' + target + '"'
      endelse
    end
    else: spawn, 'xdg-open "' + target + '" &'
  endcase
end
