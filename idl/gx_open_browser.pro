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

; POSIX single-quoted shell argument (safe for embedded " and spaces).
function gx_open_browser_sh_quote, s
  compile_opt idl2, hidden
  parts = strsplit(s, "'", /extract, /preserve_null)
  return, "'" + strjoin(parts, "'\''") + "'"
end

; Windows cmd.exe double-quoted argument (embedded " -> "").
function gx_open_browser_cmd_quote, s
  compile_opt idl2, hidden
  parts = strsplit(s, '"', /extract, /preserve_null)
  return, '"' + strjoin(parts, '""') + '"'
end

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
    path = strjoin(strsplit(path, '"', /extract, /preserve_null), '%22')
    target = 'file://' + path
  endif else begin
    ; Encode stray quotes in URLs so they cannot break the shell command line.
    target = strjoin(strsplit(target, '"', /extract, /preserve_null), '%22')
  endelse

  case 1 of
    !version.os_family eq 'Windows': $
      spawn, 'cmd /c start "" ' + gx_open_browser_cmd_quote(target), /hide
    !version.os eq 'darwin': begin
      if strlen(local_file) gt 0 then begin
        ; Safari is always present; avoids Launch Services sending .md to MacDown.
        spawn, 'open -a Safari ' + gx_open_browser_sh_quote(local_file)
      endif else begin
        spawn, 'open -u ' + gx_open_browser_sh_quote(target)
      endelse
    end
    else: spawn, 'xdg-open ' + gx_open_browser_sh_quote(target) + ' &'
  endcase
end
