;+
; NAME:
;   gx_help_readme_target
;
; PURPOSE:
;   Resolve the best Help target for the package README: prefer the
;   GitHub-rendered page for the installed release tag, then master, then
;   the local README.md file when offline or unreachable.
;
; CALLING SEQUENCE:
;   target = gx_help_readme_target()
;
; OUTPUTS:
;   URL or local filesystem path string. Empty string if nothing found.
;-

; Return 1 if URL is reachable via sock_check; 0 on any failure/missing helper.
function gx_help_readme_url_ok, url
  compile_opt idl2, hidden
  ok = 0
  catch, err
  if err ne 0 then begin
    catch, /cancel
    return, 0
  endif
  ok = sock_check(url)
  catch, /cancel
  return, keyword_set(ok)
end

function gx_help_readme_target
  compile_opt idl2
  blob = 'https://github.com/Gelu-Nita/GX_SIMULATOR/blob/'
  master_url = blob + 'master/README.md'
  ver = gx_version()

  online = 0
  catch, err
  if err ne 0 then begin
    catch, /cancel
    online = 0
  endif else begin
    online = have_network()
    catch, /cancel
  endelse

  if online then begin
    if ver ne 'unknown' then begin
      tag = 'v' + ver
      raw = 'https://raw.githubusercontent.com/Gelu-Nita/GX_SIMULATOR/' + tag + '/README.md'
      if gx_help_readme_url_ok(raw) then return, blob + tag + '/README.md'
    endif
    raw_master = 'https://raw.githubusercontent.com/Gelu-Nita/GX_SIMULATOR/master/README.md'
    if gx_help_readme_url_ok(raw_master) then return, master_url
  endif

  readme = gx_findfile('README.md')
  if ~isa(readme, /string) || ~file_test(readme, /regular) then begin
    which, 'gx_simulator', outfile=out, /quiet
    if isa(out, /string) && strlen(out[0]) gt 0 then $
      readme = filepath('README.md', root=file_dirname(file_dirname(out[0])))
  endif
  if isa(readme, /string) && file_test(readme, /regular) then return, readme
  return, ''
end
