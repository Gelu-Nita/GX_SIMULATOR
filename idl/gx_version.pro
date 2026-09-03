;+
; NAME:
;   gx_version
;
; PURPOSE:
;   Return the GX Simulator package version from the repository VERSION file.
;   The VERSION file is kept in sync with GitHub release tags by CI.
;
; CALLING SEQUENCE:
;   ver = gx_version()
;   print, gx_version(/verbose)
;
; KEYWORDS:
;   VERBOSE - if set, return 'GX Simulator <version>' instead of bare version
;
; OUTPUTS:
;   Version string (e.g. '4.1.0'), or 'unknown' if VERSION cannot be read.
;-
function gx_version, verbose=verbose
  compile_opt idl2
  ver = 'unknown'
  version_file = gx_findfile('VERSION')
  if isa(version_file, /string) && file_test(version_file, /regular) then begin
    openr, lun, version_file, /get_lun
    line = ''
    if ~eof(lun) then readf, lun, line
    free_lun, lun
    line = strtrim(line, 2)
    if strlen(line) gt 0 then ver = line
  endif
  if keyword_set(verbose) then return, 'GX Simulator ' + ver
  return, ver
end
