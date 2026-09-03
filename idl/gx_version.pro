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
  lun = -1L
  ; Resolve package root from this routine's location (idl/../VERSION) so
  ; /version works without SSW helpers such as which/default/gx_findfile.
  this = routine_filepath('gx_version', /either)
  if strlen(this) eq 0 then goto, done
  version_file = filepath('VERSION', root=file_dirname(file_dirname(this)))
  if ~file_test(version_file, /regular) then goto, done

  catch, err
  if err ne 0 then begin
    catch, /cancel
    if lun ge 0 then begin
      free_lun, lun
      lun = -1L
    endif
    goto, done
  endif

  openr, lun, version_file, /get_lun
  line = ''
  if ~eof(lun) then readf, lun, line
  free_lun, lun
  lun = -1L
  catch, /cancel
  line = strtrim(line, 2)
  if strlen(line) gt 0 then ver = line

  done:
  if keyword_set(verbose) then return, 'GX Simulator ' + ver
  return, ver
end
