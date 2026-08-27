;+
; List .sav / FITS files in a directory (deduplicated, sorted).
;-
function gx_ref2chmp_list_ref_files, dir, count=count
  patterns = ['*.sav', '*.fits', '*.fts', '*.fit', '*.SAV', '*.FITS', '*.FTS', '*.FIT']
  files = !null
  foreach pat, patterns do begin
    f = file_search(dir, pat, count=c)
    if c gt 0 then begin
      if n_elements(files) eq 0 then files = f else files = [files, f]
    endif
  endforeach
  if n_elements(files) eq 0 then begin
    count = 0
    return, !null
  endif
  ; Deduplicate (case-insensitive FS may match twice)
  files = files[uniq(files, sort(files))]
  count = n_elements(files)
  return, files
end
