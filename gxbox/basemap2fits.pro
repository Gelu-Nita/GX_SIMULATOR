;+
  ; :Description:
  ;    Saves the result of the PREPARE_BASEMAPS function into fits files.
  ;
  ; :Params:
  ;    basemap - Structure ruturned by the PREPARE_BASEMAPS function
  ;    name_out - String identifying the output files.
  ;              E.g., if it is specified as name_out='AR001',
  ;              the procedure appends the suffixes and extensions and writes
  ;              the files
  ;              'AR001.Br.fits' - the radial component,
  ;              'AR001.Bt.fits' - the latitudinal component,
  ;              'AR001.Bp.fits' - the longitudinal component,
  ;              'AR001.Ic.fits' - continuum intensity.
  ;
  ;
  ;
  ; :Author: Sergey Anfinogentov  (anfinogentov@iszf.irk.ru)
  ;-
pro basemap2fits, basemaps, name_out
  
  file_Bp = name_out + '.Bp.fits'
  file_Bt = name_out + '.Bt.fits'
  file_Br = name_out + '.Br.fits'
  file_IC = name_out + '.Ic.fits'
  
  hdr = wcs2fitshead(basemaps.wcs)
  
  writefits, file_bp, basemaps.bp, hdr
  writefits, file_bt, basemaps.bt, hdr
  writefits, file_br, basemaps.br, hdr
  writefits, file_Ic, basemaps.ic, hdr


end