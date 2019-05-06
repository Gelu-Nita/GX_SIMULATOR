function gx_box_get_file, data_dir,  field = field, inclination = inclination, azimuth = azimuth,$
  disambig=disambig, continuum=continuum, magnetogram = magnetogram, AIA_94 = AIA_94, AIA_131 = AIA_131,$
   AIA_171 = AIA_171, AIA_193 = AIA_193, AIA_211 = AIA_211, AIA_304 = AIA_304, AIA_335 = AIA_335,$
   AIA_1600 = AIA_1600, AIA_1700 = AIA_1700
  CASE 1 OF
    keyword_set(field):        tmpl = '*field.fits'
    keyword_set(inclination):  tmpl = '*.inclination.fits'
    keyword_set(azimuth):      tmpl = '*.azimuth.fits'
    keyword_set(disambig):     tmpl = '*.disambig.fits'
    keyword_set(continuum):    tmpl = '*.continuum.fits'
    keyword_set(magnetogram):  tmpl = '*.magnetogram.fits'
    keyword_set(AIA_94):       tmpl = 'aia.lev1*.94.fits'
    keyword_set(AIA_131):      tmpl = 'aia.lev1*.131.fits'
    keyword_set(AIA_171):      tmpl = 'aia.lev1*.171.fits'
    keyword_set(AIA_193):      tmpl = 'aia.lev1*.193.fits'
    keyword_set(AIA_211):      tmpl = 'aia.lev1*.211.fits'
    keyword_set(AIA_304):      tmpl = 'aia.lev1*.304.fits'
    keyword_set(AIA_335):      tmpl = 'aia.lev1*.335.fits'
    keyword_set(AIA_1600):     tmpl = 'aia.lev1*.1600.fits'
    keyword_set(AIA_1700):     tmpl = 'aia.lev1*.1700.fits'
    

  ENDCASE

  file = file_search(filepath(tmpl, root_dir = data_dir))
  return, file[0]
end