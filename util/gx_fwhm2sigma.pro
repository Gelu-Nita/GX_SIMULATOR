function gx_fwhm2sigma, fwhm,semi=semi
default, fwhm,1d
sigma=fwhm/(2*sqrt(2*alog(2)))
return,keyword_set(semi)?2*sigma:sigma
end