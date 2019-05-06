;; $Id: //depot/Release/ENVI52_IDL84/idl/idldir/lib/gaussian_function.pro#1 $
;;
;; Copyright (c) 2010-2014, Exelis Visual Information Solutions, Inc. All
;       rights reserved. Unauthorized reproduction is prohibited.
;+
;; Gaussian_Function
;;
;; Purpose:
;;   This function returns a Gaussian kernel
;;
;-

;;---------------------------------------------------------------------------
;; Gaussian_Function
;;
;; Purpose:
;;   Create a guassian kernel
;;
;; Parameters:
;;   SIGMA - sigma value
;;
;; Keywords:
;;   DOUBLE - If set, use double precision
;;
;;   MAXIMUM - Set this keyword to the value to be used as the maximum
;;             value of the resulting array
;;
;;   NORMALIZE - If this keyword is set the peak height shall be
;;               determined such that the total of the Gaussian is 1.0
;;
;;   WIDTH - desired width of the array
;;
FUNCTION gaussian_function, sigmaIn, DOUBLE=doubleIn, MAXIMUM=maxIn, $
                                     NORMALIZE=normalIn, WIDTH=widthIn
  compile_opt hidden, idl2
on_error, 2                                     
    
  double = KEYWORD_SET(doubleIn)
  sigma = (N_ELEMENTS(sigmaIn) eq 0) ? 1.0 : $
    FIX(sigmaIn[*] > 0.00001d, TYPE=double+4)
  maximumSet = KEYWORD_SET(maxIn)
  maximum = (N_ELEMENTS(maxIn) eq 0) ? 1.0 : FIX(maxIn[0], TYPE=double+4)
  normal = KEYWORD_SET(normalIn)
  ;; fill in width if not provided
  if (N_ELEMENTS(widthIn) eq 0) then begin
    width = CEIL(sigma*3)
    ;; ensure width is odd
    width or= 1
    width *= 2
    width++
  endif else begin
    width = widthIn[*]
  endelse

  width = FIX(width) > 1
  nSigma = N_ELEMENTS(sigma)
  if (nSigma gt 8) then begin
    message, 'Sigma can have no more than 8 elements'
    return, 0
  endif
  nWidth = N_ELEMENTS(width)
  if (nWidth gt nSigma) then begin
    message, 'Incorrect width specification'
    return, 0
  endif
  if ((nWidth eq 1) && (nSigma gt 1)) then $
    width = REPLICATE(width, nSigma)

  kernel = replicate((keyword_set(double) ? 0.0d : 0.0), width)

  ;; Fill in all 8 dimensions
  temp = intarr(8)
  temp[0] = width
  width = temp  
    
  a = (b = (c = (d = (e = (f = (g = (h = 0)))))))  
  ;; create indices
  switch nSigma of
    8 : h = (keyword_set(double) ? $
      dindgen(width[7])-width[7]/2 : $
      findgen(width[7])-width[7]/2) + $
      (width[7] and 1 ? 0 : 0.5)
    7 : g = keyword_set(double) ? $
      dindgen(width[6])-width[6]/2 : $
      findgen(width[6])-width[6]/2 + $
      (width[6] and 1 ? 0 : 0.5)
    6 : f = keyword_set(double) ? $
      dindgen(width[5])-width[5]/2 : $
      findgen(width[5])-width[5]/2 + $
      (width[5] and 1 ? 0 : 0.5)
    5 : e = keyword_set(double) ? $
      dindgen(width[4])-width[4]/2 : $
      findgen(width[4])-width[4]/2 + $
      (width[4] and 1 ? 0 : 0.5)
    4 : d = keyword_set(double) ? $
      dindgen(width[3])-width[3]/2 : $
      findgen(width[3])-width[3]/2 + $
      (width[3] and 1 ? 0 : 0.5)
    3 : c = keyword_set(double) ? $
      dindgen(width[2])-width[2]/2 : $
      findgen(width[2])-width[2]/2 + $
      (width[2] and 1 ? 0 : 0.5)
    2 : b = keyword_set(double) ? $
      dindgen(width[1])-width[1]/2 : $
      findgen(width[1])-width[1]/2 + $
      (width[1] and 1 ? 0 : 0.5)
    1 : a = keyword_set(double) ? $
      dindgen(width[0])-width[0]/2 : $
      findgen(width[0])-width[0]/2 + $
      (width[0] and 1 ? 0 : 0.5)
  endswitch
  
  ;; create kernel
  for hh=0,width[7]-1>0 do $
    for gg=0,width[6]-1>0 do $
      for ff=0,width[5]-1>0 do $
        for ee=0,width[4]-1>0 do $
          for dd=0,width[3]-1>0 do $
            for cc=0,width[2]-1>0 do $
              for bb=0,width[1]-1>0 do $
                for aa=0,width[0]-1>0 do $
                  kernel[aa,bb,cc,dd,ee,ff,gg,hh] = $
                    exp(-((a[aa]^2)/(2*sigma[[0]]^2) + $
                          (b[bb]^2)/(2*sigma[[1]]^2) + $
                          (c[cc]^2)/(2*sigma[[2]]^2) + $
                          (d[dd]^2)/(2*sigma[[3]]^2) + $
                          (e[ee]^2)/(2*sigma[[4]]^2) + $
                          (f[ff]^2)/(2*sigma[[5]]^2) + $
                          (g[gg]^2)/(2*sigma[[6]]^2) + $
                          (h[hh]^2)/(2*sigma[[7]]^2)))
  
  if (KEYWORD_SET(maximumSet)) then begin
    kernel *= maximum
  endif else begin
    if (KEYWORD_SET(normal)) then begin
      kernel /= total(kernel, /PRESERVE_TYPE)
    endif
  endelse
  
  return, kernel

end
