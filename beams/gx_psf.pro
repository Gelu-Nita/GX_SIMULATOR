function gx_psf,sigmaIn,phi,widthIn,maxwidthIn=maxwidthIn
  ;sigmaIn=[a,b]/[dx,dy]
  sigma=(n_elements(sigmaIn) eq 1)?replicate(sigmaIn,2):sigmaIn[0:1];ignore extra dimmensions
  if (N_ELEMENTS(widthIn) eq 0) then begin
    width = CEIL(sigma*3)
    ;; ensure width is odd
    width or= 1
    width *= 2
    width++
  endif else begin
    width = widthIn
    width or= 1
  endelse

  if (n_elements(width) eq 1) then width = REPLICATE(width, 2) else width=width[0:1]
  width = LONG(width) > 1
  maxwidth=n_elements(maxwidthIn) gt 0? min([maxwidthIn,width]):max(width)
  maxwidth or=1 ;; ensure width is odd
  width=width<(maxwidth-2);;force PSF to be square and less than maxwidth while remaining odd
   
  x=(findgen(width[0])-(width[0]-1)/2d)
  y=(findgen(width[1])-(width[1]-1)/2d)
  Gauss2Drot, [x,y], [sigma[0],sigma[1],-phi*!dtor], psf,/noreform
  psf/=total(psf,/double)
  return,psf
end