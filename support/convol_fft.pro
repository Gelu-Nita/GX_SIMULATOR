; $Id: //depot/Release/ENVI52_IDL84/idl/idldir/lib/convol_fft.pro#1 $
;
; Copyright (c) 2002-2014, Exelis Visual Information Solutions, Inc. All
;       rights reserved. Unauthorized reproduction is prohibited.
;----------------------------------------------------------------------------
;+
; Name:
;   CONVOL_FFT
;
; Purpose:
;   Convolution of an image using Fourier transforms for speed
;       
; Parameters:
;   IMAGE: 2D array to be convolved with the kernel
;       
;   KERNEL: 2D array (size < or = to size of image)
;
; Keywords:
;   AUTO_CORRELATION: Compute the auto correlation function of the image, 
;     which is the convolution of the image with itself. If this keyword is 
;     set, the value of KERNEL is ignored.
;                     
;   CORRELATE: Use the Fourier transform of the kernel to compute the 
;     cross-correlation of the image and the kernel.
;
;   IMAGE_FFT: Set this keyword to the Fourier transform of the image, as 
;     returned in a previous call to CONVOL_FFT. If IMAGE_FFT is set to a 
;     named variable, the Fourier transform of the image will be returned, so 
;     that it can be used in future calculations.
;     
;   KERNEL_FFT: Set this keyword to the Fourier transform of the kernel, as 
;     returned in a previous call to CONVOL_FFT. If KERNEL_FFT is set to a 
;     named variable, the Fourier transform of the kernel will be returned, so 
;     that it can be used in future calculations.
;     
;   NO_PADDING: Set this keyword to prevent padding of the image with zeros.
;     This will decrease the memory usage of the function, but may introduce 
;     artifacts at the edges of the result.
;     
;-
function convol_fft, image, kernel, $
                     AUTO_CORRELATION=auto, CORRELATE=corr, NO_PADDING=noPad, $
                     IMAGE_FFT=imageFFT, KERNEL_FFT=kernelFFT
  compile_opt idl2
  
on_error, 2  
  catch, err
  if (err ne 0) then begin
    catch, /cancel
    message, /REISSUE_LAST
    return, -1
  endif
  
  imageNDims = size(image, /N_DIMENSIONS)
  if (imageNDims ne 2) then begin
    message, 'Image must be a 2 dimensional array'
    return, -1
  endif

  kernelNDims = size(kernel, /N_DIMENSIONS)
  if ((kernelNDims ne 2) && ~KEYWORD_SET(auto)) then begin
    message, 'Kernel must be a 2 dimensional array or AUTO_CORRELATION must be set'
    return, -1
  endif 

  imageDims = size(image, /DIMENSIONS)
  kernelDims = size(kernel, /DIMENSIONS)
  imageNElts = N_ELEMENTS(image)
  
  if KEYWORD_SET(noPad) then begin
    ; Create image fft if needed
    imageSz = size(imageFFT, /STRUCTURE)
    if ((imageSz.N_DIMENSIONS ne 2) || $
        ((imageSz.TYPE ne 6) && (imageSz.TYPE ne 9)) || $
        MAX(imageSz.DIMENSIONS[0:1] ne imageDims[0])) then $
          imageFFT = FFT(image, -1)
  
    imageDims2 = imageDims/2
      
    if KEYWORD_SET(auto) then $
      return, SHIFT(imageNElts * REAL_PART(FFT(imageFFT * CONJ(imageFFT),1)), $
        imageDims2[0], imageDims2[1])
      
    ; Create kernel fft if needed
    kernelSz = size(kernelFFT, /STRUCTURE)
    if ((kernelSz.N_DIMENSIONS ne 2) || $
        ((kernelSz.TYPE ne 6) && (kernelSz.TYPE ne 9)) || $
        MAX(kernelSz.DIMENSIONS[0:1] ne kernelDims[0])) then begin
      kernelDims2 = kernelDims/2
      loc = (imageDims2 - kernelDims2) > 0 ;Center kernel
      smallDim = (kernelDims2 - imageDims2) > 0
      largeDim = (smallDim + imageDims-1) < (kernelDims-1)
      kernelFFT = CONJ(image)*0
      kernelFFT[loc[0], loc[1]] = $
        kernel[smallDim[0]:largeDim[0], smallDim[1]:largeDim[1]]
      kernelFFT = FFT(kernelFFT, -1, /OVERWRITE)
    endif

    if KEYWORD_SET(corr) then begin
      conv = imageNElts*REAL_PART(FFT(imageFFT*CONJ(kernelFFT), 1))
    endif else begin
      conv = imageNElts*REAL_PART(FFT(imageFFT*kernelFFT, 1))
    endelse
    
    imageDims2 = imageDims2 + (imageDims MOD 2) ;Shift odd size images
    
    return, SHIFT(conv, imageDims2[0], imageDims2[1])
  endif else begin
    imageDims2 = FLOOR((imageDims-1)/2)
    imageNElts = N_ELEMENTS(image)*4. ;Create space for padding
    
    imageSz = size(imageFFT, /STRUCTURE)
    if ((imageSz.N_DIMENSIONS ne 2) || $
        ((imageSz.TYPE ne 6) && (imageSz.TYPE ne 9)) || $
        MAX(imageSz.DIMENSIONS[0:1] ne imageDims[0]*2)) then begin
      ; Double the image size and pad with zeros
      bigImage = dblarr(imageDims[0]*2,imageDims[1]*2)
      bigImage[0:imageDims[0]-1,0:imageDims[1]-1] = image
      imageFFT = FFT(bigImage,-1)
      imageNElts = N_ELEMENTS(bigImage)
    endif
    
    if (KEYWORD_SET(auto)) then begin
      tempImage = SHIFT(imageNElts*REAL_PART(FFT(imageFFT*CONJ(imageFFT),1)), $
        imageDims2[0],imageDims2[1])
      return, tempImage[0:imageDims[0]-1,0:imageDims[1]-1]
    endif
    
    kernelSz = size(kernelFFT, /STRUCTURE)
    if ((kernelSz.N_DIMENSIONS ne 2) || $
        ((kernelSz.TYPE ne 6) && (kernelSz.TYPE ne 9)) || $
        MAX(kernelSz.DIMENSIONS[0:1] ne imageDims[0]*2)) then begin
      loc = (imageDims2 - floor((kernelDims-1)/2)) > 0
      kernelTemp = dblarr(imageDims[0]*2,imageDims[1]*2)
      kernelTemp[loc[0]:loc[0]+kernelDims[0]-1, $
                 loc[1]:loc[1]+kernelDims[1]-1] = kernel
      kernelFFT = FFT(kernelTemp, -1)
    endif
    
    if (KEYWORD_SET(corr)) then begin
      conv = imageNElts*REAL_PART(FFT(imageFFT*CONJ(kernelFFT), 1))
      conv = SHIFT(conv, imageDims2[0], imageDims2[1])
    endif else begin
      conv = imageNElts*REAL_PART(FFT(imageFFT*kernelFFT, 1))
      conv = SHIFT(conv, -imageDims2[0], -imageDims2[1])
    endelse
    
    return, conv[0:imageDims[0]-1,0:imageDims[1]-1]
  endelse

  ; If we get here, something went very wrong
  return, -1
  
end
