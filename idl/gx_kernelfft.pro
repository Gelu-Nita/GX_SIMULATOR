function gx_kernelfft,imageDims,kernel
  kernelDims = size(kernel, /DIMENSIONS)
  imageDims2 = imageDims/2
  loc = (imageDims2 - floor((kernelDims-1)/2)) > 0
  kernelTemp = size(kernel, /n_dim) eq 2 ? dblarr(imageDims[0]*2,imageDims[1]*2):dblarr(imageDims[0]*2,imageDims[1]*2,kernelDims[2])
  kernelTemp[loc[0]:loc[0]+kernelDims[0]-1, $
    loc[1]:loc[1]+kernelDims[1]-1,*] = kernel
  kernelFFT = FFT(kernelTemp, -1)
  return,kernelFFT 
end