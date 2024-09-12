;+
; NAME:
;    gx_remove_border
;
; PURPOSE:
;    This function removes white borders from an image and crops it down to the area containing non-white pixels.
;    It works with RGB images by first converting them to grayscale for detecting white borders. The user can
;    optionally use a more exact method to detect purely white pixels by setting the `exact` keyword.
;
; CATEGORY:
;    Image Processing
;
; CALLING SEQUENCE:
;    cropped_image = gx_remove_border(image, maxsize=maxsize, margin=margin, /exact)
;
; INPUTS:
;    image: A 3D byte array representing the RGB image to be processed.
;
; OPTIONAL KEYWORD PARAMETERS:
;    maxsize: An optional keyword that specifies the maximum size for the cropped image (default is 690).
;    margin: An optional keyword that specifies the number of pixels to be included as margin around the cropped image (default is 5).
;    exact:   If set, the function uses an exact method to detect white pixels (where R, G, and B channels are exactly 255).
;             Otherwise, it uses a faster grayscale approximation.
;
; OUTPUTS:
;    Returns a cropped version of the input image, trimmed of white borders and optionally padded with a margin.
;
; SIDE EFFECTS:
;    If no non-white pixels are found, the function prints a message and returns the original image.
;
; EXAMPLE:
;    cropped_image = gx_remove_border(image, maxsize=500, margin=10, /exact)
;    ; This will crop the input image to remove white borders, limit the size to 500x500 pixels, add a 10-pixel margin,
;    ; and use the exact method to detect white pixels.
;
; MODIFICATION HISTORY:
;    Written by Gelu M Nita, Sep-10-2024.
;-

FUNCTION gx_remove_border, image, maxsize=maxsize, margin=margin, exact=exact
  ; Get the dimensions of the input image
  sz = SIZE(image, /DIMENSIONS)
  num_channels = sz[0]  ; Number of channels (should be 3 for RGB)
  width = sz[1]         ; Image width
  height = sz[2]        ; Image height

  ; Split the image into RGB channels
  R = reform(image[0, *, *])
  G = reform(image[1, *, *])
  B = reform(image[2, *, *])

  ; Use the exact method for detecting white pixels if the /exact keyword is set
  IF KEYWORD_SET(exact) THEN BEGIN
    ; Exact method: white is defined as R=255, G=255, B=255
    white_mask = R*0
    white_mask[WHERE(R EQ 255 AND G EQ 255 AND B EQ 255)] = 1
  ENDIF ELSE BEGIN
    ; Faster method: convert the image to grayscale and assume pixels close to 255 are white
    grayscale = BYTE(0.299 * R + 0.587 * G + 0.114 * B)
    white_mask = grayscale GT 250  ; White pixels close to 255 are considered as white
  ENDELSE

  ; Find the non-white pixels (i.e., pixels that are not in the white mask)
  black_pixels = WHERE(white_mask EQ 0, count)

  ; If no non-white pixels are found, return the original image
  IF count EQ 0 THEN BEGIN
    PRINT, 'No non-white pixels found.'
    RETURN, image
  ENDIF

  ; Get the indices of non-white pixels
  idx = ARRAY_INDICES(white_mask, black_pixels)

  ; Determine the bounding box coordinates (x1, x2, y1, y2) around the non-white pixels
  x1 = MIN(idx[0, *], max=x2)
  y1 = MIN(idx[1, *], max=y2)

  ; Apply a maximum size constraint to the cropped area
  DEFAULT, maxsize, 690  ; Default max size is 690 pixels
  max_dim = MAX([x2 - x1 + 1, y2 - y1 + 1])
  x2 += (maxsize - max_dim)  ; Adjust x2 to fit the maxsize
  y2 += (maxsize - max_dim)  ; Adjust y2 to fit the maxsize

  ; Optionally add margins around the bounding box
  DEFAULT, margin, 5  ; Default margin is 5 pixels
  x1 = MAX([0, x1 - margin])
  x2 = MIN([width-1, x2 + margin])
  y1 = MAX([0, y1 - margin])
  y2 = MIN([height-1, y2 + margin])

  ; Crop the RGB channels to the bounding box
  cropped_R = R[x1:x2, y1:y2]
  cropped_G = G[x1:x2, y1:y2]
  cropped_B = B[x1:x2, y1:y2]

  ; Combine the cropped channels back into a 3D array
  cropped_image = BYTARR(3, x2 - x1 + 1, y2 - y1 + 1)
  cropped_image[0, *, *] = cropped_R
  cropped_image[1, *, *] = cropped_G
  cropped_image[2, *, *] = cropped_B

  ; Return the cropped image
  RETURN, cropped_image
END
