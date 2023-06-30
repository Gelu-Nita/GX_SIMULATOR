;this routine interchanges the top and bottom colors in the current color table or the table supplied by the user
;Modification history
; 06/12/23 created by gelu@njit
pro gx_rgb_white2black,rgb
tvlct, rgb,/get
rgb_white=rgb[255,*,*]
rgb_black=rgb[0,*,*]
rgb[0,*,*]=rgb_white
rgb[255,*,*]=rgb_black
tvlct,rgb
end