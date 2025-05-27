;this routine interchanges the top and bottom colors in the current color table or the table supplied by the user
;Modification history
; 06/12/23 created by gelu@njit
; 05/17/25 GN: made sure that black and white are always black and white
pro gx_rgb_white2black,rgb
tvlct, rgb,/get
loadct,0,/silent
tvlct,rgb0,/get
rgb_white=rgb0[255,*,*]
rgb_black=rgb0[0,*,*]
rgb[0,*,*]=rgb_white
rgb[255,*,*]=rgb_black
tvlct,rgb
end