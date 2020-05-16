	pro tvplot, Image, x_arg, y_arg, _extra = _extra, $
sample = sample, xmargin=xmargin, ymargin=ymargin, $
fit_window = fit_window, resolution = resolution, scale = scale, $
xtick_get = xtick_get, ytick_get = ytick_get, xstyle = xstyle, ystyle = ystyle, $
xrange = xrange, yrange = yrange, missing = missing, nodata = nodata, outimage = img, $
xout = xout, yout = yout,aspect_ratio=aspect_ratio


;+
;  NAME:
; 	TVPLOT
;
;  PURPOSE:
; 	Displays an image in the brightness representation surrounded with coordinate axes like a
; 	contour plot. The functionality of the TVPLOT routine is similar to the CONTOUR routine.
;
;  CATEGORY:
; 	General graphics.
;
;  CALLING SEQUENCE:
;  	TVPLOT, Image [, X_arg, Y_arg]
;
;   INPUTS:
; 	Image: Array to be displayed. This array may be of any type.
;
;  OPTIONAL INPUT PARAMETERS:
;	X_arg: 	Argument along X axis. If supplied, then Y_arg must be supplied also.
;		The dimensions of X_arg, Y_arg must correspond to the dimensions of the Image.
;		Usage of these arguments is the same as for the CONTOUR routine.
;
;	Y_arg:  Argument along Y axis. If supplied, then X_arg must be supplied also. The
;		dimensions of X_arg, Y_arg must correspond to the dimensions of the Image.
;
;  KEYWORD PARAMETERS:
; 	Scale: 	If set and equal to zero, then the array Image is displayed as is, without scaling
; 		of brigthness (TV). Otherwise (by default), scaling is performed (TVSCL).
;
;	Resolution: Two-dimansional array or scalar integer or floating-point number specifying the
;               number of pixels
; 		to resize the Image when output to PostScript is performed. By default,
;		the Image is not resized if its least dimension exceeds 500, otherwise
;		its dimensions are resized in such a way that the least dimension becomes 500.
;		This enhances the quality of a printed image.
;
;		If many images are sent to a PostScript file, then its size can become very large
;		when all of them are resized in such a way. To prevent this, set Resize to a
;		less value.
;
;	Fit_window: If set and nonzero, then the plotting region fits displayable area similar to
;		setting xmargin = [0,0] and ymargin = [0,0]
;
;   Missing: Set this keyword to a value to be substituted for pixels in the output image that
;       map outside the input image.
;   Aspect_ratio: Set this keyword to calculate the aspect ratio from xrange and yrange
;
; 	Standard keywords: background charsize color noerase position subtitle title [xy]margin
; 		[xy]minor [xy]style [xy]thick [xy]tickformat [xy]ticklen [xy]tickname
; 		[xy]ticks [xy]tickv [xy]title [xy]range [xy]tick_get [xy]charsize
;
;
;  OPTIONAL OUTPUT KEYWORD PARAMETERS:
; 	    outimage: Output image interpolated to the plotting region.
; 	    xout, yout: Actual output arguments along X, Y axes.
;
;  COMMON BLOCKS:
; 	None.
;
;  SIDE EFFECTS:
; 	The array is displayed on the current graphics device.
;
;  RESTRICTIONS:
; 	If either of X_arg, Y_arg is supplied, then both of them must present. The dimensions of
; 	X_arg, Y_arg must correspond to the dimensions of the Image.
;
;	Neither the XRANGE nor YRANGE are not processed in the logarithmic scale.
;
; 	The image always fits the displayable region of the graphics device, and the aspect ratio
; 	is not maintained.
;
;  PROCEDURE:
; 	TV(SCL) + INTERPOLATE or CONGRID (if SAMPLE = 1, i.e., without interpolation) are used.
;       In case of PostScript device, pixels of the Image are scaled.
;
;  EXAMPLE 1. Simple display
;
;   Define an image array centered at [x0, y0] and a pixel size pix1:
;
;   x0 = -10
;   y0 = 20
;
;   image1 = shift(dist(300, 400), -x0, -y0)
;
;   Get the dimensions of the image:
;
;   Sz1 = size(image1)
;
;   pix1 = 1.2
;
;   center1 = [x0, y0]*pix1
;
;   Define X and Y arguments:
;
;   x1 = (findgen(Sz1[1])-(Sz1[1]-1)*0.5)*pix1 + center1[0]
;   y1 = (findgen(Sz1[2])-(Sz1[2]-1)*0.5)*pix1 + center1[1]
;
;   Create a graphic window to display the image:
;
;   window, 0, xsize = Sz1[1]*2, ysize = Sz1[2]*2
;
;   Display the image:
;
;   tvplot, image1, x1, y1
;
;   EXAMPLE 2. Scaling an image array (zoom in or out)
;
;   Specify a scaling factor:
;
;   factor = 0.73 (or, e.g., factor = 1.6)
;
;   Specify the X and Y ranges to be scaled:
;
;   xrange0 = [-sz1[1], sz1[1]]*0.5*pix1
;   yrange0 = [-sz	1[2], sz1[2]]*0.5*pix1
;
;   tvplot, image1, x1, y1, xrange = center1[0] + xrange0*factor, $
;     yrange = center1[1] + yrange0*factor, missing = 0
;
;   (It is useful to  specify the missing value if zoom out is performed)
;
;   EXAMPLE 3. Overlay contours of an image on top of another image displaied in TV representation:
;
;   Define the second image:
;
;   image2 = dist(200, 150)
;
;   Sz2 = size(image2)
;
;   pix2 = 1.5
;
;   center2 = [0, 0]
;
;   Define X and Y arguments for the second image:
;
;   x2 = (findgen(Sz2[1])-(Sz2[1]-1)*0.5)*pix2 + center2[0]
;   y2 = (findgen(Sz2[2])-(Sz2[2]-1)*0.5)*pix2 + center2[1]
;
;   Overlay contours:
;
;   contour, /overplot, image2, x2, y2, levels = max(image2)*[0.2, 0.5, 0.8], c_colors = 0
;
;
; MODIFICATION HISTORY:
;
;	ISTP SD RAS, 1999.
;	Victor Grechnev (grechnev@iszf.irk.ru): Initially written (TVCON).
;
;	ISTP SD RAS, Jul, 2002.
;	Natalia Meshalkina (nata@iszf.irk.ru): Help added.
;
;	ISTP SD RAS, 2005, Jan.
;	Victor Grechnev (grechnev@iszf.irk.ru):
;	Modified to use the _EXTRA keyword parameter passing technique.
;
;	ISTP SD RAS, 2009, Dec.
;	Victor Grechnev (grechnev@iszf.irk.ru):
;	The XRANGE and YRANGE parameters introduced. The routine has been
;	essentially rewritten abd renamed to TVPLOT.
;	
;	ISTP SB RAS, 2013, Jun.
; Sergey Anfinogentov (anfinogentov@iszf.irk.ru):
; The aspect_ratio parameter introduced. .
; 
; NJIT, 2020, May
; Gelu Nita (gnita@njit.edu)
; REFORM of input data applied to ensure 2-dimensuons 
; if data is provided as a 3D array slice
;-

Image=reform(Image)

Sz=size(Image)

if Sz[0] ne 2 then message, 'The argument must be 2-d array'


if n_elements(xstyle) le 0 then xstyle=!x.style
if n_elements(ystyle) le 0 then ystyle=!y.style
if n_elements(xmargin) le 0 then xmargin=!x.margin
if n_elements(ymargin) le 0 then ymargin=!y.margin
if n_elements(scale) le 0 then scale = 1

	if keyword_set(fit_window) then begin
xmar = [0,0]
ymar = [0,0]
	endif else begin
xmar = xmargin
ymar = ymargin
	endelse
	


Pmsave=!P.multi

arguments=1

	if n_elements(x_arg) le 0 then begin
x_arg = findgen(Sz[1])
arguments = 0
	endif

if n_elements(y_arg) le 0 then y_arg = findgen(Sz[2])

Xmin = min(x_arg, max = Xmax)
Ymin = min(y_arg, max = Ymax)

not_xrange = 1
not_yrange = 1

if n_elements(xrange) le 0 then xrange = [Xmin, Xmax] else not_xrange = 0
if n_elements(yrange) le 0 then yrange = [Ymin, Ymax] else not_yrange = 0

; aspect ratio--------
  if keyword_set(aspect_ratio) then begin
    
     aspect_ratio=float(xrange[1]-xrange[0])/(yrange[1]-yrange[0])
    
    x_size=!d.x_size
    y_size=!d.y_size
    
    xmar_d=xmar*float(!d.x_CH_SIZE)
    ymar_d=ymar*float(!d.y_CH_SIZE)
    
    xcsize=x_size-total(xmar_d)
    ycsize=y_size-total(ymar_d)
    
    current_aspect=xcsize/ycsize
    char_size = 1.
    if !p.CHARSIZE gt 0 then char_size = !p.charsize
    if current_aspect gt aspect_ratio then begin
       xmar_d=xmar_d+0.5*(xcsize-aspect_ratio*ycsize)
       xmar=xmar_d/(float(!d.x_CH_SIZE)*char_size)
    endif
    
    if current_aspect lt aspect_ratio then begin
       ymar_d=ymar_d+0.5*(ycsize-xcsize/aspect_ratio)
       ymar=ymar_d/(float(!d.y_CH_SIZE)*char_size)
    endif
  endif
;--end of aspect ratio----

SzN = Sz[1:2]

	if max(SzN/500.) lt 1 or n_elements(resolution) gt 0 then begin
if n_elements(resolution) le 0 then resolution = 500.
if SzN[0] gt SzN[1] then SzN = round(SzN*float(resolution[0])/SzN[0]) else SzN = round(SzN*float(resolution[0])/SzN[1])
	endif

; Define plotting region to get quantities for new arguments

plot, x_arg, y_arg, xst=5, yst=5, /nodata, xmargin=xmar, ymargin=ymar, _extra = _extra, $
	xrange = xrange, yrange = yrange

if !x.type eq 0 then X_crange = float(!x.crange) else X_crange = float(10.^!x.crange)
if !y.type eq 0 then Y_crange = float(!y.crange) else Y_crange = float(10.^!y.crange)

if X_crange[0] lt float(xrange[0]) or X_crange[1] gt float(xrange[1]) then $
	print, '% Warning: plotting X range ', X_crange[0], X_crange[1], ' exceeds specified XRANGE', xrange[0], xrange[1]

if Y_crange[0] lt float(yrange[0]) or Y_crange[1] gt float(yrange[1]) then $
	print, '% Warning: plotting Y range ', Y_crange[0], Y_crange[1], ' exceeds specified YRANGE', yrange[0], yrange[1]


if !x.type eq 1 and not_xrange eq 0 then print, '% Warning: XRANGE not supported in the log scale'
if !y.type eq 1 and not_yrange eq 0 then print, '% Warning: YRANGE not supported in the log scale'

Pmsave1 = !P.multi
!P.multi = Pmsave

	if !d.name ne 'PS' then begin

Nx_pix = round(!d.x_size*(!x.window[1]-!x.window[0]))
Ny_pix = abs(round(!d.y_size*(!y.window[1]-!y.window[0])))

	endif else begin

Nx_pix = SzN[0]
Ny_pix = SzN[1]

	endelse


xout = findgen(Nx_pix)/(Nx_pix-1)*(xrange[1] - xrange[0]) + xrange[0]
yout = findgen(Ny_pix)/(Ny_pix-1)*(yrange[1] - yrange[0]) + yrange[0]
x_arg_pix = float(xout - Xmin)/(Xmax - Xmin)*(Sz[1]-1)
y_arg_pix = float(yout - Ymin)/(Ymax - Ymin)*(Sz[2]-1)


	if keyword_set(sample) then begin

Xmin1 = min(x_arg_pix > 0 < (Sz[1]-1), max = Xmax1)
X0_1 = round(Xmin1)
Nx1 = round(Xmax1 - Xmin1)

Ymin1 = min(y_arg_pix > 0 < (Sz[2]-1), max = Ymax1)
Y0_1 = round(Ymin1)
Ny1 = round(Ymax1 - Ymin1)

img = make_array(Nx_pix, Ny_pix, type = Sz[Sz[0]+1])

if keyword_set(missing) then img[*] = missing

X00 = (where(x_arg_pix ge 0))[0]
X01 = (where(x_arg_pix gt (Sz[1]-1), count))[0]
	if count eq 0 then X01 = Nx_pix-1

y00 = (where(y_arg_pix ge 0))[0]
Y01 = (where(y_arg_pix gt (Sz[2]-1), count))[0]
	if count eq 0 then Y01 = Ny_pix-1

img[X00, Y00] = congrid(image[X0_1:X0_1+Nx1, Y0_1:Y0_1+Ny1], X01-X00+1, Y01-Y00+1)

	endif else $

img = interpolate(float(Image), float(x_arg_pix), float(y_arg_pix), /grid, cubic = -0.5, missing = missing)


if scale then routine = 'tvscl' else routine = 'tv'

if not(keyword_set(nodata)) then CALL_PROCEDURE, routine, Img, $
			xsize=!d.x_size*(!x.window[1]-!x.window[0]),	$
			ysize=!d.y_size*(!y.window[1]-!y.window[0]),	$
			!d.x_size*!x.window[0],!d.y_size*!y.window[0], /dev


	CASE arguments OF

0:	plot, xout, yout, xst = (1 or xstyle), yst = (1 or ystyle),	$
		/noerase, /nodata, xmargin = xmar, ymargin = ymar, _extra = _extra, $
			xtick_get = xtick_get, ytick_get = ytick_get, $
				xrange = xrange, yrange = yrange

1:	contour, Img, xout, yout, xst=(1 or xstyle), yst=(1 or ystyle), $
		/noerase, xmargin = xmar, ymargin = ymar, title=title, /nodata, _extra = _extra, $
			xtick_get = xtick_get, ytick_get = ytick_get, $
				xrange = xrange, yrange = yrange

	ENDCASE


!P.multi = Pmsave1



	end
