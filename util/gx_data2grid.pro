;+
; NAME:
;    gx_data2grid
;
; PURPOSE:
;    This procedure takes a two-dimensional array "data" and two one-dimensional coordinate arrays "x" and "y"
;    (not necessarily uniformly spaced) and returns a two-dimensional array "gridded_data", as well as uniformly
;    gridded arrays "x_grid" and "y_grid". The procedure also returns the uniform spacings "dx" and "dy", which
;    are determined to represent the smallest distances between the elements of the original "x" and "y" arrays.
;
;    By default, "gridded_data" is filled with zeros where no values are found in the input data. If the /NAN keyword
;    is set, the "gridded_data" is filled with NaNs instead.
;
; INPUTS:
;    x       - One-dimensional array of x coordinates (not necessarily uniformly spaced).
;    y       - One-dimensional array of y coordinates (not necessarily uniformly spaced).
;    data    - Two-dimensional array of data values corresponding to the (x, y) coordinates.
;
; OUTPUTS (Keyword):
;    gridded_data - A two-dimensional array with uniformly spaced data values.
;    x_grid       - A one-dimensional array of uniformly spaced x values.
;    y_grid       - A one-dimensional array of uniformly spaced y values.
;    dx           - A scalar representing the uniform spacing in the x direction.
;    dy           - A scalar representing the uniform spacing in the y direction.
;
; KEYWORDS:
;    /NAN - If set, fills the "gridded_data" array with NaNs where no data values are available.
;
; EXAMPLE USAGE:
;    gx_data2grid, x, y, data, gridded_data=grid_data, x_grid=xg, y_grid=yg, dx=dx, dy=dy, /nan
;
; REVISIONS:
;    Gelu M Nita - 08/31/24 - Initial version.
;
;-
PRO gx_data2grid, x, y, data, gridded_data=gridded_data, x_grid=x_grid, y_grid=y_grid, dx=dx, dy=dy, NAN=nan_flag

  ; Step 1: Sort x and y in ascending order
  x_sorted = x[SORT(x)]
  y_sorted = y[SORT(y)]

  ; Step 2: Determine the smallest intervals dx and dy
  dx = MIN(ABS(x_sorted - SHIFT(x_sorted, 1)))
  dy = MIN(ABS(y_sorted - SHIFT(y_sorted, 1)))

  ; Step 3: Create the x_grid and y_grid arrays with dx and dy spacing
  x_min = MIN(x_sorted)
  x_max = MAX(x_sorted)
  y_min = MIN(y_sorted)
  y_max = MAX(y_sorted)

  x_grid = DINDGEN(FIX((x_max - x_min) / dx) + 1) * dx + x_min
  y_grid = DINDGEN(FIX((y_max - y_min) / dy) + 1) * dy + y_min

  ; Step 4: Initialize the gridded_data array
  nx = N_ELEMENTS(x_grid)
  ny = N_ELEMENTS(y_grid)
  IF KEYWORD_SET(nan_flag) THEN BEGIN
    gridded_data = DBLARR(nx, ny) + !VALUES.F_NAN
  ENDIF ELSE BEGIN
    gridded_data = DBLARR(nx, ny)
  ENDELSE

  ; Step 5: Fill the gridded_data array with input data
   for i=0, nx-1 do begin
    for j=0, ny-1 do begin
      ai=where(x_grid[i] eq x,ca)
      bi=where(y_grid[j] eq y,cb)
      if ca*cb ne 0 then gridded_data[i,j]=data[ai,bi]
    endfor
  endfor

END
