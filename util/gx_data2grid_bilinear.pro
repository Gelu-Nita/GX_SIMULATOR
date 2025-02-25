PRO gx_data2grid_bilinear, data, x, y, gridded_data, x_grid, y_grid, dx, dy, NAN=use_nan
  ;+
  ; NAME:
  ;   gx_data2grid_bilinear
  ;
  ; PURPOSE:
  ;   Maps irregular grid data onto a uniform grid using bilinear interpolation.
  ;   Generates uniformly spaced grid arrays from irregular coordinate arrays
  ;   and fills in missing data using bilinear interpolation.
  ;
  ; CATEGORY:
  ;   Data gridding and interpolation
  ;
  ; CALLING SEQUENCE:
  ;   gx_data2grid_bilinear, data, x, y, gridded_data, x_grid, y_grid, dx, dy, [NAN=use_nan]
  ;
  ; INPUTS:
  ;   data - 2D array of values corresponding to input x and y coordinates.
  ;   x    - 1D array of x-coordinates (irregularly spaced).
  ;   y    - 1D array of y-coordinates (irregularly spaced).
  ;
  ; OUTPUTS:
  ;   gridded_data - 2D array of interpolated data on a uniform grid.
  ;   x_grid       - 1D array of uniformly spaced x-coordinates.
  ;   y_grid       - 1D array of uniformly spaced y-coordinates.
  ;   dx           - Spacing between grid points along the x-axis.
  ;   dy           - Spacing between grid points along the y-axis.
  ;
  ; OPTIONAL KEYWORDS:
  ;   NAN - If set, fills empty grid points with NaN instead of zeros.
  ;
  ; MODIFICATION HISTORY:
  ;   Written by Gelu M Nita and ChatGPT, February 2025.
  ;-

  ; Validate inputs
  IF N_PARAMS() LT 5 THEN BEGIN
    PRINT, 'Usage: gx_data2grid_bilinear, data, x, y, gridded_data, x_grid, y_grid, dx, dy, [NAN=use_nan]'
    RETURN
  ENDIF

  ; Define grid boundaries and spacing
  x_min = MIN(x)
  x_max = MAX(x)
  y_min = MIN(y)
  y_max = MAX(y)

  ; Estimate grid spacing based on nearest neighbor differences
  dx = (x_max - x_min) / (N_ELEMENTS(x) - 1)
  dy = (y_max - y_min) / (N_ELEMENTS(y) - 1)

  ; Generate regular grid coordinates
  x_grid = FINDGEN((x_max - x_min) / dx + 1) * dx + x_min
  y_grid = FINDGEN((y_max - y_min) / dy + 1) * dy + y_min

  nx = N_ELEMENTS(x_grid)
  ny = N_ELEMENTS(y_grid)
  gridded_data = FLTARR(nx, ny)

  IF KEYWORD_SET(use_nan) THEN gridded_data[*] = !VALUES.F_NAN ELSE gridded_data[*] = 0.0

  ; Bilinear interpolation for each grid point
  FOR j = 0, ny - 1 DO BEGIN
    FOR i = 0, nx - 1 DO BEGIN
      xg = x_grid[i]
      yg = y_grid[j]

      ; Find indices of the surrounding points
      x1_idx = WHERE(x LE xg, count1, COMPLEMENT=1)
      x2_idx = WHERE(x GE xg, count2, COMPLEMENT=1)
      y1_idx = WHERE(y LE yg, count3, COMPLEMENT=1)
      y2_idx = WHERE(y GE yg, count4, COMPLEMENT=1)

      IF (count1 GT 0 AND count2 GT 0 AND count3 GT 0 AND count4 GT 0) THEN BEGIN
        x1 = x[x1_idx[0]]
        x2 = x[x2_idx[0]]
        y1 = y[y1_idx[0]]
        y2 = y[y2_idx[0]]

        f11 = data[x1_idx[0], y1_idx[0]]
        f21 = data[x2_idx[0], y1_idx[0]]
        f12 = data[x1_idx[0], y2_idx[0]]
        f22 = data[x2_idx[0], y2_idx[0]]

        ; Compute bilinear interpolation
        denom = (x2 - x1) * (y2 - y1)
        IF denom NE 0 THEN BEGIN
          interp_value = (f11 * (x2 - xg) * (y2 - yg) + $
            f21 * (xg - x1) * (y2 - yg) + $
            f12 * (x2 - xg) * (yg - y1) + $
            f22 * (xg - x1) * (yg - y1)) / denom
          gridded_data[i, j] = interp_value
        ENDIF
      ENDIF
    ENDFOR
  ENDFOR
END
