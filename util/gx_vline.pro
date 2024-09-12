;+
; NAME:
;    gx_vline
;
; PURPOSE:
;    This function returns the Y-axis range, either in its original scale or transformed if the Y-axis is logarithmic.
;
; CATEGORY:
;    Plot Utilities
;
; CALLING SEQUENCE:
;    y_range = gx_vline()
;
; OUTPUTS:
;    The function returns the Y-axis range based on whether the Y-axis is logarithmic or linear:
;      - If the Y-axis is in logarithmic scale, the range returned is the exponentiated version of the current Y-axis range.
;      - If the Y-axis is in linear scale, it returns the current Y-axis range.
;
; SIDE EFFECTS:
;    None.
;
; EXAMPLE:
;    y_range = gx_vline()
;    ; This returns the appropriate Y-axis range for the current plot.
;
; MODIFICATION HISTORY:
;    Written by Gelu M. Nita, 09/10/2024.
;-

FUNCTION gx_vline
  ; Check if the Y-axis is logarithmic (!Y.TYPE EQ 1). If so, return 10^!Y.CRANGE to get the original values.
  ; Otherwise, return !Y.CRANGE for a linear Y-axis.
  RETURN, !Y.TYPE ? 10^!Y.CRANGE : !Y.CRANGE
END
