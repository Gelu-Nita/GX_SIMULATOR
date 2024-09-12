;+
; NAME:
;    gx_hline
;
; PURPOSE:
;    This function returns the X-axis range, either in its original scale or transformed if the X-axis is logarithmic.
;
; CATEGORY:
;    Plot Utilities
;
; CALLING SEQUENCE:
;    x_range = gx_hline()
;
; OUTPUTS:
;    The function returns the X-axis range based on whether the X-axis is logarithmic or linear:
;      - If the X-axis is in logarithmic scale, the range returned is the exponentiated version of the current X-axis range.
;      - If the X-axis is in linear scale, it returns the current X-axis range.
;
; SIDE EFFECTS:
;    None.
;
; EXAMPLE:
;    x_range = gx_hline()
;    ; This returns the appropriate X-axis range for the current plot.
;
; MODIFICATION HISTORY:
;    Written by Gelu M Nita, 09/10/2024.
;-

FUNCTION gx_hline
  ; Check if the X-axis is logarithmic (!X.TYPE EQ 1). If so, return 10^!X.CRANGE to get the original values.
  ; Otherwise, return !X.CRANGE for a linear X-axis.
  RETURN, !X.TYPE ? 10^!X.CRANGE : !X.CRANGE
END
