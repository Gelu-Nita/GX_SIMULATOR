;+
; Name: get_tlb
;
; Purpose: Return top-level base widget ID of widget hierarchy containing widgetID
;
; Calling sequence: tlb = get_parent(widgetID)
;
; Arguments:
;   widgetID - scalar widget ID
;   
; Output:  Returns top-level base widget ID.  Returns -1 if widgetID is not an active widget.
;
; Written: Kim Tolbert  13-Feb-2012
; Modifications:
;
;-
 
function get_tlb, widgetID
 
parent = widgetID[0]
if ~xalive(widgetID) then return, -1

WHILE Widget_Info(parent, /Parent) NE 0 DO $
  parent = Widget_Info(parent, /Parent)

return, parent
end
