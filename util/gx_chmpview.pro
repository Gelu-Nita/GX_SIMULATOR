;+
; NAME:
;    gx_chmpview
;
; PURPOSE:
;    Creates and manages the CHMP Results Viewer widget using the `cw_gxchmpview` compound widget.
;    Handles event management, including widget close requests through `gx_chmpview_event`.
;
; CATEGORY:
;    Widget Interface and Event Handling
;
; INPUTS:
;    None directly, though the widget system handles user interaction events.
;
; OPTIONAL INPUT KEYWORDS:
;    _extra: Passes additional arguments to the compound widget (`cw_gxchmpview`).
;
; SIDE EFFECTS:
;    - Creates a graphical interface for viewing CHMP results.
;    - Displays a confirmation dialog when the user attempts to close the viewer.
;    - Starts the XMANAGER to manage the widget.
;
; CALLING SEQUENCE:
;    gx_chmpview, _extra=_extra
;
; MODIFICATION HISTORY:
;    Written by Gelu M. Nita, 09/10/2024
;-

PRO gx_chmpview_event, event
  ; Handle widget close (kill request) event
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    answ = DIALOG_MESSAGE('Do you want to quit CHMP Viewer?', /QUESTION)
    IF STRUPCASE(answ) NE 'YES' THEN RETURN
    WIDGET_CONTROL, event.top, /DESTROY
    RETURN
  END

  ; Case statement for other event handling, which can be expanded
  CASE STRUPCASE(WIDGET_INFO(event.id, /UNAME)) OF
    ELSE:
  ENDCASE
END

PRO gx_chmpview, _extra=_extra
  ; Set the font for the viewer
  gx_setfonts, font=font
  subdirectory=['resource', 'bitmaps']
  ; Create the main widget base for the CHMP Results Viewer with a menu bar
  tlb = WIDGET_BASE(TITLE='CHMP Results Viewer', /COLUMN, $
    MBAR=mbar, /TLB_SIZE_EVENTS, $
    /TLB_KILL_REQUEST_EVENTS, UNAME='gx_chmpview')

  ; Initialize the compound widget 
  wgxchmpview = cw_gxchmpview(tlb, _extra=_extra)

  ; Set the user value and realize the widget
  WIDGET_CONTROL, tlb, SET_UVALUE=wgxchmpview
  WIDGET_CONTROL, tlb, /REALIZE

  ; Start the XMANAGER to manage the widget interface
  XMANAGER, 'gx_chmpview', tlb, /NO_BLOCK
END


