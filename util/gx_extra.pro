;+
; NAME:
;    gx_extra
;
; PURPOSE:
;    This function captures and returns any additional keyword arguments passed to it, packaging them into a structure.
;
; CATEGORY:
;    Utility function
;
; CALLING SEQUENCE:
;    result = gx_extra(key1='value1', /key2)
;
; INPUTS:
;    None.
;
; KEYWORD PARAMETERS:
;    _EXTRA: This function accepts any number of additional keyword parameters via the _EXTRA keyword.
;            These keywords are captured and returned in a structure where the values of set keywords are
;            preserved and boolean keywords (set with "/") are assigned a value of 1.
;
; OUTPUTS:
;    Returns a structure containing all the keywords passed in. The structure fields will be named after 
;    the keywords, and any boolean keywords will have a value of 1.
;
; SIDE EFFECTS:
;    None.
;
; EXAMPLE:
;    result = gx_extra(key1='value1', /key2)
;    ; This will return the structure {KEY1: 'value1', KEY2: 1}.
;
; MODIFICATION HISTORY:
;    Written by Gelu M Nita, Sep-10-2024.
;-

FUNCTION gx_extra, _extra=_extra
   ; Returns the structure containing all the extra keywords passed in
   RETURN, _extra
END
