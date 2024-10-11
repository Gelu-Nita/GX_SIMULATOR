;+
; NAME:
;    gx_is_valid_idl_savefile
;
; PURPOSE:
;    Determines if the file provided is a valid IDL save file by attempting to create an IDL_Savefile object.
;
; CATEGORY:
;    File I/O
;
; CALLING SEQUENCE:
;    result = gx_is_valid_idl_savefile(filename)
;
; INPUTS:
;    filename:  [STRING] The name (and path, if applicable) of the file to check.
;
; OUTPUTS:
;    result:  [INTEGER] Returns 1 (true) if the file is a valid IDL save file, 0 (false) otherwise.
;
; EXAMPLES:
;    is_valid = gx_is_valid_idl_savefile('myfile.sav')
;    PRINT, is_valid
;
; RESTRICTIONS:
;    The function attempts to create an IDL_Savefile object from the given file.
;    If the file cannot be opened or is not a valid save file, it returns 0.
;
; MODIFICATION HISTORY:
;    Written by Gelu Nita, 10/11/2024
;
;-
FUNCTION gx_is_valid_idl_savefile, filename
  
  IF SIZE(filename,/tname) NE 'STRING' THEN RETURN,0
  ; Check if the file exists
  IF ~FILE_TEST(filename, /READ) THEN RETURN, 0

  ; Use a CATCH block to safely handle any errors during the object creation attempt
  CATCH, error_status
  IF error_status NE 0 THEN BEGIN
    ; If an error occurs, cancel the error and return 0
    CATCH, /CANCEL
    RETURN, 0
  ENDIF

  ; Try to create an instance of the IDL_Savefile object
  osav = OBJ_NEW('IDL_Savefile', filename)

  ; Destroy the object and return 1 (valid save file)
  OBJ_DESTROY, osav
  RETURN, 1
END
