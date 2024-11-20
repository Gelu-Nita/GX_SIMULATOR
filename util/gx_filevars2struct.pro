;+
; NAME:
;    gx_filevars2struct
;
; PURPOSE:
;    This function restores all variables from an IDL save file and packs them into a structure.
;    Each variable in the save file becomes a tag in the structure, with the tag name being
;    the same as the variable name. If the /pointer keyword is set, arrays are stored as pointers
;    to avoid conflicts in case of differing array sizes across files.
;
; INPUT:
;    fname - A string specifying the name of the IDL save file (.sav) from which to restore variables.
;
; OPTIONAL INPUT:
;    pointer - If set, array variables are stored as pointers instead of directly as array tags.
;
; OUTPUT:
;    A structure containing all variables from the save file. Each variable is stored
;    as a tag in the structure, where the tag name corresponds to the original variable name.
;    If /pointer is set, array variables are stored as pointers.
;
; EXAMPLE USAGE:
;    myStruct = gx_filevars2struct('datafile.sav')
;    myStruct = gx_filevars2struct('datafile.sav', /pointer)
;
; REVISIONS:
;    Gelu M. Nita - 8/26/24 - Initial version.
;
;-
FUNCTION gx_filevars2struct, fname, pointer=pointer
  ; Create an object for the IDL_Savefile associated with the file
  o = OBJ_NEW('IDL_Savefile', /relaxed,fname)

  ; Get the list of variable names in the save file
  names = o->NAMES()

  ; Initialize an empty structure
  s = {}

  ; Loop through each variable name
  FOR k = 0, N_ELEMENTS(names) - 1 DO BEGIN
    ; Restore the variable with the current name
    o->RESTORE, names[k]

    ; Dynamically assign the variable to a temporary variable 'var'
    void = EXECUTE('var = TEMPORARY(' + names[k] + ')')

    ; Check if the variable is an array and if /pointer is set
    IF KEYWORD_SET(pointer) AND (SIZE(var))[0] gt 0 THEN BEGIN
      ; Create a pointer to the array and add it to the structure
      var_ptr = PTR_NEW(var)
      s = CREATE_STRUCT(s, names[k], var_ptr)
    ENDIF ELSE BEGIN
      ; Add the variable directly to the structure
      s = CREATE_STRUCT(s, names[k], var)
    ENDELSE
  ENDFOR

  ; Return the structure containing all variables
  RETURN, s
END
