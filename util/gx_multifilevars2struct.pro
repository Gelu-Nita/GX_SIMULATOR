;+
; NAME:
;    gx_multifilevars2struct
;
; PURPOSE:
;    This function processes all IDL save files (.sav) in a specified directory, filtering
;    files by a pattern if provided, and using gx_filevars2struct with /pointer to restore
;    the variables from each file and pack them into a structure. It returns an array of
;    these structures. Files that do not match the expected variable names are ignored.
;
; INPUT:
;    directory - A string specifying the path to the directory containing the IDL save files.
;
; OPTIONAL INPUT:
;    filter - A string specifying a pattern to filter the file names. The default is '*', which matches all files.
;
; OUTPUT:
;    An array of structures, each containing the variables from one save file.
;
; EXAMPLE USAGE:
;    structArray = gx_multifilevars2struct('/path/to/directory/', filter='fits*')
;
; REVISIONS:
;    Gelu M Nita - 8/26/24 - Initial version.
;
;-
FUNCTION gx_multifilevars2struct, directory, filter=filter
  ; Default the filter to '*' if not provided
  IF N_ELEMENTS(filter) EQ 0 THEN filter = '*'

  ; Apply the filter to search for matching .sav files in the directory
  files = FILE_SEARCH(directory + path_sep()+filter + '*.sav')

  ; Initialize an empty list to hold the valid structures
  valid_structs = []

  ; Initialize the expected variable names from the first valid file
  first_valid_file = ''

  ; Process each file
  FOR i = 0, N_ELEMENTS(files) - 1 DO BEGIN
    ; Use gx_filevars2struct with /pointer to get the structure for the current file
    file_struct = gx_filevars2struct(files[i], /pointer)

    ; If it's the first valid file, set the expected variable names
    IF first_valid_file EQ '' THEN BEGIN
      expected_names = TAG_NAMES(file_struct)
      first_valid_file = files[i]
    ENDIF

    ; Get the variable names for this structure
    current_names = TAG_NAMES(file_struct)

    ; Compare the current names with the expected names
    IF N_ELEMENTS(current_names) EQ N_ELEMENTS(expected_names) AND $
      (MIN(current_names EQ expected_names)) THEN BEGIN
      ; If names match, add the structure to the list
      valid_structs = [valid_structs, file_struct]
    ENDIF
  ENDFOR

  ; Return the array of valid structures
  RETURN, valid_structs
END
