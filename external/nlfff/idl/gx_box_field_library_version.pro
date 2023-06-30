;
; IDL Wrapper to external call to get version of
;   Weighted Wiegelmann NLFF Field Reconstruction library
;   
; v 3.4.22.1025 (rev.626)
; min WWWNLFFFReconstruction version: v 3.4.22.1025 (rev.626)
; 
; Call (see parameters and comments below):
; version_info = gx_box_field_library_version(lib_location, returnCode = returnCode)
; 
; Parameters required:
;   (in)      lib_location   (string)       full path to calling library
;   
; Parameters optional (in):
;   (in)      returnCode     (integer)      length of version_info string     
;   Return value:
;     string containing version information. Sample:
;     "Weighted Wiegelmann NLFFF Reconstruction Library v.3.4.22.1025 (rev.626). Copyright (C) Alexey G. Stupishin (agstup@yandex.ru), 2017-2022, St. Petersburg State University, Russia"
;    
;   Note, that wrapping library also provides interfaces for C/C++ and MATLAB
;   
; (c) Alexey G. Stupishin, Saint Petersburg State University, Saint Petersburg, Russia, 2017-2022
;     mailto:agstup@yandex.ru
;
;--------------------------------------------------------------------------;
;     \|/     Set the Controls for the Heart of the Sun           \|/      ;
;    --O--        Pink Floyd, "A Saucerful Of Secrets", 1968     --O--     ;
;     /|\                                                         /|\      ;  
;--------------------------------------------------------------------------;
;                                                              
function gx_box_field_library_version, lib_location, returnCode = returnCode 

b = bytarr(512)
b(*) = 32B
version_info = STRING(b)
returnCode = CALL_EXTERNAL(lib_location, 'mfoNLFFFVersion', version_info)

return, version_info

end
