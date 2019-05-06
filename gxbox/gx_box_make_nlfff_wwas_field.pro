;
; Wrapper to external call of Weighted Wiegelmann NLFFF Field Reconstruction Method library
; v 0.1.0
; 
; NB! Alpha-version!
; 
; Parameters required:
;   (in)      dll_location - full path to calling DLL
;   (in/out)  box           - GX-simulator box with initial model of field bx, by, bz
;   
; Parameters optional (in):
;   ...... (subject to specify) 
; 
; Return code:
;   0, if everything is OK
;   non-zero in the case of errors
;   ...... (values are subject to specify)
; 
; (c) Alexey G. Stupishin, Saint Petersburg State University, 2017
;

;-------------------------------------------------------------------------------------------------
function gx_box_make_nlfff_wwas_field, dll_location, box, _extra = _extra

  abs_field = 0L
  abs_field_weight = 0L
  los_projection = 0L
  los_projection_weight = 0L
  los_projection_dir_cos = 0L
  field_component_x = 0L
  field_component_x_weight = 0L
  field_component_y = 0L
  field_component_y_weight = 0L
  field_component_z = 0L
  field_component_z_weight = 0L
  value = bytarr(16)
  value[0:4] = 0 
  value[5:15] = 1 
  
  n = n_tags(_extra)
  parameterMap = replicate({itemName:'',itemvalue:0d},n+1)
  nParameters = 0;
  if n gt 0 then begin
    keys = strlowcase(tag_names(_extra))
    for i = 0, n-1 do begin
      case keys[i] of
        'abs_field': begin
            abs_field = _extra.(i)
            value[5] = 0
          end
        'abs_field_weight': begin 
            abs_field_weight = _extra.(i)
            value[6] = 0
          end
        'los_projection': begin
            los_component = _extra.(i)
            value[7] = 0
          end
        'los_projection_weight': begin 
            los_component_weight = _extra.(i)
            value[8] = 0
          end
        'los_projection_dir_cos': begin 
            los_component_dir_cos = _extra.(i)
            value[9] = 0
          end
        'field_component_x': begin 
            field_component_x = _extra.(i)
            value[10] = 0
          end
        'field_component_x_weight': begin 
            field_component_x_weight = _extra.(i)
            value[11] = 0
          end
        'field_component_y': begin 
            field_component_y = _extra.(i)
            value[12] = 0
          end
        'field_component_y_weight': begin 
            field_component_y_weight = _extra.(i)
            value[13] = 0
          end
        'field_component_z': begin 
            field_component_z = _extra.(i)
            value[14] = 0
          end
        'field_component_z_weight': begin 
            field_component_z_weight = _extra.(i)
            value[15] = 0
          end
        else: begin
            parameterMap[nParameters].itemName = keys[i]
            parameterMap[nParameters].itemValue = _extra.(i)
            nParameters = nParameters + 1
          end
      endcase
    endfor
  endif
  parameterMap[nParameters].itemName = '!____idl_map_terminator_key___!';

  bx = transpose(box.by, [1, 0, 2])
  by = transpose(box.bx, [1, 0, 2])
  bz = transpose(box.bz, [1, 0, 2])

  sz = size(bx)
  sz = sz[1:3]
  returnCode = CALL_EXTERNAL(dll_location, 'mfoNLFFF', parameterMap, sz, bx, by, bz, $
              abs_field, abs_field_weight, $
              los_projection, los_projection_weight, los_projection_dir_cos, $
              field_component_x, field_component_x_weight, $
              field_component_y, field_component_y_weight, $
              field_component_z, field_component_z_weight, $
              VALUE = value, /CDECL, /UNLOAD)

  box.bx = transpose(by, [1, 0, 2])
  box.by = transpose(bx, [1, 0, 2])
  box.bz = transpose(bz, [1, 0, 2])
  
  expr = stregex(box.id,'(.+)\.([A-Z]+)',/subexpr,/extract)
  box.id = expr[1] + '.NAS'


  return, returnCode
end
