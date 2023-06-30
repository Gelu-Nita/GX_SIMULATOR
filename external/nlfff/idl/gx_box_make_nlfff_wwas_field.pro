;
; IDL Wrapper to external call of Weighted Wiegelmann NLFF Field Reconstruction Method library
; v 2.3.21.217 (rev.392)
; min WWWNLFFFReconstruction version: v 2.3.21.217 (rev.392)
; 
; Call:
; rc = gx_box_make_nlfff_wwas_field(lib_location, box, _extra = _extra)
; 
; Parameters description (see also section Comments below):
; 
; Parameters required:
;   (in)      lib_location    - full path to calling library
;   (in/out)  box             - GX-simulator box with initial model of field bx, by, bz
;   
; Parameters optional (in):
;   (in)      weight_bound_size    - weight bounary buffer zone size (in parts of corresponding dim. size),
;                                    default is 0.1 (i.e. buffer zone is 10% of dimension size from all boundaries,
;                                    except photosphere plain). Use weight_bound_size = 0 for no-buffer-zone approach 
; 
; Parameters optional (in): conditions general
;   add_conditions_mode       - 0 - no add conditions, 
;                               1 - conditions as functional (default, if any of conditions applied, otherwise 0), 
;                               2 - directly applied
;   
; Parameters optional (in): Abs. Field (BABS) conditions
;   abs_field                 - 3D field abs. value (for exact and "no less than" conditions)
;   abs_field_weight          - 3D weights for abs_field conditions
;   add_conditions_abs        - 0 - ignored, 1 - no less than, 2 - equal (default, if both abs_field and abs_field_weight are set, otherwise 0)
;   abs_field_max             - 3D field abs. value (for "no greater than" conditions)
;   abs_field_max_weight      - 3D weights for abs_field_max conditions
;   add_conditions_abs_max    - 0 - ignored, 1 - no greater than (default, if both abs_field_max and abs_field_max_weight are set, otherwise 0)
;   
; Parameters optional (in): Line-Of-Sight (LOS) conditions
;   los_projection            - 3D LOS field value
;   los_projection_weight     - 3D weights for LOS conditions
;   los_projection_dir_cos    - Directive cosines of LOS direction (3-elements array: 
;                                 [0] - along latitude, [1] - along longitude, [2] - normal to photosphere, 
;                                 default is [0, 0, 1] (= top view))
;   add_conditions_los        - 0 - ignored, 2 - equal (default, if both los_projection and los_projection_weight are set, otherwise 0)
;   
; Parameters optional (in): By-Component (COMP) conditions
;   field_component_x         - 3D x-component
;   field_component_x_weight  - 3D x-component weights
;   field_component_y         - 3D y-component
;   field_component_y_weight  - 3D y-component weights
;   field_component_z         - 3D z-component
;   field_component_z_weight  - 3D z-component weights
;   
; Return code:
;   0, if everything is OK
;   non-zero in the case of errors
;   ...... (values are subject to specify)
; 
; Comments:
;   All additional fields and weights are 3D arrays of the same size as GX-box.  
;   Values <add_conditions_abs = 1> ("no less than") and <add_conditions_max_abs = 1> ("no greater than") 
;       can be applied only if <add_conditions_mode = 2> (directly applied conditions)
;   All conditions applied only if both value array and corresponding weight are defined, otherwise ignored
;   BABS and LOS conditions can be combined
;   Any COMP conditions can be combined
;   COMP cannot be combined with BABS and LOS
;        
;   Note, that wrapping library also provides interfaces for C/C++ and MATLAB
;   
; (c) Alexey G. Stupishin, Saint Petersburg State University, Saint Petersburg, Russia, 2017-2022
;     mailto:agstup@yandex.ru
;
;
;--------------------------------------------------------------------------;
;     \|/     Set the Controls for the Heart of the Sun           \|/      ;
;    --O--        Pink Floyd, "A Saucerful Of Secrets", 1968     --O--     ;
;     /|\                                                         /|\      ;  
;--------------------------------------------------------------------------;
;
;-------------------------------------------------------------------------------------------------
function gx_box_make_nlfff_wwas_field, lib_location, box, version_info = version_info, _extra = _extra

  version_info = gx_box_field_library_version(lib_location)
;    print, version_info
  
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
  abs_field_max = 0L
  abs_field_max_weight = 0L
  los_projection_max = 0L
  los_projection_max_weight = 0L
  field_component_x_max = 0L
  field_component_x_max_weight = 0L
  field_component_y_max = 0L
  field_component_y_max_weight = 0L
  field_component_z_max = 0L
  field_component_z_max_weight = 0L
  value = bytarr(26)
  value[0:4] = 0 
  value[5:25] = 1 
  
  n = n_tags(_extra)
  parameterMap = replicate({itemName:'',itemvalue:0d},n+1)
  nParameters = 0;
  if n gt 0 then begin
    keys = strlowcase(tag_names(_extra))
    for i = 0, n-1 do begin
      case keys[i] of
        'abs_field': begin
            abs_field = double(_extra.(i))
            value[5] = 0
          end
        'abs_field_weight': begin 
            abs_field_weight = double(_extra.(i))
            value[6] = 0
          end
        'los_projection': begin
            los_projection = double(_extra.(i))
            value[7] = 0
          end
        'los_projection_weight': begin 
            los_projection_weight = double(_extra.(i))
            value[8] = 0
          end
        'los_projection_dir_cos': begin 
            los_projection_dir_cos = double(_extra.(i))
            value[9] = 0
          end
        'field_component_x': begin 
            field_component_x = double(_extra.(i))
            value[10] = 0
          end
        'field_component_x_weight': begin 
            field_component_x_weight = double(_extra.(i))
            value[11] = 0
          end
        'field_component_y': begin 
            field_component_y = double(_extra.(i))
            value[12] = 0
          end
        'field_component_y_weight': begin 
            field_component_y_weight = double(_extra.(i))
            value[13] = 0
          end
        'field_component_z': begin 
            field_component_z = double(_extra.(i))
            value[14] = 0
          end
        'field_component_z_weight': begin 
            field_component_z_weight = double(_extra.(i))
            value[15] = 0
          end

        'abs_field_max': begin
            abs_field_max = double(_extra.(i))
            value[16] = 0
          end
        'abs_field_max_weight': begin 
            abs_field_max_weight = double(_extra.(i))
            value[17] = 0
          end
        'los_projection_max': begin
            los_projection_max = double(_extra.(i))
            value[18] = 0
          end
        'los_projection_max_weight': begin 
            los_projection_max_weight = double(_extra.(i))
            value[19] = 0
          end
        'field_component_x_max': begin 
            field_component_x_max = double(_extra.(i))
            value[20] = 0
          end
        'field_component_x_max_weight': begin 
            field_component_x_max_weight = double(_extra.(i))
            value[21] = 0
          end
        'field_component_y_max': begin 
            field_component_y_max = double(_extra.(i))
            value[22] = 0
          end
        'field_component_y_max_weight': begin 
            field_component_y_max_weight = double(_extra.(i))
            value[23] = 0
          end
        'field_component_z_max': begin 
            field_component_z_max = double(_extra.(i))
            value[24] = 0
          end
        'field_component_z_max_weight': begin 
            field_component_z_max_weight = double(_extra.(i))
            value[25] = 0
          end
        else: begin
            v = _extra.(i)
            if isa(v, /number) && ~isa(v, /array) && imaginary(v) eq 0 then begin
                parameterMap[nParameters].itemName = keys[i]
                parameterMap[nParameters].itemValue = v
                nParameters = nParameters + 1
            end
          end
      endcase
    endfor
  endif
  parameterMap[nParameters].itemName = '!____idl_map_terminator_key___!';

  bx = double(box.bx)
  by = double(box.by)
  bz = double(box.bz)

  sz = size(bx)
 ; returnCode = 0
  returnCode = CALL_EXTERNAL(lib_location, 'mfoNLFFF', parameterMap, sz[1:3], bx, by, bz, $
              abs_field, abs_field_weight, $
              los_projection, los_projection_weight, los_projection_dir_cos, $
              field_component_x, field_component_x_weight, $
              field_component_y, field_component_y_weight, $
              field_component_z, field_component_z_weight, $
              abs_field_max, abs_field_max_weight, $
              los_projection_max, los_projection_max_weight, $
              field_component_x_max, field_component_x_max_weight, $
              field_component_y_max, field_component_y_max_weight, $
              field_component_z_max, field_component_z_max_weight, $
              VALUE = value, /CDECL, /UNLOAD)

  box.bx = bx
  box.by = by
  box.bz = bz
  
  expr = stregex(box.id,'(.+)\.([A-Z]+)',/subexpr,/extract)
  box.id = expr[1] + '.NAS'

  return, returnCode
end
