;
; IDL Wrapper to external call to claculate magnetic fields line properties
;   using Weighted Wiegelmann NLFF Field Reconstruction library
; v 2.0.19.908 (rev.200)
; min WWWNLFFFReconstruction.dll version: 2.0.19.810
; 
; NB! Beta-version!
; 
; Call:
; rc = gx_box_calculate_lines(dll_location, box, status, physLength, avField, startIdx, endIdx, _extra = _extra)
; 
; Parameters description (see also section Comments below):
; 
; Parameters required:
;   (in)      dll_location    - full path to calling DLL
;   (in/out)  box             - GX-simulator box with initial model of field bx, by, bz
;   (out)     status          - (lonarr) status of voxel processing 
;   (out)     physLength      - (dblarr) length of line passed through the voxel in units of solar radius
;                                (line starts/ends at the specified chromosphere level or
;                                 at the non-photospheric bounds)  
;   (out)     avField         - (dblarr) average field of line passed through the voxel in units of GX-box field  
;   (out)     startIdx        - (lonarr) voxel index of the line start 
;   (out)     endIdx          - (lonarr) voxel index of the line end 
;   
; Parameters optional (in): conditions general
;   reduce_passed             - if non-zero, voxels that already crossed by some closed line
;                               will be marked as already passed (default = 1)     
;   chromo_level              - chromosphere level in units of km (default = 1000) 
;
; Comments:
;   Output arrays and indexing:
;     All 5 output arrays allocated inside the wrapper exactly as specified (LONARR or DBLARR).
;     Arrays are one-dimensional of size equal to the number of voxels of specified GX-box.
;     Indexing is natural sequence of voxels (as if REFORM function will be applied).
;     
;   status values:
;     Values are status of voxel, bitwise. Meaning of the bits:
;     0 - set if voxel has been visited (internal flag, can be ignored)
;     1 - set if voxel belong to some line
;     2 - set if it is closed line
;     3 - set if line was calculated from this voxel  
;
;------------------------------------------------------------------------------------------
function gx_box_calculate_lines, dll_location, box, status, physLength, avField, startIdx, endIdx, version_info = version_info, _extra = _extra

  b = bytarr(512)
  b(*) = 32B
  version_info = STRING(b)
  returnCode = CALL_EXTERNAL(dll_location, 'mfoNLFFFVersion', version_info)
;    print, version_info
  
  Rx = 0L
  Ry = 0L
  Rz = 0L
  nVox = 0L
  linesLength = 0L
  codes = 0L
  globalIdx = 0L
  maxLength = 0L
  totalLength = 0L
  coord = 0L
  linesStart = 0L
  linesGlobalIdx = 0L
  value = bytarr(22)
  value[0:4] = 0 
  value[5:21] = 1 
  value[9]  = 0
  value[10] = 0
  value[11] = 0
  value[15] = 0
  value[16] = 0
  
  n = n_elements(box.bx)

  status = lonarr(n)
  physlength = dblarr(n)
  avfield = dblarr(n)
  startidx = lonarr(n)
  endidx = lonarr(n)
  
  transpose_index = 1
  n = n_tags(_extra)
  parameterMap = replicate({itemName:'',itemvalue:0d},n+1)
  nParameters = 0;
  if n gt 0 then begin
    keys = strlowcase(tag_names(_extra))
    for i = 0, n-1 do begin
      parameterMap[nParameters].itemName = keys[i]
      if strcmp(keys[i], 'transpose_index') then begin
        transpose_index = _extra.(i)
      endif     
      if strcmp(keys[i], 'chromo_level') then begin
        parameterMap[nParameters].itemValue = double(_extra.(i))/wcs_rsun()/box.dr[2] *1000
      endif else begin
        parameterMap[nParameters].itemValue = _extra.(i)
      endelse     
      nParameters = nParameters + 1
    endfor
  endif
  parameterMap[nParameters].itemName = '!____idl_map_terminator_key___!';

  if transpose_index gt 0 then begin
    transpose_index = 1
    bx = double(transpose(box.by, [1, 0, 2]))
    by = double(transpose(box.bx, [1, 0, 2]))
    bz = double(transpose(box.bz, [1, 0, 2]))
  endif else begin
    bx = box.bx
    by = box.by
    bz = box.bz
  endelse
  sz = size(bx)
  
  returnCode = CALL_EXTERNAL(dll_location, 'mfoLines', parameterMap, sz[1:3], bx, by, bz, $
              Rx, Ry, Rz, $
              nVox, $
              status, physLength, avField, $
              linesLength, codes, globalIdx, $
              startIdx, endIdx, $
              maxLength, totalLength, coord, linesStart, linesGlobalIdx, $
              VALUE = value, /CDECL, /UNLOAD)

  physLength=physLength*box.dr[0]
  
  return, returnCode
end
