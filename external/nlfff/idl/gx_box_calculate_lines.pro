;
; IDL Wrapper to external call to claculate magnetic fields line properties
;   using Weighted Wiegelmann NLFF Field Reconstruction library
;   
; v 2.3.21.217 (rev.392)
; min WWWNLFFFReconstruction version: v 2.3.21.217 (rev.392)
; 
; Call (see parameters and comments below):
; non_stored = gx_box_calculate_lines( $
;                          lib_location, box $ ; Required
;                        , inputSeeds = <array>, maxLength = <number> $ ; Optional input
;                        , reduce_passed = <number>, n_processes = <number>, chromo_level = <number>, line_step = <number> $ ; Optional input  
;                        , inputSeeds = <array>, maxLength = <number> $ ; Optional input
;                        , reduce_passed = <number>, n_processes = <number>, chromo_level = <number>, line_step = <number> $ ; Optional input  
;                        , status = status, physLength = physLength, avField = avField $ ; Optional output
;                        , startIdx = startIdx, endIdx = endIdx, apexIdx = apexIdx, seedIdx = seedIdx $ ; Optional output
;                        , totalLength = totalLength, nLines = nLines, nPassed = nPassed $ ; Optional output
;                        , coords = coords, linesPos = linesPos, linesLength = linesLength, linesIndex = linesIndex $ ; Optional output
;                        , codes = codes $ ; Optional output
;                        , version_info = version_info ; Optional output
;                                    )
; 
; Parameters description (see also section Comments below):
; 
; Parameters required:
;   (in)      lib_location   (string)       full path to calling library
;   (in)      box            (structure)    GX-simulator box with fields bx, by, bz
;   
; Parameters optional (in):
;   (in)      reduce_passed  (integer)      flag to passinng by already crossed pixels (bitwise, see Comments)     
;   (in)      chromo_level   (double)       chromosphere level in units of km (default = 1000)
;   (in)      line_step      (double)       step along line in voxel size units (default = 1)
;   (in)      n_processes    (integer)      number of parallel threads (default = 0, see Comments)
;   (in)      inputSeeds     (3xN double)   coordinates of line seeds; if omitted or !NULL, all voxels
;                                           are considered as seeds (for coordinates [0, 0, 0], [1, 0, 0] etc.)     
;   (in)      maxLength      (integer)      maximum number of stored points for all calculated lines 
;                                           (can be uint64). If not set or eq 0, no coords, linesPos, linesLength, 
;                                           linesIndex will be stored 
;   
; Parameters optional (out):
;   (for arrays indexing see Comments below)
;   (out)     status          (N lonarr)    status of voxel processing (bitwise, see Comments)
;   (out)     physLength      (N dblarr)    length of line passed through the voxel in units of solar radius
;                                           (line starts/ends at the specified chromosphere level or
;                                            at the non-photospheric bounds)  
;   (out)     avField         (N dblarr)    average field of line passed through the voxel in units of GX-box field  
;   (out)     startIdx        (N lonarr)    voxel index of the line start 
;   (out)     endIdx          (N lonarr)    voxel index of the line end 
;   (out)     apexIdx         (N lonarr)    voxel index of the line apex (with minimal field) 
;   (out)     seedIdx         (N lonarr)    voxel index of the line seed
;    
;   (out)     nPassed         (long)        number of processed voxels (normally = nLines)
;   
;   (out)     totalLength     (uint64)      resulting number of stored points for all calculated lines 
;   (out)     nLines          (long)        number of stored lines
;   (out)     coords          (4xtotalLength double)    coordinates of the lines' points (first 3 columns)
;                                                       distance from apex (4th column)  
;   (out)     linesPos        (nLines uint64arr)        index of line first point in "coords" array
;   (out)     linesLength     (nLines lonarr)           line length (number of points) in "coords" array
;   (out)     linesIndex      (nLines lonarr)           index of line in common arrays (such as "avField" etc.)
;   
;   (out)     codes           (N lonarr)    codes of calculation process (not specified here yet, mainly for debugging) 
;                        
;   (out)     version_info    (string)      WWNLFFFReconstruction.dll library version information
;
; Comments:
;   Output arrays and indexing:
;     Common arrays (physLength, avField, startIdx, endIdx, seedIdx, apexIdx, status, codes):
;       Allocated inside the wrapper exactly as specified (LONARR or DBLARR).
;       Arrays are one-dimensional of size equal to the number of voxels of specified GX-box.
;       In the case when seeds omitted (all voxels are seeds):
;           Indexing is natural sequence of voxels (as if REFORM function will be applied).
;       In the case when seeds specified:
;           Indexing is the same as sequence of seeds.
;   
;   coords array: 
;     If number of calculated points exceeds "maxLength", not all lines will be stored; 
;       in this case "nLines" will be less than "nPassed", function returns the number of non-stored lines
;     Note! Only coronal part (defined by "chromo_level" value) of line is stored; in the case of crossing
;       chromosphere level corresponding start/end point placed exactly at this level.  
;   
;   n_processes: if == 0, all possible cores will be used; it should not block other actions with computer,
;     because threads started with low priority.
;     
;   reduce_passed - meaning of the bits:
;     0 - if set, voxels that already crossed by some closed line will be marked as already passed
;     1 - if set, voxels that already crossed by some opened line will be marked as already passed
;       default 0, if inputSeed defined; otherwise 3
;          
;   status - meaning of the bits:
;     0 - set if the voxel has been visited (internal flag, can be ignored)
;     1 - set if the voxel belong to some line
;     2 - set if it is closed line
;     3 - set if the line was calculated from this voxel (the voxel is seed)  
;
;   Return value:
;     Normally 0. If not all lines are stored, returns the number of non-stored lines
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
;----------------------------------------------------------------------------------------------
pro gxl_setNULL, var, value, n
    var = 0L
    value[n] = 1
end

;------------------------------------------------------------------------------------------
function gxl_inverteIndex, index, sizes ; index and sizes in AGS CS
    
    kx =  index mod sizes[0]
    kyz = index / sizes[0]
    ky = kyz mod sizes[1]
    kz = kyz / sizes[1]

    return, (kz*sizes[0] + kx)*sizes[1] + ky
    
end

;------------------------------------------------------------------------------------------
function gxl_inverteIndexArr, data, sizes ; index and sizes in AGS CS

    tdata = make_array(n_elements(data), type = size(data, /TYPE))
    for i = 0, n_elements(data)-1 do tdata[i] = gxl_inverteIndex(data[i], sizes)
    
    return, tdata
    
end

;------------------------------------------------------------------------------------------
function gxl_transpIdx, data, sizes, isTransp ; sizes in AGS CS
    
    if not isTransp then return, data
    
    tdata = make_array(n_elements(data), type = size(data, /TYPE))
    for i = 0, n_elements(data)-1 do tdata[gxl_inverteIndex(i, sizes)] = data[i]

    return, tdata
    
end

;------------------------------------------------------------------------------------------
function gx_box_calculate_lines $
    , lib_location, box $ ; Required
    , inputSeeds = inputSeeds, maxLength = maxLength $ ; Optional input
    , _extra = _extra $ ; Optional input
    , status = status, physLength = physLength, avField = avField $ ; Optional output
    , startIdx = startIdx, endIdx = endIdx, apexIdx = apexIdx, seedIdx = seedIdx $ ; Optional output
    , totalLength = totalLength, nLines = nLines, nPassed = nPassed $ ; Optional output
    , coords = coords, linesPos = linesPos, linesLength = linesLength, linesIndex = linesIndex $ ; Optional output
    , codes = codes $ ; Optional output
    , version_info = version_info ; Optional output

    version_info = gx_box_field_library_version(lib_location)
;    print, version_info
  
    value = bytarr(23)
  
    Nseeds = lonarr(1) 
    if keyword_set(inputSeeds) and not isa(inputSeeds, /NULL) then begin
        szs = size(inputSeeds)
        Nseeds[0] = szs[0] eq 1 ? 1 : szs[2]
        tseeds = dblarr(3, Nseeds[0])
        tseeds[0, *] = inputSeeds[1, *]
        tseeds[1, *] = inputSeeds[0, *]
        tseeds[2, *] = inputSeeds[2, *]
        vseeds = reform(tseeds, [1, n_elements(inputSeeds)])
        NVox = Nseeds[0]
        reduce_passed = 0
    endif else begin
        vseeds = 0L
        value[12] = 1
        NVox = n_elements(box.bx)
        reduce_passed = 3
    endelse
  
    if arg_present(status)      then vstatus      = lonarr(NVox) else gxl_setNULL, vstatus,      value, 5 
    if arg_present(physLength)  then vphysLength  = dblarr(NVox) else gxl_setNULL, vphysLength,  value, 6 
    if arg_present(avField)     then vavField     = dblarr(NVox) else gxl_setNULL, vavField,     value, 7 
    if arg_present(startIdx)    then vstartIdx    = lonarr(NVox) else gxl_setNULL, vstartIdx,    value, 8 
    if arg_present(endIdx)      then vendIdx      = lonarr(NVox) else gxl_setNULL, vendIdx,      value, 9 
    if arg_present(apexIdx)     then vapexIdx     = lonarr(NVox) else gxl_setNULL, vapexIdx,     value, 10 
    if arg_present(seedIdx)     then vseedIdx     = lonarr(NVox) else gxl_setNULL, vseedIdx,     value, 11
    
    if arg_present(codes)       then vcodes       = lonarr(NVox) else gxl_setNULL, vcodes,       value, 22 
     
    vmaxLength = ulon64arr(1)
    if keyword_set(maxLength) then vmaxLength[0] = maxLength
    
    vtotalLength = ulon64arr(1)
    vnLines = lonarr(1)
    vnPassed = lonarr(1)
    
    vm = vmaxLength[0]
    use_coords = vm gt 0 and arg_present(coords) 
    
    if use_coords then                              vcoords      = dblarr(4*vm)    else gxl_setNULL, vcoords,      value, 18 
    if use_coords and arg_present(linesPos)    then vlinesPos    = ulon64arr(NVox) else gxl_setNULL, vlinesPos,    value, 19
    if use_coords and arg_present(linesLength) then vlinesLength = lonarr(NVox)    else gxl_setNULL, vlinesLength, value, 20 
    if use_coords and arg_present(linesIndex)  then vlinesIndex  = lonarr(NVox)    else gxl_setNULL, vlinesIndex,  value, 21 
     
    n = n_tags(_extra)
    parameterMap = replicate({itemName:'',itemvalue:0d},n+2)
    nParameters = 0;
    if n gt 0 then begin
        keys = strlowcase(tag_names(_extra))
        for i = 0, n-1 do begin
            if strcmp(keys[i], 'reduce_passed') then begin
                reduce_passed = _extra.(i)
            endif else begin    
                if strcmp(keys[i], 'line_step') then begin
                    parameterMap[nParameters].itemName = 'step'
                endif else begin
                    parameterMap[nParameters].itemName = keys[i]
                endelse
                     
                if strcmp(keys[i], 'chromo_level') then begin
                    parameterMap[nParameters].itemValue = double(_extra.(i))/wcs_rsun()/box.dr[2] *1000
                endif else begin
                    parameterMap[nParameters].itemValue = _extra.(i)
                endelse     
                nParameters = nParameters + 1
            endelse    
         endfor
    endif
    parameterMap[nParameters].itemName = 'reduce_passed'
    parameterMap[nParameters].itemValue = reduce_passed
    nParameters = nParameters + 1
    parameterMap[nParameters].itemName = '!____idl_map_terminator_key___!';

    bx = double(transpose(box.by, [1, 0, 2]))
    by = double(transpose(box.bx, [1, 0, 2]))
    bz = double(transpose(box.bz, [1, 0, 2]))
    sz = size(bx)
    s3D = sz[1:3]
  
    non_stored = CALL_EXTERNAL(lib_location, 'mfoLines', parameterMap $                 ; 0
                          , s3D, bx, by, bz $                                           ; 1-4
                          , vstatus, vphysLength, vavField $                            ; 5-7
                          , vstartIdx, vendIdx, vapexIdx, vseedIdx $                    ; 8-11
                          , vseeds, Nseeds $                                            ; 12-13                                                    
                          , vmaxLength, vtotalLength, vnLines, vnPassed $               ; 14-17
                          , vcoords, vlinesPos, vlinesLength, vlinesIndex, vcodes $     ; 18-22
                          , VALUE = value, /CDECL, /UNLOAD)

    isTransp = not isa(vseeds, /ARRAY)
    if arg_present(nPassed)    then nPassed    = vnPassed[0]
    if arg_present(status)     then status     = gxl_transpIdx(vstatus, s3D, isTransp)
    if arg_present(physLength) then physLength = gxl_transpIdx(vphysLength, s3D, isTransp)*box.dr[0]
    if arg_present(avField)    then avField    = gxl_transpIdx(vavField, s3D, isTransp)

    if arg_present(startIdx)   then startIdx   = gxl_transpIdx(gxl_inverteIndexArr(vstartIdx, s3D), s3D, isTransp)
    if arg_present(endIdx)     then endIdx     = gxl_transpIdx(gxl_inverteIndexArr(vendIdx, s3D), s3D, isTransp)
    if arg_present(apexIdx)    then apexIdx    = gxl_transpIdx(gxl_inverteIndexArr(vapexIdx, s3D), s3D, isTransp)
    if arg_present(seedIdx)    then seedIdx    = gxl_transpIdx(gxl_inverteIndexArr(vseedIdx, s3D), s3D, isTransp)
    
    if arg_present(codes)      then codes      = gxl_transpIdx(vcodes, s3D, isTransp)
    
    if arg_present(totalLength)  then totalLength = vtotalLength[0]
    
    iscoords = isa(vcoords, /ARRAY) and vtotalLength[0] gt 0
    if arg_present(nLines) and iscoords      then nLines      = vnLines[0]               else nLines      = []
    if arg_present(linesLength) and iscoords then linesLength = vlinesLength[0:nLines-1] else linesLength = []
    if arg_present(linesIndex) and iscoords  then linesIndex  = vlinesIndex[0:nLines-1]  else linesIndex  = []
    if arg_present(linesPos) and iscoords    then linesPos    = vlinesPos[0:nLines-1]    else linesPos    = []
    
    if arg_present(coords) then begin
        if iscoords then begin
            vcoords = vcoords[0:4*vtotalLength[0]-1]
            tcoords = reform(vcoords, [4, vtotalLength[0]])
            coords  = dblarr(4, vtotalLength[0])
            coords[0, *] = tcoords[1, 0:vtotalLength[0]-1]
            coords[1, *] = tcoords[0, 0:vtotalLength[0]-1]
            coords[2, *] = tcoords[2, 0:vtotalLength[0]-1]
            coords[3, *] = tcoords[3, 0:vtotalLength[0]-1]
        endif else coords = []
    endif

    return, non_stored
end
