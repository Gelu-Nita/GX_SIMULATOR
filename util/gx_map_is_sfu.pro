;+
; :Description:
;    Return 1b if an IDL map structure is already in flux / sfu units
;    (so Tb->sfu conversion must not be applied).
;
;    Checks (case-insensitive):
;      dataunit / dataunits / units  containing 'sfu'  -> sfu
;      datatype containing 'flux' (and unit not Kelvin) -> sfu
;      datatype containing 'brightness'/'temperature', or unit 'K'/'kelvin' -> not sfu
;-
function gx_map_is_sfu, map
  if size(map, /tname) ne 'STRUCT' then return, 0b

  dtype = ''
  if tag_exist(map, 'datatype') then dtype = strlowcase(strtrim(string(map.datatype), 2))
  if dtype eq '' and tag_exist(map, 'data_type') then dtype = strlowcase(strtrim(string(map.data_type), 2))

  ustr = ''
  if tag_exist(map, 'dataunits') then ustr = strlowcase(strtrim(string(map.dataunits), 2))
  if ustr eq '' and tag_exist(map, 'dataunit') then ustr = strlowcase(strtrim(string(map.dataunit), 2))
  if ustr eq '' and tag_exist(map, 'units') then ustr = strlowcase(strtrim(string(map.units), 2))

  has_sfu_unit = strpos(ustr, 'sfu') ge 0
  has_kelvin = (strpos(ustr, 'kelvin') ge 0) or (ustr eq 'k')
  has_flux_type = strpos(dtype, 'flux') ge 0
  has_tb_type = (strpos(dtype, 'brightness') ge 0) or (strpos(dtype, 'temperature') ge 0)

  if has_sfu_unit then return, 1b
  if has_flux_type and ~has_kelvin then return, 1b
  if has_tb_type or has_kelvin then return, 0b
  return, 0b
end
