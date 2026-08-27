;+
; Deprecated alias for gx_plotbestchmpmodels_ebtel (MW-era name; also handles EUV).
; Prefer the new name in new code.
;-
pro gx_plotbestmwmodels_ebtel, result, psDir, _ref_extra=extra
  compile_opt idl2
  message, 'gx_plotbestmwmodels_ebtel is deprecated; use gx_plotbestchmpmodels_ebtel', /info
  gx_plotbestchmpmodels_ebtel, result, psDir, _extra=extra
end
