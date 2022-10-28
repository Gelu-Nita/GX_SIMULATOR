function gx_nlfff_version,returncode=returncode
  b = bytarr(512)
  b(*) = 32B
  version_info = STRING(b)
  returncode = CALL_EXTERNAL(gx_libpath('nlff'), 'mfoNLFFFVersion', version_info)
  return,version_info
end  