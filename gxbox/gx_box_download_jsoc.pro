function gx_box_download_jsoc,url, cache_dir = cache_dir

  if not keyword_set(cache_dir) then begin
    out_file = file_basename(url)
    sock_copy,url, out_file
    return, out_file
  endif
  local_file = str_replace(url,'http://','')
  local_file = str_replace(local_file,'//','/'); a fix of URL
  local_file = str_replace(local_file,'/',path_sep()); insert proper path separation to be compatible with windows systems
  local_file = filepath(local_file, root_dir = cache_dir)
  
  if file_test(local_file) then begin
    message,'The requested data "'+ url+ '"has been found in the cache directory. The existing file is used.',/info
  endif else begin
    message,'Downloading data from '+ url + " ...",/info
    file_mkdir,file_dirname(local_file)
    sock_copy,url, local_file
  endelse
  return, local_file

end