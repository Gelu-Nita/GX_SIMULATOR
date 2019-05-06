function gx_box_jsoc_make_query,t1,t2,ds,segment, waves = waves
  query = ssw_jsoc_time2query(t1, t2, ds=ds)
  if keyword_set(waves) then query=query+'['+arr2str(strtrim(waves,2))+']'
  query=query+'{'+segment+'}'
  return, query[0]
end


;+
    ; :Description:
    ;    Trys to locate predownloaded data in local cache directory
    ;
    ; :Params:
    ;    dir - cache directory
    ;    query - JSOC query
    ;
    ;
    ;
    ; :Author: Sergey Anfinogentov
    ;-
function gx_box_jsoc_try_cache, dir, query
  if not file_test(dir) then file_mkdir,dir
  index_file = filepath('index.sav', root = dir)
  If not file_test(index_file) then begin
    queries = []
    files = []
    save, queries, files, file = index_file
    return, ''
  Endif
  restore, index_file
  if n_elements(queries) eq 0 then return, ''
  ind = where(queries eq query)
  if ind[0] eq -1 then return, ''
  file = files[ind]
  if not file_test(file) then begin ;File is not found
    ind = where(queries ne query)
    if ind[0] eq -1 then begin
      queries = []
      files = []
      save, queries, files, file = index_file
      return, ''
    endif
    queries = queries[ind]
    files = files[ind]
    save, queries, files, file = index_file
    return, ''
  endif
  return, file[0]
end

;+
    ; :Description:
    ;    Saves downloaded data into local cache
    ;
    ; :Params:
    ;    dir - cache directory
    ;    query - JSOC query associated with the data
    ;    data - data array
    ;    index - index structure
    ;    file - filename obtained with the GX_BOX_JSOC_MAKE_FILENAME routine
    ;
    ;
    ;
    ; :Author: Sergey Anfinogentov
    ;-
pro gx_box_jsoc_save2cache, dir, query, data, index, file
  index_file = filepath('index.sav', root = dir)
  if not file_test(dir) then file_mkdir,dir
  If not file_test(index_file) then begin
    queries = []
    files = []
  Endif else restore, index_file
  if n_elements(queries) eq 0 then begin
    queries = []
    files = [] 
  endif
  
  
 ; file = gx_box_jsoc_make_filename(index, ds, wave)
  
  date_dir = anytim(strreplace(index.t_rec,'.','-'),/ccsds,/date)
  file_mkdir,filepath( date_dir, root = dir)
  
  local_file = filepath(file, subdir = date_dir, root = dir)
  
  writefits, local_file, data, struct2fitshead(index)
  
  queries = [queries, query]
  files = [files, local_file]
  
  save, queries, files, file = index_file
  
end

function gx_box_jsoc_make_filename, index, ds, segment, wave = wave
  
  time_s = strreplace(index.t_rec,'.','')
  time_s = strreplace(time_s,':','')
  
  if keyword_set(wave) then begin
    file = ds+'.'+time_S+'.'+segment+'.'+wave+'.fits'
  endif else begin
    file = ds+'.'+time_S+'.'+segment+'.fits'
  endelse
  return, file
end


function gx_box_jsoc_get_fits, t1, t2, ds, segment, cache_dir, wave = wave

  query = gx_box_jsoc_make_query(t1,t2,ds,segment, wave = wave)
  
  result = gx_box_jsoc_try_cache(cache_dir, query)
  
  if result ne '' then return, result


  ssw_jsoc_time2data, t1, t2, index, urls, /urls_only, ds=ds, segment=segment, wave=wave, count = count
  
  if n_elements(urls) eq 0 then begin
    message, 'can not download data for ds ="'+ds+'" and wave = ' + strcompress(wave), /info
    return , ''
  endif
  
  t_request = (anytim(t1) + anytim(t2))*.5
  times_str = str_replace(strmid((index.t_obs),0,10),'.','-') + strmid((index.t_obs),10)
  t_found =anytim(times_str);anytim(index.t_obs)
  foo = min(abs(t_request - t_found),ind)
  
  index = index[ind]
  url  = urls[ind] 
  local_file = gx_box_jsoc_make_filename(index, ds, segment,wave = wave)
  tmp_dir = GETENV('IDL_TMPDIR')
  tmp_file = filepath(local_file, /tmp)
  
  sock_copy,url, tmp_file
  read_sdo, tmp_file, tmp_index, data, /uncomp_delete
  file_delete, tmp_file
  
  gx_box_jsoc_save2cache, cache_dir, query, data, index, file_basename(local_file)
  return, gx_box_jsoc_try_cache(cache_dir, query)
  
end
