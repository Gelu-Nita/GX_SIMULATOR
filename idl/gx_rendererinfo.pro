function gx_RendererInfo,renderer,info=info
  which,'gx_simulator',outfile=outfile,/quiet
  cdir=curdir()
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    message,!ERROR_STATE.MSG,/info
    goto,invalid_renderer
  end
  dirpath=file_dirname(renderer,/mark)
  cd,dirpath
  break_file, renderer, dsk_log, dir, filename, ext
  compile_test=execute('RESOLVE_ROUTINE, filename , /COMPILE_FULL_FILE ,/either')
  cd,cdir
  par=ROUTINE_INFO(filename,/par)
  if par.num_args lt 2 or par.num_kw_args lt 1 then goto,invalid_renderer
  template=filename+',parms,rowdata'
  for i=2,par.num_args-1 do template+=','+strlowcase(par.args[i])
  for i=0,par.num_kw_args-1 do begin
    if strupcase(par.kw_args[i]) ne 'INFO' then template+=','+strlowcase(par.kw_args[i])+'='+strlowcase(par.kw_args[i])
  end
  if execute(filename+',INFO=INFO') then begin
    if size(info,/tname) ne 'STRUCT' then goto,invalid_renderer
    return,CREATE_STRUCT('execute',template,info)
  end
  invalid_renderer:
  cd,cdir
  return,!null
end