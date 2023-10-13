function gx_readbox,file,info=info,first=first
  box=!null
  if n_elements(file) eq 0 then file=dialog_pickfile(filter='*.sav',$
    DEFAULT_EXTENSION='sav',$
    /read,/must_exist,$
    title='Please select a file to restore a saved GX box structure')
  if ~file_exist(file) then return,box
  osav=obj_new('idl_savefile',file)
  names=osav->names()
  for i=0,n_elements(names)-1 do begin
    catch, err
    if err ne 0 then begin
      catch,/cancel
      restore,source,/relaxed
      result=box
      goto,skip
    endif
    osav->restore,names[i]
    skip:
    e=execute('result=size('+names[i]+',/tname)')
    if result eq 'STRUCT' then begin
      e=execute('tmp=temporary('+names[i]+')')
      if gx_validbox(tmp) then begin
        box=temporary(tmp)
        if arg_present(info) and tag_exist(box,'execute') then begin
          exec=strsplit(box.execute,'&',/extract)
          n=n_elements(exec)
          if n gt 0 then begin
           case 1 of
            keyword_set(first): e=execute(exec[0]+',info=info_')
            else: e=execute(exec[n-1]+',info=info_')
           endcase
          endif else e=execute(exec[0]+',info=info_')
        endif
      endif
    endif
  endfor
  if size(info_,/tname) eq 'STRUCT' then begin
    info=temporary(info_)  
    if ~tag_exist(info,'time') then info=add_tag(info,gx_id2time(box.id),'time',/no_copy,/duplicate)
  endif
  return,box
 end 
