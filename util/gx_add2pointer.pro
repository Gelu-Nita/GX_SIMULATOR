;this procedure replaces or add tag values to an existing pointer structure
;if the new data strucure matches the previous ne, the pointer is preserved
;otherwise,  the original pointer is freed and a new pointer is created
function gx_add2pointer,pointer,replaced=replaced,_extra=_extra
  if ~isa(_extra,'struct') then return,pointer
  names=tag_names(_extra)
  for k=0, n_tags(_extra)-1 do begin
    if ptr_valid(pointer) or n_elements(data) gt 0 then begin
      if n_elements(data)  eq 0 then begin
        if tag_exist(*pointer,names[k],index=index) then begin
          if (size((*pointer).(index),/type) eq size(_extra.(k),/type)) then begin
            if (n_elements((*pointer).(index)) eq n_elements(_extra.(k))) then begin
              (*pointer).(index)=_extra.(k)
            endif else begin
              data=rep_tag_value(*pointer,_extra.(k),names[k],/no_copy,/duplicate)
              ptr_free,pointer
            endelse
          endif else begin
            data=rep_tag_value(*pointer,_extra.(k),names[k],/no_copy,/duplicate)
            ptr_free,pointer
          endelse
        endif else begin
            data=rep_tag_value(*pointer,_extra.(k),names[k],/no_copy,/duplicate)
            ptr_free,pointer
        endelse
      endif else begin
        if tag_exist(data,names[k],index=index) then begin
          if (size((data).(index),/type) eq size(_extra.(k),/type)) then begin
            if (n_elements(data.(index)) eq n_elements(_extra.(k))) then begin
              (data).(index)=_extra.(k)
            endif else begin
              data=rep_tag_value(data,_extra.(k),names[k],/no_copy,/duplicate)
            endelse
          endif else begin
            data=rep_tag_value(data,_extra.(k),names[k],/no_copy,/duplicate)
          endelse
        endif else data=rep_tag_value(data,_extra.(k),names[k],/no_copy,/duplicate)
      endelse
    endif else pointer=ptr_new(create_struct(names[k],_extra.(k)))
  endfor
  if n_elements(data) gt 0 then begin
    replaced=1
    pointer=ptr_new(data)
  endif else replaced=0
  return,pointer
end