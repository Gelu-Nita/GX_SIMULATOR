function gx_validbox,box
 if size(box,/tname) ne 'STRUCT' then return,0b
 validbox=1b
 bxyz_tags=['BX','BY','BZ']
 for i=0,2 do validbox=validbox and tag_exist(box,bxyz_tags[i])
 validbox=validbox or tag_exist(box,'bcube')
 if ~validbox then return,validbox
 basic_tags=['DR','ADD_BASE_LAYER','BASE','INDEX','REFMAPS','ID','EXECUTE']
 for i=0, n_elements(basic_tags)-1 do begin
  validbox=validbox and tag_exist(box,basic_tags[i])
 endfor
 return, validbox
end
