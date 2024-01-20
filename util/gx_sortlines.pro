pro gx_sortlines,status,startidx,endidx,redo=redo
 if (size(status))[0] ne 3 or (size(startidx))[0] ne 3 or (size(endidx))[0] ne 3 then message,'input arrays sgould all have three dimensions'
 status=byte(status)
 oidx=where((status and 16b) eq 16b,count)
 if count gt 0 and ~keyword_set(redo) then return
 idx_startidx=array_indices(startidx,startidx)
 idx_endidx=array_indices(endidx,endidx)
 oidx=where(((status and 2l) eq 2l)  and ((status and 4l) ne 4l) $
  and (((idx_startidx[2,*] le 1) and (idx_endidx[2,*] gt 1)) $
      or ((idx_startidx[2,*] gt 1) and (idx_endidx[2,*] le 1))),count) 
 if count gt 0 then status[oidx]=(status[oidx] or 16b)
end