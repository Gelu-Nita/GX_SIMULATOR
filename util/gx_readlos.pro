pro gx_readlos,parms,data,header=header
 file=dialog_pickfile(Title='Please choose a GX Simulator LOS file to read',filter=['*.los','*.gxl','GX*.log'])
 if ~file_exist(file) then return
 rec=MULTI_RESTORE(lun,file=file, header=header,/new,/verb)
 stat=fstat(lun)
 ny=(stat.size-stat.cur_ptr)/n_tags(rec,/data)
 parmdim=size(rec.parms,/dim)
 parmdim=[parmdim[0],ny,parmdim[1:*]]
 if tag_exist(rec,'data') then hasdata=1 else hasdata=0
 if hasdata then begin
  datadim=size(rec.data,/dim)
  datadim[1]=ny
 endif
 row=lonarr(ny)
 parms=make_array(parmdim,/float)
 if hasdata then data=make_array(datadim,/float)
 for i=0,ny-1 do begin
   rec=MULTI_RESTORE(lun,file=file)
   row=rec.row
   if row lt ny then begin
     parms[*,row,*,*,*]=rec.parms
   if hasdata then data[*,row,*,*,*,*]=rec.data
   end
 end   
end   