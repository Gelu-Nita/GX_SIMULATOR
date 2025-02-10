pro gx_readlos,parms,data,grid,header=header,file=file
  if ~file_exist(file) then begin
    file=dialog_pickfile(Title='Please choose a GX Simulator LOS file to read',filter=['*.los','*.gxl','GX*.log'])
  end
 if ~file_exist(file) then return
 rec=MULTI_RESTORE(lun,file=file, header=header,/new,/verb)
 stat=fstat(lun)
 ny=(stat.size-stat.cur_ptr)/n_tags(rec,/data)
 parmdim=size(rec.parms,/dim)
 parmdim=[parmdim[0],ny,parmdim[1:*]]
 if arg_present(grid) then begin
 if tag_exist(rec,'data') then hasdata=1 else hasdata=0
   if hasdata then begin
    datadim=size(rec.data,/dim)
    datadim[1]=ny
   endif
 endif else hasdata=0
 if arg_present(grid) then begin
 if tag_exist(rec,'grid') then hasgrid=1 else hasgrid=0
   if hasgrid then begin
      griddim=size(rec.grid,/dim)
      griddim=[griddim[0],ny,griddim[1:*]]
   endif
 endif else hasgrid=0
 row=lonarr(ny)
 parms=make_array(parmdim,/double)
 if hasdata then data=make_array(datadim,/float)
 if hasgrid then grid=make_array(griddim,/float)
 for i=0,ny-1 do begin
   rec=MULTI_RESTORE(lun,file=file)
   row=rec.row
   if row lt ny then begin
     parms[*,row,*,*,*]=rec.parms
   if hasdata then data[*,row,*,*,*,*]=rec.data
   if hasgrid then grid[*,row,*,*,*,*]=rec.grid
   end
 end   
end   