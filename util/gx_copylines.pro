;To copy field ines from one model to another
;WARNING: It is assumed that the reference and destination models have the same grid!
;WARNING: The destination file is overwritten!
;Use remove_all keyword to delete all already existent fieldlines from the destination model.
;If remove_all is not set, duplicate lines may be created

pro gx_copylines,reffile,destfile,remove_old=remove_old
 if n_elements(reffile) eq 0 then reffile=dialog_pickfile(filter='*.gxm',title='Please select the reference object file to copy field lines from' )
 if n_elements(destfile) eq 0 then destfile=dialog_pickfile(filter='*.gxm',title='Please select the destination object file to add lines to' )
 ref=gx_read(reffile)
 dest=gx_read(destfile)
 if keyword_set(remove_old) then begin
  old=dest->get(/all,isa='gxbline',count=n)
  for k=0,n-1 do begin
    dest->remove,old[k]
    obj_destroy,old[k]
  endfor
 endif
 all=ref->get(/all,isa='gxbline',count=count)
 for k=0,count-1 do begin
  all[k]->GetProperty,data=data
  data=data[*,where(data[2,*] ge 0)]
  z0=min(data[2,*],imin)
  footpoint=reform(data[*,imin])
  print, 'adding line ',k+1,' out of ',count, 'at ', footpoint
  line=dest->GetBline(footpoint)
  dest->add,line
 endfor
 obj_destroy,ref
 model=dest
 save,model,file=destfile; gx_simulator expects to find a variable named model in the gxm file
 obj_destroy,dest
end