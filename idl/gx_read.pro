function gx_read,file
 if n_elements(file) eq 0 then file=dialog_pickfile(filter='*.gxm',$
                    DEFAULT_EXTENSION='gxm',$
                    /read,/must_exist,$
                    title='Please select a file to restore a saved gxModel object')
 if file ne '' then begin
 catch, error_stat
 if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return,obj_new()
 end
 dummy=obj_new('gxvolume');update definition of te gxvolume object
 restore,file,/relaxed
  ;start provision for arbitrarily named model objects
 osav=obj_new('idl_savefile',file)
 names=osav->names()
 for i=0,n_elements(names)-1 do begin
   e=execute('model='+names[i])
 end  
 obj_destroy,osav
  ;end provision for arbitrarily named model objects
  FixIDLBug,model
  model->UpdateDef
  model->DisplayMap,2
  return,model
 endif else return, obj_new()
end