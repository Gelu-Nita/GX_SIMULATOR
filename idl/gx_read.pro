function gx_read,file
 void=obj_new('gxvolume')
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
  restore,file,/relaxed
  FixIDLBug,model
  model->UpdateDef
  return,model
 endif else return, obj_new()
end