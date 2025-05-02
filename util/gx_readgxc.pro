function gx_readgxc,file
  if n_elements(file) eq 0 then file=dialog_pickfile(filter='*.gxc',$
    DEFAULT_EXTENSION='gxc',$
    /read,/must_exist,$
    title='Please select a file to restore a saved GXC data structure')
  if file ne '' then begin
    catch, error_stat
    if error_stat ne 0 then begin
      catch, /cancel
      MESSAGE, /INFO, !ERROR_STATE.MSG
      return,obj_new()
    end
    restore,file
    return,gxcube
   end
   return,!null
 end   