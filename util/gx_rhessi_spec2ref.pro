pro gx_rhessi_spec2ref
 catch, error_stat
 if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return
 end
restore,dialog_pickfile(filter='*.sav',title='Please select a RHESSI spectrum file')
t=anytim(flux.time)
if n_elements(t) gt 1 then t=mean(t)
x=flux.energy
y=x*0
y[0:n_elements(flux.flux_nonthermal)-1]=flux.flux_nonthermal
y[0:n_elements(flux.flux_thermal)-1]=y[0:n_elements(flux.flux_thermal)-1]+flux.flux_thermal
y=y#replicate(1,2)
ref={t:t,x:x,y:y}
save,ref, file=dialog_pickfile(filter='*.ref',title='Please choose a RHESSI ref filename')
end