pro gx_compute_coronal_model,tr_height_km,_extra=_extra
  default,tr_height_km,1000
  tr_height=tr_height_km/(gx_rsun(unit='km'))
  file=dialog_pickfile(filter='*.gxm')
  model=gx_read(file)
  model->computecoronalmodel,tr_height=tr_height,/compute,_extra=_extra
  save,model,file=file
  tr_height_km=tr_height*(gx_rsun(unit='km'))
end