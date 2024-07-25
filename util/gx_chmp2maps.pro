function gx_chmp2maps,obj_metrics
  keys=gx_getEBTELparms(obj_metrics->get(/gx_key),a,b,q0,f=formula,ebtel=ebtel)
  modI=obj_metrics->get(0,/map)
  R=modI.roi_metrics
  obsI=obj_metrics->get(1,/map)
  dx=tag_exist(obsI,'orig_xc')?(obsI.xc-obsI.orig_xc):0.0
  dy=tag_exist(obsI,'orig_yc')?(obsI.yc-obsI.orig_yc):0.0
  obsIsdev=obj_metrics->get(2,/map)
  mod_dS=modI.dx*modI.dy
  npix=obj_metrics->get(3,/roi_metrics)
  RES_NORM_MAP=obj_metrics->get(5,/map)
  res=res_norm_map.roi_metrics
  bad=where(RES_NORM_MAP.data eq 1,nbad,ncomp=ncomp)
  if nbad gt 0 then RES_NORM_MAP.data[bad]=0
  RES2_MAP=obj_metrics->get(7,/map)
  res2=RES2_MAP.roi_metrics
  CHI_MAP=obj_metrics->get(8,/map)
  chi=CHI_MAP.roi_metrics
  CHI2_MAP=obj_metrics->get(9,/map)
  chi2=CHI2_MAP.roi_metrics
  freq=obj_metrics->get(/freq)
  if n_elements(freq) eq 1 then return,{modI:modI,obsI:obsI,obsIsdev:obsIsdev,RES_NORM_MAP:RES_NORM_MAP,CHI2_MAP:CHI2_MAP,a:a,b:b,q0:q0,freq:freq,formula:formula,ebtel:ebtel,npix:npix,R:R,chi:chi,chi2:chi2,res:res,res2:res2}
  chan=obj_metrics->get(/chan)
  if n_elements(chan) eq 1 then return,{modI:modI,obsI:obsI,obsIsdev:obsIsdev,RES_NORM_MAP:RES_NORM_MAP,CHI2_MAP:CHI2_MAP,a:a,b:b,q0:q0,chan:chan,formula:formula,ebtel:ebtel,npix:npix,R:R,chi:chi,chi2:chi2,res:res,res2:res2}
  return,!null
end
