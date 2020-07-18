function find_distance, image

  n_levels = 30
  thr = linspace(0.25, 1.0, n_levels)
  for i =0, n_levels -1 do begin
    regions = label_region(image gt (thr[i]*max(image)))
    if max(regions) eq 2 then begin
      threshold = thr[i]
      break
    endif
  endfor
  ;find 1st maximum
  im = image
  im[where(regions eq 1)] = 0
  foo = maxi(im, crd1)

  ;find 2nd maximum
  im = image
  im[where(regions eq 2)] = 0
  foo = maxi(im, crd2)

  distance = sqrt(total((crd1 - crd2)^2))
  if distance gt 20 then stop
  return, distance
end

function gx_source_distance_image, data_model, data_obs
 
    dist_model = find_distance(data_model)
    dist_obs = find_distance(data_obs)
 
  return,{ dist_model: dist_model, dist_obs:dist_obs}


end