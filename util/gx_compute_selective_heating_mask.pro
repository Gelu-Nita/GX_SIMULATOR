function gx_compute_selective_heating_mask,heat2volume
  default,heat2volume,{in:complex(1,0),$
    nw:complex(1,0),$
    enw:complex(1,0),$
    pl:complex(1.1,0),$
    fac:complex(1.2,0),$
    pen:complex(1.3,0),$
    umb:complex(0,1.4),$
    umb2:0.1,$
    mask:fltarr(7,7)}
  for i=0, 6 do begin
    for j=0,6 do begin
      base=heat2volume.(i)*heat2volume.(j)
      heat2volume.mask[i,j]=(real_part(base)+imaginary(base))
    endfor
  endfor
  heat2volume.mask[where(heat2volume.mask lt 0)]=heat2volume.umb2
  return,heat2volume
end