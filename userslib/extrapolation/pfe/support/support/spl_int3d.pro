function spl_int3d, v,h,x=x,y=y,z=z, definite = definite,reverse = reverse
  if not keyword_set(h) then h =[1d,1d,1d]
  sz = size(v)
  result = v
  if keyword_set(definite) then begin
    if keyword_set(x) then begin
      result = dblarr(1,sz[2], sz[3])
      for iy = 0,sz[2]-1 do begin
        for iz = 0,sz[3]-1 do begin
          result[0,iy,iz] = spl_int(v[*,iy,iz],h[0],/def)
        endfor
      endfor
    endif
    if keyword_set(y) then begin
      result = dblarr(sz[1],1, sz[3])
      for ix = 0,sz[1]-1 do begin
        for iz = 0,sz[3]-1 do begin
          result[ix,0,iz] = spl_int(v[ix,*,iz],h[1],/def)
        endfor
      endfor
    endif
    if keyword_set(z) then begin
      result = dblarr(sz[1],sz[2], 1)
      for ix = 0,sz[1]-1 do begin
        for iy = 0,sz[2]-1 do begin
          result[ix,iy,0] = spl_int(v[ix,iy,*],h[2],/def)
        endfor
      endfor
    endif
    return, result  
  
  endif
  
  
  if keyword_set(x) then begin
    for iy = 0,sz[2]-1 do begin
      for iz = 0,sz[3]-1 do begin
        result[*,iy,iz] = spl_int(reform(result[*,iy,iz]),h[0],reverse = reverse)
      endfor
    endfor
  endif
  if keyword_set(y) then begin
    for ix = 0,sz[1]-1 do begin
      for iz = 0,sz[3]-1 do begin
        result[ix,*,iz] = spl_int(reform(result[ix,*,iz]),h[1],reverse = reverse)
      endfor
    endfor
  endif
  if keyword_set(z) then begin
    for ix = 0,sz[1]-1 do begin
      for iy = 0,sz[2]-1 do begin
        result[ix,iy,*] = spl_int(reform(result[ix,iy,*]),h[2],reverse = reverse)
      endfor
    endfor
  endif
  return, result
end