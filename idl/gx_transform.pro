;this fuction applies  transformation matrix to 3xN array of points
function gx_transform,data,tm,invert=invert
sz=size(data)
sztm=size(tm)
if sztm[0] ne 2 then return,data
if sztm[1] ne 4 then return,data
if sztm[2] ne 4 then return,data
if sz[0] eq 1 then begin
  if sz[1] ne 3 then return,data
  return,(transpose(matrix_multiply(transpose([data,1d]),keyword_set(invert)?invert(tm):tm)))[0:2,*]
endif
if sz[0] ne 2 then return,data
if sz[1] ne 3 then return,data
return,(transpose(matrix_multiply([[transpose(data)],[replicate(1,sz[2])]],keyword_set(invert)?invert(tm):tm)))[0:2,*]
end