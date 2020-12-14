function gx_rerender,parms_cube,orig_data=orig_data,header=header
if n_elements(parms_cube) eq 0 then begin
 gx_readlos,parms_cube,orig_data,header=header
endif
info=header.info
sz=size(parms_cube)
nx=sz[1]
ny=sz[2]
rowdata=make_array([nx,(info).pixdim],/float)
dim=[nx,ny,info.pixdim]
data=make_array(dim,/float)
t0=systime(/s)
for row=0, ny-1 do begin
 print,strcompress(string(row+1,ny,format="('computing image row ', i5,' out of', i5)"))
 rowdata[*]=0
 parms=reform(parms_cube[*,row,*,*])
 if tag_exist(info,'nparms') then nparms=info.nparms.value
 if tag_exist(info,'rparms') then rparms=info.rparms.value
 result=execute(info.execute)
 data[*,row,*,*,*]=rowdata
endfor
print,strcompress(string(systime(/s)-t0,format="('Computation done in ',f10.3,' seconds')"))
return,data
end