function gx_rerender,parms_cube,orig_data=orig_data,header=header,moi=moi,idx_range=idx_range
if n_elements(parms_cube) eq 0 then begin
 gx_readlos,parms_cube,orig_data,header=header
endif
if tag_exist(header,'EBTEL') then begin
  ebtel_path=gx_ebtel_path(header.ebtel)
endif
info=header.info
nx=header.nx
ny=header.ny
rowdata=make_array([nx,(info).pixdim],/float)
dim=[nx,ny,info.pixdim]
data=make_array(dim,/float)
if tag_exist(info,'nparms') then nparms=info.nparms.value
if tag_exist(info,'rparms') then rparms=info.rparms.value
if tag_exist(info,'aparms') then begin
  aparms=moi->concatenate_aparms()
  if size(aparms,/tname) eq 'STRUCT' then begin
    info=rep_tag_value(info,aparms,'aparms',/rep)
  end
  e_arr=info.aparms.e_arr
  mu_arr=info.aparms.mu_arr
  f_arr=info.aparms.f_arr
endif
freqlist=info.spectrum.x.axis
t0=systime(/s)
case n_elements(idx_range) of
  0:idx_range=[0,ny-1]
  1:idx_range=idx_range*[1,1]
  else:
endcase
for row_idx=idx_range[0], idx_range[1] do begin
 print,strcompress(string(row_idx+1,ny,format="('computing image row ', i5,' out of', i5)"))
 rowdata[*]=0
 parms=reform(parms_cube[*,row_idx,*,*])
 result=execute(info.execute)
 data[*,row_idx,*,*,*]=rowdata
endfor
print,strcompress(string(systime(/s)-t0,format="('Computation done in ',f10.3,' seconds')"))
return,data[*,idx_range[0]:idx_range[1],*,*,*]
end