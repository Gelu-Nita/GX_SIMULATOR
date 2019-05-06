function gx_convolve,map,a,b,phi,beam=beam
 default,phi,0
 default,beam,gaussian_function([a,b]/[map.dx,map.dy],/normalize,width=2*size(map.data,/dim))
 beam_map=make_map(beam,xc=map.xc,yc=map.yc,dx=map.dx,dy=map.dy,time=map.time)
 beam_map=rot_map(beam_map,phi)
 sub_map,beam_map,beam_map,ref_map=map
 beam_map.roll_angle=0
 beam=beam_map.data
 result=map
 result.data=convol_fft(map.data,beam)
 return,result
end