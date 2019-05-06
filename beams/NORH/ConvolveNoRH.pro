pro ConvolveNoRH,file,map
  norh_rd_img,file, index, data
  beam=norh_beam(index) 
  dim=size(beam,/dim);21x21 array   
  spp=index.norh.sec_per_pix ;NoRH beam array resolution   
  beam=congrid(beam, spp*dim[0]/map.dx, spp*dim[1]/map.dy, /center, cubic=-0.5) ;making a new beam with data_pixsz resolution 
  beam=beam/total(beam) ;normalizing
  map.data=float(convol(double(map.data), double(beam) ,/edge_zero)>1.e-37) ;convolving
end
