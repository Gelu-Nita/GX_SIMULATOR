pro ConvolveSSRT, map
  GetSSRTangles, map.time, dEW, dNS, pEW, pNS
  MakeSSRTbeam, dEW, dNS, pEW, pNS, 50, 50, map.dx, map.dy, xb, yb, beam
  beam=beam/total(beam) ;normalizing
  map.data=float(convol(double(map.data), double(beam) ,/edge_zero)>1.e-37)
end  