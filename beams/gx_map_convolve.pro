pro gx_map_convolve,map,beam,ssp
  sz=size(beam)
  if ~valid_map(map) then begin
    message,'No valid map input, nothing to convolve. Operation aborted!',/cont
    return
  endif
  dim=size(map.data,/dim)
  case sz[1] of
    2:begin 
      if n_elements(spp) then begin
        message,'No beam pixel size provided. Operation aborted!',/cont
        return
      endif
      beam2=congrid(beam, spp*sz[1]/map.dx, spp*sz[2]/map.dy, /center, cubic=-0.5) ;making a new beam with data_pixsz resolution 
      end
    3:begin
       x=(findgen(51)-25)*map.dx
       y=(findgen(51)-25)*map.dy
       Gauss2Drot, [x,y], beam, beam2,/noreform
      end
    else:begin
          message,'No beam array, nor beam ellipse provided, Operation aborted!',/cont
          return
         end
  endcase
  beam2=beam2/total(beam2) ;normalizing
  map.data=float(convol(double(map.data), double(beam2) ,/edge_zero)>1.e-37) ;convolving
end
