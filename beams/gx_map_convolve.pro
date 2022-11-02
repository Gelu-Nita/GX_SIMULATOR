pro gx_map_convolve,map,beam,spp,beam_array=beam_array,nofft=nofft,widthIn=widthIn
  default,widthIn,51
  width=widthIn
  width or=1; force it to be odd
  sz=size(beam)
  if ~valid_map(map) then begin
    message,'No valid map input, nothing to convolve. Operation aborted!',/info
    return
  endif
  dim=size(map.data,/dim)
  case sz[0] of
    2:begin 
      if n_elements(spp) eq 0 then begin
        message,'No beam pixel size provided. Operation aborted!',/info
        return
      endif
      beam2=congrid(beam, spp*sz[1]/map.dx, spp*sz[2]/map.dy, /center, cubic=-0.5) ;making a new beam with data_pixsz resolution 
      end
    1:begin
       x=(findgen(width[0])-(width[0]-1)/2d)*map.dx
       y=(findgen(width[0])-(width[0]-1)/2d)*map.dy
       Gauss2Drot, [x,y], beam, beam2,/noreform
      end
    else:begin
          message,'No beam array, nor beam ellipse provided, Operation aborted!',/info
          return
         end
  endcase
  beam2=beam2/total(beam2,/double) ;normalizing
  if keyword_set(nofft) then map.data=float(convol(double(map.data), double(beam2) ,/edge_zero)>1.e-37) else $
       map.data=float(convol_fft(double(map.data),double(beam2)));convolving
  beam_array=beam2
end
