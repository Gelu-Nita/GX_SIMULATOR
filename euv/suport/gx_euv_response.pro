function gx_euv_response,response_date,instrument,_extra=_extra
  default,response_date,reltime(/now)
  default,instrument,'aia'
  case strupcase(instrument) of
    'TRACE':begin
      response=gx_get_trace_response(response_date)
    end
    'SXT':begin
      response=gx_get_sxt_response(response_date)
    end
    'SOLO-FSI':begin
      response=gx_get_eui_response(response_date,/fsi)
    end
    'SOLO-HRI':begin
      response=gx_get_eui_response(response_date,/hri)
    end
    'STEREO-A':begin
      response=gx_get_euvi_response(response_date,/a)
    end
    'STEREO-B':begin
      response=gx_get_euvi_response(response_date,/b)
    end
    'AIA2':begin
      response=gx_get_aia_response(response_date)
    end
    else: response=gx_get_aia_response(response_date,/Tresp,_extra=_extra)
  endcase
  return,response
 end