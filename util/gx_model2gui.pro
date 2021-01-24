pro gx_model2gui,model,wconsole,_extra=_extra
  if obj_isa(model,'gxmodel') then begin
    gx_message,'Uploading the model in the GX_Simulator GUI...',wconsole
    if xalive('gx_simulator',/name,id=id) then begin
      widget_control,id[0],send_event={MODEL2GX,id:0l,top:0l,handler:0l,model:model}
    endif else begin
      gx_message,'Please wait until the GX Simulator GUI is launched...',wconsole
      gx_simulator,model,_extra=_extra
    endelse
  endif 
end