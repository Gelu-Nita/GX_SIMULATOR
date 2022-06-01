; Purpose     : Wrapper around IDL_BRIDGE class defined in SSW (which is a child of
;               the IDL_IDL_BRIDGE that overrides SETVAR and GETVAR methods in order to allow passing structures,
;               pointers, and objects). 
;               
;               The gxBridge class assumes that the userdata is an object that has a OnCallBack method, 
;               which is automatically called by the gxBridge:OnCallMethod 
pro gxBridge::OnCallback,Status, Error
 (*self.userdata)->OnCallback,Status, Error,self
end

pro gxBridge::Reset
  ;this method resets the bridge and 
  ;makes sure that the GX EBTEL enviroment variables
  ;match the one set in the main session
  self->Execute,'message,/reset & catch,/cancel & retall' 
  ebtel=gx_ebtel_path()
  self->Execute,'ebtel=GETENV("ebtel")'
  if self->GetVar('ebtel') ne ebtel then begin
    self->Execute,'setenv, "ebtel='+ebtel+'"'
    self->Execute,'message,"ebtel enviroment variable set to '+ebtel+'",/cont'
  endif
end

pro gxBridge__define
struct_hide,{gxBridge,inherits IDL_Bridge}
end