
function gxBridge::Init,_extra=_extra
 catch, error_status
 if error_status ne 0 then begin
    catch, /cancel
    return, 0
 endif
 void=self->IDL_IDLBridge::Init(_extra=_extra)
 which,'gxBridge__define',outfile=outfile
 selfdir=file_dirname(outfile,/mark)
 self->SetVar,'selfdir',selfdir
 self->Execute,'cd,selfdir'
 chk=self->getvar("getenv('SSW_GEN')")
 if is_blank(chk) then begin
   self->execute, '@' + pref_get('IDL_STARTUP')
 end
 self->execute, '@gx_startup.pro'
 return,1
end

pro gxBridge::OnCallback,Status, Error
 (*self.userdata)->OnCallback,Status, Error,self
end

pro gxBridge__define
struct_hide,{gxBridge,inherits IDL_IDLBridge}
end