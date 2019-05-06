function gx_root,obj
if ~obj_isa(obj,'IDLgrModel') then return, obj_new()
if obj_isa(obj,'gxModel') then return,obj
root=obj
repeat begin
  root->GetProperty,parent=root
endrep until obj_isa(root,'gxModel') or ~obj_valid(root)
if obj_isa(root,'gxModel') then return,root else return, obj_new()
end