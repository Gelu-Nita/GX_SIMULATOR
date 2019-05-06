pro FixIDLBug,obj
 obj->GetProperty,parent=parent
 if obj_isa(obj,'IDL_Container') then begin
 all=obj->Get(/all,count=count)
 for i=0,count-1 do FixIDLBug,all[i]
 end
end