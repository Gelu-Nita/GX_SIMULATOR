pro FixIDLBug,obj
 ;this piece of code was found to fix an apparent IDL bug 
 ;resulting in issuing the message 
 ;% GX_READ: Attempt to call undefined method: 'GXVOLUME::RESTORE'
 ;at the time of attempting to retrieve model's volume vertex attribute data
 obj->GetProperty,parent=parent
 if obj_isa(obj,'IDL_Container') then begin
 all=obj->Get(/all,count=count)
 for i=0,count-1 do FixIDLBug,all[i]
 end
end