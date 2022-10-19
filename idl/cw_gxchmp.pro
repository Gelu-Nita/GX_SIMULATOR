function gxchmp::INIT,wBase,uname=uname, GXMpath=GXMpath,RefDataPath=RefDataPath,modDir=modDir,psDir=psDir,$
                       RefDataStruct=RefDataStruct,TmpDir=TmpDir,alist=alist,blist=blist,qlist=qlist,$
                       levels=levels,solution=solution, renderer=renderer,EBTELpath=EBTELpath,$
                       fov=fov,res=res,_extra=_extra
  compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return, 0
  end
  self.WinOS=!version.os_family eq 'Windows'
  default,modDir,curdir()+path_sep()+'moddir'
  self.modDir=modDir
  default,psDir,curdir()+path_sep()+'PsDir'
  self.psDir=psDir
  default,tmpDir,curdir()+path_sep()+'tmpDir'
  self.tmpDir=TmpDir
  default,refdatapath,''
  self.refdatapath=refdatapath
  default,gxmpath,''
  self.gxmpath=gxmpath
  default,EBTELpath,gx_findfile('ebtel.sav',folder='')
  self.EBTELpath=EBTELpath
  default,renderer,gx_findfile(self.WinOS?'AR_GRFF_nonLTE.pro':'mwgrtransfer.pro',folder='')
  self.renderer=renderer
  info_renderer=gx_rendererinfo(renderer)
  valid=size(info_renderer,/tname) eq 'STRUCT'
  if valid eq 1 then begin
    self.renderer=renderer
    ptr_free,self.info_renderer
    self.info_renderer=ptr_new(info_renderer)
  endif
  default,alist,''
  self.alist=alist
  default,blist,''
  self.blist=blist
  default,qlist,''
  self.qlist=qlist
  default,levels,'12,20,30,50,80'
  self.levels=levels
  default,fov,'center=[0.0,0.0]; range=[100.0,100.0]'
  self.fov=fov
  default,res,'raw=[200,200]; resize=[100,100]'
  self.res=res
  default,tasks,list()
  self.tasks=tasks
  default,solution,list()
  self.solution=list()
  self.bridges=obj_new('IDL_Container')
  void=self->gxWidget::Init(wBase,self,KILL_NOTIFY='objgxchmpKill',_extra=_extra)
  self->SetBridges
  return,1
end

pro gxchmp__define
  struct_hide,{gxchmp, inherits gxwidget,Id:0l,GXMpath:'',RefDataPath:'',modDir:'',psDir:'',TmpDir:'',EbtelPath:'',$
    renderer:'',alist:'',blist:'',qlist:'',levels:'',fov:'',res:'',completed:0l,$
    RefDataStruct:ptr_new(),solution:obj_new(),bridges:obj_new(),tasks:obj_new(),WinOS:0l,displays:lonarr(5),info_renderer:ptr_new()}
end

pro objgxchmpKill,wBase
  widget_control,wBase,get_uvalue=obj
  obj->GetProperty,bridges=bridges,solution=solution,info_renderer=info_renderer
  if isa(bridges, 'OBJ') then obj_destroy,bridges
  if isa(solution, 'OBJ') then obj_destroy,solution
  if ptr_valid(info_renderer) then ptr_free,info_renderer
  obj->GetProperty,GXMpath=GXMpath,RefDataPath=RefDataPath,modDir=modDir,psDir=psDir,tmpDir=tmpDir,$
    alist=alist,blist=blist,qlist=qlist,levels=levels,renderer=renderer,$
    fov=fov,res=res, ebtelpath=ebtelpath
  save,GXMpath,RefDataPath,modDir,psDir,tmpDir,alist,blist,qlist,levels,renderer,fov,res,ebtelpath,file='gxchmp.ini'
  obj_destroy,obj
end


function gxchmp::SaveSolution,question=question
  solution_unsaved=(self.solution->Count() gt 0)
  if keyword_set(question) then begin
      if solution_unsaved gt 0 then begin
         answ=dialog_message('The search results located at '+self.tmpDir+path_sep()+'gxchmp_solution.sav '+ $
      'will be overwritten if a new search is performed! Do you want to save this solution to a permanent location before proceeding further?',/question,/cancel)
      endif else answ='No'
  endif else answ=(solution_unsaved gt 0)?'Yes':'No' 
  if strupcase(answ) eq 'YES' then begin
    file=dialog_pickfile(filter='*.sav',title='Select a filename to save the current solution',/write,/overwrite)
    if file ne '' then begin
      result=self.solution->ToArray()
      save,result,file=file
      self.completed=0
    endif else answ='Cancel'
  endif
  return,answ
end

pro gxchmp::Solution2PS
  tvlct,rgb_curr,/get
  widget_control,widget_info(self.wBase,find_by_uname='color_table'),get_uvalue=rgb
  if n_elements(rgb) gt 0 then tvlct,rgb
  gx_plotbestmwmodels_ebtel, self.solution->ToArray(), self.psDir,levels=self->str2arr(self.levels)
  tvlct,rgb_curr
end

pro gxchmp::message,msg,_extra=_extra
  wConsole=widget_info(self.wIDBase,find_by_uname='console')
  gx_message,msg,wConsole,_extra=_extra
  widget_control,wConsole,get_value=txt
  widget_control,wConsole,SET_TEXT_TOP_LINE=(n_elements(txt)-4)>1
end

function gxchmp::valid_path,path
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    answ=dialog_message([path,'is not a valid path name!'],/INFO)
    return, 0
  end 
 dir=file_dirname(path)
 if ~(file_info(dir)).exists then error=not_a_valid_path 
 base=file_basename(path)
 tmpdir=filepath('',/tmp)+path_sep()+base
 file_mkdir,tmpdir
 file_delete,tmpdir
 return,1
end

pro gxchmp::SetBridges, nbridges
  default,nbridges,0
  
  if (self.bridges->Count() gt nbridges) then begin
    n=self.bridges->Count()-nbridges
    self->message,string(n,format=(n gt 1)?"('Removing ',g0,' bridges')":"('Removing ',g0,' bridge')"),/info
    for i= nbridges, self.bridges->Count()-1  do begin
      bridge=self.bridges->Get(position=self.bridges->Count()-1 )
      self.bridges->Remove,bridge
      obj_destroy,bridge
    endfor
    goto,exit_point
  endif
  
  if (self.bridges->Count() eq nbridges) then begin
    if nbridges gt 0 then answ=dialog_message('Requested number of threads matches the number already existing, nothing to be done!',/info)
    goto,exit_point
  endif
    
  if (nbridges gt !CPU.HW_NCPU )then begin
    answ=dialog_message(string(!CPU.HW_NCPU, nbridges,format="('The number of threads is by default limited to the ',g0, ' available CPUs! Are you sure you want to set the number of bridges to ', g0,'?')"))
    if answ eq 'No' then goto,exit_point
  endif
  
  if not file_test(self.tmpDir) then file_mkdir,self.tmpDir
  wStatus=widget_info(self.wBase,find_by_uname='bridge_status')
 
  if (self.bridges->Count() lt nbridges) then begin
   n=nbridges-self.bridges->Count()
   self->message,string(n,format=(n gt 1)?"('Adding ',g0,' parallel threads')":"('Adding ',g0,' parallel thread')"),/info
   start_index=self.bridges->Count()
   status={status:'Init',task:'',time:'',calls:'',error:''}
   if start_index gt 0 then begin
     widget_control,wStatus,get_value=bridge_status
     bridge_status=[bridge_status,replicate(status,nbridges-start_index)]
   endif else bridge_status=replicate(status,nbridges)
   for i=start_index,nbridges-1 do begin
     widget_control,widget_info(self.wBase,find_by_uname='bridge_status'),set_value=bridge_status,table_ysize=n_elements(bridge_status)
     self->message,string(i,format="('Initializing thread #',i3)"),/info
     bridge=obj_new('gxBridge',userdata=self,out=self.tmpdir+path_sep()+strcompress('Bridge'+string(i)+'.log',/rem))
     if obj_valid(bridge) then begin
       bridge->SetVar,'id',i
       code=bridge->Status(error=error)
       case code of
         0:bridge_status[i].status='Idle'
         1:bridge_status[i].status='Active'
         2:bridge_status[i].status='Completed'
         3:bridge_status[i].status='Error'
         4:bridge_status[i].status='Aborted'
         else:bridge_status[i].status='Unknown'
       endcase
       bridge_status[i].error=error
       self.bridges->Add,bridge
       widget_control,widget_info(self.wBase,find_by_uname='bridge_status'),set_value=bridge_status,table_ysize=n_elements(bridge_status)
     end
   end
  end  
  exit_point:
  self->UpdateBridgeStatus
end  
               
pro gxchmp::UpdateBridgeStatus,reset=reset,oncallback=oncallback
    wStatus=widget_info(self.wBase,find_by_uname='bridge_status')
    nbridges=self.bridges->Count()
    if keyword_set(reset) and nbridges gt 0 then begin
      bridge_status=replicate({status:'Init',task:'',time:'',calls:'',error:''},nbridges)
    endif else widget_control,wStatus,get_value=bridge_status
    bridge_status=bridge_status[0:(n_elements(bridge_status)<nbridges)-1]
    active=0
    for i=0,nbridges-1 do begin
      bridge=self.bridges->Get(position=i)
      code=bridge->Status(error=error)
      case code of
        0:bridge_status[i].status='Idle'
        1:begin 
            bridge_status[i].status='Active'
            active+=1
          end  
        2:bridge_status[i].status='Completed'
        3:bridge_status[i].status='Error'
        4:bridge_status[i].status='Aborted'
        else:bridge_status[i].status='Unknown'
      endcase
      bridge_status[i].error=error
      if n_elements(oncallback) gt 0 then begin
        if oncallback eq i then begin
          id=bridge->GetVar('id')
          task_id=bridge->GetVar('task_id')
          calls=bridge->GetVar('calls')
          t_start=bridge->GetVar('t_start')
          t_end=bridge->GetVar('t_end')
          a=bridge->GetVar('a')
          b=bridge->GetVar('b')
          bridge_status[i].task=strcompress(string(a,b,format="('a=',a0,', b=',a0)"),/rem)
          bridge_status[i].time=strcompress(string(t_end-t_start,format="(g0,'s')"),/rem)
          bridge_status[i].calls=strcompress(string(calls,format="(i0)"),/rem)
        end
      endif
    endfor
  widget_control,wStatus,set_value=bridge_status,table_ysize=n_elements(bridge_status)
  self->ControlWidgets,sensitive=~active
end          

function gxchmp::list2dblarr,strlist
  s='['+arr2str(str2arr(str_replace(str_replace(str_replace('['+arr2str(strlist)+']','[',''),']',''),'d',''))+'d')+']'
  code=execute('arr='+s)
  return,arr  
end

function gxchmp::SolutionParmList
 parmlist=list()
 for i=0,self.solution->Count()-1 do begin
   parmlist->Add,{a:float(self.solution(i).a),b:float(self.solution(i).b),q:float(self.solution(i).q_start)}
 endfor
 return,parmlist
end

pro gxchmp::tasks_remove, a, b
 match=self->match(self.tasks,{a:a,b:b},count=count)
 for k=0,count-1 do self.tasks->remove,match[k]
end

pro gxchmp::solution_remove, a, b
  match=self->match(self.solution,{a:a,b:b},count=count)
  for k=0,count-1 do self.solution->remove,match[k]
end

pro gxchmp::AddTasks
    if self.alist eq '' or self.blist eq '' or self.qlist eq '' then return
      a=float(self->list2dblarr(self.alist))
      b=float(self->list2dblarr(self.blist))
      q=minmax(self->list2dblarr(self.qlist))
    for i=0,n_elements(a)-1 do begin
      for j=0,n_elements(b)-1 do begin
        self->tasks_remove, a[i], b[j]
        self.tasks->add,{id:self.tasks->Count(),a:float(a[i]),b:float(b[j]),q:float(q),status:'pending',completed:0b}
      endfor
    endfor
    self.alist=''
    self.blist=''
    widget_control,widget_info(self.wbase,find_by_uname='alist'),set_value=self.alist
    widget_control,widget_info(self.wbase,find_by_uname='blist'),set_value=self.blist
    self->UpdateTaskQueue
end   
    
pro gxchmp::UpdateTaskQueue
    rows=[]
    rows_labels=[]
    for k=0,self.tasks->Count()-1 do begin
      task=self.tasks(k)
      rows_labels=[rows_labels,string(task.id,format='(g0)')]
      q=task.q
      rows=[rows,{a:string(task.a,format="(g0)"),$
                  b:string(task.b,format="(g0)"),$
                  q1:string(min(task.q),format="(g0)"),$
                  q2:string(max(task.q),format="(g0)"),$
                  status:task.status}]
    endfor
      wQueue=widget_info(self.wBase,find_by_uname='Queue')
      widget_control,wQueue,table_ysize=n_elements(rows)
      if n_elements(rows) gt 0 then widget_control,wQueue,set_value=rows,row_labels=[rows_labels]
      self->PlotTasks
end          

pro gxchmp::GetProperty,GXMpath=GXMpath,RefDataPath=RefDataPath,modDir=modDir,psDir=psDir,$
                       RefDataStruct=RefDataStruct,TmpDir=TmpDir,alist=alist,blist=blist,qlist=qlist,$
                       levels=levels,solution=solution, tasks=tasks,EBTELpath=EBTELpath,$
                       renderer=renderer,bridges=bridges,fov=fov,res=res,nBridges=nBridges,completed=completed,info_renderer=info_renderer,$
                       _ref_extra=extra
    GXMpath=self.GXMpath
    RefDataPath=self.RefDataPath
    modDir=self.modDir
    psDir=self.psDir
    TmpDir=self.tmpDir
    RefDataStruct=self.RefDataStruct
    renderer=self.renderer
    solution=self.solution
    tasks=self.tasks
    bridges=self.bridges
    alist=self.alist
    blist=self.blist
    qlist=self.qlist
    levels=self.levels
    EBTELpath=self.EBTELpath
    nBridges=ptr_valid(self.Bridges)?n_elements(*self.Bridges):0
    res=self.res
    fov=self.fov
    completed=self.completed
    info_renderer=self.info_renderer
    self->gxwidget::GetProperty,_extra=extra
end

function gxchmp::str2arr,strlist
 compile_opt idl2, static
 if strcompress(strlist,/rem) eq '' then return, !null
 result=execute('list=['+strlist+']')
 if n_elements(list) eq 0 then begin
  answ=dialog_message(['Invalid syntax!', 'A comma separated list of numbers or a start:end:delta triad is expected.'],/error)
 endif
 return,list
end

function gxchmp::Arr2Str,arr
  return,arr2str(trim(arr),delim=", ")
end

pro gxchmp::EditTasks,action
  default,action,0
  case action of
    0:  widget_control,widget_info(self.wIdBase,find_by_uname='input_base'),map=1
    1:  widget_control,widget_info(self.wIdBase,find_by_uname='input_base'),map=0
    2:  begin 
         widget_control,widget_info(self.wIdBase,find_by_uname='input_base'),map=0
         self->AddTasks
        end 
    else:
  endcase
end
function gxchmp::TasksTemplate
  return,{VERSION: 1.00000,$
    DATASTART: 1,$
    DELIMITER:',',$
    MISSINGVALUE: !values.f_nan,$
    COMMENTSYMBOL:';',$
  FIELDCOUNT: [4L],$
    FIELDTYPES: [7L, 7, 7, 7],$
    FIELDNAMES:['a','b','q1','q2'],$
    FIELDLOCATIONS: [0l, 5l, 9l, 15l],$
    FIELDGROUPS: [0L, 1, 2, 3]}
end

pro gxchmp::ExportTasks
  tasks=obj_clone(self.tasks)
  if ~obj_valid(tasks) then begin
    answ=dialog_message('No tasks to be saved!',/info)
    return
  endif
  l=tasks
  completed=l.map(Lambda(l:l.completed eq 1))
  if ~completed.IsEmpty() then begin
    completed=where(completed.toarray(),count)
    if count gt 0 then begin
      tasks.remove, completed
      file=dialog_pickfile(default_extension=['*.sav'],title='Please select a filename to save the not yet completed task list',/write)
      if file eq '' then return
      save,tasks,file=file
    endif else  begin
      answ=dialog_message('All tasks have been completed, do you have to save a copy of all tasks?',/question)
      if strupvcase(answ) eq 'YES' then begin
        file=dialog_pickfile(default_extension=['*.sav'],title='Please select a filename to save the current task list',/write)
        if file eq '' then return
        save,tasks,file=file
      endif
    endelse
  end
end

pro gxchmp::ImportTasks
 template=self->TasksTemplate()
 file=dialog_pickfile(filter=['*.sav','*.csv','*.txt'],title='Please select a GXCMP compatible task list SAV or CSV file',/must_exist)
 if file eq '' then return
 break_file, file, disk_log, dir, filnam, ext,/last_dot
 case strlowcase(ext) of
  '.csv':begin
          tasks=read_ascii(file,template=template)
          a=self->str2arr(arr2str(tasks.a))
          b=self->str2arr(arr2str(tasks.b))
          q1=self->str2arr(arr2str(tasks.q1))
          q2=self->str2arr(arr2str(tasks.q2))
          if(n_elements(a) eq 0) or $
            (n_elements(b) eq 0) or $
            (n_elements(q1) eq 0) or $
            (n_elements(q2) eq 0) then begin
            answ=dialog_message(['Invalid CSV file content!', 'A comma separated list of numbers is expected.'],/error)
            return
          endif else begin
            for i=0,n_elements(a)-1 do begin
              self->tasks_remove, a[i], b[i]
              self.tasks->add,{id:self.ID++,a:float(a[i]),b:float(b[i]),q:float([q1[i],q2[i]]),status:'pending',completed:0b}
            endfor
          end  
         end
     '.sav': begin
               osav=obj_new('idl_savefile',file)
               names=osav->names()
               valid=0
               for i=0,n_elements(names)-1 do begin
                 osav->restore,names[i];,/RELAXED_STRUCTURE_ASSIGNMENT
                 e=execute('vartype=size('+names[i]+',/tname)')
                 if (vartype eq 'STRUCT') or (vartype eq 'OBJREF') then begin
                   e=execute('tasks=temporary('+names[i]+')')
                   tag_names=strlowcase(tag_names(tasks(0)))
                   tags=[where(tag_names eq 'a'),where(tag_names eq 'b'),where(tag_names eq 'q' or tag_names eq 'q_start')]
                   if tags[0] eq -1 or tags[1] eq -1 then begin
                     answ=dialog_message('The structure or list object restored from this file does not have the reqired tags "a",and "b"',/error)
                     return           
                   endif
                   count=(vartype eq 'STRUCT')?n_elements(tasks):tasks->Count()
                   for k=0,count-1 do begin
                     a=tasks(k).(tags[0])
                     b=tasks(k).(tags[1])
                     q=(tags[2] ne -1)?minmax(tasks(k).(tags[2])):[0.001,0.01]
                     self->tasks_remove, a, b
                     self.tasks->add,{id:self.ID++,a:float(a),b:float(b),q:float(q),status:'pending',completed:0d}
                   endfor
                  endif  
               endfor
              end   
     else:answ=dialog_message(['Invalid file content!',$
                               'A GXMCHP comma separated text file is expected',$
                               'or and IDL save file containg an GXMCHP compatible structure or LIST object',$
                               'having the minimum required tags "a","b" and "q"'],/info)   
 endcase
   self->UpdateTaskQueue
end

pro gxchmp::abort,question=question
  if keyword_set(question) then begin
   answ=dialog_message('Do you want to abort all running tasks?',/question)
  endif else answ='YES'
  if strupcase(answ) eq 'YES' then begin 
    bridges=self.bridges->Get(/all,count=count)
    for i=0,count-1 do begin
      code=bridges[i]->Status(error=error)
      if code eq 1 then bridges[i]->Abort
    end
  end
end  

pro gxchmp::delete_pending,question=question
  if keyword_set(question) then begin
    answ=dialog_message("Delete all pending tasks?",/question)
  endif else answ='YES'
  if strupcase(answ) eq 'YES' then begin
    l=self.tasks
    pending=l.map(Lambda(l:strupcase(l.status) eq 'PENDING'))
    if ~pending.IsEmpty() then begin
      pending=where(pending.toarray(),count)
      if count gt 0 then self.tasks.remove, pending
    end
    self->UpdateTaskQueue
  end
end

pro gxchmp::flush_queue,question=question
  if keyword_set(question) then begin
   answ=dialog_message("Abort all active tasks and flush the task queue?",/question)
  endif else answ='YES'
  if strupcase(answ) eq 'YES' then begin
    self->abort
    self.tasks->Remove,/all
    self->UpdateTaskQueue
  end
end

pro gxchmp::delete_solutions,question=question
  answ=self.SaveSolution(question=question)
  if strupcase(answ) ne 'CANCEL' then self.solution->Remove,/all
end

pro gxchmp::AssignTask,bridge,task_id,OnStartSearch=OnStartSearch
    task=self.tasks[task_id]
    if ~keyword_set(OnStartSearch) then begin
      bridge->SetVar,'info',*self.info_renderer
      calls=bridge->GetVar('calls')
      bridge->SetVar,'calls',calls+1
    endif else bridge->SetVar,'calls',0
    bridge->SetVar,'task_id',task.id
    bridge->SetVar,'t_start',systime(/s)
    bridge->SetVar,'a',task.a
    bridge->SetVar,'b',task.b
    bridge_id=bridge->GetVar('id')
    task.status=string(bridge_id,format="('#',i0)")+'@'+strmid(systime(),11,8)
    self.tasks[task_id]=task
    bridge->Execute,'delvar,result'
    bridge->Execute,self->GetScript(task_id=task_id),/nowait
    self->UpdateTaskQueue
  end
  
function gxchmp::IsCompatible,result
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    return, 0
  end
 if self.solution->IsEmpty() then return,1
 dummy=[self.solution(0),result[0]]
 if ~(tag_exist(self.solution(0),'refdatapath') and tag_exist(result[0],'refdatapath') $
  and tag_exist(self.solution(0),'gxmpath') and tag_exist(result[0],'gxmpath')) then return,1 ;hopefully the users know what their are doing
 return, file_basename(self.solution(0).refdatapath) eq file_basename(result[0].refdatapath) and $
         file_basename(self.solution(0).gxmpath) eq file_basename(result[0].gxmpath)
end
pro gxchmp::AddSolution,result
  if ~self.solution->IsEmpty() then begin
    old=self.solution->ToArray()
    self.solution->Remove,/all
    for k=0,n_elements(old)-1 do begin
      match=where(result.a eq old[k].a and result.b eq old[k].b, count)
      if count eq 0 then result=[result,old[k]]
    endfor
  endif
  self.solution->Add,result,/extract
  self->DisplaySolution,/best
end 

function gxchmp::Match,alist,anitem,count=count
  compile_opt idl2, static
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    count=0
    return,-1
  end
 l=alist
 cmd=string(anitem.a,anitem.b,format="('match=l.map(Lambda(l:(l.a eq ', g0,') and (l.b eq ', g0,')))')")
 void=execute(cmd)
 return,where(match->ToArray(),count)
end 


pro gxchmp::AddResult,result
  match=self.Match(self.solution,result,count=count)
  for k=0,count-1 do self.solution->remove,match[k]
  self.solution->add,result
  solfile=strcompress(self.tmpDir+path_sep()+'gxchmp_solution.sav')
  result=self.solution->ToArray()
  save,result,file=solfile
end
  
  
function gxchmp::GetScript,task_id=task_id
script='result=gx_search4bestq('
script+='  gxmpath="'+self.gxmpath+'"'
script+=', modDir="'+self.moddir+'"'
script+=', psDir="'+self.psdir+'"'
script+=', tmpDir="'+self.tmpdir+'"'
script+=', refdatapath="'+self.refdatapath+'"'
script+=', ebtel_path="'+self.ebtelpath+'"'
script+=', renderer="'+self.renderer+'"'
str_arr=str2arr(self.res,';')
for k=0,0 do begin
  err=execute(str_arr[k])
endfor
script+=strcompress(string(raw,str_arr[1],format="(', Nx=',i0,', Ny=', g0,', ',a0)"))
str_arr=str2arr(self.fov,';')
for k=0,n_elements(str_arr)- 1 do begin
  err=execute(str_arr[k])
endfor
script+=strcompress(string(center,range,format="(', xc=',g0,', yc=', g0,', xfov=',g0,', yfov=', g0)"))

if widget_valid(self.wIDBase) then begin
  wRedo=widget_info(self.wIDBase,find_by_uname='redo')
  if widget_valid(wRedo) then begin
   widget_control,wRedo,get_value=redo
   script+=strcompress(string(redo[0],format="(', redo= ',i2)"))
  endif
endif

default,task_id,0
task=self.tasks[task_id]
script+=strcompress(string(task.a,format="(', a_arr= ',g0)"))
script+=strcompress(string(task.b,format="(', b_arr= ',g0)"))
script+=', q_start=['+arr2str(task.q)+']'
script+=string(arr2str((str2arr(', levels=['+self.levels+']')),format="(a0)"))
script+=')'
return,strcompress(script)
end

pro gxchmp::OnStartSearch
if self->check_fields() eq 0 then return
if self.bridges->Count() eq 0 then begin
  answ=dialog_message('At least one parallel thread should be intialized before the search is started! Please do so and try again!',/info)
  return
endif
if self.completed gt 0 then begin
  answ=self->SaveSolution(/question)
  if strupcase(answ) eq 'CANCEL' then return
endif
self.completed=0
self->ControlWidgets,sensitive=0
ntasks=self.tasks->Count()
nbridges=self.bridges->Count()
i=0
j=0
while i lt ntasks do begin
  if j lt nbridges then begin
   if (self.tasks[i]).status eq 'pending' then begin
     bridge=self.bridges->Get(position=j)
     self->AssignTask,bridge,i,/OnStartSearch
     wait,1; to prevent disk access clash
     j++
   endif 
  i++
  endif else i=ntasks
endwhile
end

pro gxchmp::OnCallback,Status, Error,bridge
  bridge->SetVar,'t_end',systime(/s)
  id=bridge->GetVar('id')
  if status eq 4 then begin
    self->UpdateBridgeStatus,oncallback=id
    return
  endif
  self->UpdateBridgeStatus,oncallback=id
  ntasks=self.tasks->Count()
  task_id=bridge->GetVar('task_id')
  task_id=(where((self.tasks->ToArray()).id eq task_id))[0]
  result=bridge->GetVar('result')
  task=self.tasks(task_id)
  t_start=bridge->GetVar('t_start')
  t_end=bridge->GetVar('t_end')
  if n_elements(result) eq 0 then begin
    msg='Aborted after '
    goto,update_task
  endif
  if self.solution->Count() gt 0 then begin
    if n_tags(result) eq n_tags(self.solution(0)) then begin
     tmp=self.solution(0)
     copy_struct,result,tmp
     self->AddResult,tmp
     msg='C@'+strmid(systime(),11,8)+' in '
    endif else msg='Aborted after '
  endif else begin
    self->AddResult,result
    msg='C@'+strmid(systime(),11,8)+' in '
  endelse
  update_task:
  task.status=strcompress(string(msg,t_end-t_start,format="(a0,g0,'s')"))
  task.completed=1
  self.tasks(task_id)=task
  self->UpdateTaskQueue
  self.completed+=1
  self->message,strcompress(string(n_elements(self.solution), ntasks, task.a, task.b, format="('Completed ', i0,' tasks out of ',i0,'; a=',g0, ' b=', g0)"))
  self->DisplaySolution,/best
  skip:
  if self.completed ne ntasks then begin
  i=0
  while i lt ntasks do begin
  if (self.tasks[i]).status eq 'pending' then begin
    wait,1
    self->AssignTask,bridge,i
    i=ntasks
  endif else i++
  endwhile 
  endif else self->OnEndSearch
end

pro gxchmp::ControlWidgets,sensitive=sensitive
 widget_list=['settings_base']
 for k=0,n_elements(widget_list)-1 do begin
  wid=widget_info(self.wIDBase,find_by_uname=widget_list[k])
  if widget_valid(wid) then widget_control,wid,sensitive=keyword_set(sensitive)
 endfor
end

pro gxchmp::OnEndSearch
  self->ControlWidgets,/sensitive
  solfile=strcompress(self.tmpDir+path_sep()+'gxchmp_solution.sav')
  result=self.solution->ToArray()
  save,result,file=solfile
  self->message,'Solution temporary saved in '+solfile
  self->message,'Generating the best solutions plots...'
  self->Solution2PS
  files=[strcompress(self.psDir+path_sep()+'BestRES.ps'),strcompress(self.psDir+path_sep()+'BestCHI.ps')]
  if n_elements(result) ge 4 then files=[files,strcompress(self.psDir+path_sep()+'Best of Bests.ps')]
  self->message,['Best solutions plots saved to:..',files]
end

pro slider_set_value,id,value
  widget_control,id,get_uvalue=label
  widget_control,label.(0),set_value=string(strlowcase((tag_names(label))[0]),label.list[value],format="(a0,'=',g0)")
  widget_control,id,pro_set_value=''
  widget_control,id,set_value=value
  widget_control,id,pro_set_value='slider_set_value'
end

pro gxchmp::CreatePanel,xsize=xsize,ysize=ysize
  device, get_screen_size=scr
  subdirectory=['resource', 'bitmaps']
  if not exist(xsize) then xsize = fix (scr[0] * .45)
  if not exist(ysize) then ysize = xsize *1.1
  
  
  main_Base=widget_base(self.wBase,/row)
  left_base=widget_base(main_base,/column)
  right_base=widget_base(main_base,/column)

  toolbar= widget_base(left_base, /row,/toolbar)
  wOpen=widget_button(toolbar,value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)),tooltip=' Upload saved solution',/bitmap,uname='open')
  tvlct,rgb_curr,/get
  loadct,39,/silent
  tvlct,rgb,/get
  tvlct,rgb_curr
  wPalette = widget_button( toolbar, $
    value=gx_bitmap(filepath('palette.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Change Color Table',uname='color_table',uvalue=rgb)
  wPlot=widget_button(toolbar,value=gx_bitmap(filepath('plot.bmp', subdirectory=subdirectory)),tooltip=' Send results to PS file',/bitmap,uname='2ps')
  wSave=widget_button(toolbar,value=gx_bitmap(filepath('save.bmp', subdirectory=subdirectory)),tooltip='Save search results',/bitmap,uname='save') 
  toolbar= widget_base(right_base, /row,/toolbar)
  wExecute=widget_button(toolbar,value=gx_bitmap(gx_findfile('play.bmp')),tooltip='Execute Search Tasks',/bitmap,uname='execute')
  wAbort=widget_button(toolbar,value=gx_bitmap(gx_findfile('abort.bmp')),tooltip='Abort all running tasks',/bitmap,uname='abort')
  wAddTasks=widget_button(font=font, toolbar, $
    value=gx_bitmap(filepath('ascii.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Add tasks',uname='add_tasks')
  wImportTasks=widget_button(font=font, toolbar, $
    value=gx_bitmap(filepath('importf.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Import task list',uname='import_tasks')
  wExportTasks=widget_button(font=font, toolbar, $
    value=gx_bitmap(filepath('export.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Export task list',uname='export_tasks')
  wCleanTasks= widget_button(font=font, toolbar, $
    value=gx_bitmap(gx_findfile('clean.bmp')), $
    /bitmap,tooltip='Delete all pending tasks',uname=+'delete_pending')
  
  wFlush=widget_button(toolbar,value=gx_bitmap(gx_findfile('cancel.bmp')),tooltip='Flush the task queue',/bitmap,uname='flush_queue')

  
  wClearLog=widget_button(toolbar,value=gx_bitmap(filepath('delete.bmp', subdirectory=subdirectory)),tooltip='Clear execution log',/bitmap,uname='clearlog')

  
  wLabelBase=widget_base(right_base,/row,scr_xsize=scr_xsize,/frame)
  wLabelBase=widget_base(wLabelBase,/row)
  wlabel=widget_label(wLabelBase,value='Execution Log',scr_xsize=scr_xsize)
  wConsole=widget_text(right_base,scr_ysize=scr[1]/16,$
    value='',/scroll,uname='console',/wrap)
  
  ysize=0.85*ysize  
  wTab=Widget_Tab(left_base)
  
  wSettings = WIDGET_BASE(wTab, TITLE='Input/Output Settings', /COLUMN)
  wSettingsPanel=widget_base(wSettings,/column,xsize=scr[0],ysize=scr[1],$
    x_scroll_size=xsize,y_scroll_size=ysize,/scroll)
  geom = widget_info (wSettingsPanel, /geom)
  scr_xsize=0.98*geom.scr_xsize
  scr_ysize=0.98*geom.scr_ysize
  wSettingsBase=widget_base(wSettingsPanel,/column,uname='settings_base')
  
 
  wLabelBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wLabelBase=widget_base(wLabelBase,/row)
  wlabel=widget_label(wLabelBase,value='GX Model and Reference Data Input',scr_xsize=scr_xsize,/align_center)

  wGXMBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wGXMpathBase=widget_base(wGXMBase,/row)
  wlabel=widget_label(wGXMpathBase,value='GX Model Path                 ',scr_xsize=label_scr_xsize)
  label=widget_info(wlabel,/geometry)
  label_scr_xsize=label.scr_xsize
  wSelectGXMpath= widget_button(wGXMpathBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Select the path of a valid GX model',uname='gxmpath_select')
  geom = widget_info (wGXMpathBase, /geom)
  wGXMpathPath=widget_text(wGXMpathBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='GXMpath',/editable,$
    value=self.GXMpath)
    
  wRefDataBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wRefDatapathBase=widget_base(wRefDataBase,/row)
  wlabel=widget_label(wRefDatapathBase,value='Reference Data Path          ',scr_xsize=label_scr_xsize)
  wSelectRefDatapath= widget_button(wRefDatapathBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Select the path to a valid reference data structure',uname='refdatapath_select')
  geom = widget_info (wRefDatapathBase, /geom)
  wRefDatapathPath=widget_text(wRefDatapathBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='refdatapath',/editable,$
    value=self.RefDataPath)   
  wLabelBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wLabelBase=widget_base(wLabelBase,/row)
  wlabel=widget_label(wLabelBase,value='Search Pipeline Respositories',scr_xsize=scr_xsize,/align_center)
  
  wModDirBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wModBase=widget_base(wModDirBase,/row)
  wlabel=widget_label(wModBase,value='Model Maps Repository',scr_xsize=label_scr_xsize)
  wSelectModDir= widget_button(wModBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Select a path where to store model maps',uname='moddir_select')
  geom = widget_info (wModBase, /geom)
  wModDirPath=widget_text(wModDirBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='moddir',/editable,$
    value=self.ModDir)  
    
  wPsDirBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wPsBase=widget_base(wPsDirBase,/row)
  wlabel=widget_label(wPsBase,value='PostScript Repository',scr_xsize=label_scr_xsize)
  wSelectPsDir= widget_button(wPsBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Select a path where to store postscript figures',uname='psdir_select')
  geom = widget_info (wPsBase, /geom)
  wPsDirPath=widget_text(wPsDirBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='psdir',/editable,$
    value=self.PsDir) 
    
  wTmpDirBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wTmpBase=widget_base(wTmpDirBase,/row)
  wlabel=widget_label(wTmpBase,value='Temporary Directory',scr_xsize=label_scr_xsize)
  wSelectTmpDir= widget_button(wTmpBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Select a path where to store temporary results',uname='tmpdir_select')
  geom = widget_info (wTmpBase, /geom)
  wTmpDirPath=widget_text(wTmpDirBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='tmpdir',/editable,$
    value=self.tmpdir)    
    
    
  wLabelBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wLabelBase=widget_base(wLabelBase,/row)
  wlabel=widget_label(wLabelBase,value='Computing Engine Setup',scr_xsize=scr_xsize,/align_center)
  
  wRendererBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wRendererpathBase=widget_base(wRendererBase,/row)
  wlabel=widget_label(wRendererpathBase,value='Fast Code Renderer Path          ',scr_xsize=label_scr_xsize)
  wSelectRendererpath= widget_button(wRendererpathBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Select the path to a valid IDL Fast Code Rendering Routine',uname='rendererpath_select')
  geom = widget_info (wRendererpathBase, /geom)
  wRendererpathPath=widget_text(wRendererpathBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='rendererpath',/editable,$
    value=self.renderer)
    
  wEBTELpathBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wEBTELpathpathBase=widget_base(wEBTELpathBase,/row)
  wlabel=widget_label(wEBTELpathpathBase,value='EBTEL DEM Table Path',scr_xsize=label_scr_xsize)
  wSelectEBTELpathpath= widget_button(wEBTELpathpathBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Select the path to a valid EBTEL DEM table',uname='EBTELpath_select')
  geom = widget_info (wEBTELpathpathBase, /geom)
  wEBTELpathpathPath=widget_text(wEBTELpathpathBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='EBTELpath',/editable,$
    value=self.EBTELpath)  
    
  wFOVBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wFOVBase=widget_base(wFOVBase,/row)
  wlabel=widget_label(wFOVBase,value='Model Maps FOV (arcsecs)',scr_xsize=label_scr_xsize)
  wFOVimport= widget_button(wFOVBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Import FOV from a map structure or map object',uname='fov_import')
  geom = widget_info (wFOVBase, /geom)
  wFOV=widget_text(wFOVBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='fov',/editable,$
    value=self.FOV)
    
  wResBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wResBase=widget_base(wResBase,/row)
  wlabel=widget_label(wResBase,value='Model Maps Resolution (pixels)',scr_xsize=label_scr_xsize)
  wResimport= widget_button(wResBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Import Res from a map structure or map object',uname='res_import')
  geom = widget_info (wResBase, /geom)
  wRes=widget_text(wResBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='res',/editable,$
    value=self.res)  
    
  wLevelsBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wLevelsBase=widget_base(wLevelsBase,/row)
  wlabel=widget_label(wLevelsBase,value='[Mask, ROI] Levels (%)',scr_xsize=label_scr_xsize)
  wResetLevels= widget_button(wLevelsBase, $
    value=gx_bitmap(filepath('reset.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Reset [Mask, ROI] levels to default',uname='levels_reset')
  geom = widget_info (wLevelsBase, /geom)
  wLevels=widget_text(wLevelsBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='levels',/editable,$
    value=self.levels)  
    
  wtask=WIDGET_DRAW(wSettingsBase,xsize=xsize,ysize=ysize/2,UNAME='tasks_plot',$
    /expose_events, $
    retain=1,$
    graphics_level=1 $
    )  
    
       
  scr_xsize=scr_xsize/2
  right_base=widget_base(right_base,/column,xsize=scr_xsize,ysize=scr[1],$
    x_scroll_size=scr_xsize,y_scroll_size=ysize,/scroll)
  wTaskBase=widget_base(right_base,title='GXCHMP Task Base',/column)
  wLabelBase=widget_base(wTaskBase,/row,scr_xsize=scr_xsize,/frame)
  wLabelBase=widget_base(wLabelBase,/row)
  wlabel=widget_label(wLabelBase,value='Computing Parallel Threads',scr_xsize=scr_xsize,/align_center)
  
  wNThreadsBase=widget_base(wTaskBase,/row,scr_xsize=scr_xsize,/frame)
  wNThreadsBase=widget_base(wNThreadsBase,/row)
  wNthreads=cw_objfield(wNThreadsBase,label=' Number of parallel threads',$
    scr_labelsize=label_scr_xsize,xtextsize=3,value=self.bridges->Count(),min=1,max=!CPU.HW_NCPU,inc=1,$
    lfont=!defaults.font,font=!defaults.font,uname='nthreads',/int)
 
  wBridges=widget_table(right_base,font=!defaults.font,ysize=0,xsize=5,$
    y_scroll_size=0,x_scroll_size=0,$
    COLUMN_WIDTHS =[scr_xsize/10,scr_xsize/10,scr_xsize/7.5,scr_xsize/10,4*scr_xsize/10],$
    edit=0,format=format,$
    column_labels=['status','task','on task','calls','error message'],uname='bridge_status',$
    scr_xsize=scr_xsize,scr_ysize=scr_ysize/5,/resizeable_columns)
  
  wLabelBase=widget_base(right_base,/row,scr_xsize=scr_xsize,/frame)
  wLabelBase=widget_base(wLabelBase,/row)
  wlabel=widget_label(wLabelBase,value='Best Models Search Queue',scr_xsize=scr_xsize,/align_center)

  wQueue=widget_table(right_base,font=!defaults.font,ysize=0,xsize=5,$
    y_scroll_size=0,x_scroll_size=0,$
    COLUMN_WIDTHS =[scr_xsize/10,scr_xsize/10,scr_xsize/10,scr_xsize/10,4*scr_xsize/10],$
    edit=0,format=format,$
    column_labels=['a','b','q1','q2','task status'],uname='Queue',$
    scr_xsize=scr_xsize,scr_ysize=scr_ysize/2,/resizeable_columns)
  widget_control,wQueue,table_ysize=0
  
  wInputParmsBase=widget_base(right_base,/column,uname='input_base',map=0)
  wQBase=widget_base(wInputParmsBase,/row,scr_xsize=scr_xsize,/frame)
  wQFormulaBase=widget_base(wQBase,/row)
  wlabel=widget_label(wQFormulaBase,value='EBTEL Coronal Heating Model: Q=q0*(B/100)^a/(L/1e9)^b',scr_xsize=scr_xsize,/align_center)

  wAlistBase=widget_base(wInputParmsBase,/row,scr_xsize=scr_xsize,/frame)
  wAlistBase=widget_base(wAlistBase,/row)

  wlabel=widget_label(wAlistBase,value='"a" parameter space',scr_xsize=label_scr_xsize)
  label=widget_info(wlabel,/geometry)
  label_scr_xsize=label.scr_xsize
  wResetAlist= widget_button(wAlistBase, $
    value=gx_bitmap(filepath('reset.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Reset "a" parameter list to default',uname='alist_reset')
  geom = widget_info (wAlistBase, /geom)
  wAlist=widget_text(wAlistBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='alist',/editable,$
    value=self.alist)

  wBlistBase=widget_base(wInputParmsBase,/row,scr_xsize=scr_xsize,/frame)
  wBlistBase=widget_base(wBlistBase,/row)
  wlabel=widget_label(wBlistBase,value='"b" parameter space',scr_xsize=label_scr_xsize)
  wResetBlist= widget_button(wBlistBase, $
    value=gx_bitmap(filepath('reset.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Reset "b" parameter list to default',uname='blist_reset')
  geom = widget_info (wBlistBase, /geom)
  wBlist=widget_text(wBlistBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='blist',/editable,$
    value=self.blist)

  wQlistBase=widget_base(wInputParmsBase,/row,scr_xsize=scr_xsize,/frame)
  wQlistBase=widget_base(wQlistBase,/row)
  wlabel=widget_label(wQlistBase,value='"q0" initial guess set',scr_xsize=label_scr_xsize)
  wResetQlist= widget_button(wQlistBase, $
    value=gx_bitmap(filepath('reset.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Reset "qo" parameter list to default',uname='qlist_reset')
  geom = widget_info (wQlistBase, /geom)
  wQlist=widget_text(wQlistBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='qlist',/editable,$
    value=self.qlist)
  wInputActionBase=widget_base(wInputParmsBase,/row,scr_xsize=scr_xsize,/frame)
  wEnqueue=widget_button(wInputActionBase,value='Enqueue tasks',uname='enqueue_tasks',scr_xsize=scr_xsize/2)
  wCancel=widget_button(wInputActionBase,value='Cancel',uname='cancel_enqueue',scr_xsize=scr_xsize/2)
  
   
  wResults = WIDGET_BASE(wTab, TITLE='Results', /COLUMN)
  wResultsPanel=widget_base(wResults,/column,xsize=scr[0],ysize=scr[1],$
    x_scroll_size=xsize,y_scroll_size=ysize,/scroll)
  geom = widget_info (wResultsPanel, /geom)
  xsize=0.8*geom.scr_xsize/2
  ysize=0.8*geom.scr_xsize/2
  wResultsBase=widget_base(wResultsPanel,/column,uname='results_base')
  wResultsToolbar=widget_base(wResultsBase,/toolbar,/row,scr_xsize=xsize)
  wSelectMetrics=cw_bgroup(wResultsToolbar,['Res2','Chi2'],set_value=0,/exclusive,uname='metrics_select',/row)
  
  wLogMetrics=cw_bgroup(widget_base(wResultsToolbar,/frame),['Log Scale'],uname='metrics_log',/non)
  wcharsize=cw_objfield( widget_base(wResultsToolbar,/frame),value=!version.os_family eq 'Windows'?2:1,inc=0.1,min=0.1,max=5,label='Plot Charsize: ',font=!defaults.font,xtextsize=6,uname='charsize')

  wBestMetrics=widget_button(wResultsToolbar,value=gx_bitmap(filepath('find.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Display Best Metrics',uname='metrics_best')
  wDeleteSolution=widget_button(wResultsToolbar,value=gx_bitmap(filepath('delete.bmp', subdirectory=subdirectory)),tooltip='Discard the current solution',/bitmap,uname='remsol')
  

  wDisplayBase=widget_base(wResultsBase,/row,uname='display_base')
  left_panel=widget_base(wDisplayBase,/column,uname='left_panel')
  middle_panel=widget_base(wDisplayBase,/column,uname='middle_panel')
  right_panel=widget_base(wDisplayBase,/row,uname='right_panel')
  wa_label=widget_label(wDisplayBase,value='',/dynamic_resize,uname='a_select_label')
  wa_select=WIDGET_SLIDER(wDisplayBase,/vertical,uname='a_select',max=100000,sensitive=0,value=0,uvalue={a:wa_label},PRO_SET_VALUE='slider_set_value',/suppress)
  wb_label=widget_label(wDisplayBase,value='',/dynamic_resize,uname='b_select_label')
  wb_select=WIDGET_SLIDER(wDisplayBase,/vertical,uname='b_select',max=100000,sensitive=0,value=0,uvalue={b:wb_label},PRO_SET_VALUE='slider_set_value',/suppress)
  wgrid=WIDGET_DRAW(left_panel,xsize=xsize,ysize=xsize,UNAME='GRID',$
                          ;/button_events, $
                          ;/motion_events, $
                          retain=1,$
                          /expose_events, $
                          graphics_level=1 $
                          )                      
  wmap=WIDGET_DRAW(middle_panel,xsize=xsize,ysize=xsize,UNAME='MAP',$
                          /expose_events, $
                          retain=1,$
                          graphics_level=1 $
                          )
  wres=WIDGET_DRAW(left_panel,xsize=xsize,ysize=xsize,UNAME='RES',$
                          /expose_events, $
                          retain=1,$
                          graphics_level=1 $
                          ) 
  wchi=WIDGET_DRAW(middle_panel,xsize=xsize,ysize=xsize,UNAME='CHI',$
                          /expose_events, $
                          retain=1,$
                          graphics_level=1 $
                          )  
  self.displays=[wgrid, wmap, wres,  wchi,wtask]                                              
end

function gxchmp::check_fields
  mismatch=[]
  widget_control,widget_info(self.wBase,find_by_uname='GXMpath'),get_value=gxmpath
  if gxmpath ne self.gxmpath then mismatch=[mismatch,'GX Model Path']
  widget_control,widget_info(self.wBase,find_by_uname='refdatapath'),get_value=refdatapath
  if refdatapath ne self.refdatapath then mismatch=[mismatch,'Reference Data Path']
  widget_control,widget_info(self.wBase,find_by_uname='moddir'),get_value=moddir
  if moddir ne self.moddir then mismatch=[mismatch,'Model Model Maps Repository']
  widget_control,widget_info(self.wBase,find_by_uname='psdir'),get_value=psdir
  if psdir ne self.psdir then mismatch=[mismatch,'PostScript Repository']
  widget_control,widget_info(self.wBase,find_by_uname='tmpdir'),get_value=tmpdir
  if tmpdir ne self.tmpdir then mismatch=[mismatch,'Temporary Directory']
  widget_control,widget_info(self.wBase,find_by_uname='rendererpath'),get_value=renderer
  if renderer ne self.renderer then mismatch=[mismatch,'Renderer Path']
  widget_control,widget_info(self.wBase,find_by_uname='EBTELpath'),get_value=ebtelpath
  if ebtelpath ne self.ebtelpath then mismatch=[mismatch,'EBTEL DEM Table Path']
  widget_control,widget_info(self.wBase,find_by_uname='levels'),get_value=levels
  if levels ne self.levels then mismatch=[mismatch,'ROI levels']
  widget_control,widget_info(self.wBase,find_by_uname='fov'),get_value=fov
  if fov ne self.fov then mismatch=[mismatch,'Model Maps FOV']
  widget_control,widget_info(self.wBase,find_by_uname='res'),get_value=res
  if res ne self.res then mismatch=[mismatch,'Model Maps Resolution']
  if n_elements(mismatch) eq 0 then return,1
  answ=dialog_message(['Input field(s) inconsistency detected:',mismatch,'Please check each field reported here and press <ENTER> to accepted the currently displayed values and try again!'],/info)
  return,0
end

pro gxchmp::OnPallete
  tvlct,rgb_curr,/get
  xloadct,/silent,/block
  tvlct,rgb,/get
  tvlct,rgb_curr
  widget_control,widget_info(self.wBase,find_by_uname='color_table'),set_uvalue=rgb
  self->DisplaySolution
end

pro gxchmp::DisplaySolution,best=best
 self->PlotTasks
 if self.solution->IsEmpty() then begin
   thisP=!p
   thisD=!d.name
   widget_control,widget_info(self.wBase,find_by_uname='charsize'), get_value=charsize
   !p.charsize=charsize
   tvlct,rgb_curr,/get
   widget_control,widget_info(self.wBase,find_by_uname='color_table'),get_uvalue=rgb
   tvlct,rgb
   set_plot,self.WinOS?'win':'x'
  for k=0,3 do begin
    widget_control,self.displays[k],get_value=win
    wset,win
    erase,255
  endfor
  widget_control,widget_info(self.wbase,find_by_uname='a_select'),set_slider_max=1,sensitive=0,set_value=0,$
  set_uvalue={a:widget_info(self.wbase,find_by_uname='a_select_label'), list:[0]}
  widget_control,widget_info(self.wbase,find_by_uname='b_select'),set_slider_max=1,sensitive=0,set_value=0,$
  set_uvalue={b:widget_info(self.wbase,find_by_uname='b_select_label'), list:[0]}  
  !p=thisP
  set_plot,thisD
  return
 endif
 grid=gx_chmp2grid(self.solution->ToArray())
 if (size(grid.res2_best))[0] eq 1 then return
 widget_control,self.displays[0],get_value=wgrid
 widget_control,self.displays[1],get_value=wmap
 widget_control,self.displays[2],get_value=wres
 widget_control,self.displays[3],get_value=wchi
 widget_control,widget_info(self.wBase,find_by_uname='metrics_select'),get_value=chi2
 widget_control,widget_info(self.wBase,find_by_uname='metrics_log'),get_value=metrics_log
 na=n_elements(grid.a)
 nb=n_elements(grid.b)
 if keyword_set(best) then begin
   idx=array_indices([na,nb],/dim,keyword_set(chi2)?grid.idx_chi2_best:grid.idx_res2_best)
   widget_control,widget_info(self.wbase,find_by_uname='a_select'),set_slider_max=n_elements(grid.a)-1,/sensitive,set_value=idx[0],$
    set_uvalue={a:widget_info(self.wbase,find_by_uname='a_select_label'), list:grid.a}
   widget_control,widget_info(self.wbase,find_by_uname='b_select'),set_slider_max=n_elements(grid.b)-1,/sensitive,set_value=idx[1],$
    set_uvalue={b:widget_info(self.wbase,find_by_uname='b_select_label'), list:grid.b}
 endif else begin
   widget_control,widget_info(self.wbase,find_by_uname='a_select'),get_value=a_idx
   widget_control,widget_info(self.wbase,find_by_uname='b_select'),get_value=b_idx
   idx=[a_idx,b_idx]
 endelse
 if keyword_set(chi2) then begin
  obj_metrics=grid.obj_chi2_best[idx[0],idx[1]]
  metrics=reform(grid.chi2_best,na,nb)
  metrics_label='CHI!U2!N'
 endif else begin
  obj_metrics=grid.obj_res2_best[idx[0],idx[1]]
  metrics=reform(grid.res2_best,na,nb)
  metrics_label='RES2!U2!N'
 endelse
 if keyword_set(metrics_log) then begin
  metrics_label='Log['+ metrics_label+']'
  metrics=reform(alog10(metrics),na,nb)
 endif
 
 thisP=!p
 thisD=!d.name
 thisX=!x
 thisY=!y
 tvlct,rgb_curr,/get
 widget_control,widget_info(self.wBase,find_by_uname='color_table'),get_uvalue=rgb
 if n_elements(rgb) gt 0 then tvlct,rgb
 set_plot,self.WinOS?'win':'x'

 !p.background=255
 !p.color=0
 !p.charthick=2
 !p.font=-1
 !p.multi=0
 !x.margin=[6,6]
 !y.margin=[6,6]
 widget_control,widget_info(self.wBase,find_by_uname='charsize'), get_value=charsize
 !p.charsize=charsize
 a=grid.a
 b=grid.b
 wset,wgrid
 if na gt 1 and nb gt 1 then begin
  bad=where(finite(metrics) eq 0,count)
  if count gt 0 then metrics[bad]=1.05*max(metrics,/nan)
  tvplot,metrics,grid.a,grid.b,title=metrics_label,xtitle='a',ytitle='b',/sample
  oplot,grid.a[idx[[0,0]]],!y.crange,color=255,symsize=symsize,thick=3,linesty=2
  oplot,!x.crange,grid.b[idx[[1,1]]],color=255,symsize=symsize,thick=3,linesty=2
  plot_map_colorbar,minmax(metrics)
 endif else erase

 gx_chmp2displays,obj_metrics,charthick=3,res_min=-1,res_max=1,win=[wmap,wres,wchi],metrics_log=metrics_log,levels=self->str2arr(self.levels)
 !p=thisP
 !x=thisX
 !y=thisY
 set_plot,thisD
 tvlct,rgb_curr
end

pro gxchmp::PlotTasks
  widget_control,self.displays[4],get_value=wplot
  thisP=!p
  thisD=!d.name
  set_plot,self.WinOS?'win':'x'
  tvlct,rgb_curr,/get
  widget_control,widget_info(self.wBase,find_by_uname='color_table'),get_uvalue=rgb
  if n_elements(rgb) ne 0 then begin
    tvlct,rgb
    linecolors
    ; INDEX NUMBER   COLOR PRODUCED (if use default colors)
    black=0
    maroon=1
    red=2
    pink=3
    orange=4
    yellow=5
    olive=6
    green=7
    darkgreen=8
    cyan=9
    blue=10
    darkblue=11
    magenta=12
    purple=13 
    !p.background=255
    !p.color=black
  endif
  !p.charthick=2
  !p.font=-1
  !p.multi=0
  widget_control,widget_info(self.wBase,find_by_uname='charsize'), get_value=charsize
  !p.charsize=charsize
  wset,wplot
  erase
  a_sol=[]
  b_sol=[]
  a_task=[]
  b_task=[]
  
  for k=0,self.solution->Count()-1 do begin
    a_sol=[a_sol,self.solution(k).a]
    b_sol=[b_sol,self.solution(k).b]
  endfor
  
  for k=0,self.tasks->Count()-1 do begin
    if self.tasks(k).status eq 'pending' then begin
      a_task=[a_task,self.tasks(k).a]
      b_task=[b_task,self.tasks(k).b]
    end
  endfor
  
  a=[a_sol,a_task]
  b=[b_sol,b_task]
  
  if n_elements(a) gt 2 and n_elements(b) gt 2 then begin
    plot,a,b,psym=1,color=0,thick=3,xtitle='a parameter space',ytitle='b parameter space',/nodata,charthick=2,/iso
    if n_elements(a_sol) gt 1 then plots,a_sol,b_sol,thick=3,color=blue,psym=1
    if n_elements(a_task) gt 1 then plots,a_task,b_task,thick=3,color=red,psym=1
  endif 
  
  gx_plot_label,1.05,0.1,string(n_elements(a_task),format="(i0,' pending tasks')"),color=red,charthick=2,_extra=_extra
  gx_plot_label,1.05,0.2,string(n_elements(a_sol),format="(i0,' completed tasks')"),color=blue,charthick=2,_extra=_extra
  !p=thisP
  set_plot,thisD
  tvlct,rgb_curr
end  

function gxchmp::HandleEvent, event
  compile_opt hidden
  catch, error_status
  if error_status ne 0 then begin
    catch, /cancel
    void = dialog_message( $
      dialog_parent=self.wIDBase, $
      title='Error', $
      /error, $
      !error_state.msg + ' ' + !error_state.sys_msg $
      )
    return, self->Rewrite(event)
  endif
  case widget_info(event.id,/uname) of
    'GXMpath': begin
                widget_control,event.id,get_value=gxmpath
                goto,gxmpath_select
               end
    'gxmpath_select':begin
      gxmpath=dialog_pickfile(filter='*.gxm',$
        DEFAULT_EXTENSION='gxm',/read,$
        title='Please select a GX model file',/must_exist)  
      gxmpath_select:
      if file_exist(gxmpath) then begin
        model=gx_read(gxmpath)
        if obj_isa(model,'gxmodel') then begin
          self.GXMpath=gxmpath
          obj_destroy,model
          valid=1
        endif else valid=0
      endif else valid=0
      if valid eq 1 then self.GXMpath=gxmpath else answ=dialog_message('Not a valid path or a valid GX model file structure!')
      widget_control,widget_info(self.wBase,find_by_uname='GXMpath'),set_value=self.GXMpath
    end
    'refdatapath': begin
      widget_control,event.id,get_value=refdatapath
      goto,refdatapath_select
    end
    'refdatapath_select':begin
        refdatapath=dialog_pickfile(filter='*.sav',$
        DEFAULT_EXTENSION='sav',/read,$
        title='Please select a data reference file',/must_exist)
      refdatapath_select:
      if file_exist(refdatapath) then begin
        restore,refdatapath
        if size(ref,/tname) eq 'STRUCT' then begin
          if tag_exist(ref,'a_beam') then valid=1
        endif else valid=0
      endif else valid=0
      if valid eq 1 then self.refdatapath=refdatapath else answ=dialog_message('Not a valid path or a valid data referance file structure!')
      widget_control,widget_info(self.wBase,find_by_uname='refdatapath'),set_value=self.refdatapath
    end
    'rendererpath':begin
                     widget_control,event.id,get_value=renderer
                     goto,rendererpath_select
                    end
    'rendererpath_select':begin
        which,'gx_simulator',outfile=outfile
        cdir=file_dirname(file_dirname(outfile))
        path=cdir+path_sep()+'userslib'+path_sep()+'radio_nonflaring'+path_sep()+(self.WinOS?'windows':'unix')+path_sep()
        renderer=dialog_pickfile(filter='*.pro',TITLE='Please select a renderer IDL routine/wrapper',path=path,/must_exist)
        rendererpath_select:
        info_renderer=gx_rendererinfo(renderer)
        valid=size(info_renderer,/tname) eq 'STRUCT'
        if valid eq 1 then begin
          self.renderer=renderer 
          ptr_free,self.info_renderer
          self.info_renderer=ptr_new(info_renderer)
        endif else answ=dialog_message('Not a valid renderer routine!')
        widget_control,widget_info(self.wBase,find_by_uname='rendererpath'),set_value=self.renderer 
       end
    'EBTELpath':begin
         widget_control,event.id,get_value=EBTELpath
         goto,ebtelpath_select
       end
    'EBTELpath_select':begin
         EBTELpath=dialog_pickfile(filter='*.sav',TITLE='Please select a file containing a precomputed EBTEL table',path=file_dirname(gx_findfile('ebtel.sav',folder='')),/must_exist)
         ebtelpath_select:
         valid=file_exist(EBTELpath)
         if valid eq 1 then self.EBTELpath=EBTElpath else answ=dialog_message('Not a valid EBTEL file!')
         widget_control,widget_info(self.wBase,find_by_uname='EBTELpath'),set_value=self.EBTELpath
       end  
      
       'psdir':begin
         widget_control,event.id,get_value=psdir
         goto,psdir_select
       end
       'psdir_select':begin
         psdir=dialog_pickfile(TITLE='Please select a repository for the PostScript figures output',path=curdir(),/dir)
         psdir_select:
         if self->valid_path(psdir) then self.psdir=psdir
         widget_control,widget_info(self.wBase,find_by_uname='psdir'),set_value=self.psdir
       end 
       
       'moddir':begin
         widget_control,event.id,get_value=moddir
         goto,moddir_select
       end
       'moddir_select':begin
         moddir=dialog_pickfile(TITLE='Please select a repository for the model maps output',path=curdir(),/dir)
         moddir_select:
         if self->valid_path(moddir) then self.moddir=moddir
         widget_control,widget_info(self.wBase,find_by_uname='moddir'),set_value=self.moddir
       end  
       
       'tmpdir':begin
         widget_control,event.id,get_value=tmpdir
         goto,tmpdir_select
       end
       'tmpdir_select':begin
         tmpdir=dialog_pickfile(TITLE='Please select a repository for the temporary files output',path=curdir(),/dir)
         tmpdir_select:
         if self->valid_path(tmpdir) then self.tmpdir=tmpdir
         widget_control,widget_info(self.wBase,find_by_uname='tmpdir'),set_value=self.tmpdir
       end 
       
     'levels': begin
                 widget_control,event.id,get_value=levels
                 if n_elements(self->str2arr(levels[0])) eq 0 then begin
                   answ=dialog_message('Invalid syntax!')
                   widget_control,event.id,set_value=self.levels
                 endif else self.levels=levels[0]
                 self->DisplaySolution
               end 
     'levels_reset':begin
                    self.levels='12,20,30,50,80'
                    widget_control,widget_info(self.wbase,find_by_uname='levels'),set_value=self.levels
                    self->DisplaySolution
                    end  
                    
     'open':begin
              answ=self->SaveSolution(/question)
              if strupcase(answ) ne 'CANCEL' then begin
               file=dialog_pickfile(filter='*.sav',/must_exist)
               if file_exist(file) then begin
                restore,file
                if tag_exist(result,'TASK_ID')then result=rem_tag(result,'TASK_ID')
                if tag_exist(result,'REF')then result=rem_tag(result,'REF')
                if ~self->IsCompatible(result) then begin
                  answ=dialog_message('The solution being imported is not compatible with the one already in memory! Do you want to replace it?',/question)
                  if strupcase(answ) eq 'YES' then begin
                    self.solution->Remove,/all
                  endif else goto,exit_point
                endif
               if tag_exist(result,'mask') then begin
                  levels=self->str2arr(self.levels)
                  levels=[result[0].mask,levels]
                  levels=levels[uniq(levels,sort(levels))]
                  self.levels=self->arr2str(levels)
                  widget_control,widget_info(self.wbase,find_by_uname='levels'),set_value=self.levels
                endif
                if tag_exist(result,'modDir') then begin
                  if self->valid_path(result[0].moddir) then begin
                    self.moddir=result[0].moddir
                    widget_control,widget_info(self.wBase,find_by_uname='moddir'),set_value=self.moddir
                  endif
                endif else result=add_tag(result,self.moddir,'modDir')
                if tag_exist(result,'psDir') then begin
                  if self->valid_path(result[0].psdir) then begin
                    self.psdir=result[0].psdir
                    widget_control,widget_info(self.wBase,find_by_uname='psdir'),set_value=self.psdir
                  endif
                endif else result=add_tag(result,self.psdir,'psDir')
                if tag_exist(result,'refdatapath') then begin
                  refdatapath=result[0].refdatapath
                  if self->valid_path(refdatapath) then begin
                    if file_exist(refdatapath) then begin
                      restore,refdatapath
                      if size(ref,/tname) eq 'STRUCT' then begin
                        if tag_exist(ref,'a_beam') then valid=1
                      endif else valid=0
                    endif else valid=0
                    if valid eq 1 then begin
                      self.refdatapath=refdatapath
                      widget_control,widget_info(self.wBase,find_by_uname='refdatapath'),set_value=self.refdatapath
                    endif
                  endif
                endif else result=add_tag(result,self.refdatapath,'refdatapath')
                if tag_exist(result,'gxmpath') then begin
                  gxmpath=result[0].gxmpath
                  if file_exist(gxmpath) then begin
                    model=gx_read(gxmpath)
                    if obj_isa(model,'gxmodel') then begin
                      self.GXMpath=gxmpath
                      obj_destroy,model
                      valid=1
                    endif else valid=0
                  endif else valid=0
                  if valid eq 1 then begin
                    self.GXMpath=gxmpath
                    widget_control,widget_info(self.wBase,find_by_uname='GXMpath'),set_value=self.GXMpath
                  endif
                endif else result=add_tag(result,self.gxmpath,'gxmpath')
                if tag_exist(result,'res_best_metrics') then begin
                  fovmap=result[0].res_best_metrics
                  renderer=gx_findfile(fovmap->get(/renderer),folder='')
                  info_renderer=gx_rendererinfo(renderer)
                  valid=size(info_renderer,/tname) eq 'STRUCT'
                  if valid eq 1 then begin
                    self.renderer=renderer
                    ptr_free,self.info_renderer
                    self.info_renderer=ptr_new(info_renderer)
                    widget_control,widget_info(self.wBase,find_by_uname='rendererpath'),set_value=self.renderer
                  endif else answ=dialog_message('Not a valid renderer routine!')
                  widget_control,widget_info(self.wBase,find_by_uname='rendererpath'),set_value=self.renderer
                  keys=gx_getEBTELparms(fovmap->get(/gx_key),ebtel=ebtel_path)
                  ebtel_path=file_exist(ebtel_path)?ebtel_path:gx_findfile(ebtel_path,folder='')
                  if file_exist(ebtel_path) then begin
                    self.EBTELpath=ebtel_path
                    widget_control,widget_info(self.wBase,find_by_uname='EBTELpath'),set_value=self.EBTELpath
                  endif
                  xc=fovmap->get(/orig_xc)
                  yc=fovmap->get(/orig_yc)
                  xfov=delta(get_map_xrange(fovmap->get(/map),/edge))
                  yfov=delta(get_map_yrange(fovmap->get(/map),/edge))
                  if n_elements(xc) ne 0 then begin
                    self.fov=strcompress(string(xc,yc,xfov,yfov,format="('center=[',g0,', ',g0,']; range=[',g0,', ',g0,']')"))
                    widget_control,widget_info(self.wIDBase,find_by_uname='fov'),set_value=self.fov
                  end
                  sz=size(fovmap->get(/data))
                  if n_elements(sz) ne 0 then begin
                    strarr=str2arr(self.res,';')
                    for i=0,n_elements(strarr)- 1 do begin
                      void=execute(strarr[i])
                    endfor
                    self.res=strcompress(string(sz[[1,2,1,2]],format="('raw=[',i0,', ',i0,']; resize=[',i0,', ',i0,']')"))
                    widget_control,widget_info(self.wIDBase,find_by_uname='res'),set_value=self.res
                  end
                endif
               end
               self->AddSolution,result
              end
             end
                           
     'alist': begin
                 widget_control,  event.id,get_value=list
                 if n_elements(self->str2arr(list[0])) eq 0 then begin
                   widget_control,event.id,set_value=self.alist
                 endif else begin
                  self.alist=list[0]
                 endelse
               end  
      'blist': begin
                 widget_control,event.id,get_value=list
                 if n_elements(self->str2arr(list[0])) eq 0 then begin
                   widget_control,event.id,set_value=self.blist
                 endif else begin
                   self.blist=list[0]
                 endelse
               end  
               
       'qlist': begin
                 widget_control,event.id,get_value=list
                 if n_elements(self->str2arr(list[0])) eq 0 then begin
                   widget_control,event.id,set_value=self.qlist
                 endif else begin
                   self.qlist=list[0]
                 endelse
               end          
     'alist_reset': begin
                   self.alist='0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3'
                   widget_control,widget_info(self.wbase,find_by_uname='alist'),set_value=self.alist
               end
     'blist_reset': begin
                   self.blist='0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2'
                   widget_control,widget_info(self.wbase,find_by_uname='blist'),set_value=self.blist
               end    
     'qlist_reset': begin
                 self.qlist='0.001,0.01'
                 widget_control,widget_info(self.wbase,find_by_uname='qlist'),set_value=self.qlist
               end   
               
     'a_select':begin
                 widget_control,event.id,set_value=event.value
                 self->DisplaySolution
                end   
     'b_select':begin
                 widget_control,event.id,set_value=event.value
                 self->DisplaySolution
                end  
     'metrics_select': if event.select eq 1 then self->DisplaySolution 
     'metrics_best':self->DisplaySolution,/best 
     'remsol':begin
               answ=self->SaveSolution(/question)
               if strupcase(answ) ne 'CANCEL' then begin
                self.Solution->Remove,/all
                self->DisplaySolution
               end 
              end 
     'metrics_log': self->DisplaySolution    
     'charsize':Self->DisplaySolution                                                      
     'fov': begin
             widget_control,event.id,get_value=fov
             if n_elements(fov) eq 0 then begin
               answ=dialog_message([['Invalid syntax!'],['Expected:'],['center=[Xc,Yc]; range=[Xsize,Ysize]']])
               widget_control,event.id,set_value=self.fov
             endif else begin
              strarr=str2arr(fov,';')
              for i=0,n_elements(strarr)- 1 do begin
                result=execute(strarr[i])
              endfor
              if ~(n_elements(center) eq 2 and n_elements(range) eq 2) then begin
                answ=dialog_message([['Invalid syntax!'],['Expected:'],['center=[Xc,Yc]; range=[Xsize,Ysize]']])
                widget_control,event.id,set_value=self.fov
              endif else self.fov=fov
             endelse
           end 
     'fov_import': begin
                   file=dialog_pickfile(title='Please select a file containing an IDL map structure or object to import its FOV',filter=['*.sav','*.map'],path=gx_findfile(folder='demo'),/must_exist)
                   if file eq '' then return,self->Rewrite(event)
                   osav=obj_new('idl_savefile',file)
                   names=osav->names()
                   valid=0
                   for i=0,n_elements(names)-1 do begin
                     osav->restore,names[i];,/RELAXED_STRUCTURE_ASSIGNMENT
                     e=execute('result=size('+names[i]+',/tname)')
                     if (result eq 'STRUCT') or (result eq 'OBJREF') then begin
                       e=execute('m=temporary('+names[i]+')')
                       if valid_map(m) then fovmap=temporary(m)
                     endif
                   endfor
                   case size(fovmap,/tname) of
                    'STRUCT':begin
                              xc=fovmap.xc
                              yc=fovmap.yc
                              xfov=delta(get_map_xrange(fovmap,/edge))
                              yfov=delta(get_map_yrange(fovmap,/edge))
                             end
                    'OBJREF':begin
                               xc=fovmap->get(/xc)
                               yc=fovmap->get(/yc)
                               xfov=delta(get_map_xrange(fovmap->get(/map),/edge))
                               yfov=delta(get_map_yrange(fovmap->get(/map),/edge))
                              end         
                     else:begin
                          answ=dialog_message('Invalid file format!',/info)
                         end
                    endcase 
                    if n_elements(xc) ne 0 then begin
                      self.fov=strcompress(string(xc,yc,xfov,yfov,format="('center=[',g0,', ',g0,']; range=[',g0,', ',g0,']')"))
                    widget_control,widget_info(self.wIDBase,find_by_uname='fov'),set_value=self.fov   
                    end           
                   end      
     'res': begin
             widget_control,event.id,get_value=res
             if n_elements(res) eq 0 then begin
                answ=dialog_message([['Invalid syntax!'],['Expected:'],['raw=[Nx,Ny]; resize=[Nx,Ny]']])
               widget_control,event.id,set_value=self.res
             endif else begin
               strarr=str2arr(res,';')
               for i=0,n_elements(strarr)- 1 do begin
                 result=execute(strarr[i])
               endfor
               if ~(n_elements(raw) eq 2 and n_elements(resize) eq 2) then begin
                 answ=dialog_message([['Invalid syntax!'],['Expected:'],['raw=[Nx,Ny]; resize=[Nx,Ny]']])
                 widget_control,event.id,set_value=self.res
               endif else self.res=res
             endelse
           end 
     'res_import': begin
             file=dialog_pickfile(title='Please select a file containing an IDL map structure or object to import its resolution',filter=['*.sav','*.map'],path=gx_findfile(folder='demo'),/must_exist)
             if file eq '' then return,self->Rewrite(event)
             osav=obj_new('idl_savefile',file)
             names=osav->names()
             valid=0
             for i=0,n_elements(names)-1 do begin
               osav->restore,names[i];,/RELAXED_STRUCTURE_ASSIGNMENT
               e=execute('result=size('+names[i]+',/tname)')
               if (result eq 'STRUCT') or (result eq 'OBJREF') then begin
                 e=execute('m=temporary('+names[i]+')')
                 if valid_map(m) then fovmap=temporary(m)
               endif
             endfor
             case size(fovmap,/tname) of
               'STRUCT':begin
                 sz=size(fovmap.data)
               end
               'OBJREF':begin
                 sz=size(fovmap->get(/data))
               end
               else:begin
                 answ=dialog_message('Invalid file format!',/info)
               end
             endcase
             if n_elements(sz) ne 0 then begin
               strarr=str2arr(self.res,';')
               for i=0,n_elements(strarr)- 1 do begin
                 result=execute(strarr[i])
               endfor
               self.res=strcompress(string(sz[[1,2,1,2]],format="('raw=[',i0,', ',i0,']; resize=[',i0,', ',i0,']')"))
               widget_control,widget_info(self.wIDBase,find_by_uname='res'),set_value=self.res
             end
           end        
     'nthreads': begin
                  widget_control,event.id,get_value=nthreads
                  self.SetBridges,nthreads
                 end  
     'clearlog':begin
                   widget_control,widget_info(self.wIDBase,find_by_uname='console'),set_value=''
                 end  
                 
     'add_tasks':self->EditTasks,0  
     'cancel_enqueue':self->EditTasks,1
     'enqueue_tasks':self->EditTasks,2 
     'import_tasks':self->ImportTasks   
     'export_tasks':self->ExportTasks       
     'execute':self->OnStartSearch  
     'abort':self->Abort,/question   
     'delete_pending':self->delete_pending,/question
     'flush_queue':self->flush_queue,/question
     'save':answ=self.SaveSolution()   
     'color_table':self->OnPallete
     '2ps':self->Solution2PS                              
    else:
  endcase
  exit_point:
  return,self->Rewrite(event)
end


function cw_gxchmp,Base,_extra=_extra
  obj=obj_new('gxchmp',Base,_extra=_extra)
  obj->GetProperty,widget_id=widget_id
  return,widget_id
end