pro chmp_help
  print,'% IDL-> chmp, nthreads; to set,increase, or decrease the number of ashyncronious threads to be used
  print,'% IDL-> chmp, /alist; to print the current a-parameter list
  print,'% IDL-> chmp, /blist; to print the current b-parameter list
  print,'% IDL-> chmp, /levels; to print the current ROI levels list
  print,'% IDL-> chmp, /fov; to print the current FOV settings
  print,'% IDL-> chmp, /res; to print the current map resolution settings
  print,'% IDL-> chmp, /refdatapath; to print the current reference data path
  print,'% IDL-> chmp, /gxmpath; to print the current GX model data path
  print,'% IDL-> chmp, /bridges; to print the current status of the parallel execution threads
  print,'% IDL-> chmp, /status; to report the status of the application, including all of the above
  print,'% IDL-> chmp, /start; to start processing the task queue
  print,'% IDL-> chmp, /flush; to flush the pending task queue
  print,'% IDL-> chmp, /abort; to abort all active tasks and flush the pending task queue
  print,'% IDL-> chmp, /quiet; to turn off run-time execution progress messages
  print,'% IDL-> chmp, /loud; to turn on run-time execution progress messages
  print,'% IDL-> chmp, /exit; to abort all active tasks, flush the pending task queue, and exit the application
  gx_message,'% Any logical combination of the arguments and keywords listed above should result in a valid single-line calling sequence',/cont,/info,level=-1
end

pro chmp_self
  common chmp, self
  if ~isa(self,'chmp') then begin
    self={chmp,active:0l,GXMpath:'',RefDataPath:'',modDir:'',psDir:'',TmpDir:'',EbtelPath:'',$
      renderer:'',alist:'',blist:'',qlist:'',levels:'',fov:'',res:'',completed:0l,$
      RefDataStruct:ptr_new(),solution:obj_new(),bridges:obj_new(),tasks:obj_new(),WinOS:0l,quiet:0L}
    if file_exist('gxchmp.ini') and ~keyword_set(fresh) then restore,'gxchmp.ini'
    self.WinOS=(!version.os_family eq 'Windows')
    default,modDir,curdir()+path_sep()+'moddir'
    self.modDir=modDir
    default,psDir,curdir()+path_sep()+'psdir'
    self.psDir=psDir
    default,tmpDir,curdir()+path_sep()+'tmpdir'
    self.tmpDir=TmpDir
    default,refdatapath,''
    self.refdatapath=refdatapath
    default,gxmpath,''
    self.gxmpath=gxmpath
    default,EBTELpath,gx_findfile('ebtel.sav',folder='')
    self.EBTELpath=EBTELpath
    default,renderer,gx_findfile(self.WinOS?'AR_GRFF_nonLTE.pro':'mwgrtransfer.pro',folder='')
    self.renderer=renderer
    default,alist,'0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3'
    self.alist=alist
    default,blist,'0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2'
    self.blist=blist
    default,qlist,'0.0001,0.001'
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
    message,'CHMP has been initialized!',/info
  endif
end

pro chmp_set_bridges, nbridges,new=new,force=force
  common chmp, self
  chmp_self
  if ~obj_valid(self.bridges) then  self.bridges=obj_new('IDL_Container')
  if n_elements(nbridges) eq 0 then nbridges=1 else nbridges=nbridges>1
  if (nbridges gt !CPU.HW_NCPU ) and (~keyword_set(force)) then begin
    message, string(!CPU.HW_NCPU, nbridges,format="('The number of bridges is limited to the ',g0, ' available CPUs! Use IDL> chmp,nbridges,/force if you are sure you really want to create ',g0,' bridges!')"),/info
    nbridges=!CPU.HW_NCPU
    no_other_message=1
  endif
  if obj_valid(self.bridges) then begin
    if keyword_set(new) then begin
      message,'Replacing all bridges',/info
      obj_destroy,self.bridges
      self.bridges=obj_new('IDL_Container')
      start_index=0
    endif else begin

      if (self.bridges->Count() eq nbridges) then begin
        if ~keyword_set(no_other_message) then message,'Requested number of bridges matches the number already existing, nothing to be done!',/info
        goto,exit_point
      endif

      if (self.bridges->Count() gt nbridges) then begin
        n=self.bridges->Count()-nbridges
        message,string(n,format=(n gt 1)?"('Removing ',g0,' bridges')":"('Removing ',g0,' bridge')"),/info
        for i= nbridges, self.bridges->Count()-1  do begin
          bridge=self.bridges->Get(position=self.bridges->Count()-1 )
          self.bridges->Remove,bridge
          obj_destroy,bridge
        endfor
        goto,exit_point
      endif

      if (self.bridges->Count() lt nbridges) then begin
        n=nbridges-self.bridges->Count()
        message,string(n,format=(n gt 1)?"('Adding ',g0,' bridges')":"('Adding ',g0,' bridge')"),/info
        start_index=self.bridges->Count()
      endif
    endelse
  endif else begin
    bridges=obj_new('IDL_Container')
    start_index=0
  endelse

  bridge_state=replicate({status:'',task:'',time:'',calls:'',error:''},nbridges)
  for i=start_index,nbridges-1 do begin
    message,string(i+1,format="('Initializing bridge #',i3)"),/info
    bridge=obj_new('IDL_Bridge',userdata=main_base,callback='chmp_callback',out=GETENV('IDL_TMPDIR')+GETENV('USER')+strcompress('chmp_bridge'+string(i)+'.log',/rem))
    if obj_valid(bridge) then begin
      bridge->SetVar,'id',i+1
      code=bridge->Status(error=error)
      case code of
        0:bridge_state[i].status='Idle'
        1:bridge_state[i].status='Active'
        2:bridge_state[i].status='Completed'
        3:bridge_state[i].status='Error'
        4:bridge_state[i].status='Aborted'
        else:bridge_state[i].status='Unknown'
      endcase
      bridge_state[i].error=error
      self.bridges->Add,bridge
    end
  end
  exit_point:
  chmp_status,/bridges
end

function chmp_list2dblarr,strlist
  s='['+arr2str(str2arr(str_replace(str_replace(str_replace('['+arr2str(strlist)+']','[',''),']',''),'d',''))+'d')+']'
  code=execute('arr='+s)
  return,arr
end

pro chmp_reset,_extra=_extra
  common chmp, self
  chmp_self
  a=chmp_list2dblarr(self.alist)
  b=chmp_list2dblarr(self.blist)
  q=minmax(chmp_list2dblarr(self.qlist))
  if obj_valid(self.tasks) then self.tasks->Remove,/all else self.tasks=list()
  for i=0,n_elements(a)-1 do begin
    for j=0,n_elements(b)-1 do begin
      self.tasks->add,{id:self.tasks->Count(),a:a[i],b:b[j],q:q,duration:0d,status:'pending'}
    endfor
  endfor
  chmp_status,_extra=_extra
end

pro chmp_abort
  common chmp, self
  chmp_self
  bridges=self.bridges
  allbridges=bridges->Get(/all,count=count)
  active=0
  for i=0,count-1 do begin
    code=allbridges[i]->Status(error=error)
    if code eq 1 then begin
      active+=1
      message,strcompress(string(0,format="('Working on aborting the active task on Bridge # ', g0,', please wait....')")),/cont
      allbridges[i]->Abort
      id=allbridges[i]->GetVar('id')
      message,strcompress(string(id,format="('Bridge # ', g0,' execution aborted on user request!')")),/cont
    endif
  end
  if active eq 0 then message, 'No active tasks, nothing to be aborted!',/cont else $
    message,'All active tasks aborted on user request!',/cont
  chmp_flush_queue
  message,'All pending tasks have been deleted from memory on user request!',/cont
  chmp_status,/bridges
end

function chmp_where,pending=pending,completed=completed,aborted=aborted,active=active,count
  common chmp, self
  chmp_self
  tasklist=self.tasks
  if ~obj_valid(tasklist) then begin
    count=0
    return,-1
  endif
  l=tasklist
  if keyword_set(pending) then p=l.map(Lambda(l:strupcase(l.status) eq 'PENDING')) else p=l.map(Lambda(l:strupcase(l.status) eq ''))
  if keyword_set(completed) then c=l.map(Lambda(l:strupcase(l.status) eq 'COMPLETED')) else c=l.map(Lambda(l:strupcase(l.status) eq ''))
  if keyword_set(aborted) then ab=l.map(Lambda(l:strupcase(l.status) eq 'ABORTED')) else ab=l.map(Lambda(l:strupcase(l.status) eq ''))
  if keyword_set(active) then ac=l.map(Lambda(l:strupcase(l.status) eq 'ACTIVE')) else ac=l.map(Lambda(l:strupcase(l.status) eq ''))
  if p.IsEmpty() then begin
    pcount=0
    p=[-1]
  endif else p=where(p.toarray(),pcount)
  if c.IsEmpty() then begin
    ccount=0
    c=[-1]
  endif  else c=where(c.toarray(),ccount)
  if ab.IsEmpty() then begin
    abcount=0
    ab=[-1]
  endif else ab=where(ab.toarray(),abcount)
  if ac.IsEmpty() then begin
    account=0
    ac=[-1]
  endif else ac=where(ac.toarray(),account)
  count=pcount+ccount+abcount+account
  selected=[-1]
  selected=[p,c,ab,ac]
  sidx=where(selected ne -1,scount)
  selected=(scount ne 0)?selected[sidx]:-1
  return,selected
end

pro chmp_flush_queue
  common chmp, self
  chmp_self
  if obj_valid(self.tasks) eq 0 then self.tasks=list()
  self.tasks->Remove,/all
  pending=chmp_where(/pending,count)
  message,string (count,format="('Tasks list updated: ',g0, ' pending tasks in the proceesing queue')"),/info
end

pro chmp_close_bridges
  common chmp, self
  chmp_self
  if obj_valid(self.bridges) then obj_destroy,self.bridges
end

function chmp_script,task_id=task_id
  common chmp, self
  chmp_self
  script='result=gx_search4bestq('
  script+='  gxmpath="'+self.gxmpath+'"'
  script+=', modDir="'+self.moddir+'"'
  script+=', psDir="'+self.psdir+'"'
  script+=', tmpDir="'+self.tmpdir+'"'
  script+=', refdatapath="'+self.refdatapath+'"'
  script+=', ebtelpath="'+self.ebtelpath+'"'
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
  default,task_id,0
  task=self.tasks[task_id]
  script+=strcompress(string(task.a,format="(', a_arr= ',g0)"))
  script+=strcompress(string(task.b,format="(', b_arr= ',g0)"))
  script+=', q_start=['+arr2str(task.q)+']'
  script+=string(arr2str((str2arr(', levels=['+self.levels+']')),format="(a0)"))
  script+=')'
  return,strcompress(script)
end


pro chmp_status,status=status,_extra=_extra
  common chmp, self
  chmp_self
    if keyword_set(status) then begin
      tags=tag_names(self)
    endif else tags= (size(_extra,/tname) eq 'STRUCT')?tag_names(_extra):tag_names(self)
    for k=0,n_elements(tags)-1 do void=execute(tags[k]+'=1')
    if keyword_set(alist) then message,'a='+self.alist,/info
    if keyword_set(blist) then message,'b='+self.blist,/info
    if keyword_set(qlist)then message,'q='+self.qlist,/info
    if keyword_set(levels)then message,'levels='+self.levels,/info
    if keyword_set(res)then message,'maps res: '+self.res,/info
    if keyword_set(fov)then message,'maps FOV: '+self.FOV,/info
    if keyword_set(ebtelpath)then message,'ebtelpath='+self.ebtelpath,/info
    if keyword_set(refdatapath)then message,'refdatapath='+self.refdatapath,/info
    if keyword_set(gxmpath)then message,'gxmpath='+self.gxmpath,/info
    if keyword_set(renderer)then message,'renderer='+self.renderer,/info
    if keyword_set(moddir)then message,'moddir='+self.moddir,/info
    if keyword_set(psdir)then message,'psdir='+self.psdir,/info
    if keyword_set(tmpdir)then message,'tmpdir='+self.tmpdir,/info
    if keyword_set(script)then print,chmp_script()
    if keyword_set(tasks)then begin
      tasklist=self.tasks
      pending=chmp_where(/pending,pending_count)
      completed=chmp_where(/completed,completed_count)
      aborted=chmp_where(/aborted,aborted_count)
      active=chmp_where(/active,active_count)
      if obj_valid(tasklist) then begin
        count=tasklist->Count()
      endif else begin
        count=0
      endelse
      if count eq 0 then message,'WARNING: No task list exists in memory! Use "IDL>chmp_add_tasks,tasks" to provide one or more.',/info
      if self.quiet eq 0 then begin
        if aborted_count gt 0 then message, strcompress(string(aborted_count,count,format="('ABORTED TASKS: ', g0, '/',  g0)")),/info
        if pending_count gt 0 then message, strcompress(string(pending_count,count,format="('PENDING TASKS: ', g0, '/',  g0)")),/info
        if active_count gt 0 then message, strcompress(string(active_count,count,format="('ACTIVE TASKS: ', g0, '/',  g0)")),/info
      end
    endif
    if keyword_set(bridges)then begin  
      bridges=self.bridges
      if ~obj_valid(bridges) then begin
         message, 'No bridges alive, use IDL>chmp,nbridges to create at least one.',/info
        return
      endif
      bridge_array=bridges->Get(/all,count=count)
      if count eq 0 then begin
        if (self.quiet eq 0 or keyword_set(nobridges)) then  message, 'No bridges alive, use IDL>chmp,nbridges to create at least one.',/info
        return
      endif
      for i=0,count-1 do begin
        code=bridge_array[i]->Status(error=error)
        case code of
          0:status='Idle'
          1:status='Active'
          2:status='Completed'
          3:status='Error'
          4:status='Aborted'
          else:status='Unknown'
        endcase
        if self.quiet eq 0 then message, strcompress('Bridge ID='+string(i+1)+' status: '+ status),/info
      end
    end
end

function chmp_arr2str,arr
 return,arr2str(string(arr,format="(g0)"))
end

function chmp_solution
  common chmp, self
  chmp_self
  return,self.solution->ToArray()
end

function chmp_tasks
  common chmp, self
  chmp_self
  return,self.tasks->ToArray()
end

function chmp_checklist,anylist
common chmp, self
chmp_self
if self.active then begin
  message,'Currently busy, request ignored!',/info,/cont
  chmp_status
  return,!null
endif
case 1 of
  isa(anylist,/number,/array): begin
                             return,chmp_arr2str(anylist)
                             end
  isa(anylist,/string): begin
                             good=execute('list=['+anylist+']')
                             return, (good gt 0)?chmp_arr2str(list):!null
                     end  
  else: return,!null                                            
endcase
end

pro chmp_add_task,tasks
 common chmp, self
 if tag_exist(tasks,'a') and tag_exist(tasks,'b') and tag_exist(tasks,'b') then begin
  if n_elements(tasks[0].q) eq 2 then begin
    if ~self.tasks->IsEmpty()then begin
    l=self.tasks->ToArray()
    for k=0,n_elements(tasks)-1 do begin
     idx=where(l.a eq tasks[k].a and l.b eq tasks[k].b,count)
     if count eq 0 then begin
      self.tasks->add,{id:self.tasks->Count(),a:double(tasks[k].a),b:double(tasks[k].b),q:double(tasks[k].q),duration:0d,status:'pending'}
     endif else begin
      if strupcase(l[idx].status) ne 'ACTIVE' then begin
      t=self.tasks->remove(idx)
      t.a=double(tasks[k].a)
      t.b=double(tasks[k].b)
      t.q=double(tasks[k].q)
      self.tasks->add,t,idx    
      message,string(double(tasks[k].a),double(tasks[k].b),format="('An already existing task having a=',g0,' and b=',g0,' has been updated!')"),/info
      endif else message,string(double(tasks[k].a),double(tasks[k].b),format="('A task having a=',g0,' and b=',g0,' is active. Request ingnored!')"),/info 
     endelse
    end
    endif else begin
     for k=0,n_elements(tasks)-1 do $
      self.tasks->add,{id:self.tasks->Count(),a:double(tasks[k].a),b:double(tasks[k].b),q:double(tasks[k].q),duration:0d,status:'pending'}
    endelse
  endif else message,'Input q tag exoecte to be a two elements array',/info
 endif else message,'Invalid task list input. Expected input structure tags: a,b,q!',/info
 chmp_status,/tasks
end


pro chmp_assigntask,bridge,task,OnStartSearch=OnStartSearch
  common chmp, self
  chmp_self
  if ~keyword_set(OnStartSearch) then begin
    calls=bridge->GetVar('calls')
    bridge->SetVar,'calls',calls+1
  endif else bridge->SetVar,'calls',0
  bridge->SetVar,'task_id',task.id
  bridge->SetVar,'start_time',systime(/s)
  bridge->SetVar,'a',task.a
  bridge->SetVar,'b',task.b
  bridge_id=bridge->GetVar('id')
  bridge->Execute,'delvar,result'
  script=chmp_script(task_id=task.id)
  bridge->Execute,script,/nowait
  chmp,/tasks,/bridges
end

function chmp_ready
 common chmp, self
 chmp_self
  if ~isa(self) then return,0
  if self.Bridges->Count() eq 0 then begin
    message,'Not parallel computing bridge initialized yet!'+$
      string(10b)+'use "chmp, N" to initialize at least one!',/info,/cont
    return,0  
  endif
 ;-------------------------------------------------------------------- 
 if file_exist(self.refdatapath) and self.refdatapath ne '' then begin
   restore,self.refdatapath
   if size(ref,/tname) eq 'STRUCT' then begin
     if tag_exist(ref,'a_beam') then valid_ref=1
   endif else valid_ref=0
 endif else valid_ref=0
 if ~valid_ref then begin
   chmp_status,/refdatapath
   message,'Not a valid path or a valid data referance file structure defined,'+$
     string(10b)+'use "chmp, refdatapath=reference data path" to define one!',/info,/cont
   return,0  
 endif
 ;------------------------------------
 if file_exist(self.gxmpath) then begin
   model=gx_read(self.gxmpath)
 if obj_isa(model,'gxmodel') then begin
   obj_destroy,model
   valid_gxm=1
 endif else valid_gxm=0
 endif else valid_gxm=0
 if ~valid_gxm then begin
   chmp_status,/gxmpath
   message,'Not a valid path or a valid GX model file structure defined,'+$
     string(10b)+'use "chmp, gxmpath=model path" to define one!',/info,/cont
   return,0  
 endif
 ;----------------------------------------
return,1
end

pro chmp_add,result
  common chmp, self
  chmp_self
  if size(result,/tname) eq 'STRUCT' then begin
     i=0
     count=0
     FOREACH r, self.solution DO BEGIN
      if (r.a eq result[0].a) and (r.b eq result[0].b) then begin
        idx=i
        count+=1
        goto,matched
      endif
      i+=1
     ENDFOREACH
  endif
  matched:
  if count gt 0 then begin
    self.solution[i]=result[0]
    message,string(self.solution[i].a,self.solution[i].b,format="('Already existent [a=',g0,',b=',g0,'] solution replaced by a new one!')"),/cont
  endif else self.solution->Add,result[0]
end

pro chmp_start
  common chmp, self
  chmp_self
  if ~chmp_ready() then goto,no_start
  tasklist=self.tasks
  bridges=self.bridges
  if tasklist.IsEmpty() then begin
    message,'No tasks in queue, nothing to be done!',/info
    return
  endif
  pending=chmp_where(/pending,task_count)
  if task_count eq 0 then begin
    message,'No pending tasks in queue, nothing to be done!',/info
    return
  endif

  allbridges=bridges->Get(/all,count=count)
  if count eq 0 then begin
    message,'No parallel processing bridges are alive, use "IDL->chmp,N" to initialize at least one!',/info
    goto,no_start
  endif
  for i=0, count-1 do begin
    code=allbridges[i]->Status(error=error)
    if code ne 1 then freebridges=n_elements(freebridges) eq 0?allbridges[i]:[freebridges,allbridges[i]]
  end
  bridge_count=n_elements(freebridges)
  if bridge_count eq 0 then begin
    message,'All bridges are busy, tasks will be waiting in the queue!',/info
    return
  endif
  cp_start_time=systime(/seconds)
  for i=0,min([bridge_count,task_count])-1 do begin
    task=tasklist(pending[i])
    chmp_assigntask,freebridges[i],task,/onstartsearch
  endfor
  no_start:
  chmp_status,/tasks
end

pro chmp_callback,status,error,bridge,userdata
  common chmp, self
  chmp_self
  tasklist=self.tasks
  bridges=self.bridges
  bridge_id=bridge->GetVar('id')
  task_id=bridge->GetVar('task_id')
  start_time=bridge->GetVar('start_time')
  duration=systime(/seconds)-start_time
  if tasklist->IsEmpty() then return
  task=tasklist(task_id)
  if task_id ne task.id then message,'WARNING! Task ID mismatch- Data may be corrupted!!!', /info
  if status eq 4 then begin
    task.status='Aborted'
    task.duration=duration
    tasklist(task.id)=task
    abort=1
  endif else  begin
    result=bridge->GetVar('result')
    if size(result,/tname) eq 'STRUCT' then self.solution->add,result
    task.status='Completed'
    task.duration=duration
    tasklist(task.id)=task
  endelse
  if  self.quiet eq 0 then begin
    print,'________________________'
    message,strcompress(string(task.a, task.b,task.duration,format="('[a=',g0,', b=',g0,'] solution computed in ',g0,' seconds')")),/info
    chmp_status,/tasks
    print,'________________________'
  end
  
  pending=chmp_where(/pending,task_count)
  if task_count gt 0 and ~keyword_set(abort) then begin
    task=tasklist(pending[0])
    chmp_assigntask,bridge,task
  endif else  begin
    completed=chmp_where(/completed,/aborted,ccount)
    if ccount eq tasklist->Count() then begin
      solfile=strcompress(self.tmpDir+path_sep()+'gxchmp_solution.sav')
      result=self.solution->ToArray()
      save,result,file=solfile
      message, 'All tasks have been processed and the results temporary saved to!'+$
                string(10b)+solfile,/info
      chmp_flush_queue
    endif
  endelse
end
   

pro chmp, nthreads,fresh=fresh, _extra=_extra
  compile_opt IDL2,hidden
  common chmp, self
  chmp_self

  if (size(_extra,/tname) eq 'STRUCT') then begin
    tags=tag_names(_extra)
    for k=0,n_elements(tags)-1 do begin
      (SCOPE_VARFETCH(tags[k],/enter, LEVEL=0)) =_extra.(k)
    endfor
  endif else if n_params() eq 0 then help=1

  if keyword_set(exit) then begin
    if keyword_set(exit) then begin
      GXMpath=self.GXMpath
      RefDataPath=self.RefDataPath
      modDir=self.modDir
      psDir=self.psDir
      alist=self.alist
      blist=self.blist
      qlist=self.qlist
      levels=self.levels
      renderer=self.renderer
      fov=self.fov
      res=self.res
      ebtelpath=self.ebtelpath
      save,GXMpath,RefDataPath,modDir,psDir,alist,blist,qlist,levels,renderer,fov,res,ebtelpath,file='gxchmp.ini'
      obj_destroy,self.tasks
      obj_destroy,self.bridges
      obj_destroy,self.solution
      self=!null
      message,'Bye!',/cont,/info
      return
    endif
  endif

  if isa(nthreads,/number) then chmp_set_bridges,nthreads,_extra=_extra

  if keyword_set(alist)then begin
    alist=chmp_checklist(alist)
    if isa(alist,/string) then begin
      self.alist=alist
    endif
    chmp_reset,/alist
  end

  if keyword_set(blist) then begin
    blist=chmp_checklist(blist)
    if isa(blist,/string) then begin
      self.blist=blist
    endif
    chmp_reset,/blist
  end

  if keyword_set(levels) then begin
    levels=chmp_checklist(levels)
    if isa(levels,/string) then begin
      self.levels=levels
    endif
    chmp_reset,/levels
  end

  if keyword_set(qlist) then begin
    qlist=chmp_checklist(qlist)
    if isa(qlist,/string) then begin
      self.qlist=qlist
    endif
    chmp_reset,/qlist
  end

  if keyword_set(ref) and ~keyword_set(refdatapath) then refdatapath=ref
  if keyword_set(refdata) and ~keyword_set(refdatapath) then refdatapath=refdata

  if isa(refdatapath) then begin
    if isa(refdatapath,/string) then begin
      if file_exist(refdatapath) and refdatapath ne '' then begin
        restore,refdatapath
        if size(ref,/tname) eq 'STRUCT' then begin
          if tag_exist(ref,'a_beam') then valid=1
        endif else valid=0
      endif else valid=0
      if valid eq 1 then self.refdatapath=refdatapath else message,'Not a valid path or a valid data referance file structure!',/info,/cont
    endif
    if ~keyword_set(status) then chmp_status,/refdatapath
  end

  if isa(gxmpath) then begin
    if isa(gxmpath,/string) then begin
      if file_exist(gxmpath) then begin
        model=gx_read(gxmpath)
        if obj_isa(model,'gxmodel') then begin
          self.GXMpath=gxmpath
          obj_destroy,model
          valid=1
        endif else valid=0
      endif else valid=0
      if valid eq 1 then self.GXMpath=gxmpath else message,'Not a valid path or a valid GX model file structure!',/info,/cont
    endif
    if ~keyword_set(status) then chmp_status,/gxmpath
  endif

  if isa(renderer) then begin
    info=gx_rendererinfo(renderer)
    valid=isa(info)?1:0
    if valid eq 1 then self.renderer=renderer else $
      message,'Not a valid renderer!',/info,/cont
    if ~keyword_set(status) then chmp_status,/renderer
  endif

  if isa(res) then begin
    if isa(res,/string)then begin
      strarr=str2arr(res,';')
      for i=0,n_elements(strarr)- 1 do begin
        result=execute(strarr[i])
      endfor
      if ~(n_elements(raw) eq 2 and n_elements(resize) eq 2) then begin
        message,'Invalid syntax! Expected: "res=raw=[Nx,Ny]; resize=[Nx,Ny]"',/info
      endif else self.res=res
    end
    chmp_status,/res
  endif

  if isa(fov) then begin
    if isa(fov,/string)then begin
      strarr=str2arr(fov,';')
      for i=0,n_elements(strarr)- 1 do begin
        result=execute(strarr[i])
      endfor
      if ~(n_elements(center) eq 2 and n_elements(range) eq 2) then begin
        message,'Invalid syntax! Expected: fov="center=[Xc,Yc]; range=[Xarcsec,Yarcsec]"',/info
      endif else self.fov=fov
    end
    chmp_status,/fov
  endif

  if isa(ebtelpath) then begin
    if isa(ebtelpath,/string) then begin
      if ebtelpath ne '' then path=gx_findfile(file_basename(ebtelpath),folder='') else path=!null
      if isa(path,/string)then self.ebtelpath=path else $
        message, 'No EBTEL table named "'+file_basename(ebtelpath)+'" found in '+file_dirname(self.ebtelpath),/cont
    endif
    chmp_status,/ebtelpath
  endif

  if keyword_set(flush) then chmp_flush_queue
  if keyword_set(help) then chmp_help
  if keyword_set(bridges) then chmp_status,/bridges
  if keyword_set(tasks) then chmp_status,/tasks
  if keyword_set(status) then chmp_status,/status
  if keyword_set(script)then mprint,chmp_script()
  if keyword_set(start) then chmp_start
  if keyword_set(abort) then chmp_abort
  if keyword_set(quiet) then self.quiet=1;
  if keyword_set(loud) then self.quiet=0;
  if keyword_set(reset) then begin
    chmp_reset
    chmp_status
  endif
end

