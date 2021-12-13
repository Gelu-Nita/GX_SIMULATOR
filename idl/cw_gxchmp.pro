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
  default,alist,'0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3'
  self.alist=alist
  default,blist,'0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2'
  self.blist=blist
  default,qlist,'0.001,0.01'
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
  self.WinOS=!version.os_family eq 'Windows'
  void=self->gxWidget::Init(wBase,self,KILL_NOTIFY='objgxchmpKill',_extra=_extra)
  self->SetBridges
  self->ResetTasks
  return,1
end

pro gxchmp__define
  struct_hide,{gxchmp, inherits gxwidget,GXMpath:'',RefDataPath:'',modDir:'',psDir:'',TmpDir:'',EbtelPath:'',$
    renderer:'',alist:'',blist:'',qlist:'',levels:'',fov:'',res:'',completed:0l,$
    RefDataStruct:ptr_new(),solution:obj_new(),bridges:obj_new(),tasks:obj_new(),WinOS:0l}
end

pro objgxchmpKill,wBase
  widget_control,wBase,get_uvalue=obj
  obj->GetProperty,bridges=bridges,solution=solution
  if isa(bridges, 'OBJ') then obj_destroy,bridges
  if isa(solution, 'OBJ') then obj_destroy,solution
  obj->GetProperty,GXMpath=GXMpath,RefDataPath=RefDataPath,modDir=modDir,psDir=psDir,$
    alist=alist,blist=blist,qlist=qlist,levels=levels,renderer=renderer,$
    fov=fov,res=res, ebtelpath=ebtelpath
  save,GXMpath,RefDataPath,modDir,psDir,alist,blist,qlist,levels,renderer,fov,res,ebtelpath,file='gxchmp.ini'
  obj_destroy,obj
end


function gxchmp::SaveSolution,question=question
  solution_unsaved=(self.solution->Count() gt 0)
  if keyword_set(question) then begin
      if solution_unsaved gt 0 then begin
         answ=dialog_message('The search results located at '+self.tmpDir+path_sep()+'gxchmp_solution.sav '+ $
      'will be overwritten if a new search is performed! Do you want to save this solution to a permanent location before exiting the application?',/question,/cancel)
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
   status={status:'Initializing',task:'',time:'',calls:'',error:''}
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
      bridge_status=replicate({status:'Initializing',task:'',time:'',calls:'',error:''},nbridges)
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

pro gxchmp::ResetTasks
    code=execute('a=['+self.alist+']')
    code=execute('b=['+self.blist+']')
    if obj_valid(self.tasks) then self.tasks->Remove,/all else self.tasks=list()
    for i=0,n_elements(a)-1 do begin
      for j=0,n_elements(b)-1 do begin
        self.tasks->add,{id:self.tasks->Count(),a:string(a[i],format='(g0)'),b:string(b[j],format='(g0)'),res2:0.0d,qres2:0.0d,chi2:0.0d,qchi2:0.0d,status:'pending'}
      endfor
    endfor
    self->UpdateTaskQueue
    self->UpdateBridgeStatus,/reset
end   
    
pro gxchmp::UpdateTaskQueue
    rows=[]
    rows_labels=[]
    for k=0,self.tasks->Count()-1 do begin
      task=self.tasks(k)
      rows_labels=[rows_labels,string(task.id,format='(g0)')]
      rows=[rows,rem_tag(task,'ID')]
    endfor
      wQueue=widget_info(self.wBase,find_by_uname='Queue')
      widget_control,wQueue,table_ysize=n_elements(rows)
      if n_elements(rows) gt 0 then widget_control,wQueue,set_value=rows,row_labels=[rows_labels]
end          

pro gxchmp::GetProperty,GXMpath=GXMpath,RefDataPath=RefDataPath,modDir=modDir,psDir=psDir,$
                       RefDataStruct=RefDataStruct,TmpDir=TmpDir,alist=alist,blist=blist,qlist=qlist,$
                       levels=levels,solution=solution, tasks=tasks,EBTELpath=EBTELpath,$
                       renderer=renderer,bridges=bridges,fov=fov,res=res,nBridges=nBridges,completed=completed,$
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
    self->gxwidget::GetProperty,_extra=extra
end

function gxchmp::Str2Arr,strlist
 compile_opt idl2, static
 result=execute('list=['+strlist+']')
 return,list
end

pro gxchmp::abort
  bridges=self.bridges->Get(/all,count=count)
  for i=0,count-1 do begin
    code=bridges[i]->Status(error=error)
    if code eq 1 then bridges[i]->Abort
  end
end

pro gxchmp::AssignTask,bridge,task_id,OnStartSearch=OnStartSearch
    task=self.tasks[task_id]
    if ~keyword_set(OnStartSearch) then begin
      calls=bridge->GetVar('calls')
      bridge->SetVar,'calls',calls+1
    endif else bridge->SetVar,'calls',0
    bridge->SetVar,'task_id',task.id
    bridge->SetVar,'t_start',systime(/s)
    bridge->SetVar,'a',task.a
    bridge->SetVar,'b',task.b
    bridge_id=bridge->GetVar('id')
    task.status=string(bridge_id,format="('Assigned to thread #',i0)")
    self.tasks[task_id]=task
    bridge->Execute,'delvar,result'
    bridge->Execute,self->GetScript(task_id=task_id),/nowait
    self->UpdateTaskQueue
  end
  
  
function gxchmp::GetScript,task_id=task_id
script='result=gx_search4bestq('
script+='  gxm_path="'+self.gxmpath+'"'
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
script+=string(arr2str((str2arr(', q_start=['+self.qlist+']')),format="(a0)"))
script+=string(arr2str((str2arr(', levels=['+self.levels+']')),format="(a0)"))
script+=')'
return,strcompress(script)
end

pro gxchmp::OnStartSearch
if self.bridges->Count() eq 0 then begin
  answ=dialog_message('At least one parallel thread should be intialized before the search is started! Please do so and try again!',/info)
  return
endif
if self.completed gt 0 then begin
  answ=self.SaveSolution(/question)
  if strupcase(answ) eq 'CANCEL' then return
endif
self.solution->Remove,/all
self.completed=0
self.ResetTasks
self->ControlWidgets,sensitive=0
ntasks=self.tasks->Count()
nbridges=self.bridges->Count()
i=0
j=0
while i lt ntasks and j lt nbridges do begin
  bridge=self.bridges->Get(position=j)
  self->AssignTask,bridge,i,/OnStartSearch
  i++ & j++
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
  result=bridge->GetVar('result')
  self.solution->add,create_struct(result,'task_id',task_id)
  task=self.tasks[task_id]
  t_start=bridge->GetVar('t_start')
  t_end=bridge->GetVar('t_end')
  task.status=strcompress(string(t_end-t_start,format="('Completed in ',g0,'s')"))
  task.res2=result.RES2_BEST
  task.chi2=result.CHI2_BEST
  task.qres2=result.Q_RES2_BEST
  task.qchi2=result.Q_CHI2_BEST
  self.tasks[task_id]=task
  self->UpdateTaskQueue
  self.completed+=1
  self->message,strcompress(string(n_elements(self.solution), ntasks, task.a, task.b, format="('Completed ', i0,' tasks out of ',i0,'; a=',g0, ' b=', g0)"))
  skip:
  if self.completed ne ntasks then begin
  i=0
  while i lt ntasks do begin
  if (self.tasks[i]).status eq 'pending' then begin
    self->AssignTask,bridge,i
    i=ntasks
  endif else i++
  endwhile 
  endif else self->OnEndSearch
end

pro gxchmp::ControlWidgets,sensitive=sensitive
 widget_list=['settings_base','search_base','results_base']
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
  gx_plotbestmwmodels_ebtel, result, self.psDir,levels=self->Str2Arr(self.levels)
  files=[strcompress(self.psDir+path_sep()+'BestRES.ps'),strcompress(self.psDir+path_sep()+'BestCHI.ps')]
  if n_elements(result) ge 4 then files=[files,strcompress(self.psDir+path_sep()+'Best of Bests.ps')]
  self->message,['Best solutions plots saved to:..',files]
end

pro gxchmp::CreatePanel,xsize=xsize,ysize=ysize
  device, get_screen_size=scr
  if not exist(xsize) then xsize = fix (scr[0] * .45)
  if not exist(ysize) then ysize = xsize *1.1
  
  
  main_Base=widget_base(self.wBase,/column)

  toolbar= widget_base(main_base, /row,/toolbar)
  wExecute=widget_button(toolbar,value=gx_bitmap(gx_findfile('play.bmp')),tooltip='Execute Search Tasks',/bitmap,uname='execute')
  wSave=widget_button(toolbar,value=gx_bitmap(filepath('save.bmp', subdirectory=['resource', 'bitmaps'])),tooltip='Save search results',/bitmap,uname='save') 
  wAbort=widget_button(toolbar,value=gx_bitmap(gx_findfile('abort.bmp')),tooltip='Abort all tasks',/bitmap,uname='abort')

  wRedo=CW_BGROUP(toolbar,'Recompute and overwrite already existing maps in the model maps repository',/nonexclusive,set_value=[0],uname='redo')
  wClearLog=widget_button(toolbar,value=gx_bitmap(filepath('delete.bmp', subdirectory=['resource', 'bitmaps'])),tooltip='Clear execution log',/bitmap,uname='clearlog')
  wLabelBase=widget_base(main_base,/row,scr_xsize=scr_xsize,/frame)
  wLabelBase=widget_base(wLabelBase,/row)
  wlabel=widget_label(wLabelBase,value='Execution Console',scr_xsize=scr_xsize)
  wConsole=widget_text(main_base,scr_ysize=scr[1]/16,$
    value='',/scroll,uname='console',/wrap)
  
  ysize=0.75*ysize  
  wTab=Widget_Tab(main_base)
  
  wSettings = WIDGET_BASE(wTab, TITLE='Input/Output Settings', /COLUMN)
  wSettingsPanel=widget_base(wSettings,/column,xsize=scr[0],ysize=scr[1],$
    x_scroll_size=xsize,y_scroll_size=ysize,/scroll)
  geom = widget_info (wSettingsPanel, /geom)
  scr_xsize=0.98*geom.scr_xsize
  scr_ysize=0.98*geom.scr_ysize
  wSettingsBase=widget_base(wSettingsPanel,/column,uname='settings_base')
  
  wSearch = WIDGET_BASE(wTab, TITLE='Search Engine', /COLUMN)
  wSearchmpanel=widget_base(wSearch,/column,xsize=scr[0],ysize=scr[1],$
    x_scroll_size=xsize,y_scroll_size=ysize,/scroll)
  geom = widget_info (wSearchmpanel, /geom)
  scr_xsize=0.98*geom.scr_xsize
  scr_ysize=0.98*geom.scr_ysize
  wSearchBase=widget_base(wSearchmpanel,/column,uname='search_base')
  
  ;wResults = WIDGET_BASE(wTab, TITLE='Results', /COLUMN)

  
  wQBase=widget_base(wSearchBase,/row,scr_xsize=scr_xsize,/frame)
  wQFormulaBase=widget_base(wQBase,/row)
  wlabel=widget_label(wQFormulaBase,value='EBTEL Coronal Heating Model: Q=q0*B^a/L^b',scr_xsize=scr_xsize,/align_center)

  wAlistBase=widget_base(wSearchBase,/row,scr_xsize=scr_xsize,/frame)
  wAlistBase=widget_base(wAlistBase,/row)

  wlabel=widget_label(wAlistBase,value='"a" parameter space           ',scr_xsize=label_scr_xsize)
  label=widget_info(wlabel,/geometry)
  label_scr_xsize=label.scr_xsize
  wResetAlist= widget_button(wAlistBase, $
    value=gx_bitmap(filepath('reset.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Reset "a" parameter list to default',uname='alist_reset')
  geom = widget_info (wAlistBase, /geom)
  wAlist=widget_text(wAlistBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='alist',/editable,$
    value=self.alist)

  wBlistBase=widget_base(wSearchBase,/row,scr_xsize=scr_xsize,/frame)
  wBlistBase=widget_base(wBlistBase,/row)
  wlabel=widget_label(wBlistBase,value='"b" parameter space',scr_xsize=label_scr_xsize)
  wResetBlist= widget_button(wBlistBase, $
    value=gx_bitmap(filepath('reset.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Reset "b" parameter list to default',uname='blist_reset')
  geom = widget_info (wBlistBase, /geom)
  wBlist=widget_text(wBlistBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='blist',/editable,$
    value=self.blist)

  wQlistBase=widget_base(wSearchBase,/row,scr_xsize=scr_xsize,/frame)
  wQlistBase=widget_base(wQlistBase,/row)
  wlabel=widget_label(wQlistBase,value='"q0" initial guess set',scr_xsize=label_scr_xsize)
  wResetQlist= widget_button(wQlistBase, $
    value=gx_bitmap(filepath('reset.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Reset "qo" parameter list to default',uname='qlist_reset')
  geom = widget_info (wQlistBase, /geom)
  wQlist=widget_text(wQlistBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='qlist',/editable,$
    value=self.qlist)
    
  wLevelsBase=widget_base(wSearchBase,/row,scr_xsize=scr_xsize,/frame)
  wLevelsBase=widget_base(wLevelsBase,/row)
  wlabel=widget_label(wLevelsBase,value='[Mask, ROI] Levels (%)',scr_xsize=label_scr_xsize)
  wResetLevels= widget_button(wLevelsBase, $
    value=gx_bitmap(filepath('reset.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Reset [Mask, ROI] levels to default',uname='levels_reset')
  geom = widget_info (wLevelsBase, /geom)
  wLevels=widget_text(wLevelsBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='levels',/editable,$
    value=self.levels)   
  
  wLabelBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wLabelBase=widget_base(wLabelBase,/row)
  wlabel=widget_label(wLabelBase,value='GX Model and Reference Data Input',scr_xsize=scr_xsize,/align_center)

  wGXMBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wGXMpathBase=widget_base(wGXMBase,/row)
  wlabel=widget_label(wGXMpathBase,value='GX Model Path',scr_xsize=label_scr_xsize)
  wSelectGXMpath= widget_button(wGXMpathBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Select the path of a valid GX model',uname='gxmpath_select')
  geom = widget_info (wGXMpathBase, /geom)
  wGXMpathPath=widget_text(wGXMpathBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='GXMpath',/editable,$
    value=self.GXMpath)
    
  wRefDataBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wRefDatapathBase=widget_base(wRefDataBase,/row)
  wlabel=widget_label(wRefDatapathBase,value='Reference Data Path          ',scr_xsize=label_scr_xsize)
  wSelectRefDatapath= widget_button(wRefDatapathBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
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
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Select a path where to store model maps',uname='moddir_select')
  geom = widget_info (wModBase, /geom)
  wModDirPath=widget_text(wModDirBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='moddir',/editable,$
    value=self.ModDir)  
    
  wPsDirBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wPsBase=widget_base(wPsDirBase,/row)
  wlabel=widget_label(wPsBase,value='PostScript Repository',scr_xsize=label_scr_xsize)
  wSelectPsDir= widget_button(wPsBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Select a path where to store postscript figures',uname='psdir_select')
  geom = widget_info (wPsBase, /geom)
  wPsDirPath=widget_text(wPsDirBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='psdir',/editable,$
    value=self.PsDir) 
    
  wTmpDirBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wTmpBase=widget_base(wTmpDirBase,/row)
  wlabel=widget_label(wTmpBase,value='Temporary Directory',scr_xsize=label_scr_xsize)
  wSelectTmpDir= widget_button(wTmpBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
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
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Select the path to a valid IDL Fast Code Rendering Routine',uname='rendererpath_select')
  geom = widget_info (wRendererpathBase, /geom)
  wRendererpathPath=widget_text(wRendererpathBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='rendererpath',/editable,$
    value=self.renderer)
    
  wEBTELpathBase=widget_base(wSearchBase,/row,scr_xsize=scr_xsize,/frame)
  wEBTELpathpathBase=widget_base(wEBTELpathBase,/row)
  wlabel=widget_label(wEBTELpathpathBase,value='EBTEL DEM Table Path',scr_xsize=label_scr_xsize)
  wSelectEBTELpathpath= widget_button(wEBTELpathpathBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Select the path to a valid EBTEL DEM table',uname='EBTELpath_select')
  geom = widget_info (wEBTELpathpathBase, /geom)
  wEBTELpathpathPath=widget_text(wEBTELpathpathBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='EBTELpath',/editable,$
    value=self.EBTELpath)    
    
  wFOVBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wFOVBase=widget_base(wFOVBase,/row)
  wlabel=widget_label(wFOVBase,value='Model Maps FOV (arcsecs)',scr_xsize=label_scr_xsize)
  wFOVimport= widget_button(wFOVBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Import FOV from a map structure or map object',uname='fov_import')
  geom = widget_info (wFOVBase, /geom)
  wFOV=widget_text(wFOVBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='fov',/editable,$
    value=self.FOV)
    
  wResBase=widget_base(wSettingsBase,/row,scr_xsize=scr_xsize,/frame)
  wResBase=widget_base(wResBase,/row)
  wlabel=widget_label(wResBase,value='Model Maps Resolution (pixels)',scr_xsize=label_scr_xsize)
  wResimport= widget_button(wResBase, $
    value=gx_bitmap(filepath('open.bmp', subdirectory=['resource', 'bitmaps'])), $
    /bitmap,tooltip='Import Res from a map structure or map object',uname='res_import')
  geom = widget_info (wResBase, /geom)
  wRes=widget_text(wResBase,scr_xsize=scr_xsize-geom.scr_xsize,uname='res',/editable,$
    value=self.res)    
 
  wTaskBase=widget_base(wSearchBase,title='GXchmp Task Base',/column)
  wLabelBase=widget_base(wTaskBase,/row,scr_xsize=scr_xsize,/frame)
  wLabelBase=widget_base(wLabelBase,/row)
  wlabel=widget_label(wLabelBase,value='Computing Parallel Threads',scr_xsize=scr_xsize,/align_center)
  
  wNThreadsBase=widget_base(wTaskBase,/row,scr_xsize=scr_xsize,/frame)
  wNThreadsBase=widget_base(wNThreadsBase,/row)
  wNthreads=cw_objfield(wNThreadsBase,label=' Number of parallel threads',$
    scr_labelsize=label_scr_xsize,xtextsize=3,value=self.bridges->Count(),min=1,max=!CPU.HW_NCPU,inc=1,$
    lfont=!defaults.font,font=!defaults.font,uname='nthreads',/int)
 
  wBridges=widget_table(wSearchmpanel,font=!defaults.font,ysize=0,xsize=5,$
    y_scroll_size=0,x_scroll_size=0,$
    COLUMN_WIDTHS =[scr_xsize/10,2*scr_xsize/10,scr_xsize/10,scr_xsize/10,4*scr_xsize/10],$
    edit=0,format=format,$
    column_labels=['status','task','on task','calls','error message'],uname='bridge_status',$
    scr_xsize=scr_xsize,scr_ysize=scr_ysize/4,/resizeable_columns)
  
  wLabelBase=widget_base(wSearchmpanel,/row,scr_xsize=scr_xsize,/frame)
  wLabelBase=widget_base(wLabelBase,/row)
  wlabel=widget_label(wLabelBase,value='Best Models Search Queue',scr_xsize=scr_xsize,/align_center)

  wQueue=widget_table(wSearchmpanel,font=!defaults.font,ysize=0,xsize=7,$
    y_scroll_size=0,x_scroll_size=0,$
    COLUMN_WIDTHS =[scr_xsize/10,scr_xsize/10,scr_xsize/10,scr_xsize/10,scr_xsize/10,scr_xsize/10,4*scr_xsize/10],$
    edit=0,format=format,$
    column_labels=['a','b','Res2','Qres2','Chi2','Qchi2','task status'],uname='Queue',$
    scr_xsize=scr_xsize,scr_ysize=scr_ysize/2,/resizeable_columns)
  widget_control,wQueue,table_ysize=0
  
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
                     widget_control,event.id,get_value=rendererpath
                     goto,rendererpath_select
                    end
    'rendererpath_select':begin
        which,'gx_simulator',outfile=outfile
        cdir=file_dirname(file_dirname(outfile))
        path=cdir+path_sep()+'userslib'+path_sep()+'radio_nonflaring'+path_sep()+(self.WinOS?'windows':'unix')+path_sep()
        renderer=dialog_pickfile(filter='*.pro',TITLE='Please select a renderer IDL routine/wrapper',path=path,/must_exist)
        rendererpath_select:
        valid=file_exist(renderer)
        if valid eq 1 then self.renderer=renderer else answ=dialog_message('Not a valid renderer routine!')
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
                 if n_elements(self->Str2Arr(levels[0])) eq 0 then begin
                   answ=dialog_message('Invalid syntax!')
                   widget_control,event.id,set_value=self.levels
                 endif else self.levels=levels[0]
               end 
     'levels_reset':begin
                    self.levels='12,20,30,50,80'
                    widget_control,widget_info(self.wbase,find_by_uname='levels'),set_value=self.levels
                    end          
     'alist': begin
                 widget_control,event.id,get_value=list
                 if n_elements(self->Str2Arr(list[0])) eq 0 then begin
                   answ=dialog_message('Invalid syntax!')
                   widget_control,event.id,set_value=self.alist
                 endif else begin
                  self.alist=list[0]
                  self->ResetTasks
                 endelse
               end  
      'blist': begin
                 widget_control,event.id,get_value=list
                 if n_elements(self->Str2Arr(list[0])) eq 0 then begin
                   answ=dialog_message('Invalid syntax!')
                   widget_control,event.id,set_value=self.blist
                 endif else begin
                   self.blist=list[0]
                   self->ResetTasks
                 endelse
               end  
               
       'qlist': begin
                 widget_control,event.id,get_value=list
                 if n_elements(self->Str2Arr(list[0])) eq 0 then begin
                   answ=dialog_message('Invalid syntax!')
                   widget_control,event.id,set_value=self.qlist
                 endif else begin
                   self.qlist=list[0]
                   self->ResetTasks
                 endelse
               end          
     'alist_reset': begin
                   self.alist='0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3'
                   widget_control,widget_info(self.wbase,find_by_uname='alist'),set_value=self.alist
                   self->ResetTasks
               end
     'blist_reset': begin
                   self.blist='0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2'
                   widget_control,widget_info(self.wbase,find_by_uname='blist'),set_value=self.blist
                   self->ResetTasks
               end    
     'qlist_reset': begin
                 self.qlist='0.001,0.01'
                 widget_control,widget_info(self.wbase,find_by_uname='qlist'),set_value=self.qlist
                 self->ResetTasks
               end                                
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
     'execute':self->OnStartSearch  
     'abort':self->Abort   
     'save':answ=self.SaveSolution()                                 
    else:
  endcase
  return,self->Rewrite(event)
end


function cw_gxchmp,Base,_extra=_extra
  obj=obj_new('gxchmp',Base,_extra=_extra)
  obj->GetProperty,widget_id=widget_id
  return,widget_id
end