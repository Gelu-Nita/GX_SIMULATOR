;this is general purpose macro that may be used to compute radiation transfer images programatically
function gx_render,model,renderer,info=info,all_at_once=all_at_once,logfile=logfile,quiet=quiet,_extra=_extra
  t0=systime(/s)
  if ~isa(model) then begin
    message,'None or invalid model provided! Operation aborted!',/info
    return,!null
  endif else begin
    if ~obj_valid(model) then begin
      message,'None or invalid model provided! Operation aborted!',/info
      return,!null
    endif else begin
      if ~obj_isa(model,'gxmodel') then begin
        message,'Invalid gx model provided! Operation aborted!',/info
        return,!null
      end  
    endelse
  endelse
  if size(info,/tname) ne 'STRUCT' then begin
    info=gx_rendererinfo(renderer)
    if ~isa(info) then begin
      message,'Invalid renderer routine! Operation aborted!',/info
      return,!null
    endif
    info=model->UpdateEUVinfo(info)
  end
  if isa(_extra) then begin
    names=tag_names(_extra)
    for k=0,n_elements(names)-1 do begin
      gx_setparm,info,names[k],_extra.(k)
    endfor
    if tag_exist(_extra,'freqlist') then begin
      gx_setparm,info,'freqlist',_extra.freqlist
      freqlist=info.spectrum.x.axis
    endif
  endif
  dr=model->GetFovPixSize(unit='cm')
  gx_setparm,info,'dS',dr[0]*dr[1]
  gx_setparm,info,'distance',model->GetAu()
  fovmap=model->GetFovMap()
  sz=size(fovmap->get(/data))
  nx=sz[1]
  ny=sz[2]
  chanlist=info.spectrum.x.axis
  nchan=n_elements(chanlist)
  if tag_exist(info,'nparms') then begin
    if ~ptr_valid(model->GetGrid())then begin
      xc=fovmap->get(/xc)
      yc=fovmap->get(/yc)
      xfov=delta(get_map_xrange(fovmap->get(/map),/edge))
      yfov=delta(get_map_yrange(fovmap->get(/map),/edge))
      void=model->SetFov(nx=nx,ny=ny,xfov=xfov,yfov=yfov,/compute_grid)
    endif
    nvox=(size(*(model->GetGrid()),/dim))[3]
    nparms=long(info.nparms.value)
    nparms[0:2]=[nx,nvox,nchan]
    info.nparms.value=nparms
  endif
  if tag_exist(info,'rparms') then rparms=info.rparms.value
  if tag_exist(info,'sparms') then sparms=info.sparms.value
  if tag_exist(info,'aparms') then begin
    info=rep_tag_value(info,model->concatenate_aparms(),'aparms')
    E_arr=info.aparms.E_arr
    mu_arr=info.aparms.mu_arr
    f_arr=info.aparms.f_arr
  endif
  
  if keyword_set(all_at_once) then begin
    if size(logfile,/tname) eq 'STRING' then message,'The option to create a parameter log file is not available if the /all_at_once computation mode !',/info
    grid=model->GetGrid()
    sz=size(*grid)
    if n_elements(nparms) gt 0 then nparms[0]=sz[2]*sz[3]
    if n_elements(nparms) gt 1 then nparms[1]=sz[4]
    grid=reform(*grid,4,sz[2]*sz[3],1,sz[4])
    Model->Slice,info.parms,row,grid,scanner=scanner
    parms=(*scanner).parms
    rowdata=make_array([sz[2]*sz[3],(info).pixdim],/float)
    result=execute(info.execute)
    data=reform(temporary(rowdata),[nx,ny,info.pixdim])
  endif else begin
    rowdata=make_array([nx,info.pixdim],/float)
    dim=[nx,ny,info.pixdim]
    data=make_array(dim,/float)
    for row=0, ny-1 do begin
      if ~keyword_set(quiet) then print,strcompress(string(row+1,ny,format="('computing image row ', i5,' out of', i5,'...')"))
      rowdata[*]=0
      if ptr_valid(scanner) then for k=1,n_tags(*scanner)-1 do (*scanner).(k)[*]=0
      model->Slice,info.parms,row,scanner=scanner
      parms=(*scanner).parms
      timenow=systime(1)
      result=execute(info.execute)
      if ~keyword_set(quiet) then print,strcompress(string(systime(1)-timenow,format="(' computed in ',g0, ' s')"))
      data[*,row,*,*,*]=rowdata
      if size(logfile,/tname) eq 'STRING' then begin
        if row eq 0 then begin
         MULTI_SAVE,/new,log,{row:-1L,parms:parms,$
          data:rowdata,grid:transpose(reform((*(*scanner).grid)[*,*,0,*]),[1,2,0])},file=logfile, $
          header={renderer:renderer,info:info,fovmap:fovmap,nx:nx,ny:ny,xrange:fovmap->get(/xrange),yrange:fovmap->get(/yrange),ebtel:gx_ebtel_path()}
        endif
        MULTI_SAVE,log,{row:long(row),parms:parms,data:rowdata,grid:transpose(reform((*(*scanner).grid)[*,*,row,*]),[1,2,0])},file=logfile, header=info
      endif
    endfor
    if size(logfile,/tname) eq 'STRING' then begin
      free_lun,log,/force
    endif
  endelse
  model->getproperty,xcoord_conv=dx,ycoord_conv=dy
  dx=dx[1]
  dy=dy[1]
  dim=model->Size()
  dz=model->GetVertexData('dz')
  if n_elements(dz) eq 0 then begin
    model->getproperty,zcoord_conv=dz
    dz=dz[1]
  endif
  gxcube={info:info,data:data,renderer:renderer,fovmap:fovmap,model:{dx:dx,dy:dy,dz:dz,dim:dim[1:3]}}
  message,strcompress(string(systime(/s)-t0,format="('Computation done in ',f10.3,' seconds')")),/info
  return,gxcube
end