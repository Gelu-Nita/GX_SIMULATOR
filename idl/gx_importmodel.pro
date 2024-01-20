function gx_ImportModel,source,box=box
 catch, error_stat
 if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return,obj_new()
 end
   if n_elements(source) eq 0 then begin
    source=dialog_pickfile(filter='*.sav',$
                  DEFAULT_EXTENSION='sav',$
                  /read,/must_exist,$
                  title='Please select a file containig a {Bx,By,Bz} datacube structure')
   if ~file_exist(source)  then return, obj_new()            
   endif
   if ~(size(source,/tname) eq 'STRING' or size(source,/tname) eq 'STRUCT') then begin
    message,'Input variable is neither a file name, nor a structure, operation aborted!' ,/info
    return, obj_new()
   endif
   if size(source,/tname) eq 'STRING' then begin
    file=source
    osav=obj_new('idl_savefile',file)
    names=osav->names()
    valid=0
    for i=0,n_elements(names)-1 do begin
      catch, err
      if err ne 0 then begin
        catch,/cancel
        restore,source,/relaxed
        result=box
        i--
        goto,skip
      endif
      osav->restore,names[i]
      skip:
      e=execute('result=size('+names[i]+',/tname)')
      if result eq 'STRUCT' then begin
       e=execute('box=temporary('+names[i]+')')
       validBxByBz=(tag_exist(box,'Bx') and tag_exist(box,'By') and tag_exist(box,'Bz'))
       validB=tag_exist(box,'bcube')
      endif 
     endfor
    endif else begin
     if size(source,/tname) eq 'STRUCT' then begin
      box=source
      validBxByBz=(tag_exist(box,'Bx') and tag_exist(box,'By') and tag_exist(box,'Bz'))
      validB=tag_exist(box,'bcube')
     end  
    endelse 
    valid=(validBxByBz eq 1) or (validB eq 1)
    if ~valid then begin
     message,'Invalid datacube file:'+$
     'No structure having the required Bx,By and Bz or Bcube tags has been found!',/info
     return,obj_new()
    end
    if tag_exist(box,'index') and tag_exist(box,'base') then begin
      model=gx_box2model(box)
      if obj_isa(model,'gxmodel') then goto,box2model
      return,model
    endif
    ns=tag_exist(box,'lat')?box.lat:0
    ew=tag_exist(box,'lon')?box.lon:0
    if tag_exist(box,'dr') then begin
     dr=box.dr
     case n_elements(dr) of 
       3:begin
          dx=dr[0]
          dy=dr[1]
          dz=dr[2]
         end
       1:begin
          dx=dr[0]
          dy=dr[0]
          dz=dr[0]
         end  
     else: 
     endcase     
    endif else begin
      r=962.97562
      res=1.98
      dx=res/r
      dy=res/r
      dz=res/r
    end   
    if validBxByBz eq 1 then size=(size(box.bx))[0:3] else size=(size(box.bcube))[0:3]
    model=obj_new('gxmodel')
    sx=size[1]
    sy=size[2]
    sz=size[3]
    XCOORD_CONV=[0,dx]
    YCOORD_CONV=[0,dy]
    ZCOORD_CONV=[0,dz]
    data=bytarr(sx,sy,sz)
    volume=obj_new('gxVolume',data,name='Volume',XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV,/interpolate,hints=2)   
    if validBxByBz eq 1 then begin
     volume->SetVertexAttributeData,'Bx',box.Bx
     volume->SetVertexAttributeData,'By',box.By
     volume->SetVertexAttributeData,'Bz',box.Bz
    endif else begin
     volume->SetVertexAttributeData,'Bx',box.Bcube[*,*,*,0]
     volume->SetVertexAttributeData,'By',box.Bcube[*,*,*,1]
     volume->SetVertexAttributeData,'Bz',box.Bcube[*,*,*,2]
    end
    p=dblarr(3,8)
    x=[0,0,sx,sx,0,0,sx,sx]
    y=[0,sy,sy,0,0,sy,sy,0]
    z=[0,0,0,0,sz,sz,sz,sz]
    xrange=[-sx/2.,sx/2.]*dx
    yrange=[-sy/2.,sy/2.]*dy
    zrange=[0,sz]*dz 
    model->SetProperty,NS=NS,EW=EW,$
           XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV,$
           XRANGE=XRANGE,YRANGE=YRANGE,ZRANGE=ZRANGE,$
           subgridpts=subgridpts,steps=steps,bscale=bscale,size=size 
    if ~tag_exist(box,'refmaps') then begin
      if tag_exist(box,'time') then time=box.time
      if tag_exist(box,'date') then time=box.date
      if n_elements(time) gt 0 then begin
        xy=lonlat2xy([ew,ns],time)
        xc=xy[0]
        yc=xy[0]
        ans=get_rb0p(time)
        delta_x=dx*ans[0]
        delta_y=dy*ans[0]
      endif
     model->SetBaseMap,volume=volume,time=time,xc=xc,yc=yc,dx=delta_x,dy=delta_y
    endif else begin
     model->SetBaseMap,volume=volume,refmaps=*(box.refmaps)
     if ~tag_exist(box,'lat') or ~tag_exist(box,'lon') then begin
      map=(*(box.refmaps))[0]->get(0,/map)
      LatLon=arcmin2hel(map.xc/60.,map.yc/60.,date=map.time)
      model->SetProperty,NS=latlon[0],EW=latlon[1]
     end
    end 
    model->SetProperty,volume=volume
    box2model:
    model->SetRoi
    model->UpdateDef
    model->DisplayMap,2 
    return,model
end