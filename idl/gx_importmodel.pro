function gx_ImportModel,source,box=box
 catch, error_stat
 if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return,obj_new()
 end
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
     answ=dialog_message('Invalid datacube file:'+string(10b)+$
     'No structure having the required Bx,By and Bz or Bcube tags has been found!')
     return,0
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
    volume=model->GetVolume()
    if tag_exist(box,'n') then begin
      volume->SetVertexAttributeData,'n',box.n
    end
    if tag_exist(box,'T') then begin
      volume->SetVertexAttributeData,'T',box.T
    end
    if tag_exist(box,'idx') then begin
      volume->SetVertexAttributeData,'idx',box.idx
    end
    if tag_exist(box,'bmed') then begin
      volume->SetVertexAttributeData,'bmed',box.bmed
    end
    if tag_exist(box,'length') then begin
      volume->SetVertexAttributeData,'length',box.length
    end
    if tag_exist(box,'alpha') then begin
      volume->SetVertexAttributeData,'alpha',box.alpha
    end
    if tag_exist(box,'curlb') then begin
      volume->SetVertexAttributeData,'curlb',box.curlb
    end
    if tag_exist(box,'foot1') then begin
      volume->SetVertexAttributeData,'foot1',box.foot1
    end
    if tag_exist(box,'foot2') then begin
      volume->SetVertexAttributeData,'foot2',box.foot2
    end
    if tag_exist(box,'oidx') then begin
      volume->SetVertexAttributeData,'oidx',box.oidx
    end
    if tag_exist(box,'obmed') then begin
      volume->SetVertexAttributeData,'obmed',box.obmed
    end
    if tag_exist(box,'olength') then begin
      volume->SetVertexAttributeData,'olength',box.olength
    end
    if tag_exist(box,'ofoot1') then begin
      volume->SetVertexAttributeData,'ofoot1',box.ofoot1
    end
    if tag_exist(box,'ofoot2') then begin
      volume->SetVertexAttributeData,'ofoot2',box.ofoot2
    end
    
    if tag_exist(box,'chromo_idx') then begin
      volume->SetVertexAttributeData,'chromo_idx',box.chromo_idx
    end
    if tag_exist(box,'chromo_n') then begin
      volume->SetVertexAttributeData,'chromo_n',box.chromo_n
    end
    if tag_exist(box,'chromo_T') then begin
      volume->SetVertexAttributeData,'chromo_T',box.chromo_T
    end
    if tag_exist(box,'n_htot') then begin
      volume->SetVertexAttributeData,'n_htot',box.n_htot
    end
    if tag_exist(box,'n_hi') then begin
      volume->SetVertexAttributeData,'n_hi',box.n_hi
    end
    if tag_exist(box,'n_p') then begin
      volume->SetVertexAttributeData,'n_p',box.n_p
    end
    if tag_exist(box,'dz') then begin
      volume->SetVertexAttributeData,'dz',box.dz
    end
    if tag_exist(box,'tr') then begin
      volume->SetVertexAttributeData,'tr',box.tr
    end
    if tag_exist(box,'chromo_layers') then begin
      volume->SetVertexAttributeData,'chromo_layers',box.chromo_layers
    end
    model->SetRoi
    model->UpdateDef
    model->ResetPosition  
    return,model
end