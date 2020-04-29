function gxVolume::Init,data,_extra=_extra
compile_opt hidden
 catch, error_stat
 if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return, 0
 end
 result=self->IDLgrVolume::Init(data,_extra=_extra)
 Self->SetProperty,zbuffer=1
 self.select='n_0'
 self.bscale=1
 self.nscale=1
 self.tscale=1
 flags=self->setflags(/NTstored,/TRadd,/TRMask,/TRfactor)
 rgb_curr=bytarr(3,256)
 tvlct,rgb_curr,/get
 loadct,39
 rgb=bytarr(3,256)
 tvlct,rgb,/get
 self->SetProperty,rgb_table0=rgb
 tvlct,rgb_curr
 return,result
end

function gxVolume::GetB,p,bx=Bx,by=By,Bz=Bz
;p is expressed in self's box fractional index coordinates
self->GetVertexAttributeData,'Bx',Bx
self->GetVertexAttributeData,'By',By
self->GetVertexAttributeData,'Bz',Bz
if (n_elements(p) eq 3) and (n_elements(Bx) ne 0) $
then return,[interpolate(bx,p[0],p[1],p[2]),interpolate(by,p[0],p[1],p[2]),interpolate(bz,p[0],p[1],p[2])]$
else return,[0,0,0]
end

function gxVolume::Size
 self->GetProperty,data0=data0
 return,size(data0)
end

pro gxVolume::SetProperty,wParent=wParent,select=select,tscale=tscale,nscale=nscale,bscale=bscale,gyro=gyro,_extra=extra
 if exist(wParent) then self.wParent=wParent
 if exist(select) then self.select=select
 if exist(bscale) then self.bscale=bscale
 if exist(nscale) then self.nscale=nscale
 if exist(tscale) then self.tscale=tscale
 if exist(gyro) then self.gyro=gyro
 self->IDLgrVolume::SetProperty,_extra=extra
end

pro gxVolume::GetProperty,wParent=wParent,select=select,tscale=tscale,nscale=nscale,bscale=bscale,gyro=gyro,_ref_extra=extra
 wParent=self.wParent
 select=self.select
 tscale=self.tscale
 nscale=self.nscale
 bscale=self.bscale
 gyro=self.gyro
 self->IDLgrVolume::GetProperty,_extra=extra
end

function gxVolume::Undefined,data
 return,((size(data))[0] ne 3)
end

pro gxVolume::SetColor,index,auto=auto
   default,index,0
   if keyword_set(auto) then begin
    case self.select of
     'Bx'  : index=1
     'By'  : index=1
     'Bz'  : index=1
     'B'  :  index=1
     'curlB':index=1
     'divB+':index=1
     'divB-':index=3
     'helB+':index=1
     'helB-':index=3
     'theta':index=0 
     'N':index=0          
     'n_0':index=3
     'T_0':index=3  
     'n_b':index=39
     'dMu':index=0  
     'theta_C':index=0
     'theta_b':index=0        
     'a_4':index=0  
     'Q':index=3 
     'Q0':index=3    
    else:
    endcase
   end
   rgb_curr=bytarr(3,256)
   tvlct,rgb_curr,/get
   loadct,index
   rgb=bytarr(3,256)
   tvlct,rgb,/get
   self->SetProperty,rgb_table0=rgb
   tvlct,rgb_curr
end     


pro gxVolume::PlotModelAttributes 
     if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'
     self.parent->GetProperty,wParent=wParent
     if ~widget_valid(wParent) then return
     wAttributePlot=widget_info(wparent,find_by_uname='GXMODEL:AttributePlot')
     if ~widget_valid(wAttributePlot) then return
     widget_control,wAttributePlot,get_value=wDraw

     self->GetVertexAttributeData,'idx',idx
     
     self->GetVertexAttributeData,'length',l
     
     self->GetVertexAttributeData,'bmed',b
     if n_elements(b) gt 0 then b=b*self.bscale
     
     self->GetVertexAttributeData,'T0',t
     if n_elements(t) eq 0 then begin
      self->GetVertexAttributeData,'T',t   
      if n_elements(t) gt 0 then T=T*self.Tscale
     endif
     
     self->GetVertexAttributeData,'n0',n
     if n_elements(n) eq 0 then begin
      self->GetVertexAttributeData,'n',n
      if n_elements(n) gt 0 then n=n*self.Nscale
     endif
     
     wset,wDraw
     loadct,39,/silent
     erase,255
     !p.multi=0
     wAttributePlotOptions=widget_info(wparent,find_by_uname='GXMODEL:AttributePlotOptions')
     widget_control,wAttributePlotOptions,get_value=objPlotOptions
     objPlotOptions->GetProperty,range=range,xrange=pxrange,yrange=pyrange,xlog=xlog,ylog=ylog
     wXAttribute=widget_info(wparent,find_by_uname='GXMODEL:xAttribute')
     wYAttribute=widget_info(wparent,find_by_uname='GXMODEL:yAttribute')
     widget_control,wXAttribute, get_value=xAttribute
     xselect=widget_info(wXAttribute,/DROPLIST_SELECT)
     xAttribute=xAttribute[xselect]
     widget_control,wYAttribute, get_value=yAttribute
     yselect=widget_info(wYAttribute,/DROPLIST_SELECT)
     yAttribute=yAttribute[yselect]

     case xselect Of
      0: self->GetVertexAttributeData,'Bx',xx
      1: self->GetVertexAttributeData,'By',xx
      2: self->GetVertexAttributeData,'Bz',xx
      3:begin
         self->GetVertexAttributeData,'Bx',Bx
         self->GetVertexAttributeData,'By',By
         self->GetVertexAttributeData,'Bz',Bz
         xx=sqrt(temporary(Bx)^2+temporary(By)^2+temporary(Bz)^2)
        end
      4: xx=n
      5: xx=T
      6: begin
          xx=gx_rsun()*l
         end
      7: begin
          self->GetVertexAttributeData,'Q'
          xx=Q
          xAttribute='Closed loop heating rate (Q)'
         end 
      8: begin
          self->GetVertexAttributeData,'alpha',alpha
          xx=alpha
         end  
      9: begin
           self->GetVertexAttributeData,'Q0',Q0
           xx=q0
         end   
      10: begin
           self->GetVertexAttributeData,'curlb',curlb
           xx=curlb
         end    
     else:
     endcase
     case yselect Of
      0: self->GetVertexAttributeData,'Bx',yy
      1: self->GetVertexAttributeData,'By',yy
      2: self->GetVertexAttributeData,'Bz',yy
      3:begin
         self->GetVertexAttributeData,'Bx',Bx
         self->GetVertexAttributeData,'By',By
         self->GetVertexAttributeData,'Bz',Bz
         yy=sqrt(temporary(Bx)^2+temporary(By)^2+temporary(Bz)^2)
        end
      4: yy=n
      5: yy=T
      6: begin
          yy=gx_rsun()*l
         end 
      7: begin
          self->GetVertexAttributeData,'Q',Q
          yy=Q  
          yAttribute='Closed loop heating rate (Q)'
         end  
         8: begin
           self->GetVertexAttributeData,'alpha',alpha
           yy=alpha
         end 
         9: begin
           self->GetVertexAttributeData,'Q0',Q0
           yy=q0
         end
         10: begin
           self->GetVertexAttributeData,'curlb',curlb
           yy=curlb
         end     
     else:
     endcase
     if yselect le 3 then yy=self.bscale*yy
     if xselect le 3 then xx=self.bscale*xx
     xx=abs(xx)
     yy=abs(yy)
     if n_elements(xx) gt 0 and n_elements(yy) gt 0 then begin
     wRotateXY=widget_info(wparent,find_by_uname='GXMODEL:RotateXY')
     if widget_info(wRotateXY,/button_set) then begin
      temp=temporary(yy)
      yy=temporary(xx)
      xx=temporary(temp)
      temp=yAttribute
      yAttribute=xAttribute
      xAttribute=temp
     end
     wXHistogram=widget_info(wparent,find_by_uname='GXMODEL:XHistogram')
     if widget_info(wXHistogram,/button_set) then begin 
      if keyword_set(xlog) then begin
        x=alog10(xx)
      endif else x=temporary(xx)
      range=minmax(x)
      yy=histogram(x,loc=xx,nbins=100,min=range[0],max=range[1])
      if keyword_set(xlog) then xx=10^xx
      if n_elements(pxrange) eq 0 then pxrange=minmax(xx)
      if n_elements(pyrange) eq 0 then pyrange=minmax(yy)+1 
      hist_plot,xx,yy,xtitle=xAttribute,ytitle='Counts',ylog=ylog,xlog=xlog,color=0,back=255,xrange=pxrange,yrange=pyrange
     endif else begin
       if n_elements(xx) gt n_elements(yy) then xx=xx[idx]
       if n_elements(xx) lt n_elements(yy) then yy=yy[idx]
       if n_elements(xx) eq n_elements(yy) then begin
         good=where(xx ne 0 and yy ne 0)
         xx=xx[good]
         yy=yy[good]
        if n_elements(xx) gt 5000 then begin
         xrange=minmax(xx)
         yrange=minmax(yy)
         idx=(n_elements(xx)-1)*randomu(seed,5000)
         xx=[xx[idx],xrange]
         yy=[yy[idx],yrange]
        endif 
        s=sort(xx)
        plot,xx[s],yy[s],psym=3,color=0,back=255,xtitle=xAttribute,ytitle=yAttribute,$
        xrange=pxrange,yrange=pyrange,xlog=xlog,ylog=ylog ,xmargin=[15,2]
        if self.flags.NTDEM then begin
          if (xselect eq 6 or xselect eq 7) and (yselect eq 6 or yselect eq 7) then begin
            filename=keyword_set(self.flags.NTSSDEM)?'ebtel_ss.sav':'ebtel.sav'
            ebtel_file=gx_findfile(filename,folder='userslib'+path_sep()+'aia')
            if ebtel_file ne '' then begin
              restore,ebtel_file
              if xselect eq 6 and widget_info(wRotateXY,/button_set) eq 0 then oplot,2*lrun,qrun,psym=3,color=250 else $
              oplot,qrun,2*lrun,psym=3,color=250
            end
          end
        endif
       endif else begin
        plot,findgen(10),/nodata,back=255
        xyouts,1,5,'Incompatible X-Y pair selection',color=0
       end
      end
     end
  end
  
pro gxVolume::DisplayModelStatistics,data 
     self.parent->GetProperty,wParent=wParent 
     self.parent->GetProperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz
     dv=dx[1]*dy[1]*dz[1]*((gx_rsun())^3)
     good=where(data,count,ncomp=ncomp)
     if count gt 0 then data=data[good]
     m=moment(data,sdev=sdev,/nan)
     n=n_elements(data)
     mm=minmax(data)
     print,'Mean of '+self.select+'=',m[0]
     print,'Sdev of '+self.select+'=',sdev
     print,'Voxel volume (dv])=',dv,'cm^3'
     print,'Model volume=',n*dv,'cm^3'
     print,'Integral['+self.select+'dv]=',m[0]*n*dv
     print,'Integral['+self.select+'^2dv]=',m[1]*n*dv
     if ~widget_valid(wParent) then return
     wDisplay=widget_info(wParent,find_by_uname='GXMODEL:SelectedParm')
     if ~widget_valid(wDisplay) then return
     widget_control,wDisplay,set_value='Selected Parameter: '+self.select
     widget_control,widget_info(wParent,find_by_uname='GXMODEL:min'),set_value=mm[0]
     widget_control,widget_info(wParent,find_by_uname='GXMODEL:max'),set_value=mm[1]
     widget_control,widget_info(wParent,find_by_uname='GXMODEL:mean'),set_value=m[0]
     widget_control,widget_info(wParent,find_by_uname='GXMODEL:sdev'),set_value=sdev
end

function gxVolume::SetQ,q_formula
  q_default='q0*(B/q[1])^2/(L/q[2])'
  self->GetVertexAttributeData,'q_formula',undo
  if n_elements(undo) eq 0 then undo=q_default
  undo=string(undo)
  if n_elements(q_formula) eq 0 then q_formula=q_default else q_formula=string(q_formula)
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    answ= dialog_message(/INFO, ['Q formula error: '+!ERROR_STATE.MSG,'Rolling it back to: '+undo])
    q_formula=undo
    result=execute('newQ='+q_formula)
    goto,skip
  end
  self->GetVertexAttributeData,'q0_coeff',q
  self->GetVertexAttributeData,'Q0',Q0
  self->GetVertexAttributeData,'Q',oldQ
  self->GetVertexAttributeData,'bmed',B
  self->GetVertexAttributeData,'alpha',alpha 
  self->GetVertexAttributeData,'curlb',curlb 
  self->GetVertexAttributeData,'length',L
  self->GetVertexAttributeData,'dz',dz
  if n_elements(dz) gt 0 then dz=max(dz) else dz=self.zcoord_conv[1]
  short_idx=where(L lt dz,short_count)
  if short_count gt 0 then L[short_idx]=0
  l=gx_rsun()*l/2
  result=execute('newQ='+q_formula)
  if result eq  0 then begin
    answ= dialog_message(/error, 'Q formula error: '+!ERROR_STATE.MSG+' . Requested change undone!' )
    q_formula=undo
    if (n_elements(oldQ) ne 0) then newqQ=oldQ else  result=execute('newQ='+q_formula)
  endif
  if n_elements(newQ) ne n_elements(L) then begin
    answ= dialog_message(/error, 'Q formula error: Expression returns unexpected array size. Requested change undone!')
    q_formula=undo
    result=execute('newQ='+q_formula)
  endif
  skip:
  self->SetVertexAttributeData,'q_formula',byte(q_formula)
  self->SetVertexAttributeData,'Q',newQ
  flags=self->setflags(newNT=total(abs(minmax(oldq-newq))) ne 0)
  if flags.newNT then self->PlotModelAttributes
  return,q_formula
end

function gxVolume::SetQ0,q0_formula,q_formula=q_formula
  q0_default='q[0]';'q[0]*(q[1]+tanh((abs(alpha)-q[2])*q[4])-tanh((abs(alpha)-q[3])*q[4]))'
  self->GetVertexAttributeData,'q0_formula',undo
  if n_elements(undo) eq 0 then undo=q0_default
  undo=string(undo)
  if n_elements(q0_formula) eq 0 then q0_formula=q0_default else q0_formula=string(q0_formula)
  
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    answ= dialog_message(/INFO, ['Q0 formula error: '+!ERROR_STATE.MSG,'Rolling it back to: '+undo])
    q0_formula=undo
    result=execute('q0='+q0_formula)
    goto,skip
  end
  self->GetVertexAttributeData,'q0_coeff',q
  self->GetVertexAttributeData,'bmed',B
  self->GetVertexAttributeData,'alpha',alpha 
  self->GetVertexAttributeData,'curlb',curlb 
  self->GetVertexAttributeData,'length',L
  l=gx_rsun()*l/2
  result=execute('q0='+q0_formula)
  if result eq  0 then begin
    answ= dialog_message(/INFO, ['Q0 formula error: '+!ERROR_STATE.MSG,'Rolling it back to: ',undo])
    q0_formula=undo
    result=execute('q0='+q0_formula)
  endif
  skip:
  self->SetVertexAttributeData,'q0_formula',byte(q0_formula)
  self->SetVertexAttributeData,'Q0',Q0
  self->GetVertexAttributeData,'q_formula',q_formula
  q_formula=self->SetQ(q_formula)
  return,q0_formula
end

function gxVolume::Selected
 return.self.select
end




      
pro gxVolume::UpdateVoxelId,force=force
 ;Bitwise Voxel ID Convetions 07-Dec-2017 GN
 ;Bit 0 (ID and 1UL eq 1UL)= Do not interpolate! (includes all Chromo voxels)
 ;Bit 1 (ID and 2UL eq 2UL)= TR voxel
 ;Bit 2 (ID and 4UL eq 4UL)= Coronal voxel 
 ;Bit 3 (ID and 8UL eq 8 UL)= EUV active TR voxel (not masked by the TR mask) 
 
 start_time=systime(/s)
  if ~keyword_set(force) then if ~self.flags.NewID then return
  prog_id = gx_progmeter(/INIT,label='VoxelID Update Progress')
  self->GetVertexAttributeData,'voxel_id',id
  old_id=id
  id[*]=gx_voxelid(/corona)
  sz=size(id)
  corona=self.parent->Corona()  
  corona->GetProperty,chromo_h=chromo_h
    self->GetVertexAttributeData,'tr',tr
    if n_elements(tr) ne sz[1]*sz[2] then begin
      self->GetVertexAttributeData,'chromo_idx',chromo_idx
      if n_elements(chromo_idx) ne 0 then begin
        tr=lonarr(sz[1],sz[2])
        self->GetVertexAttributeData,'chromo_T',chromo_t
        vol_t=fltarr(sz[1],sz[2],sz[3])
        vol_t[chromo_idx]=temporary(chromo_t)
        vol_n=fltarr(sz[1],sz[2],sz[3])
        self->GetVertexAttributeData,'chromo_n',chromo_n
        vol_n[chromo_idx]=temporary(chromo_n)
        for i=0,sz[1]-1 do for j=0, sz[2]-1 do begin
          tr[i,j]=max(where(vol_n[i,j,*] ne 0 and vol_t[i,j,*] ne 0 ))+1
        endfor
       self->SetVertexAttributeData,'tr',tr
      endif
    endif
  
    if n_elements(tr) eq sz[1]*sz[2] then begin
      ;Here it is found that this is a combo model 
      id[*]=gx_voxelid() 
      id[*,*,0:max(tr)]=gx_voxelid(/chromo); Do not Interpolate! TR or Chromo
      for i=0,sz[1]-1 do begin
        for j=0, sz[2]-1 do begin
          id[i,j,tr[i,j]]+=gx_voxelid(/tr,/euv) ;Transition Region(TR voxels are by default EUV active TR voxels)
          id[i,j,tr[i,j]:*]+=gx_voxelid(/corona) ;Corona (TR is also part of corona)
        endfor
      endfor
      ;Here I am assigning EUV active TR IDs to those Coronal voxels that are direct neighbors of Chromo voxels
     coridx=where(id eq gx_voxelid(/chromo,/corona), count)
     if count gt 0 then edge=where(id[coridx+1] eq 1UL or id[coridx-1] eq 1,count)
     if count gt 0 then id[coridx[edge]]+=gx_voxelid(/tr,/euv)
    end  



  if n_elements(tr) eq 0 then begin
    ;This is done only if TR not been defined above
    id[*]=gx_voxelid(/corona)
    idx_tr=0
    chromo_tr=0
    self->GetVertexAttributeData,'idx',idx
    if self->hasBL(idx=idx) gt 0 then begin
      idx=array_indices(id,idx)
      idx_tr=min(idx[2,*])
    endif
    if keyword_set(chromo_h) then begin
     r=self.parent->R()
     r=reform(r,sz[1],sz[2],sz[3])
     chromo_owned=where(r lt (1+chromo_h),chromo_count)
     if chromo_count gt 0 then begin
       chromo_owned=array_indices(id,chromo_owned)
       chromo_tr=max(chromo_owned[2,*])+1
     endif
    endif
    tr=max([idx_tr,chromo_tr])
    if tr gt 0 then id[*,*,0:tr]=gx_voxelid(/chromo)
    id[*,*,tr]=gx_voxelid(/chromo,/corona,/tr,/euv)
    id[*,*,tr+1:*]=gx_voxelid(/corona)
  endif
  
  if self.flags.TRmask then begin
  refmaps=self.parent->RefMaps()
    if ptr_valid(refmaps) then begin
      refmaps=(*refmaps)[0]
      count=refmaps->Get(/count)
      match=-1
      for i=0, count-1 do if (refmaps->Get(i,/id) eq 'BASE TR_Mask') or (refmaps->Get(i,/id) eq 'CEA-TR Mask') then match=i
      if match ge 0 then begin
        tr_mask=not refmaps->Get(match,/data)
        tr_idx=where((id and gx_voxelid(/tr,/euv)) ne 0, tr_count)
        if tr_count gt 0 then begin
           idx=array_indices(id,tr_idx)
           id[tr_idx]=id[tr_idx] -tr_mask[idx[0,*],idx[1,*]]*gx_voxelid(/euv)
        end
      endif
    end
  end
  
  
  if widget_valid(prog_id) then status=gx_progmeter(prog_id,0.33) 
  
  self->ComputeN0T0,tube_id=tube_id
  
  voxel_id=ulong(id) or ishft(ulong(tube_id),8)
  
  height_id=ishft(ulong(1+lindgen(sz[3])),16)
  for i=0,sz[1]-1 do for j=0,sz[2]-1 do voxel_id[i,j,*]=voxel_id[i,j,*] or height_id
  

refmaps=self.parent->RefMaps()
if ptr_valid(refmaps) then begin
  refmaps=(*refmaps)[0]
  ref_count=refmaps->Get(/count)
  for i=0,ref_count-1 do begin
    if strupcase(refmaps->Get(i,/id)) eq strupcase('BASE CHROMO_MASK') or $
      strupcase(refmaps->Get(i,/id)) eq strupcase('CEA-CHROMO MASK') then begin
      mask=refmaps->Get(i,/data)
      goto,mask_exit
    endif
  endfor
end
mask_exit:
if n_elements(mask) eq sz[1]*sz[2] then begin
  mask=ishft(ulong(mask),24)
  for i=0, sz[1]-1 do for j=0, sz[2]-1 do voxel_id[i,j,*]=voxel_id[i,j,*] or mask[i,j]
endif
  
  if widget_valid(prog_id) then status=gx_progmeter(prog_id,1) 
  self->SetVertexAttributeData,'voxel_id',voxel_id
  dummy=where((voxel_id and not(2UL)) ne (id and not(2UL)), count)
  flags=self->setflags(NewID=0,/newdata,newgrid=(count eq 0)?0:1)
  
  tubes=self.parent->Get(/all,isa='gxfluxtube',count=count) 
  for i=0,count-1 do tubes[i]->Display_EM
  
  if widget_valid(prog_id) then status = gx_progmeter(prog_id,/DESTROY)
end  

function gxVolume::VoxelId
 self->GetVertexAttributeData,'voxel_id',voxel_id
 return,voxel_id
end

pro gxVolume::UpdateBoundaries
  ;Update Boundaries
  ;  id=voxel_id
  ;  dim=size(voxel_id,/dim)
  ;  Nx=dim[0]
  ;  Ny=dim[1]
  ;  Nz=dim[2]
  ;  for i=0, Nx-1 do begin
  ;    status=gx_progmeter(prog_id,i/float(Nx-1))
  ;    for j=0, Ny-1 do begin
  ;      for r=0, Nz-1 do begin
  ;        ue=where(id[(i-1)>0 : (i+1)<(Nx-1), $
  ;          (j-1)>0 : (j+1)<(Ny-1), $
  ;          (r-1)>0 : (r+1)<(Nz-1)] ne voxel_id[i, j, r] or (voxel_id[i, j, r] and 1UL ne 0), ke)
  ;        if ke ne 0 then begin
  ;          voxel_id[i,j,r]=voxel_id[i,j,r] or ishft(1ul, 30)
  ;        endif
  ;      endfor
  ;    endfor
  ;  endfor 
end  

pro gxVolume::Update,select,data=data,plot_model_attributes=plot_model_attributes,getdata=getdata,$
              force=force,update=update,chromo_view=chromo_view,data_range=data_range,pwr_idx=pwr_idx
  compile_opt hidden
  catch, error_stat
 if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return
 end
  if ~obj_valid(self.parent) then return
  if self.flags.newNT then begin
    if dialog_message(['Volume n-T configuration needs to be updated!','Do you want to recompute n-T now?',$
      'If you need to do more volume changes before this potentially time consuming action, you can later do it manualy by pressing the "Store/Compute n-T from EBTEL" button located in the Model tab'],$ 
      /question) eq 'Yes' then self->ComputeNT
  endif
  self->UpdateVoxelId,force=force;It will do it only if self.flags.NewID is set or explicitely requested by /force keyword is set
  if ~(self.flags.newData or  keyword_set(update)) then return; nothing to update or no explicit request
  self->GetVertexAttributeData,'voxel_id',voxel_id
  corona=self.parent->Corona()
  if ~keyword_set(chromo_view) then begin
    if obj_isa(corona,'gxCorona') then begin
      corona->GetProperty,chromo_view=chromo_view
    endif else chromo_view=0
  end
  if n_elements(select) ne 0 then self.select=strcompress(select,/rem)
  self->SetColor,/auto
  case self.select of
  'Bx'  :begin
         self->GetVertexAttributeData,'Bx',data
         if self->undefined(data) then goto,undefined
        end
  'By'  :begin
         self->GetVertexAttributeData,'By',data
         if self->undefined(data) then goto,undefined
        end
  'Bz'  :begin
         self->GetVertexAttributeData,'Bz',data
         if self->undefined(data) then goto,undefined
        end
  'B'  :begin
         self->GetVertexAttributeData,'Bx',Bx
         if self->undefined(Bx)then goto,undefined
         self->GetVertexAttributeData,'By',By
         if self->undefined(By) then goto,undefined
         self->GetVertexAttributeData,'Bz',Bz
         if self->undefined(Bz) then goto,undefined
         data=sqrt(Bx^2+by^2+Bz^2)
         ;data=bytscl(data)
        end
   'curlB':begin
          if n_elements(select) eq 0 then goto, no_recompute
          widget_control,/hourglass
          self->GetVertexAttributeData,'curlb',curlb
          if n_elements(curlb) eq 0 then begin
            ;compute curl
            self->GetVertexAttributeData,'Bx',Bx
            if self->undefined(Bx)then goto,undefined
            self->GetVertexAttributeData,'By',By
            if self->undefined(By) then goto,undefined
            self->GetVertexAttributeData,'Bz',Bz
            if self->undefined(Bz) then goto,undefined
            self.parent->GetProperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz
            m=max([dx,dy,dz])
            dx=dx[1]/m
            dy=dy[1]/m
            dz=dz[1]/m
            curl,bx,by,bz,cx,cy,cz,order=3, dx=dx, dy=dy, dz=dz
            data=sqrt(cx^2+cy^2+cz^2)
            ;curl computed
          endif else begin
            self->GetVertexAttributeData,'idx',idx
            self->GetProperty,data0=data
            data=float(data*0)
            data[idx]=curlb
          endelse
        end   
   'divB+':begin
          if n_elements(select) eq 0 then goto, no_recompute
          widget_control,/hourglass
          ;compute divB
          self->GetVertexAttributeData,'Bx',Bx
          if self->undefined(Bx)then goto,undefined
          self->GetVertexAttributeData,'By',By
          if self->undefined(By) then goto,undefined
          self->GetVertexAttributeData,'Bz',Bz
          if self->undefined(Bz) then goto,undefined
          self.parent->GetProperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz
          m=max([dx,dy,dz])
          dx=dx[1]/m
          dy=dy[1]/m
          dz=dz[1]/m
          div,bx,by,bz,D,order=3, dx=dx, dy=dy, dz=dz
          data=D>0
        end
    'divB-':begin
          if n_elements(select) eq 0 then goto, no_recompute
          widget_control,/hourglass
          ;compute divB
          self->GetVertexAttributeData,'Bx',Bx
          if self->undefined(Bx)then goto,undefined
          self->GetVertexAttributeData,'By',By
          if self->undefined(By) then goto,undefined
          self->GetVertexAttributeData,'Bz',Bz
          if self->undefined(Bz) then goto,undefined
          self.parent->GetProperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz
          m=max([dx,dy,dz])
          dx=dx[1]/m
          dy=dy[1]/m
          dz=dz[1]/m
          div,bx,by,bz,D,order=3, dx=dx, dy=dy, dz=dz
          data=D<0
        end    
    'helB-':begin
          if n_elements(select) eq 0 then goto, no_recompute
          widget_control,/hourglass
          self->GetVertexAttributeData,'alpha',alpha
          if n_elements(alpha) eq 0 then begin
            ;compute curl
            self->GetVertexAttributeData,'Bx',Bx
            if self->undefined(Bx)then goto,undefined
            self->GetVertexAttributeData,'By',By
            if self->undefined(By) then goto,undefined
            self->GetVertexAttributeData,'Bz',Bz
            if self->undefined(Bz) then goto,undefined
            self.parent->GetProperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz
            m=max([dx,dy,dz])
            dx=dx[1]/m
            dy=dy[1]/m
            dz=dz[1]/m
            curl,bx,by,bz,cx,cy,cz,order=3, dx=dx, dy=dy, dz=dz
            data=bx*cx+by*cy+bz*cz
            data=data/(bx*bx+by*by+bz*bz)
          endif else begin
           self->GetVertexAttributeData,'idx',idx
           self->GetProperty,data0=data
           data=data*0
           data[idx]=alpha
          endelse
          data[where(data gt 0)]=1./0
        end   
       
      'helB+':begin
          if n_elements(select) eq 0 then goto, no_recompute
          widget_control,/hourglass
          self->GetVertexAttributeData,'alpha',alpha
          if n_elements(alpha) eq 0 then begin
            ;compute curl
            self->GetVertexAttributeData,'Bx',Bx
            if self->undefined(Bx)then goto,undefined
            self->GetVertexAttributeData,'By',By
            if self->undefined(By) then goto,undefined
            self->GetVertexAttributeData,'Bz',Bz
            if self->undefined(Bz) then goto,undefined
            self.parent->GetProperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz
            m=max([dx,dy,dz])
            dx=dx[1]/m
            dy=dy[1]/m
            dz=dz[1]/m
            curl,bx,by,bz,cx,cy,cz,order=3, dx=dx, dy=dy, dz=dz
            data=bx*cx+by*cy+bz*cz
            data=data/(bx*bx+by*by+bz*bz)
          endif else begin
           self->GetVertexAttributeData,'idx',idx
           self->GetProperty,data0=data
           data=data*0
           data[idx]=alpha
          endelse
          data[where(data lt 0)]=1./0
        end                              
   'n_0':begin 
         self->GetVertexAttributeData,'n0',data
         if chromo_view eq 0 then begin
          chromo_idx=where((voxel_id and 1) ne 0,chromo_count)
          if chromo_count gt 0 then data[chromo_idx]=0
         endif
        end
   'T_0':begin 
         self->GetVertexAttributeData,'T0',data
         if chromo_view eq 0 then begin
           chromo_idx=where((voxel_id and 1) ne 0,chromo_count)
           if chromo_count gt 0 then data[chromo_idx]=0
         endif
        end    
   'n_b':begin 
             sz=self.parent->Size()
             data=fltarr(sz[1],sz[2],sz[3])
             ndata=data
            ;LOOP OVER FLUXTUBES
            tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
            for j=0,tcount-1 do begin
              ndata[*]=0
              tubes[j]->GetProperty,centerbase=base
              base->GetVertexAttributeData,'n_nth',n_nth
              base->GetVertexAttributeData,'owned',owned
              base->GetVertexAttributeData,'N_IDX',n_idx
              if n_elements(owned) gt 1 then begin
               ndata[n_idx]=n_nth
               data[owned]=ndata[owned]
              end 
             end      
         if self->undefined(data) then goto,undefined
        end
    'dMu':begin 
          sz=self.parent->Size()
          data=fltarr(sz[1],sz[2],sz[3])
          vol=data
          tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
          ;LOOP OVER FLUXTUBES
            for j=0,tcount-1 do begin
              tubes[j]->GetProperty,T0=T0,centerbase=base
              base->GetVertexAttributeData,'owned',owned
              if n_elements(owned) gt 1 then begin
               base->GetVertexAttributeData,'N_IDX',n_idx
               base->GetVertexAttributeData,'dMu',value
               vol[n_idx]=value
               data[owned]=vol[owned]
              end    
            end
           end  
      'theta_C':begin 
          sz=self.parent->Size()
          data=fltarr(sz[1],sz[2],sz[3])
          vol=data
          tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
          ;LOOP OVER FLUXTUBES
            for j=0,tcount-1 do begin
              tubes[j]->GetProperty,T0=T0,centerbase=base
              base->GetVertexAttributeData,'owned',owned
              if n_elements(owned) gt 1 then begin
               base->GetVertexAttributeData,'N_IDX',n_idx
               base->GetVertexAttributeData,'THETA_C',value
               vol[n_idx]=value
               data[owned]=vol[owned]
              end    
            end
           end  
     'theta_b':begin 
          sz=self.parent->Size()
          data=fltarr(sz[1],sz[2],sz[3])
          vol=data
          tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
          ;LOOP OVER FLUXTUBES
            for j=0,tcount-1 do begin
              tubes[j]->GetProperty,T0=T0,centerbase=base
              base->GetVertexAttributeData,'owned',owned
              if n_elements(owned) gt 1 then begin
               base->GetVertexAttributeData,'N_IDX',n_idx
               base->GetVertexAttributeData,'THETA_B',value
               vol[n_idx]=value
               data[owned]=vol[owned]
              end    
            end
           end  
     'a_4':begin 
          sz=self.parent->Size()
          data=fltarr(sz[1],sz[2],sz[3])
          vol=data
          tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
          ;LOOP OVER FLUXTUBES
            for j=0,tcount-1 do begin
              tubes[j]->GetProperty,T0=T0,centerbase=base
              base->GetVertexAttributeData,'owned',owned
              if n_elements(owned) gt 1 then begin
               base->GetVertexAttributeData,'N_IDX',n_idx
               base->GetVertexAttributeData,'a4',value
               vol[n_idx]=value
               data[owned]=vol[owned]
              end    
            end
           end  
      'Q0': begin
             self->GetVertexAttributeData,'Q0',q0
             if n_elements(q0) eq 0 or n_elements(idx) eq 0 then goto, undefined
             self->GetProperty,data0=data
             data=data*0.0
             data[idx]=q0         
            end 
       'Q': begin
              self->GetVertexAttributeData,'Q',q
              if n_elements(q) eq 0 or n_elements(idx) eq 0 then goto, undefined
              self->GetProperty,data0=data
              data=data*0.0
              data[idx]=q
            end     
        'Length': begin
            if n_elements(length) eq 0 or n_elements(idx) eq 0 then goto, undefined
              self->GetProperty,data0=data
              data=data*0.0
              data[idx]=length
            end          
   else: begin 
          self->SetColor,0
          sz=self.parent->Size()
          result=execute('corona->GetProperty,'+self.select+'=value',1,1)
          data=fltarr(sz[1],sz[2],sz[3])
          vol=data
          if n_elements(value) eq 1 then data[*]=value
          tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
          ;LOOP OVER FLUXTUBES
            for j=0,tcount-1 do begin
             vol[*]=0
              tubes[j]->GetProperty,T0=T0,centerbase=base
              base->GetVertexAttributeData,'owned',owned
              if n_elements(owned) gt 1 then begin
               base->GetVertexAttributeData,'N_IDX',n_idx
               result=execute('tubes[j]->GetProperty,'+self.select+'=value',1,1)
               if n_elements(value) eq 0 then goto,undefined
               vol[n_idx]=value
               data[owned]=vol[owned]
              end    
            end 
            goto,defined      
         undefined:
         self->GetProperty,data0=data
         data=data*0
        end
   endcase
   defined:
   min_val=min(data[where(data gt 0)],max=max_val)
   default,data_range,[min_val,max_val]
   default,pwr_idx,1
   if keyword_set(getdata) then return
   
   message,strcompress(self.select+' range: ['+arr2str(minmax(data)))+']',/cont
   self->SetProperty,data0=bytscl(data^pwr_idx,min=data_range[0]^pwr_idx,max=data_range[1]^pwr_idx),/interpolate,volume_select=0
   ;self->SetProperty,data0=bytscl((data>data_range[0]<data_range[1])^pwr_idx),/interpolate,volume_select=0
   ;self->SetProperty,data0=bytscl(data^0.2),/interpolate,volume_select=0
   self->DisplayModelStatistics,data 
   no_recompute:
   self->PlotModelAttributes
   self.Parent->GetProperty,wParent=wParent,parent=container
   if obj_isa(container,'gxSun') then begin
     top=get_tlb(wParent)
     if widget_valid(top) then begin
      statebase=widget_info(top,Find_By_Uname='STATEBASE')
      if widget_valid(statebase) then begin
        widget_control,statebase,get_uvalue=state
        state.scanbox->Slice
      end
     endif
   end
  flags=self->setflags(newData=0)
end


function gxVolume::hasBL,length=lenght,bmed=bmed,idx=idx
  self->GetVertexAttributeData,'length',length
  self->GetVertexAttributeData,'bmed',bmed
  self->GetVertexAttributeData,'idx',idx
  return,n_elements(idx) ne 0 and ((n_elements(length) eq n_elements(bmed)) and (n_elements(length)eq n_elements(idx)))
end

function gxVolume::hasNT,n=n,t=t,idx=idx
  self->GetVertexAttributeData,'n',n
  self->GetVertexAttributeData,'T',t
  self->GetVertexAttributeData,'idx',idx
  return,n_elements(idx) ne 0 and ((n_elements(n) eq n_elements(t)) and (n_elements(n)eq n_elements(idx)))
end

pro gxVolume::RequestVolumeUpdate,condition=condition,_extra=extra
 default,extra,{newDATA:1}
 if size(condition,/tname) eq 'STRING' then begin
  if strupcase(self.select) eq strupcase(condition) then flags=self->setflags(_extra=extra)
 endif else flags=self->setflags(_extra=extra)
end

pro gxVolume::ClearVolumeUpdateRequest
   self->SetVertexAttributeData,'update',0L
end

;function gxVolume::NeedsUpdate
; self->GetVertexAttributeData,'update',update
; default,update,0L
; self->SetVertexAttributeData,'update',update
; return,update
;end 

function gxVolume::getflags
 return,self.Flags
end

pro gxVolume::Displayflags
  if obj_isa(self.parent,'gxmodel') then begin
    self.parent->getproperty,wParent=wParent
    if widget_valid(wParent) then begin
      names=tag_names(self.flags)
      for i=0,n_elements(names)-1 do begin
        wbutton=widget_info(wParent,find_by_uname=names[i])
        if widget_valid(wbutton) then widget_control,wbutton,set_button=self.flags.(i)
      endfor
       wDemBase=widget_info(wParent,find_by_uname='DEMBASE')
       if widget_valid(wDemBase) then widget_control,wDemBase,map=self.flags.NTdem
       wComputeNT=widget_info(wParent,find_by_uname='GXMODEL:COMPUTENT')
       if widget_valid(wComputeNT) then widget_control,wComputeNT,sensitive=~self.flags.NTstored and self.flags.newNT
    endif
  endif
end

function gxVolume::NewNT,newkey,oldkey
  self->GetVertexAttributeData,'q_formula',q_formula
  self->GetVertexAttributeData,'q0_formula',q0_formula
  self->GetVertexAttributeData,'q0_coeff',q0_coeff
  self->GetVertexAttributeData,'NTkey',oldkey
  self.parent->GetProperty,wparent=wparent
  id=widget_info(wparent,find_by_uname='GXMODEL:DEMAVG')
  if widget_valid(id) then widget_control,id,get_value=demavg 
  default,oldkey,byte('')
  default,newkey,oldkey
  if self.flags.NTDEM then newkey=byte(strcompress('q=['+arr2str(q0_coeff)+'] & q0='+string(byte(q0_formula))+' & q='+string(byte(q_formula))+$
    ' & NTDEM='+string(self.flags.NTDEM)+' & NTSSDEM='+string(self.flags.NTSSDEM))+(n_elements(demavg) gt 0? string(demavg,format="(' & DEMAVG=',i1)"):''))
  if self.flags.NTSS then newkey=byte(strcompress('q=['+arr2str(q0_coeff)+'] & q0='+string(byte(q0_formula))+' & q='+string(byte(q_formula))+$
    ' & NTSS='+string(self.flags.NTSS))+(n_elements(demavg) gt 0? string(demavg,format="(' & DEMAVG=',i1)"):''))
  flags=self->setflags(newNT=(string(newkey) ne string(oldkey)))
  return,flags.newNT
end

pro gxVolume::ComputeNT,question=question,quiet=quiet,force=force
  if keyword_set(force) then goto, compute
  if ~self.flags.newNT and keyword_set(question) then begin
    answ=dialog_message('The N-T pairs have been already computed using current settings. Do you want to recompute them  anyway?',/question)
    if strupcase(answ) ne 'YES' then begin
      msg='Same N-T key, computation canceled by the user'
      goto,skip
    endif else msg='Same N-T key, computation requested by the user'
  endif else if ~self.flags.newNT then begin
    msg='Same N-T key, nothing to compute'
    goto,skip
  endif
  compute:
  self->GetVertexAttributeData,'length',L
  L=gx_rsun()*L/2
  self->GetVertexAttributeData,'Q',Q
  if self.flags.NTDEM then begin
    if n_elements(L) gt 0 and (n_elements(L) eq n_elements(Q))  then begin
      widget_control,/hourglass
      self.parent->GetProperty,wparent=wparent
      id=widget_info(wparent,find_by_uname='GXMODEL:DEMAVG')
      if widget_valid(id) then widget_control,id,get_value=avgdem
      dem_interpolate,n,t,Qarr=Q,Larr=L,ss=self.flags.NTSSDEM,avgdem=avgdem,duration=duration
      id=widget_info(wparent,find_by_uname='GXMODEL:DEMDT')
      if widget_valid(id) then widget_control,id,set_value=strcompress(string(duration,format="('DEM interpolation computed in',f7.2,' s')"))
      self->SetVertexAttributeData,'n',n
      self->SetVertexAttributeData,'T',t
      if self.flags.NTSSDEM then flags=self->setflags(/storedNTSSDEM) $
      else flags=self->setflags(/storedNTDEM)
    end
  end
  if self.flags.NTSS then begin
    self->GetVertexAttributeData,'length',L
    Q1=1e4
    T = 74 * (Q/Q1)^(2./7.) * (L)^(4./7.)
    n= self.nscale*1.3e6 * T^2 / L
    T = self.Tscale*T
    self->SetVertexAttributeData,'n',n
    self->SetVertexAttributeData,'T',t
    flags=self->setflags(/storedNTSS)
  endif
  newNT=self->NewNT(newkey)
  self->SetVertexAttributeData,'NTkey',newkey
  flags=self->setflags(NewNT=0)
  if ~keyword_set(quiet) then message,string(newkey),/cont
  skip:
  if ~keyword_set(quiet) and size(msg,/tname) eq 'STRING' then message,msg,/cont
end

pro gxVolume::ComputeN0T0,tube_id=tube_id
  self->GetProperty,data0=data0,zcoord_conv=zcoord_conv
  sz=size(data0)
  z=dindgen(sz[3])*zcoord_conv[1]
  r=self.parent->R()
  r=reform(r,sz[1],sz[2],sz[3])
  cn=fltarr(sz[1],sz[2],sz[3])
  cT0=cn
  p=cn
  chromo_count=0
  corona=self.parent->Corona()
  if obj_valid(corona) then begin
    cn[*]=corona->GetDensity(r,h=h,n0=n0,T0=temp,chromo_n=chromo_n,chromo_T=chromo_T,chromo_h=chromo_h,chromo_view=chromo_view,blend=blend,ignore=ignore)
    default,ignore, 0
    if ~keyword_set(ignore) then begin
      cT0[*]=temp
      p=cT0*cn
      chromo_owned=where(r lt (1+chromo_h),chromo_count)
    endif else begin
     cn[*]=0 
    endelse
  end  
  if self->hasNT(n=n,t=t,idx=idx) then begin
   if self.flags.newNT then begin
     if self.flags.storedNTDEM and self.flags.NTSS then begin
      case dialog_message('Currently stored n-T pairs were computed from EBTEL DEM. But EBTEL analytical computation option is currently selected! Do you want to recompute n-T now?',/question) of 
        'Yes':self->ComputeNT
        else:
      endcase
     endif
     if self.flags.storedNTSS and self.flags.NTDEM then begin
       case dialog_message('Currently stored n-T pairs were computed from analytical EBTEL solution. But EBTEL DEM computation option is currently selected! Do you want to recompute n-T now?',/question) of
         'Yes':self->ComputeNT
         else:
       endcase
     endif
     if (self.flags.storedNTDEM and self.flags.NTDEM) or (self.flags.storedNTSS and self.flags.NTSS) then begin
       newNT=self->NewNT(newkey,oldkey)
       if newNT then begin  
         case dialog_message(['Currently stored n-T pairs were using different settings!','Do you want to recompute n-T now?',$
                              'Old Settings: '+string(oldkey),'New Settings: '+string(newkey)],/question) of
           'Yes':self->ComputeNT
           else:
         endcase
       end
     endif
     end
     T=T*self.Tscale
     n=n*self.nscale
     if n_elements(blend) eq 0 then blend=0
     if blend eq 1 then begin
       ;This option has been hidden to the non-expert users
       cn[idx]=cn[idx]+n
       p[idx]=p[idx]+T*n
       ct0=p/cn
     endif else begin
       cT0[idx]=T
       cn[idx]=n
       p=cT0*cn
     endelse
  endif
  
  ;FLOAT OR DOUBLE PRECISSION CHOICE FOR some of the following arrays
  ;may affect the fluxtube ownership result as shown bellow
  ;------------------------------
  ndata=fltarr(sz[1],sz[2],sz[3])
  nvol=ndata
  tvol=ndata
  p0=nvol
  tdata=ct0
  p=ct0*cn
  tube_id=ulonarr(sz[1],sz[2],sz[3])
  ;------------------------------

  tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
  
  ;LOOP OVER FLUXTUBES TO PROGRESEVELY CLAIM FLUXTUBE OWNERSHIP
  for j=0,tcount-1 do begin
    p0[*]=0
    tvol[*]=0
    nvol[*]=0
    tubes[j]->GetProperty,T0=T0,centerbase=base,hide=hide
    if keyword_set(hide) then goto,skip_tube
    base->GetVertexAttributeData,'n_th',n_th
    base->GetVertexAttributeData,'N_IDX',n_idx

    p0[n_idx]=(cT0*cn)[n_idx]+T0*n_th
    ;------------------------------
    owned=where(p0 gt p,ocount)
    ; IF P0 and P are defined above as floating point arrays, GE and GT may assign different ownership
    ;--------------------------------------

    if ocount gt 0 then begin
      p[owned]=p0[owned]
      nvol[n_idx]=n_th
      ndata[owned]=nvol[owned]
      tvol[n_idx]=p0[n_idx]/(ndata+cn)[n_idx]
      tdata[owned]=tvol[owned]
      tube_id[owned]=ulong(j+1);ulong(2)^j
    end
    skip_tube:
  end
  
  ;LOOP AGAIN OVER FLUXTUBES TO DETERMINE UNIQ FLUXTUBE OWNERSHIP
  for j=0,tcount-1 do begin
    tubes[j]->GetProperty,centerbase=base,hide=hide
    owned=where(tube_id eq ulong(j+1),ocount);ulong(2)^j,ocount)
    base->SetVertexAttributeData,'owned',owned
  endfor
  
  ;HERE WE QUESTIONABLY ADD FLUXETUBE DENSITY (IF ANY FLUXTUBE) TO LOCAL CORONA DENSITY
  ndata=ndata+cn
  ;COMMENT THE LINE ABOVE AND UNCOMMENT THE LINES BELOW TO REPLACE CORONAL DENSITY WITH FLUXTUBE DENSITIES 
  ;owned=where(tube_id ne 0,complement=corona_owned,ncomplement=count)
  ;if count gt 0 then ndata[corona_owned]=cn[corona_owned]
  ;--------------------
  
  coronal_n=ndata
  coronal_t=tdata
  
  self->GetVertexAttributeData,'chromo_idx',chromo_idx
  if n_elements(chromo_idx) ne 0 then begin
    self->GetVertexAttributeData,'chromo_n',chromo_n
    self->GetVertexAttributeData,'chromo_T',chromo_t
    ndata[chromo_idx]=chromo_n
    tdata[chromo_idx]=chromo_t
  endif else begin
    if chromo_count gt 0 then begin
      ndata[chromo_owned]=chromo_n
      tdata[chromo_owned]=chromo_t
      tr=(array_indices(r,max(chromo_owned)))[2]+1
      self->setvertexattributedata,'chromo_layers',tr
    end
  endelse
  self->SetVertexAttributeData,'n0',ndata
  self->SetVertexAttributeData,'T0',tdata
end

function gxVolume::ntubes,tubes=tubes
 tubes=self.parent->Get(/all,isa='gxfluxtube',count=count)
 return,count
end

function gxVolume::setflags,_extra=flags
  
  if n_elements(flags) eq 0 then begin
    message,'No flag provided',/cont
    return,self.flags
  endif
  

  if tag_exist(flags,'NTstored') then begin
    if keyword_set(flags.NTstored )then begin
      self.flags.NTstored=1
      self.flags.NTdem=0
      self.flags.NTss=0
    end
  endif
  
  if tag_exist(flags,'NTdem') then begin
    if keyword_set(flags.NTdem) then begin
      self.flags.NTstored=0
      self.flags.NTdem=1
      self.flags.NTss=0
    end
  endif
  
  if tag_exist(flags,'NTss') then begin
    if keyword_set(flags.NTss) then begin
      self.flags.NTstored=0
      self.flags.NTdem=0
      self.flags.NTss=1
    end
  endif
  
  if tag_exist(flags,'NTssdem') then begin
      self.flags.NTssdem=keyword_set(flags.NTssdem)
    end
    
  if tag_exist(flags,'storedNTdem') then begin
    self.flags.storedNTdem=keyword_set(flags.storedNTdem)
    if self.flags.storedNTdem then begin
     self.flags.storedNTss=0
     self.flags.storedNTssdem=0
     self.flags.newNT=0
     self.flags.newData=1
     self.flags.hasNT=1
     self.flags.newID=1
    end 
  endif
  
  if tag_exist(flags,'storedNTss') then begin
    self.flags.storedNTss=keyword_set(flags.storedNTss)
    if self.flags.storedNTss then begin
      self.flags.storedNTdem=0
      self.flags.storedNTssdem=0
      self.flags.newNT=0
      self.flags.newData=1
      self.flags.hasNT=1
      self.flags.newID=1
    endif  
  endif
  
  if tag_exist(flags,'storedNTssdem') then begin
    self.flags.storedNTssdem=keyword_set(flags.storedNTssdem)
    if self.flags.storedNTssdem then begin
      self.flags.storedNTdem=0
      self.flags.storedNTss=0
      self.flags.newNT=0
      self.flags.newData=1
      self.flags.hasNT=1
      self.flags.newID=1
    endif  
  endif

  if tag_exist(flags,'TRadd') then self.flags.TRadd=keyword_set(flags.TRadd)
  if tag_exist(flags,'TRMask') then self.flags.TRMask=keyword_set(flags.TRMask)
  if tag_exist(flags,'TRfactor') then self.flags.TRfactor=keyword_set(flags.TRfactor)
  if tag_exist(flags,'newID') then self.flags.newID=keyword_set(flags.newID)
  if tag_exist(flags,'newGrid') then self.flags.newGrid=keyword_set(flags.newGrid)
  if tag_exist(flags,'newData') then self.flags.newData=keyword_set(flags.newData)
  if tag_exist(flags,'hasBL') then self.flags.hasBL=keyword_set(flags.hasBL)
  if tag_exist(flags,'hasNT') then self.flags.hasNT=keyword_set(flags.hasNT)
  if tag_exist(flags,'newNT') then self.flags.newNT=keyword_set(flags.newNT)
  
  self->DisplayFlags
  return,self.flags
end




pro gxVolume__define
 struct_hide,{gxVolume,inherits IDLgrVolume,wParent:0l,select:'',$
  bscale:0d,nscale:0d,tscale:0d,gyro:0d,$
  flags: {gxflags,$
          NTstored:0L,$
          NTdem:0L,$
          NTss:0L,$
          NTssdem:0L,$
          hasNT:0L,$
          TRadd:0L,$
          TRMask:0L,$
          TRfactor:0L,$
          newNT:0L,$
          newID:0L,$
          newGrid:0L,$
          newData:0L,$
          storedNTdem:0L,$
          storedNTss:0L,$
          storedNTssdem:0L,$
          hasBL:0L}}
end