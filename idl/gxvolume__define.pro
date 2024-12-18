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

function gxVolume::GetB,p,bx=Bx,by=By,Bz=Bz,volume=volume
;p is expressed in self's box fractional index coordinates
self->GetVertexAttributeData,'Bx',Bx
self->GetVertexAttributeData,'By',By
self->GetVertexAttributeData,'Bz',Bz
if keyword_set(volume) then begin
  self->GetVertexAttributeData,'chromo_bcube',chromo_bcube
  self->GetVertexAttributeData,'corona_base', corona_base
  sz=size(chromo_bcube)
  if sz[0] eq 4 and n_elements(corona_base) ne 0 then begin
    Bx=[[[chromo_bcube[*,*,*,0]]],[[bx[*,*,corona_base:*]]]]
    By=[[[chromo_bcube[*,*,*,1]]],[[by[*,*,corona_base:*]]]]
    Bz=[[[chromo_bcube[*,*,*,2]]],[[bz[*,*,corona_base:*]]]]
  endif
end
if (n_elements(p) eq 3) and (n_elements(Bx) ne 0) $
then return,[interpolate(bx,p[0],p[1],p[2]),interpolate(by,p[0],p[1],p[2]),interpolate(bz,p[0],p[1],p[2])]$
else return,[0,0,0]
end

function gxVolume::GetBx,volume=volume
  ;p is expressed in self's box fractional index coordinates
  self->GetVertexAttributeData,'Bx',Bx
  if keyword_set(volume) then begin
    self->GetVertexAttributeData,'chromo_bcube',chromo_bcube
    self->GetVertexAttributeData,'corona_base', corona_base
    sz=size(chromo_bcube)
    if sz[0] eq 4 and n_elements(corona_base) ne 0 then begin
      Bx=[[[chromo_bcube[*,*,*,0]]],[[bx[*,*,corona_base:*]]]]
    endif
  end
  return,bx
end

function gxVolume::GetBy,volume=volume
  ;p is expressed in self's box fractional index coordinates
  self->GetVertexAttributeData,'By',By
  if keyword_set(volume) then begin
    self->GetVertexAttributeData,'chromo_bcube',chromo_bcube
    self->GetVertexAttributeData,'corona_base', corona_base
    sz=size(chromo_bcube)
    if sz[0] eq 4 and n_elements(corona_base) ne 0 then begin
      By=[[[chromo_bcube[*,*,*,1]]],[[by[*,*,corona_base:*]]]]
    endif
  end
  return,by
end

function gxVolume::GetBz,volume=volume
  ;p is expressed in self's box fractional index coordinates
  self->GetVertexAttributeData,'Bz',Bz
  if keyword_set(volume) then begin
    self->GetVertexAttributeData,'chromo_bcube',chromo_bcube
    self->GetVertexAttributeData,'corona_base', corona_base
    sz=size(chromo_bcube)
    if sz[0] eq 4 and n_elements(corona_base) ne 0 then begin
      Bz=[[[chromo_bcube[*,*,*,2]]],[[bz[*,*,corona_base:*]]]]
    endif
  end
  return,bz
end

function gxVolume::Size,volume=volume
 Bz=self->GetBz(volume=volume)
 return,size(Bz)
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
          xx=self->GetVertexData('Q')
          xAttribute='Loop heating rate (Q)'
         end 
      8: begin
          xx=self->GetVertexData('alpha')
         end  
      9: begin
           xx=self->GetVertexData('Q0')
         end   
      10: begin
           xx=self->GetVertexAttributeData('curlb')
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
          yy=self->GetVertexData('Q')
          yAttribute='Loop heating rate (Q)'
         end  
         8: begin
           yy=self->GetVertexData('alpha')
         end 
         9: begin
           yy=self->GetVertexData('Q0')
         end
         10: begin
           yy=self->GetVertexData('curlb')
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
            ebtel_file=gx_ebtel_path()
            if gx_ebtel_valid_path(ebtel_file) then begin
              restore,ebtel_file
              psym=3
              if xselect eq 6 and widget_info(wRotateXY,/button_set) eq 0 then oplot,2*lrun,qrun,psym=psym,color=250 else $
              oplot,qrun,2*lrun,psym=psym,color=250
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
     print,'Voxel volume (dv)=',dv,'cm^3'
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

function gxVolume::SetQ,q_formula,quiet=quiet
  q_default='q0*(B/q[1])/(L/q[2])^0.75'
  self->GetVertexAttributeData,'q_formula',undo
  if n_elements(undo) eq 0 then undo=q_default
  undo=string(undo)
  if n_elements(q_formula) eq 0 then q_formula=q_default else q_formula=string(q_formula)
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    if ~keyword_set(quiet) then answ= dialog_message(/INFO, ['Q formula error: '+!ERROR_STATE.MSG,'Rolling it back to: '+undo])
    q_formula=undo
    result=execute('newQ='+q_formula)
    goto,skip
  end
  self->GetVertexAttributeData,'q0_coeff',q
  if n_elements(q) eq 0 then begin
    q=[0.415e-3,1e2,1e9,0,0]
    self->SetVertexAttributeData,'q0_coeff',q
  end
  mask=self->GetHeat2VolumeFactor(idx=idx)
  alpha=(self->GetVertexData('alpha'))[idx]
  curlb=(self->GetVertexData('curlb'))[idx]
  self->GetVertexAttributeData,'Q0',Q0
  self->GetVertexAttributeData,'Q',oldQ
  self->GetVertexAttributeData,'bmed',B
  self->GetVertexAttributeData,'length',L
  self->GetVertexAttributeData,'dz',dz
  default,oldQ,L*0
  ;TO BE REVISITED!!!
  if n_elements(dz) gt 0 then dz=max(dz) else dz=self.zcoord_conv[1]
  short_idx=where(L lt dz,short_count)
  if short_count gt 0 then L[short_idx]=0
  ;!!!!!!!!!!!!!!!!!!
  l=gx_rsun()*l/2
  result=execute('newQ='+q_formula)
  if result eq  0 then begin
    if ~keyword_set(quiet) then answ= dialog_message(/error, 'Q formula error: '+!ERROR_STATE.MSG+' . Requested change undone!' )
    q_formula=undo
    if (n_elements(oldQ) ne 0) then newqQ=oldQ else  result=execute('newQ='+q_formula)
  endif
  if n_elements(newQ) ne n_elements(L) then begin
    if ~keyword_set(quiet) then answ= dialog_message(/error, 'Q formula error: Expression returns unexpected array size. Requested change undone!')
    q_formula=undo
    result=execute('newQ='+q_formula)
  endif
  skip:
  self->SetVertexAttributeData,'q_formula',q_formula
  self->SetVertexAttributeData,'Q',newQ,/ram
  flags=self->setflags(newNT=total(abs(minmax(oldq-newq))) ne 0)
  if flags.newNT then self->PlotModelAttributes
  return,q_formula
end

function gxVolume::SetQ0,q0_formula,q_formula=q_formula,quiet=quiet
  q0_default='q[0]'
  self->GetVertexAttributeData,'q0_formula',undo
  if n_elements(undo) eq 0 then undo=q0_default
  undo=string(undo)
  if n_elements(q0_formula) eq 0 then q0_formula=q0_default else q0_formula=string(q0_formula)
  
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    if ~keyword_set(quiet) then answ= dialog_message(/INFO, ['Q0 formula error: '+!ERROR_STATE.MSG,'Rolling it back to: '+undo])
    q0_formula=undo
    result=execute('q0='+q0_formula)
    goto,skip
  end
  self->GetVertexAttributeData,'q0_coeff',q
  if n_elements(q) eq 0 then begin
    q=[0.415e-3,1e2,1e9,0,0]
    self->SetVertexAttributeData,'q0_coeff',q
  end
  mask=self->GetHeat2VolumeFactor(idx=idx)
  self->GetVertexAttributeData,'bmed',B
  self->GetVertexAttributeData,'length',L
  alpha=(self->GetVertexData('alpha'))[idx] 
  curlb=(self->GetVertexData('curlb'))[idx]  

  l=gx_rsun()*l/2
  result=execute('q0='+q0_formula)
  if result eq  0 then begin
    if ~keyword_set(quiet) then answ= dialog_message(/INFO, ['Q0 formula error: '+!ERROR_STATE.MSG,'Rolling it back to: ',undo])
    q0_formula=undo
    result=execute('q0='+q0_formula)
  endif
  skip:
  self->SetVertexAttributeData,'q0_formula',q0_formula
  self->SetVertexAttributeData,'Q0',Q0
  self->GetVertexAttributeData,'q_formula',q_formula
  q_formula=self->SetQ(q_formula)
  return,q0_formula
end

function gxVolume::Selected
 return,self.select
end

      
pro gxVolume::UpdateVoxelId,force=force
 ;Bitwise Voxel ID Convetions 07-Dec-2017 GN
 ;Bit 0 (ID and 1UL eq 1UL)= Do not interpolate! (includes all Chromo voxels)
 ;Bit 1 (ID and 2UL eq 2UL)= TR voxel
 ;Bit 2 (ID and 4UL eq 4UL)= Coronal voxel 
 ;Bit 3 (ID and 8UL eq 8 UL)= EUV active TR voxel (not masked by the TR mask) 
 
 start_time=systime(/s)
  if ~keyword_set(force) then if ~self.flags.NewID then begin
    message,'Same voxel ID, nothing to compute',/info
    return
  endif
  prog_id = gx_progmeter(/INIT,label='VoxelID Update Progress')
  self->GetVertexAttributeData,'voxel_id',id
  if n_elements(id) eq 0 then return
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
        tr_mask=1ul-ulong(refmaps->Get(match,/data))
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

function gxVolume::GetVertexData,var,shader=shader
  if (size(var,/tname) eq 'STRING') and ~isa(var,/array) then $
    self->GetVertexAttributeData,var,data,shader=shader
  return,isa(data)?data:!null
end

pro gxVolume::Update,select,data=data,plot_model_attributes=plot_model_attributes,getdata=getdata,$
              force=force,update=update,chromo_view=chromo_view,range=range,pwr_idx=pwr_idx,$
              nt_update=nt_update,use_dem=use_dem,avgdem=avgdem,has_used_ddm=has_used_ddm,quiet=quiet
  compile_opt hidden
  catch, error_stat
 if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return
 end
  if ~obj_valid(self.parent) then return
  if keyword_set(nt_update) then  begin
    self->ComputeNT,/force,use_dem=use_dem,avgdem=avgdem,has_used_ddm=has_used_ddm
    self->UpdateVoxelId
    return
  endif
  if self.flags.newNT then begin
    if ~keyword_set(quiet) then begin
      if dialog_message(['Volume n-T configuration needs to be updated!','Do you want to recompute n-T now?',$
      'If you need to do more volume changes before this potentially time consuming action, you can later do it manualy by pressing the "Store/Compute n-T from EBTEL" button located in the Model tab'],$ 
      /question) eq 'Yes' then self->ComputeNT
    endif else self->ComputeNT,/quiet
  endif
  self->UpdateVoxelId,force=force;It will do it only if self.flags.NewID is set or explicitely requested by /force keyword is set
  if ~(self.flags.newData or  keyword_set(update)) then return; nothing to update or no explicit request

  corona=self.parent->Corona()
  if ~keyword_set(chromo_view) then begin
    if obj_isa(corona,'gxCorona') then begin
      corona->GetProperty,chromo_view=chromo_view
    endif else chromo_view=0
  end
  if n_elements(select) ne 0 then begin
    select=strcompress(select,/rem)
    if ~keyword_set(getdata) then self.select=select
  endif else select=self.select
  case select of
  'Bx'  :begin
         data=self->GetBx(/volume)
         if self->undefined(data) then goto,undefined
        end
  'By'  :begin
         data=self->GetBy(/volume)
         if self->undefined(data) then goto,undefined
        end
  'Bz'  :begin
         data=self->GetBz(/volume)
         if self->undefined(data) then goto,undefined
        end
  'B'  :begin
         bx=self->GetBx(/volume)
         if self->undefined(Bx)then goto,undefined
         by=self->GetBy(/volume)
         if self->undefined(By) then goto,undefined
         bz=self->GetBz(/volume)
         if self->undefined(Bz) then goto,undefined
         data=sqrt(Bx^2+by^2+Bz^2)
        end
   'curlB':begin
          if n_elements(select) eq 0 then goto, no_recompute
          widget_control,/hourglass
          data=self.parent->box2volume('curlb',/full)
          if ~isa(data,/number) then begin
            ;compute curl and alpha
           self.parent->ComputeCurlBandAlpha
           data=self.parent->box2volume('curlb',/full)
          endif 
        end   
   'divB+':begin
          if n_elements(select) eq 0 then goto, no_recompute
          widget_control,/hourglass
          data=self.parent->box2volume('divb',/full)
          if ~isa(data,/number) then begin
            ;compute divB
            self.parent->ComputeDivB
            data=self.parent->box2volume('divb',/full)
          end
          data=data>0
        end
    'divB-':begin
          if n_elements(select) eq 0 then goto, no_recompute
          widget_control,/hourglass
          data=self.parent->box2volume('divb',/full)
          if ~isa(data,/number) then begin
            ;compute divB
            self.parent->ComputeDivB
            data=self.parent->box2volume('divb',/full)
          end
          data=data<1
        end    
    'helB-':begin
          if n_elements(select) eq 0 then goto, no_recompute
          widget_control,/hourglass
          data=self.parent->box2volume('alpha',/full)
          if ~isa(data,/number) then begin
           ;compute curl and alpha
           self.parent->ComputeCurlBandAlpha
           data=self.parent->box2volume('alpha',/full)
          endif 
          data[where(data gt 0)]=1./0
        end    
    'helB+':begin
          if n_elements(select) eq 0 then goto, no_recompute
          widget_control,/hourglass
          data=self.parent->box2volume('alpha',/full)
          if ~isa(data,/number) then begin
           ;compute curl and alpha
           self.parent->ComputeCurlBandAlpha
           data=self.parent->box2volume('alpha',/full)
          endif 
          data[where(data lt 0)]=1./0
        end                              
     'n_0': data=self->GetVertexData('n0')
     'T_0': data=self->GetVertexData('T0')     
     'n_b':begin 
              void=self.parent->box2volume(box2vol=box2vol,bsize=sz)
              data=(ndata=dblarr(sz[1],sz[2],sz[3]))
              ;LOOP OVER FLUXTUBES
              tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
              for j=0,tcount-1 do begin
                ndata[*]=0
                tubes[j]->GetProperty,centerbase=base
                base->GetVertexAttributeData,'owned',owned
                if n_elements(owned) gt 1 then begin
                 base->GetVertexAttributeData,'n_nth',n_nth
                 base->GetVertexAttributeData,'N_IDX',n_idx
                 ndata[n_idx]=n_nth
                 data[owned]=ndata[owned]
                end 
              end 
              data=data[box2vol]     
          end
    'dMu':begin 
           void=self.parent->box2volume(box2vol=box2vol,bsize=sz)
           data=(vol=dblarr(sz[1],sz[2],sz[3]))
           tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
           ;LOOP OVER FLUXTUBES
            for j=0,tcount-1 do begin
              tubes[j]->GetProperty,centerbase=base
              base->GetVertexAttributeData,'owned',owned
              if n_elements(owned) gt 1 then begin
               base->GetVertexAttributeData,'N_IDX',n_idx
               base->GetVertexAttributeData,'dMu',value
               vol[n_idx]=value
               data[owned]=vol[owned]
              end    
            end
            data=data[box2vol] 
           end  
      'theta_C':begin 
           void=self.parent->box2volume(box2vol=box2vol,bsize=sz)
           data=(vol=dblarr(sz[1],sz[2],sz[3]))
           tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
           ;LOOP OVER FLUXTUBES
            for j=0,tcount-1 do begin
              tubes[j]->GetProperty,centerbase=base
              base->GetVertexAttributeData,'owned',owned
              if n_elements(owned) gt 1 then begin
               base->GetVertexAttributeData,'N_IDX',n_idx
               base->GetVertexAttributeData,'THETA_C',value
               vol[n_idx]=value
               data[owned]=vol[owned]
              end    
            end
            data=data[box2vol] 
           end  
     'theta_b':begin 
           void=self.parent->box2volume(box2vol=box2vol,bsize=sz)
           data=(vol=dblarr(sz[1],sz[2],sz[3]))
           tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
           ;LOOP OVER FLUXTUBES
            for j=0,tcount-1 do begin
              tubes[j]->GetProperty,centerbase=base
              base->GetVertexAttributeData,'owned',owned
              if n_elements(owned) gt 1 then begin
               base->GetVertexAttributeData,'N_IDX',n_idx
               base->GetVertexAttributeData,'THETA_B',value
               vol[n_idx]=value
               data[owned]=vol[owned]
              end    
            end
            data=data[box2vol] 
           end  
     'a_4':begin 
           void=self.parent->box2volume(box2vol=box2vol,bsize=sz)
           data=(vol=dblarr(sz[1],sz[2],sz[3]))
           tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
           ;LOOP OVER FLUXTUBES
            for j=0,tcount-1 do begin
              tubes[j]->GetProperty,centerbase=base
              base->GetVertexAttributeData,'owned',owned
              if n_elements(owned) gt 1 then begin
               base->GetVertexAttributeData,'N_IDX',n_idx
               base->GetVertexAttributeData,'a4',value
               vol[n_idx]=value
               data[owned]=vol[owned]
              end    
            end
            data=data[box2vol] 
           end  
      'Q0': begin
             data=self.parent->box2volume('Q0',/corona)
             if self->undefined(data) then goto,undefined    
            end 
      'Q': begin
             data=self.parent->box2volume('Q',/corona)
             if self->undefined(data) then goto,undefined   
            end     
      'Length': begin
             data=self.parent->box2volume('length',/corona)
             if self->undefined(data) then goto,undefined 
            end          
   else: begin      
         undefined:
         void=self.parent->box2volume(box2vol=data)
         data=data*0
        end
   endcase
   defined:
   if keyword_set(getdata) then return
   self->SetColor,/auto
   message,strcompress(self.select+' range: ['+arr2str(minmax(data)))+']',/info
   IsCombo=self.parent->IsCombo(csize=csize,bsize=bsize,chromo_layers=chromo_layers, corona_base=corona_base)
   if ~keyword_set(chromo_view) and chromo_layers gt 0 then begin
    data[*,*,0:chromo_layers-1]=0
   endif
   if IsCombo then begin
    mean_chromo_data=total(data[*,*,0:chromo_layers-1],3)/chromo_layers
    data=data[*,*,chromo_layers-corona_base:*]
    for i=0, corona_base-1 do data[*,*,i]=mean_chromo_data
   endif
   min_val=min(data[where(data gt 0)],max=max_val)
   default,range,[min_val,max_val]
   default,pwr_idx,1
   data0=bytscl(data^pwr_idx,min=range[0]^pwr_idx,max=range[1]^pwr_idx)
   self->SetProperty,data0=data0,/interpolate,volume_select=0
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

function gxVolume::hasBL,length=length,bmed=bmed,idx=idx,shader=shader
    self->GetVertexAttributeData,'length',length,shader=shader
    self->GetVertexAttributeData,'bmed',bmed,shader=shader
    self->GetVertexAttributeData,'idx',idx,shader=shader
    return,n_elements(idx) gt 1 and ((n_elements(length) eq n_elements(bmed)) and (n_elements(length) eq n_elements(idx))) 
end

function gxVolume::hasNT,n=n,t=t,idx=idx,shader=shader
  self->GetVertexAttributeData,'n',n,shader=shader
  self->GetVertexAttributeData,'T',t,shader=shader
  self->GetVertexAttributeData,'idx',idx,shader=shader
  return,n_elements(idx) gt 1 and ((n_elements(n) eq n_elements(t)) and (n_elements(n) eq n_elements(idx)))
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
  if widget_valid(wparent) then begin
    id=widget_info(wparent,find_by_uname='GXVOLUME:DEMAVG')
    if widget_valid(id) then widget_control,id,get_value=demavg 
  endif
  default,oldkey,('')
  default,newkey,oldkey
   if self.flags.NTDEM then newkey=(strcompress('q=['+arr2str(q0_coeff)+'] & q0='+string((q0_formula))+' & q='+string((q_formula))+$
    ' & NTDEM='+string(self.flags.NTDEM))+(n_elements(demavg) gt 0? string(demavg,format="(' & DEMAVG=',i1)"):'')+' & EBTEL='+file_basename(gx_ebtel_path()))
  if self.flags.NTSS then newkey=(strcompress('q=['+arr2str(q0_coeff)+'] & q0='+string((q0_formula))+' & q='+string((q_formula))+$
    ' & NTSS='+string(self.flags.NTSS))+(n_elements(demavg) gt 0? string(demavg,format="(' & DEMAVG=',i1)"):'')+'& EBTEL=Analytical SS')
  flags=self->setflags(newNT=(string(newkey) ne string(oldkey)))
  return,flags.newNT
end

pro gxVolume::ComputeNT,question=question,quiet=quiet,force=force,NTDEM=NTDEM,NTSS=NTSS,use_dem=use_dem,has_used_ddm=has_used_ddm,avgdem=avgdem
  if keyword_set(force) then goto, compute
  if ~self.flags.newNT and keyword_set(question) then begin
    if ~keyword_set(quiet) then answ=dialog_message('The N-T pairs have been already computed using current settings. Do you want to recompute them  anyway?',/question)
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
  if self.flags.NTDEM or keyword_set(NTDEM) then begin
    if n_elements(L) gt 0 and (n_elements(L) eq n_elements(Q))  then begin
      widget_control,/hourglass
      self.parent->GetProperty,wparent=wparent
      if widget_valid(wparent) then begin
        id=widget_info(wparent,find_by_uname='GXVOLUME:DEMAVG')
        if widget_valid(id) then widget_control,id,get_value=avgdem
      end
      if widget_valid(wparent) then begin
        id=widget_info(wparent,find_by_uname='GXVOLUME:DEM/DDM')
        if widget_valid(id) then begin
          widget_control,id,get_value=useDDM
          use_dem=~keyword_set(useDDM)
        endif
      end
      widget_control,/hourglass
      if widget_valid(wparent) then begin
        wdemdt=widget_info(wparent,find_by_uname='GXVOLUME:DEMDT')
        if widget_valid(wdemdt) then widget_control,wdemdt,set_value='Updating n-T...'
      end
      message,'Computing n-T from DEM/DDM....',/info
      t0=systime(/s)
      gx_dem_interpolate,n,t,Qarr=Q,Larr=L,avgdem=avgdem,duration=duration,use_dem=use_dem,has_used_ddm=has_used_ddm
      message,string(systime(/s)-t0,format="('n-T computed from DEM/DDM in ', g0,' seconds')"),/info
      if widget_valid(wdemdt) then begin
        method=keyword_set(has_used_ddm)?'DDM':'DEM'
        if widget_valid(id) then widget_control,wdemdt,set_value=strcompress(string(method,duration,format="(a4,' interpolation computed in',f7.2,' s')"))
      end
      self->SetVertexAttributeData,'n',n,/ram
      self->SetVertexAttributeData,'T',t,/ram
      flags=self->setflags(/storedNTDEM)
    end
  end
  if self.flags.NTSS or keyword_set(NTSS) then begin
    self->GetVertexAttributeData,'length',L
    Q1=1e4
    T = 74 * (Q/Q1)^(2./7.) * (L)^(4./7.)
    n= self.nscale*1.3e6 * T^2 / L
    T = self.Tscale*T
    self->SetVertexAttributeData,'n',n,/ram
    self->SetVertexAttributeData,'T',t,/ram
    flags=self->setflags(/storedNTSS)
  endif
  newNT=self->NewNT(newkey)
  self->SetVertexAttributeData,'NTkey',newkey
  flags=self->setflags(NewNT=0)
  if ~keyword_set(quiet) then message,string(newkey),/info
  skip:
  if ~keyword_set(quiet) and size(msg,/tname) eq 'STRING' then message,msg,/info
end

pro gxVolume::ComputeN0T0,tube_id=tube_id
  r=self.parent->R(/volume)
  sz=size(r)
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
    if self->hasNT() then begin;because they might have been recomputed above
      
      T=self.parent->Box2Volume('T',/corona_only)*self.Tscale
      n=self.parent->Box2Volume('n',/corona_only)*self.Tscale
      if n_elements(blend) eq 0 then blend=0
      if blend eq 1 then begin
        ;This option has been hidden to the non-expert users
        cn=cn+n
        p=p+T*n
        ct0=p/cn
      endif else begin
        idx=where((t ne 0) and (n ne 0),count)
        if count gt 0 then begin
          cT0[idx]=T[idx]
          cn[idx]=n[idx]
          p=cT0*cn
        end
      endelse
    end
  endif

  ;FLOAT OR DOUBLE PRECISSION CHOICE FOR some of the following arrays
  ;may affect the fluxtube ownership result as shown bellow
  ;------------------------------
  bsz=self.parent->Size()
  ndata=fltarr(bsz[1],bsz[2],bsz[3])
  nvol=ndata
  tvol=ndata
  p0=ndata
  box_ct0=ndata
  box_cn=ndata
  locked=ndata
  
  void=self.parent->Box2Volume(box2vol=box2vol)
  box_ct0[box2vol]=ct0
  box_cn[box2vol]=cn
  tdata=box_ct0
  p=box_ct0*box_cn
  
  
  tube_id=ulonarr(bsz[1],bsz[2],bsz[3])
  
  ;------------------------------

  tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
  if tcount gt 0 then begin
    tube_idx=lindgen(tcount)+1
    claimant=[]
    ;LOOP OVER FLUXTUBES TO CHECK IF ANY HAS EXCLUSIVE OWNERSHIPOF THE VOLUME
    for j=0,tcount-1 do begin
      tubes[j]->GetProperty,owner=owner
      claimant=[claimant,owner]
    endfor
    ;SORT THE FLUXETUBES TO MAKE SURE THAT THE EXCLUSIVE OWNER (ASSUMED TO BE JUST ONE) IS THE LAT IN QUEUE 
    sort_idx=sort(claimant)
    tubes=tubes[sort_idx]
    tube_idx=tube_idx[sort_idx]
  end
  
  ;LOOP OVER FLUXTUBES TO PROGRESEVELY CLAIM FLUXTUBE OWNERSHIP
  for j=0,tcount-1 do begin
    p0[*]=0
    tvol[*]=0
    nvol[*]=0
    tubes[j]->GetProperty,T0=T0,centerbase=base,hide=hide,owner=owner
    if keyword_set(hide) then goto,skip_tube
    base->GetVertexAttributeData,'n_th',n_th
    base->GetVertexAttributeData,'N_IDX',n_idx
    base->GetVertexAttributeData,'owned',owned
    if (n_elements(n_th) eq 0) or (n_elements(n_idx) eq 0) or (n_elements(n_th) ne n_elements(n_idx)) then goto,skip_tube
    p0[n_idx]=(box_ct0*box_cn)[n_idx]+T0*n_th
    ;------------------------------
    ; IF P0 and P are defined above as floating point arrays, GE and GT may assign different ownership
    ;--------------------------------------
    ;IF THIS FLUXTUBE IS AN OWNER DO NOT ALTER ITS VOLUME OWNERSHIP
    if (~keyword_set(owner) or (n_elements(owned) eq 0)) then begin
      owned=where(p0 gt p,ocount)
    endif else begin
      ocount=n_elements(owned)
      message, string(tube_idx[j],format="('Fluxtube ',g0,' has user-defined volume ownership')"),/cont
    endelse
    ;----------------------------------------
    if ocount gt 0 then begin
      p[owned]=p0[owned]
      nvol[n_idx]=n_th
      ndata[owned]=nvol[owned]
      tvol[n_idx]=p0[n_idx]/(ndata+box_cn)[n_idx]
      tdata[owned]=tvol[owned]
      tube_id[owned]=ulong(tube_idx[j])
    end
    skip_tube:
  end

  ;LOOP AGAIN OVER FLUXTUBES TO DETERMINE UNIQ FLUXTUBE OWNERSHIP
  for j=0,tcount-1 do begin
    tubes[j]->GetProperty,centerbase=base,hide=hide
    owned=where(tube_id eq ulong(tube_idx[j]),ocount)
    base->SetVertexAttributeData,'owned',owned
  endfor

  ;HERE WE QUESTIONABLY ADD FLUXETUBE DENSITY (IF ANY FLUXTUBE) TO LOCAL CORONA DENSITY
  
  ndata=cn+ndata[box2vol]
  tdata=tdata[box2vol]
  
  ;COMMENT THE LINE ABOVE AND UNCOMMENT THE LINES BELOW TO REPLACE CORONAL DENSITY WITH FLUXTUBE DENSITIES
  ;owned=where(tube_id ne 0,complement=corona_owned,ncomplement=count)
  ;if count gt 0 then ndata[corona_owned]=cn[corona_owned]
  ;--------------------

  if ~self.parent->IsCombo() then begin
    if chromo_count gt 0 then begin
      ndata[chromo_owned]=chromo_n
      tdata[chromo_owned]=chromo_t
      tr=(array_indices(r,max(chromo_owned)))[2]+1
      self->setvertexattributedata,'chromo_layers',tr
    endif else begin
      ;provision for beckward compatibility with old format combo models
      chromo_idx=self->GetVertexData('chromo_idx')
      if isa(chromo_idx,/number,/array) then begin
        self->GetVertexAttributeData,'chromo_n',chromo_n
        self->GetVertexAttributeData,'chromo_T',chromo_t
        ndata[chromo_idx]=chromo_n
        tdata[chromo_idx]=chromo_t
      endif
    endelse
  endif else begin
    void=self.parent->Box2Volume(box2vol=box2vol)
    tube_id=tube_id[box2vol]
    chromo_idx=self->GetVertexData('chromo_idx')
    if isa(chromo_idx,/number,/array) then begin
      self->GetVertexAttributeData,'chromo_n',chromo_n
      self->GetVertexAttributeData,'chromo_T',chromo_t
      ndata[chromo_idx]=chromo_n
      tdata[chromo_idx]=chromo_t
    endif
  endelse

  self->SetVertexAttributeData,'n0',ndata
  self->SetVertexAttributeData,'T0',tdata
end

;pro gxVolume::ComputeN0T0_back,tube_id=tube_id
;  r=self.parent->R()
;  sz=size(r)
;  cn=fltarr(sz[1],sz[2],sz[3])
;  cT0=cn
;  p=cn
;  chromo_count=0
;  corona=self.parent->Corona()
;  if obj_valid(corona) then begin
;    cn[*]=corona->GetDensity(r,h=h,n0=n0,T0=temp,chromo_n=chromo_n,chromo_T=chromo_T,chromo_h=chromo_h,chromo_view=chromo_view,blend=blend,ignore=ignore)
;    default,ignore, 0
;    if ~keyword_set(ignore) then begin
;      cT0[*]=temp
;      p=cT0*cn
;      chromo_owned=where(r lt (1+chromo_h),chromo_count)
;    endif else begin
;     cn[*]=0 
;    endelse
;  end  
;  if self->hasNT(n=n,t=t,idx=idx) then begin
;   if self.flags.newNT then begin
;     if self.flags.storedNTDEM and self.flags.NTSS then begin
;      case dialog_message('Currently stored n-T pairs were computed from EBTEL DEM. But EBTEL analytical computation option is currently selected! Do you want to recompute n-T now?',/question) of 
;        'Yes':self->ComputeNT
;        else:
;      endcase
;     endif
;     if self.flags.storedNTSS and self.flags.NTDEM then begin
;       case dialog_message('Currently stored n-T pairs were computed from analytical EBTEL solution. But EBTEL DEM computation option is currently selected! Do you want to recompute n-T now?',/question) of
;         'Yes':self->ComputeNT
;         else:
;       endcase
;     endif
;     if (self.flags.storedNTDEM and self.flags.NTDEM) or (self.flags.storedNTSS and self.flags.NTSS) then begin
;       newNT=self->NewNT(newkey,oldkey)
;       if newNT then begin  
;         case dialog_message(['Currently stored n-T pairs were using different settings!','Do you want to recompute n-T now?',$
;                              'Old Settings: '+string(oldkey),'New Settings: '+string(newkey)],/question) of
;           'Yes':self->ComputeNT
;           else:
;         endcase
;       end
;     endif
;     end
;     if self->hasNT(n=n,t=t,idx=idx) then begin;because they might have been recomputed above
;       T=T*self.Tscale
;       n=n*self.Tscale
;       if n_elements(blend) eq 0 then blend=0
;       if blend eq 1 then begin
;         ;This option has been hidden to the non-expert users
;         cn[idx]=cn[idx]+n
;         p[idx]=p[idx]+T*n
;         ct0=p/cn
;       endif else begin
;         cT0[idx]=T
;         cn[idx]=n
;         p=cT0*cn
;       endelse
;     end
;  endif
;  
;  ;FLOAT OR DOUBLE PRECISSION CHOICE FOR some of the following arrays
;  ;may affect the fluxtube ownership result as shown bellow
;  ;------------------------------
;  ndata=fltarr(sz[1],sz[2],sz[3])
;  nvol=ndata
;  tvol=ndata
;  p0=nvol
;  tdata=ct0
;  p=ct0*cn
;  tube_id=ulonarr(sz[1],sz[2],sz[3])
;  ;------------------------------
;
;  tubes=self.parent->Get(/all,ISA='gxFluxtube',count=tcount)
;  
;  ;LOOP OVER FLUXTUBES TO PROGRESEVELY CLAIM FLUXTUBE OWNERSHIP
;  for j=0,tcount-1 do begin
;    p0[*]=0
;    tvol[*]=0
;    nvol[*]=0
;    tubes[j]->GetProperty,T0=T0,centerbase=base,hide=hide
;    if keyword_set(hide) then goto,skip_tube
;    base->GetVertexAttributeData,'n_th',n_th
;    base->GetVertexAttributeData,'N_IDX',n_idx
;    p0[n_idx]=(cT0*cn)[n_idx]+T0*n_th
;    ;------------------------------
;    owned=where(p0 gt p,ocount)
;    ; IF P0 and P are defined above as floating point arrays, GE and GT may assign different ownership
;    ;--------------------------------------
;
;    if ocount gt 0 then begin
;      p[owned]=p0[owned]
;      nvol[n_idx]=n_th
;      ndata[owned]=nvol[owned]
;      tvol[n_idx]=p0[n_idx]/(ndata+cn)[n_idx]
;      tdata[owned]=tvol[owned]
;      tube_id[owned]=ulong(j+1)
;    end
;    skip_tube:
;  end
;  
;  ;LOOP AGAIN OVER FLUXTUBES TO DETERMINE UNIQ FLUXTUBE OWNERSHIP
;  for j=0,tcount-1 do begin
;    tubes[j]->GetProperty,centerbase=base,hide=hide
;    owned=where(tube_id eq ulong(j+1),ocount)
;    base->SetVertexAttributeData,'owned',owned
;  endfor
;  
;  ;HERE WE QUESTIONABLY ADD FLUXETUBE DENSITY (IF ANY FLUXTUBE) TO LOCAL CORONA DENSITY
;  ndata=ndata+cn
;  ;COMMENT THE LINE ABOVE AND UNCOMMENT THE LINES BELOW TO REPLACE CORONAL DENSITY WITH FLUXTUBE DENSITIES 
;  ;owned=where(tube_id ne 0,complement=corona_owned,ncomplement=count)
;  ;if count gt 0 then ndata[corona_owned]=cn[corona_owned]
;  ;--------------------
;  
;  if ~self.parent->IsCombo() then begin
;    if chromo_count gt 0 then begin
;      ndata[chromo_owned]=chromo_n
;      tdata[chromo_owned]=chromo_t
;      tr=(array_indices(r,max(chromo_owned)))[2]+1
;      self->setvertexattributedata,'chromo_layers',tr
;    endif else begin
;      ;provision for becakward compatibility with old format combo models
;      chromo_idx=self->GetVertexData('chromo_idx')
;      if isa(chromo_idx,/number,/array) then begin
;        self->GetVertexAttributeData,'chromo_n',chromo_n
;        self->GetVertexAttributeData,'chromo_T',chromo_t
;        ndata[chromo_idx]=chromo_n
;        tdata[chromo_idx]=chromo_t
;      endif
;    endelse
;  endif else begin
;    void=self.parent->Box2Volume(box2vol=box2vol)
;    ndata=ndata[box2vol]
;    tdata=tdata[box2vol]
;    tube_id=tube_id[box2vol]
;    chromo_idx=self->GetVertexData('chromo_idx')
;    if isa(chromo_idx,/number,/array) then begin
;      self->GetVertexAttributeData,'chromo_n',chromo_n
;      self->GetVertexAttributeData,'chromo_T',chromo_t
;      ndata[chromo_idx]=chromo_n
;      tdata[chromo_idx]=chromo_t
;    endif
;  endelse
;
;  self->SetVertexAttributeData,'n0',ndata
;  self->SetVertexAttributeData,'T0',tdata
;end

function gxVolume::ntubes,tubes=tubes
 tubes=self.parent->Get(/all,isa='gxfluxtube',count=count)
 return,count
end

function gxVolume::setflags,_extra=flags
  
  if n_elements(flags) eq 0 then begin
    message,'No flag provided',/info
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
    
  if tag_exist(flags,'storedNTdem') then begin
    self.flags.storedNTdem=keyword_set(flags.storedNTdem)
    if self.flags.storedNTdem then begin
     self.flags.storedNTss=0
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


function gxVolume::GetHeat2Volume
 if ptr_valid(self.heat2volume) then return,self.heat2volume
 self->SetHeat2Volume 
 return,self.heat2volume   
end

 
pro gxVolume::Add2RAM,replaced=replaced,_extra=_extra
 self.RAM=gx_add2pointer(self.RAM,replaced=replaced,_extra=_extra)
end 

pro gxVolume::SetVertexAttributeData,name,value,ram=ram,shader=shader
 if keyword_set(shader)then begin
  ;here the parent class procedure if used if explicitely requested
  self->IDLgrVolume::SetVertexAttributeData,name,value
  return
 endif
 ;here the attributes with the same names set in the previous versions, if any are removed
 self->IDLgrVolume::GetVertexAttributeData,name,old_value
 if n_elements(old_value) ne 0 then begin
  self->IDLgrVolume::SetVertexAttributeData,name,0b
 endif
 default,value,old_value
 
 if keyword_set(ram) then self.Add2RAM,_extra=create_struct(name,value)$
                     else self.Add2ROM,_extra=create_struct(name,value)
end

pro gxVolume::GetVertexAttributeData,name,value,shader=shader
  if keyword_set(shader)then begin
    ;here the parent class procedure if used if explicitely requested
    self->IDLgrVolume::GetVertexAttributeData,name,value
    return
  endif
  ;check the ROM first
  if ptr_valid(self.rom) then begin
    if tag_exist(*self.rom,name,index=index) then begin
      value=(*self.rom).(index)
      return
    endif
  endif
  ;check the RAM second
  if ptr_valid(self.ram) then begin
    if tag_exist(*self.ram,name,index=index) then begin
      value=(*self.ram).(index)
      return
    endif
  endif
  ;check special cases for backward compatibility
  if ptr_valid(self.rom) then begin
    if tag_exist(*self.rom,'status') and tag_exist(*self.rom,'physLength') and tag_exist(*self.rom,'avField')then begin
      if ptr_valid(self.ram) then if tag_exist(*self.ram,'idx') then begin 
      case strlowcase(name) of
        'length':value=(*self.rom).physLength[(*self.ram).idx]
        'bmed':value=(*self.rom).avField[(*self.ram).idx]
        'foot1':value=(*self.rom).startidx[(*self.ram).idx]
        'foot2':value=(*self.rom).endidx[(*self.ram).idx]
        else:
      endcase
      if n_elements(value) ne 0 then return
      endif
    endif
  end  
  ;finaly check the shader attributes
  self->IDLgrVolume::GetVertexAttributeData,name,value
end


function gxVolume::GetRAM
  return,self.RAM
end

function gxVolume::SwapRAM,newRAM
  default,newRam,ptr_new()
  RAM=self.RAM
  if size(newRAM,/tname) eq 'POINTER' then self.RAM=newRAM
  return,RAM
end

pro gxVolume::Add2ROM,replaced=replaced,_extra=_extra
  self.ROM=gx_add2pointer(self.ROM,replaced=replaced,_extra=_extra)
end


function gxVolume::GetROM
  return,self.ROM
end

pro gxVolume::SetHeat2Volume,seeds=seeds,mask=mask
  if ptr_valid(self.heat2volume) then heat2volume=*self.heat2volume
  default,heat2volume,$
    {in:1.0,$
    nw:1.0,$
    enw:1.0,$
    pl:1.1,$
    fac:1.2,$
    pen:1.3,$
    umb:1.4,$
    umb2umb:0.1,$
    seeded:1,$
    olines:0,$
    mask:fltarr(7,7),$
    oscale:'1.1-z/max(z)'}
  if n_elements(seeds) eq 8 then begin
    for k=0,7 do heat2volume.(k)=seeds[k]
    heat2volume.seeded=1
  endif
  if n_elements(mask) eq 49 then begin
    heat2volume.mask=mask
    heat2volume.seeded=0
  endif
  if heat2volume.seeded then begin
    for i=0, 6 do begin
      for j=0,6 do begin
        heat2volume.mask[i,j]=heat2volume.(i)*heat2volume.(j)
      endfor
    endfor
    heat2volume.mask[6,6]=heat2volume.umb2umb
  endif
  ptr_free,self.heat2volume
  self.heat2volume=ptr_new(heat2volume)
  flags=self->SetFlags(/NewNT)
end

function gxVolume::GetQ0formula,mask=mask
 self->GetVertexAttributeData,'q0_formula',q0_formula
 q0_formula=n_elements(q0_formula) gt 0? string(q0_formula):''
 mask=strlowcase(stregex(q0_formula,'mask',/extract,/fold))
 return,q0_formula
end

function gxVolume::GetQformula,mask=mask
  self->GetVertexAttributeData,'q_formula',q_formula
  q_formula=n_elements(q_formula) gt 0? string(q_formula):''
  return,q_formula
end

function gxVolume::GetHeat2VolumeKey
 h2v=self->GetHeat2Volume()
 key='q0_selection={'
 names=tag_names(*h2v)
 for k=0,8 do key+=strlowcase(names[k])+':'+string((*h2v).(k),',',format="(g0,a0)")
 if ~(*h2v).seeded then key+=strlowcase(names[10])+':['+arr2str((*h2v).(10))+'],'
 key+=strlowcase(names[9])+':'+string((*h2v).(9),format="(g0,a0)")
 key+=~(*h2v).olines?'}':','+strlowcase(names[11])+":'"+(*h2v).(11)+"'}"
 return,strcompress(key)
end

function gxVolume::GetHeat2VolumeFactor,idx=idx,compute=compute
  h2vfactor= self->GetVertexData('h2vfactor')
  if ~isa(h2vfactor)then compute=1
  if keyword_set(compute) then begin  
   widget_control,/hourglass
   h2v=self->GetHeat2Volume()
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
   sz=self.parent->Size()
   if ptr_valid(self.RAM) then begin
     if ~tag_exist(*self.RAM, 'alpha') then self.parent->ComputeCurlBandAlpha
   endif else self.parent->ComputeCurlBandAlpha
 
   idx=where(((*self.ROM).status and 4l) eq 4l)
   if n_elements(mask) eq sz[1]*sz[2] then begin
    foot1=(*self.ROM).startidx[idx]
    foot2=(*self.ROM).endidx[idx]
    foot1=array_indices(sz[1:3]+[0,0,10],/dim,foot1)
    foot2=array_indices(sz[1:3]+[0,0,10],/dim,foot2)
    foot1=reform(mask[foot1[0,*],foot1[1,*]])-1
    foot2=reform(mask[foot2[0,*],foot2[1,*]])-1
    h2vfactor=(*h2v).mask[foot1,foot2] 
    if (*h2v).olines then begin
       oidx=where(((*self.ROM).status and 16l) eq 16l)
       foot1=(*self.ROM).startidx[oidx]
       foot2=(*self.ROM).endidx[oidx]
       foot1=array_indices(sz[1:3]+[0,0,10],/dim,foot1)
       foot2=array_indices(sz[1:3]+[0,0,10],/dim,foot2)
       omin=min([min(foot1[2,*]),min(foot2[2,*])])
       base_idx=where(foot1[2,*] eq omin,base_count,comp=side_idx,ncomp=side_count)
       seeds=[]
       for k=0,6 do seeds=[seeds,(*h2v).(k)]
       if base_count gt 0 then  begin 
        base=seeds[reform(mask[foot1[0,base_idx],foot1[1,base_idx]])-1]
        z=findgen(sz[3])
        z=z[foot2[2,base_idx]]
        curlb=(*self.RAM).curlb[oidx[base_idx]]
        alpha=(*self.RAM).alpha[oidx[base_idx]]
        Bx=(self->GetVertexData('Bx'))[oidx[base_idx]]
        By=(self->GetVertexData('By'))[oidx[base_idx]]
        Bz=(self->GetVertexData('Bz'))[oidx[base_idx]]
        dummy=execute('side=reform('+(*h2v).oscale+')')
        h2vfactor=[h2vfactor,base*side[lindgen(n_elements(z))]]
        idx=[idx,oidx[base_idx]]
        if side_count gt 0 then  begin
           base=seeds[reform(mask[foot2[0,side_idx],foot2[1,side_idx]])-1]
           z=findgen(sz[3])
           z=z[foot1[2,side_idx]]
           curlb=(*self.RAM).curlb[oidx[side_idx]]
           alpha=(*self.RAM).alpha[oidx[side_idx]]
           Bx=(self->GetVertexData('Bx'))[oidx[side_idx]]
           By=(self->GetVertexData('By'))[oidx[side_idx]]
           Bz=(self->GetVertexData('Bz'))[oidx[side_idx]]
           dummy=execute('side=reform('+(*h2v).oscale+')')
           h2vfactor=[h2vfactor,base*side[lindgen(n_elements(z))]]
           idx=[idx,oidx[side_idx]]
         endif
       endif
     endif
   endif else  h2vfactor=replicate(1,(*h2v).olines?n_elements(idx):(n_elements(idx)+n_elements(oidx)))
   self->Add2RAM,_extra={h2vfactor:h2vfactor,idx:idx}
   q0_formula=self->SetQ0(self->GetVertexData('q0_formula'))
   self->RequestVolumeUpdate;self->Update,/quiet
 endif
 idx=self->GetVertexData('idx')
 return,h2vfactor
end



pro gxVolume__define
 struct_hide,{gxVolume,inherits IDLgrVolume,wParent:0l,select:'',$
  bscale:0d,nscale:0d,tscale:0d,gyro:0d,heat2volume:ptr_new(),rom:ptr_new(),ram:ptr_new(),$
  flags: {gxflags,$
          NTstored:0L,$
          NTdem:0L,$
          NTss:0L,$
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
          hasBL:0L}}
end