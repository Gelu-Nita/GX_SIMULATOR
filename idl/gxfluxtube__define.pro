function dot,x,y
 return, transpose(x)#y
end

FUNCTION gxFluxTube::INIT,centerline=centerline,_extra=_extra
 compile_opt hidden
 font=!defaults.font
 if ~obj_isa(centerline,'GXBLINE') then message,'A valid center line gxBline object is needed!'
 centerline->GetProperty,parent=parent
 if ~obj_isa(parent,'GXMODEL') then message,'A valid gxModel parent object is needed for this fluxtube!'
 
 
 ;**********02Sep2016
 centerline->GetProperty,data=data,lock=lock
 centerline->GetVertexAttributeData,'s',s
 centerline->GetVertexAttributeData,'B',lb
 top=min(abs(s),idx)
 p=data[*,idx]
 full=parent->GetBline(p,/full)
 parent->Remove,centerline
 centerline=full[0]
 centerline->SetProperty,lock=lock
 parent->Add,centerline
;*******02Sep2016

 result=self->IDLgrModel::Init(SELECT_TARGET=1,_extra=_extra)
 self.centerline=centerline
 self.centerline->GetProperty,data=line,lock=lock,top=top
 scale=parent->GetScale()
 dx=scale.YCOORD_CONV[1]
 dy=scale.YCOORD_CONV[1]
 dz=scale.ZCOORD_CONV[1]
 self.centerline->SetProperty,/center
 self.lock=lock
 sz=size(line)
 if total(top) eq 0 then begin
  ls=fltarr(sz[2])
  lb=fltarr(3,sz[2])
  for i=0, sz[2]-1 do begin
   lb[*,i]=parent->GetB(reform(line[*,i]))
   b=norm(lb[*,i])
   if i eq 0 then begin
    s=i
    b0=b
    ls[i]=0
   endif else ls[i]=ls[i-1]+norm((line[*,i]-line[*,i-1])*[dx,dy,dz])
   if b lt b0 then begin
    s=i
    b0=b
   end
  end
  ls=ls-ls[s]
  self.centerline->SetVertexAttributeData,'B',lb
  self.centerline->SetVertexAttributeData,'s',ls
  self.centerline->SetProperty,top=line[*,s]
 endif else m=min((line[0,*]-top[0])^2+(line[1,*]-top[1])^2+(line[2,*]-top[2])^2,s)
 self.centerindex=s
 r=min((parent->Size())[1:3])/32>1
 self.a=r
 self.b=r
 self.phi=0
 self.nphi=12
 self.nrho=1
 self.p_nth='[2.0,2.0,0.0,0.0]'
 self.nr_nth='exp(-(p[0]*x/a)^2-(p[1]*y/b)^2-(p[2]*x/a)^4-(p[3]*y/b)^4)'
 self.q_nth='[3.0,0.0,0.0]'
 self.ns_nth='exp(-(q[0]*((s-s0)/l+q[2]))^2-(q[1]*((s-s0)/l+q[2]))^4)'
 self.p_th='[2.0,2.0,0.0,0.0]'
 self.q_th='[0.0]'
 self.nr_th='exp(-(p[0]*x/a)^2-(p[1]*y/b)^2-(p[2]*x/a)^4-(p[3]*y/b)^4)'
 self.nz_th='exp(-z/R/(6.7576e-8)/T0)'
 
 self.n_nth=1.5e+007
 self.n_th=5E09
 self.T0=2e7
 self.dist_e=3
 self.dist_ang=1
 self.eps=0.05
 self.kappa=4
 self.emin=0.01
 self.emax=10
 self.ebreak=1
 self.delta1=3.2
 self.delta2=6
 self.theta_c0=60
 self.theta_b0=90
 self.dMu0=0.1
 self.a4_0=10
 self.use_clg=1

 self->SetVersors
 self->SetBase
 self->SetBlines
 self->ComputeDistance
 self->ComputeVersors
 
 ; alpha
 alpha=centerline->GetAlpha()
 return,1
END

pro gxFluxTube::GetBounds,xrange=xrange,yrange=yrange,zrange=zrange
 all=self->Get(/All)
 all=[self.centerline,all]
 if size(all[0],/tname) eq 'OBJREF' then begin
 nlines=0
 for i=0,n_elements(all)-1 do begin
  if obj_isa(all[i],'gxBline') then begin
  nlines+=1
     if nlines ne 1 then begin
      all[i]->GetProperty,xrange=xr,yrange=yr,zrange=zr
      xrange[0]=min([xrange,xr],max=max) & xrange[1]=max
      yrange[0]=min([yrange,yr],max=max) & yrange[1]=max
      zrange[0]=min([zrange,zr],max=max) & zrange[1]=max
     endif else begin
      all[i]->GetProperty,xrange=xrange,yrange=yrange,zrange=zrange
     end 
  end
 end
 end
end

PRO gxFluxTube::SetVersors
 self.centerline->GetProperty,data=line,parent=oModel
 p=line[*,self.centerindex]
 n=oModel->GetB(p)
 n=n/norm(n)
 self.n=n
 n1=oModel->GetB(p+n/10)
 n1=n1/norm(n1)
 ey=crossp(n,n1-n)
 if norm(ey) eq 0 then ey=crossp(n,randomu(seed,3))
 self.ey=ey/norm(ey)
 ex=crossp(self.n,self.ey)
 self.ex=ex/norm(ex)
END 

PRO gxFluxTube::SetBase
 phi =findgen(self.nphi)*2*!pi/(self.nphi-1)
 alpha =findgen(self.nrho+1)*self.a/(self.nrho)
 alpha=alpha[1:*]
 beta =findgen(self.nrho+1)*self.b/(self.nrho)
 beta=beta[1:*]
 data=fltarr(3,self.nrho,self.nphi)
 self.centerline->GetProperty,data=line,parent=model
 center=line[*,self.centerindex]
 for i=0,self.nrho-1 do begin
 for j=0,self.nphi-1 do begin
  data[*,i,j]=center+(alpha[i]*Cos(phi[j])*Cos(self.phi)-beta[i]*Sin(phi[j])*Sin(self.phi))*self.ex+$
                     (alpha[i]*Cos(phi[j])*Sin(self.phi)+beta[i]*Sin(phi[j])*Cos(self.phi))*self.ey
 end
 end

 datax=reform(data[0,*,*],self.nrho,self.nphi)
 datay=reform(data[1,*,*],self.nrho,self.nphi)
 dataz=reform(data[2,*,*],self.nrho,self.nphi)
 scale=model->GetScale()
 if ~obj_valid(self.base) then begin
 self.base=Obj_New('IDLgrSurface',dataz,datax,datay,name='Base',color=[255,0,0],$
 XCOORD_CONV=scale.XCOORD_CONV,YCOORD_CONV=scale.YCOORD_CONV,ZCOORD_CONV=scale.ZCOORD_CONV)
 self->Add,self.base
 endif else self.base->SetProperty,datax=datax,datay=datay,dataz=dataz
 all=self->Get(/All)
 if size(all[0],/tname) eq 'OBJREF' then begin
 for i=0,n_elements(all)-1 do begin
  if all[i] ne self.base  then begin
	   self->Remove,all[i]
	   obj_destroy,all[i]
  end
 end
 end
END

PRO gxFluxTube::SetBLines
 widget_control,/hourglass
 base=self->GetByName('Base')
 base->GetProperty,data=data
 sz=size(data)
 n=sz[2]*sz[3]
 data=reform(data,3,n)
 self.centerline->GetProperty,parent=model
 model->GetProperty,winOS=WinOS
 if ~winOS then begin
   for i=0,n-1 do begin
    self->SetBline,reform(data[*,i]),line=line
    if obj_isa(line,'gxBline') then begin
    line->GetProperty,top=top
     if n_elements(contour) eq 0 then contour=top else contour=[[contour],[top]]
    end
   end
 endif else begin
  lines=model->ComputeBlines(data,tr_height_km=0)
  good=where(obj_valid(lines) eq 1,count)
  if count gt 0 then begin
    lines=lines[good]
    self->add,lines
  endif
  for i=0,count-1 do begin
    lines[i]->GetProperty,top=top
    if n_elements(contour) eq 0 then contour=top else contour=[[contour],[top]]
  endfor
 endelse
 scale=model->GetScale()
 self->Add,obj_new('idlgrpolyline',contour[0,*],contour[1,*],contour[2,*],color=[0,0,255], name='Top',$
 XCOORD_CONV=scale.XCOORD_CONV,YCOORD_CONV=scale.YCOORD_CONV,ZCOORD_CONV=scale.ZCOORD_CONV,hide=1)
 if obj_isa(self.PARENT,'gxmodel') then self.Parent->SetROI
END

PRO gxFluxTube::SetBLine,p,line=line
 self.centerline->GetProperty,parent=parent
 line=parent->GetBLine(p,subgridpts=1,/no_tr)
 if obj_isa(line,'gxBline') then self->add,line
END

PRO gxFluxTube::ComputeVersors,ex=ex,ey=ey

self.centerline->getvertexattributedata,'B',bb
self.centerline->getvertexattributedata,'s',ss
np=n_elements(ss)
ex=fltarr(3,np)
ey=fltarr(3,np)

n1=cos(self.phi)*self.ex+sin(self.phi)*self.ey
for i=0,np-1 do begin
 nn=bb[*,i]
 nn=nn/norm(nn)
 ey[*,i]=crossp(nn,n1)
 ey[*,i]=ey[*,i]/norm(ey[*,i])
 ex[*,i]=crossp(nn,ey[*,i])
 ex[*,i]=ex[*,i]/norm(ex[*,i])
end 
   if ~obj_valid(self.base) then self->SetBase
   self.base->SetVertexAttributeData,'ex',ex
   self.base->SetVertexAttributeData,'ey',ey
END

FUNCTION gxFluxTube::GetVertexData,key
  self.base->GetVertexAttributeData,key,var
  if n_elements(var) eq 0 then self.centerline->GetVertexAttributeData,key,var
  return,n_elements(var) gt 0?var:!null
END

PRO gxFluxTube::SetVertexData,key,var,centerline=centerline
  obj=keyword_set(centerline)?self.centerline:self.base
  if obj_valid(obj) then obj->SetVertexAttributeData,key,var
END

PRO gxFluxTube::ComputeDistance,cutoff=cutoff
 ; self.base->GetVertexAttributeData,'cutoff',cutoff
  default,cutoff,3*max([self.a,self.b])>36
  self.centerline->GetProperty,data=line,parent=Model
  np=n_elements(line[0,*])
  sz=Model->size()
  box=bytarr(sz[1],sz[2],sz[3])
  box[line[0,*],line[1,*],line[2,*]]=1
  ctr=where(box eq 1)
  box = MORPH_DISTANCE (box,neighbor=3,/back,/no_copy)
  box[0,*,*]=box[1,*,*]
  box[*,0,*]=box[*,1,*]
  box[*,*,0]=box[*,*,1]
  box[sz[1]-1,*,*]=box[sz[1]-2,*,*]
  box[*,sz[2]-1,*]=box[*,sz[2]-2,*]
  box[*,*,sz[3]-1]=box[*,*,sz[3]-2]
  n_idx=where(box lt cutoff,comp=comp)
  box[comp]=0
  n_idx=where(box ne 0)
  n_idx=[n_idx,ctr]
  n_idx=n_idx[sort(n_idx)]
  idx=array_indices(box,n_idx)
  sz=size(idx)
  c_idx=intarr(sz[2])
  for i=0,sz[2]-1 do begin
   d=(line[0,*]-idx[0,i])^2+(line[1,*]-idx[1,i])^2+(line[2,*]-idx[2,i])^2
   m=min(d,imin)
   c_idx[i]=imin
  end
   if ~obj_valid(self.base) then self->SetBase
   self.base->SetVertexAttributeData,'C_IDX',c_idx
   self.base->SetVertexAttributeData,'N_IDX',n_idx
   self.base->SetVertexAttributeData,'cutoff',cutoff
END

function gxFluxTube::CheckSyntax,nr_nth=nr_nth,ns_nth=ns_nth,nr_th=nr_th,nz_th=nz_th
 
 if n_elements(nr_nth) ne 0 then begin
   result=execute('p='+self.p_nth) 
   result=execute('q='+self.q_nth)
   x=0 & y=0 & a=self.a & b=self.b & s=1 & l=1 & s0=self.s0
   success=execute('test='+nr_nth[0])
   if ~success then begin
    answ=dialog_message('Invalid non thermal radial distribution:'+STRING(10b)+nr_nth+STRING(10b)+'Check syntax!',/error)
    return,0
   end  
 end
 
 if n_elements(ns_nth) ne 0 then begin
   result=execute('p='+self.p_nth) 
   result=execute('q='+self.q_nth)
   x=0 & y=0 & a=self.a & b=self.b & s=1 & l=1 & s0=self.s0
   success=execute('test='+ns_nth[0])
   if ~success then begin
    answ=dialog_message('Invalid non thermal longitudinal distribution:'+STRING(10b)+ns_nth+STRING(10b)+'Check syntax!',/error)
    return,0
   end  
 end
 
 if n_elements(nr_th) ne 0 then begin
   result=execute('p='+self.p_th) 
   result=execute('q='+self.q_th)
   n0=self.n_nth & T0=self.T0 & x=0 & y=0 & a=self.a & b=self.b & h=1 & R=1 & s0=self.s0
   success=execute('test='+nr_th[0])
   if ~success then begin
    answ=dialog_message('Invalid thermal radial distribution:'+STRING(10b)+nr_th+STRING(10b)+'Check syntax!',/error)
    return,0
   end  
 end
 
 if n_elements(nz_th) ne 0 then begin
   result=execute('p='+self.p_th) 
   result=execute('q='+self.q_th)
   n0=self.n_nth & T0=self.T0 & x=0 & y=0 & a=self.a & b=self.b & z=1 & R=1 & s0=self.s0 & s=1 & l=1
   success=execute('test='+nz_th[0])
   if ~success then begin
    answ=dialog_message('Invalid thermal height distribution:'+STRING(10b)+nz_th+STRING(10b)+'Check syntax!',/error)
    return,0
   end  
 end
 return,1
END

PRO gxFluxTube::SelectThermalModel,usedem=usedem
      device, get_screen_size=scr
      xscale=scr[0]/1920.
      font=!defaults.font
      prefix='GXFLUXTUBE:'
      wThermalTab=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:THbase')
      child =widget_info(wThermalTab,/child)
      if widget_valid(child) then widget_control,child,/destroy
      if ~keyword_set(usedem) then begin
        error=execute('p_th='+self.p_th)
        error=execute('q_th='+self.q_th)
        wNdistribution=widget_base(wThermalTab,UNAME =prefix+ 'NT',/column)
        wParmBase=widget_base(wNdistribution,/column,/frame)
        wp=cw_objArray(wParmBase,uname=prefix+'p_th',xtextsize=5,format='(g0)',units='',$
        value=p_th,label='p',lfont=font,/frame)
        wq=cw_objArray(wParmBase,uname=prefix+'q_th',xtextsize=5,format='(g0)',units='',$
        value=q_th,label='q',lfont=font,/frame) 
        wbase=widget_base(wParmBase,/row,/frame)
        wn_nth=cw_objfield(wBase, UNAME=prefix+'n_th', LABEL='n0=',$
          INCREMENT=1e7, $
          UNITS='cm^-3', $
          VALUE=self.n_th,/frame) 
        wT0=cw_objfield(wBase, UNAME=prefix+'T0', LABEL='T0=',$
          INCREMENT=1, $
          UNITS='K', $
          VALUE=self.T0,/frame,XTEXTSIZE=10)   
        text=widget_label(font=font,wbase,value='         n(x,y,z)=n0*nr(x,y)*nz(z(s))')   
        wnr=cw_field(wNdistribution,/string,value=self.nr_th,title='nr=',/return,xsize=73,uname=prefix+'nr_th',font=font,fieldfont=font,/frame)
        wnh=cw_field(wNdistribution,/string,value=self.nz_th,title='nh=',/return,xsize=73,uname=prefix+'nz_th',font=font,fieldfont=font,/frame)
        g=widget_info(wnh,/geometry)
        xsize=fix(700*xscale)
        ysize=fix(300*xscale)
        xtextsize=18
        wDraw=widget_draw(wNdistribution,xsize=xsize,ysize=ysize,uname=prefix+'draw_th')  
        wn_chromo=widget_base(wNDistribution,/row,/frame)
        wlabel=widget_label(wn_chromo,font=font,value='Chromo Volume: ')
        wn_th_total_chromo=cw_objfield(wn_chromo, font=font,UNAME=prefix+'n0_total_chromo', LABEL='Sum(n0dv)=',$
          INCREMENT=1e7, $
          UNITS='', $
          xtextsize=xtextsize,$
          VALUE=0,/frame,/indicator) 
        wn2_th_total_chromo=cw_objfield(wn_chromo,font=font, UNAME=prefix+'n0^2_total_chromo', LABEL='Sum(n0^2dv)=',$
          INCREMENT=1e7, $
          UNITS='cm^-3', $
          xtextsize=xtextsize,$
          VALUE=0,/frame,/indicator)  
        wn_corona=widget_base(wNDistribution,/row,/frame)
        wlabel=widget_label(wn_corona,font=font,value='Coronal Volume:') 
        wn_th_total_corona=cw_objfield(wn_corona, font=font,UNAME=prefix+'n0_total_corona', LABEL='Sum(n0dv)=',$
          INCREMENT=1e7, $
          UNITS='', $
          xtextsize=xtextsize,$
          VALUE=0,/frame,/indicator)   
        wn2_th_total_corona=cw_objfield(wn_corona,font=font, UNAME=prefix+'n0^2_total_corona', LABEL='Sum(n0^2dv)=',$
          INCREMENT=1e7, $
          UNITS='cm^-3', $
          xtextsize=xtextsize,$
          VALUE=0,/frame,/indicator)  
        wn_total=widget_base(wNDistribution,/row,/frame)
        wlabel=widget_label(wn_total,font=font,value='Total Volume:  ')
        wn_th_total=cw_objfield(wn_total, font=font,UNAME=prefix+'n0_total', LABEL='Sum(n0dv)=',$
          INCREMENT=1e7, $
          UNITS='', $
          xtextsize=xtextsize,$
          VALUE=0,/frame,/indicator)
        wn2_th_total=cw_objfield(wn_total,font=font, UNAME=prefix+'n0^2_total', LABEL='Sum(n0^2dv)=',$
          INCREMENT=1e7, $
          UNITS='cm^-3', $
          xtextsize=xtextsize,$
          VALUE=0,/frame,/indicator)  
        self->Update_N_th
     endif else begin
      
       self.base->GetVertexAttributeData,'N_IDX',n_idx
       self.centerline->GetProperty,parent=model
       sz=model->Size()
       vol=fltarr(sz[1],sz[2],sz[3])
       volume=model->GetVolume()
       volume->GetVertexAttributeData,'idx',idx
       volume->GetVertexAttributeData,'bmed',b
       vol[*]=0
       vol[idx]=b
       b=vol[n_idx]
       volume->GetVertexAttributeData,'alpha',alpha
       vol[*]=0
       vol[idx]=alpha
       alpha=vol[n_idx]
       volume->GetVertexAttributeData,'length',l
       vol[*]=0
       vol[idx]=l
       l=vol[n_idx]
       l=gx_rsun()*l/2
       closed=where(l ne 0 and b ne 0 and alpha ne 0)
       l=l[closed]
       b=b[closed]
       alpha=alpha[closed]
       
       self.base->GetVertexAttributeData,'q0_coeff',q_coeff
       if n_elements(q) eq 0 then begin
        volume->GetVertexAttributeData,'q0_coeff',q_coeff
        self.base->SetVertexAttributeData,'q0_coeff',q_coeff
       endif
       
       self.base->GetVertexAttributeData,'q0_formula',q0_formula
       if n_elements(q0_formula) eq 0 then begin
         volume->GetVertexAttributeData,'q0_formula',q0_formula
         self.base->SetVertexAttributeData,'q0_formula',q0_formula
       endif
       q0_formula=string(q0_formula)
       
       self.base->GetVertexAttributeData,'Q0',q0
       if n_elements(q0) eq 0 then begin
         result=execute('q0='+q0_formula)
         self.base->SetVertexAttributeData,'Q0',q0
       endif
       
       self.base->GetVertexAttributeData,'q_formula',q_formula
       if n_elements(q_formula) eq 0 then begin
         volume->GetVertexAttributeData,'q_formula',q_formula
         self.base->SetVertexAttributeData,'q_formula',q_formula
       endif
       q_formula=string(q_formula)
       
       self.base->GetVertexAttributeData,'Q',q
       if n_elements(q0) eq 0 then begin
         result=execute('q='+q_formula)
         self.base->SetVertexAttributeData,'Q',q
       endif

       wbase=widget_base(wThermalTab,UNAME =prefix+ 'DEM',/column)
       wParmBase=widget_base(wBase,/column,uname=prefix+'q_formula_base')
       wqBase=widget_base(wParmBase,/row,/frame)
       wq=cw_objArray( wqBase,uname=prefix+'q',xtextsize=5,format='(g0)',units='',value=q_coeff,label='q',lfont=font,/frame)
       wqreset=widget_button(font=font,font=font, wqBase,value='Reset to default',uname=prefix+'q_reset')

       wq0FormulaBase=widget_base(wParmBase,/row,/frame)
       label=widget_label(font=font,wq0FormulaBase,value='     q0=  ')
       g=widget_info(wq,/geo)
       gl=widget_info(label,/geo)
       wq0f=widget_text(font=font,wq0FormulaBase,value=q0_formula,scr_xsize=g.scr_xsize-gl.scr_xsize,/edit,uname=prefix+'q0_formula')
       wq0freset=widget_button(font=font,wq0FormulaBase,value='Reset to default',uname=prefix+'q0_formula_reset')

       wqFormulaBase=widget_base(wParmBase,/row,/frame)
       label=widget_label(font=font,wqFormulaBase,value='     Q=  ',scr_xsize=gl.scr_xsize)
       wqf=widget_text(font=font,wqFormulaBase,value=q_formula,scr_xsize=g.scr_xsize-gl.scr_xsize,/edit,uname=prefix+'q_formula')
       wqfreset=widget_button(font=font,wqFormulaBase,value='Reset to default',uname=prefix+'q_formula_reset')
       
       wPlotBase=widget_base(wbase,/row,uname=prefix+'ATTRIBUTEPLOTBASE')
       xsize=fix(450*xscale)
       ysize=fix(300*xscale)
       wAttributeplot=widget_draw( wPlotBase, $
         xsize=xsize, $
         ysize=ysize, $
         retain=2, $
         uvalue=[xSize,ySize], $
         uname=prefix+'AttributePlot')
       wOptionBase=widget_base(wPlotBase,/column)
       wAttributes=['Bx','By','Bz','B']
       xselect=4
       yselect=5
       if n_elements(l) gt 0 or n_elements(n) gt 0 then wAttributes=[wAttributes,'Thermal Electron Density (cm^-3)','Temperature (K)']
       if n_elements(l) gt 0 then wAttributes=[wAttributes,'Closed Loops Length (cm)','Closed Loops Heating Rate (Q)']
       if n_elements(alpha) gt 0 then wAttributes=[wAttributes,'alpha']
       if n_elements(q0) gt 0 then wAttributes=[wAttributes,'q0']
       label=widget_label(font=font,wOptionBase,value='X Axis:',/align_left)
       wXAttribute=WIDGET_DROPLIST(wOptionBase,value=wAttributes,uname=prefix+'xAttribute')
       label=widget_label(font=font,wOptionBase,value='Y Axis:',/align_left)
       wYAttribute=WIDGET_DROPLIST(wOptionBase,value=wAttributes,uname=prefix+'yAttribute')
       wCheckBase=widget_base(wOptionBase,/column,/non)
       wRotateXY=widget_button(font=font,wCheckBase,value='Rotate XY',uname=prefix+'RotateXY')
       wHistogram=widget_button(font=font,wCheckBase,value='X Histogram',uname=prefix+'XHistogram')
       if n_elements(l) eq 0 then xselect=4 else xselect=6
       if n_elements(l) eq 0 then yselect=5 else yselect=7
       if n_elements(wAttributes) le 4 then widget_control, wHistogram,/set_button
       widget_control,wXattribute,SET_DROPLIST_SELECT=xselect
       widget_control,wYattribute,SET_DROPLIST_SELECT=yselect
       wPlotOptions=cw_objPlotOptions(wOptionBase,uname=prefix+'AttributePlotOptions',/ylog,/xlog)
     endelse
END
  
PRO gxFluxTube::Update_N_th,no_volume_update=no_volume_update
  if ~keyword_set(no_volume_update) then self->RequestVolumeUpdate, /newID
  widget_control,/hourglass
  error=execute('p='+self.p_th)
  error=execute('q='+self.q_th)
  self.centerline->GetProperty,XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV,data=line
  void=self.Parent->GetB(Bx=Bx)
  self.centerline->getvertexattributedata,'s',ss
  l=abs(ss[0]-ss[n_elements(ss)-1])
  s0=l*self.s0
  self.base->GetVertexAttributeData,'C_IDX',c
  self.base->GetVertexAttributeData,'N_IDX',n_idx
  self.base->GetVertexAttributeData,'ex',ex
  self.base->GetVertexAttributeData,'ey',ey
  b2b0=self->b2b0(c)
  s=ss[c];added January 25 2016
  a=self.a/sqrt(b2b0)
  b=self.b/sqrt(b2b0)
  idx=array_indices(Bx,n_idx)
  rr=[line[0,c]-idx[0,*],line[1,c]-idx[1,*],line[2,c]-idx[2,*]]
  x=fltarr(n_elements(c))
  y=fltarr(n_elements(c))
  for i=0,n_elements(c)-1 do begin
    x[i]=dot(rr[*,i],ex[*,c[i]])
    y[i]=dot(rr[*,i],ey[*,c[i]])
  end
  success=execute('nr_th='+self.nr_th)
  if ~success then begin
    answ=dialog_message('Invalid radial distribution:'+STRING(10b)+self.nr_th+STRING(10b)+'Check syntax!',/error)
    return
  end
  T0=self.T0
  n0=self.n_th
  z=ZCOORD_CONV[1]*reform(line[2,c])
  R=1
  success=execute('nz_th='+self.nz_th)
  if ~success then begin
    answ=dialog_message('Invalid height distribution:'+STRING(10b)+self.nz_th+STRING(10b)+'Check syntax!',/error)
    return
  end
  n_th=self.n_th*nr_th*nz_th
  self.base->SetVertexAttributeData,'n_th',n_th
END

PRO gxFluxTube::Draw_N_th
  if ~widget_valid(self.wparent) then return
  p=self.p_th
  q=self.q_th
  rad=self.nr_th
  long=self.nz_th
  a=self.a
  b=self.b
  self.centerline->getvertexattributedata,'s',ss
  l=abs(ss[0]-ss[n_elements(ss)-1])
  s0=l*self.s0
  self.centerline->GetProperty,XCOORD_CONV=XCOORD_CONV,YCOORD_CONV=YCOORD_CONV,ZCOORD_CONV=ZCOORD_CONV,data=line
  wdraw=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:draw_th')
  widget_control,wdraw,get_value=window
  wset,window
  tvlct,rgb_curr,/get
  pmulti=!p.multi
  loadct,39
  erase,window
  !p.multi=[0,2,1]
  nx=30
  self.base->GetVertexAttributeData,'cutoff',rho
  if n_elements(rho) eq 0 then rho=5*max([a,b])
  error=execute('p='+self.p_th)
  error=execute('q='+self.q_th)
  axis=[rho*(findgen(nx)/(nx-1))-rho/2,0]
  axis=axis[sort(axis)]
  x=axis
  y=0
  success=execute('nx='+self.nr_th)
  if ~success then begin
    answ=dialog_message('Invalid radial distribution:'+STRING(10b)+self.nr_th+STRING(10b)+'Check syntax!',/error)
    return
  end
  y=axis
  x=0
  success=execute('ny='+self.nr_th)
  if ~success then begin
    answ=dialog_message('Invalid radial distribution:'+STRING(10b)+self.nr_nth+STRING(10b)+'Check syntax!',/error)
    return
  end
  self->GetProperty,all=all,parent=parent
  sz=parent->Size()
  z=ZCOORD_CONV[1]*reform(line[2,*])
  R=1
  T0=self.T0
  s=ss
  success=execute('nz_th='+self.nz_th)
  if ~success then begin
    answ=dialog_message('Invalid longitudinal distribution:'+STRING(10b)+self.nz_th+STRING(10b)+'Check syntax!',/error)
    return
  end
  p={nx:nx,ny:ny,nz:nz_th,x:axis/a,y:axis/b,z:z,s:ss/l}
  xmargin=[8,3]
  xticks=3
  plot,p.x,p.nx,xmargin=xmargin,xticks=xticks,back=255,/nodata,color=0,/xsty,/ysty,xtitle='x/a; y/b'
  oplot,p.x,p.nx,color=50,thick=3,linesty=1
  gx_plot_label,0.1,0.8,'n!Dr!N(x,0)',color=50,charthick=2
  oplot,p.y,p.ny,color=250,thick=3,linesty=2
  gx_plot_label,0.1,0.7,'n!Dr!N(0,y)',color=250,charthick=2
  plot,p.s,p.nz*self.n_th,xmargin=xmargin,xticks=xticks,color=0,/xsty,xtitle='s/l',thick=3,/ysty
  gx_plot_label,0.1,0.8,'n!Ds!N',color=0,charthick=2
  !p.multi=pmulti
  tvlct,rgb_curr
END

PRO gxFluxTube::RequestVolumeUpdate,_extra=_extra
 if obj_isa(self.parent,'gxModel') then self.parent->RequestVolumeUpdate,_extra=_extra
END

PRO gxFluxTube::UpdateAll
 widget_control,/hourglass
 self->Update_N_th
 self->Update_n_nth
 self->Update_Theta_c,c_idx,B2B0=B2B0
 self->Update_Theta_b,c_idx,B2B0=B2B0
 self->Update_dMu,c_idx,B2B0=B2B0
 self->Update_a4,c_idx,B2B0=B2B0
 self->UpdateDisplays,/all
END

PRO gxFluxTube::Compute_EM,key,n_total,n2_total
 default,key,'n_0'
 self.base->GetVertexAttributeData,'owned',owned
 volume=(self.parent->GetVolume())
 volume->Update,key,data=n,/update,/getdata,/chromo_view
 n=n[owned]
 voxelid=self.parent->GetVoxelId()
 voxelid=voxelid[owned]
 corona_idx=where(voxelid and gx_voxelid(/corona),comp=chromo_idx)
 corona_n=n[corona_idx]
 chromo_n=n[chromo_idx]
 self.parent->GetProperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz
 dv=dx[1]*dy[1]*dz[1]*((gx_rsun())^3)
 n_total=[total(chromo_n,/double,/nan)*dv,total(corona_n,/double,/nan)*dv]
 n2_total=[total(chromo_n^2,/double,/nan)*dv,total(corona_n^2,/double,/nan)*dv]
 print,'Integral['+key+'dv]=',n_total
 print,'Integral['+key+'^2dv]=',n2_total
end

PRO gxFluxTube::Display_EM
  if ~widget_valid(self.wparent) then return
  self.base->GetVertexAttributeData,'owned',owned
  self.base->GetVertexAttributeData,'N_IDX',n_idx
  self.base->GetVertexAttributeData,'n_nth',n_nth
  n_nth=n_nth+self->get_nb_arr()
  volume=(self.parent->GetVolume())
  volume->GetVertexAttributeData,'n0',n0
  key='n_0'
  n=n0[owned]
  voxelid=self.parent->GetVoxelId()
  voxelid=voxelid[owned]
  corona_idx=where(voxelid and gx_voxelid(/corona),comp=chromo_idx)
  corona_n=n[corona_idx]
  chromo_n=n[chromo_idx]
  self.parent->GetProperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz
  dv=dx[1]*dy[1]*dz[1]*((gx_rsun())^3)
  n_total=[total(chromo_n,/double,/nan)*dv,total(corona_n,/double,/nan)*dv]
  n2_total=[total(chromo_n^2,/double,/nan)*dv,total(corona_n^2,/double,/nan)*dv]
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:n0_total_chromo')
  if widget_valid(wid) then widget_control,wid,set_value=n_total[0]
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:n0_total_corona')
  if widget_valid(wid) then widget_control,wid,set_value=n_total[1]
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:n0^2_total_chromo')
  if widget_valid(wid) then widget_control,wid,set_value=n2_total[0]
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:n0^2_total_corona')
  if widget_valid(wid) then widget_control,wid,set_value=n2_total[1]
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:n0_total')
  if widget_valid(wid) then widget_control,wid,set_value=total(n_total,/double)
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:n0^2_total')
  if widget_valid(wid) then widget_control,wid,set_value=total(n2_total,/double)
  key='n_b'
  n0[*]=0
  n0[n_idx]=n_nth
  n=n0[owned]
  voxelid=self.parent->GetVoxelId()
  voxelid=voxelid[owned]
  corona_idx=where(voxelid and gx_voxelid(/corona),comp=chromo_idx)
  corona_n=n[corona_idx]
  chromo_n=n[chromo_idx]
  self.parent->GetProperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz
  dv=dx[1]*dy[1]*dz[1]*((gx_rsun())^3)
  n_total=[total(chromo_n,/double,/nan)*dv,total(corona_n,/double,/nan)*dv]
  n2_total=[total(chromo_n^2,/double,/nan)*dv,total(corona_n^2,/double,/nan)*dv]
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:nb_total_chromo')
  if widget_valid(wid) then widget_control,wid,set_value=n_total[0]
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:nb_total_corona')
  if widget_valid(wid) then widget_control,wid,set_value=n_total[1]
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:nb^2_total_chromo')
  if widget_valid(wid) then widget_control,wid,set_value=n2_total[0]
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:nb^2_total_corona')
  if widget_valid(wid) then widget_control,wid,set_value=n2_total[1]
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:nb_total')
  if widget_valid(wid) then widget_control,wid,set_value=total(n_total,/double)
  wid=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:nb^2_total')
  if widget_valid(wid) then widget_control,wid,set_value=total(n2_total,/double)
end

function gxFluxtube::spine_n_nth,s=s,l=l,onepervox=onepervox
  error=execute('p='+self.p_nth)
  error=execute('q='+self.q_nth)
  self.centerline->GetProperty,data=line
  self.centerline->getvertexattributedata,'s',s
  l=delta(s)
  s0=self.s0
  success=execute('ns_nth='+self.ns_nth)
  n_nth=self.n_nth*ns_nth
  if keyword_set(onepervox) then begin
    sz=(self.parent)->Size()
    vol=bytarr(sz[1],sz[2],sz[3])
    vol[line[0,*],line[1,*],line[2,*]]=1
    idx=where(vol eq 1)
    vol=float(vol)
    vol[line[0,*],line[1,*],line[2,*]]=n_nth
    n_nth=n_nth(idx)
    vol[line[0,*],line[1,*],line[2,*]]=s
    s=s[idx]
  endif
  return,n_nth
end

function gxFluxtube::spine_n_nth,s=s,l=l,onepervox=onepervox
  error=execute('p='+self.p_nth)
  error=execute('q='+self.q_nth)
  self.centerline->GetProperty,data=line
  self.centerline->getvertexattributedata,'s',s
  l=delta(s)
  s0=self.s0
  success=execute('ns_nth='+self.ns_nth)
  n_nth=self.n_nth*ns_nth
  if keyword_set(onepervox) then begin
    sz=(self.parent)->Size()
    vol=bytarr(sz[1],sz[2],sz[3])
    vol[line[0,*],line[1,*],line[2,*]]=1
    idx=where(vol eq 1)
    vol=float(vol)
    vol[line[0,*],line[1,*],line[2,*]]=n_nth
    n_nth=vol(idx)
    vol[line[0,*],line[1,*],line[2,*]]=s
    s=vol[idx]
    idx=sort(s)
    n_nth=n_nth[idx]
    s=s[idx]
  endif
  return,n_nth
end

function gxFluxtube::spine_b,s=s,l=l,onepervox=onepervox
  self.centerline->GetProperty,data=line
  self.centerline->getvertexattributedata,'B',b
  b=sqrt(total(b^2,1))
  self.centerline->getvertexattributedata,'s',s
  l=delta(s)

  if keyword_set(onepervox) then begin
    sz=(self.parent)->Size()
    vol=bytarr(sz[1],sz[2],sz[3])
    vol[line[0,*],line[1,*],line[2,*]]=1
    idx=where(vol eq 1)
    vol=float(vol)
    vol[line[0,*],line[1,*],line[2,*]]=b
    b=vol(idx)
    vol[line[0,*],line[1,*],line[2,*]]=s
    s=vol[idx]
    idx=sort(s)
    b=b[idx]
    s=s[idx]
  endif
  return,b
end

function gxFluxTube::spine_n_th,s=s,l=l,onepervox=onepervox
  error=execute('p='+self.p_th)
  error=execute('q='+self.q_th)
  self.centerline->GetProperty,ZCOORD_CONV=ZCOORD_CONV,data=line
  self.centerline->getvertexattributedata,'s',s
  l=delta(s)
  s0=l*self.s0
  T0=self.T0
  n0=self.n_th
  z=ZCOORD_CONV[1]*reform(line[2,*])
  R=1
  success=execute('nz_th='+self.nz_th)
  n_th=self.n_th*nz_th
  if keyword_set(onepervox) then begin
    sz=(self.parent)->Size()
    vol=bytarr(sz[1],sz[2],sz[3])
    vol[line[0,*],line[1,*],line[2,*]]=1
    idx=where(vol eq 1)
    vol=float(vol)
    vol[line[0,*],line[1,*],line[2,*]]=n_th
    n_th=vol(idx)
    vol[line[0,*],line[1,*],line[2,*]]=s
    s=vol[idx]
    idx=sort(s)
    n_th=n_th[idx]
    s=s[idx]
  endif
  return,n_th
end

PRO gxFluxTube::Update_N_nth
 self->RequestVolumeUpdate, condition='n_b'
 widget_control,/hourglass
 error=execute('p='+self.p_nth) 
 error=execute('q='+self.q_nth)

 self.centerline->GetProperty,data=line
 void=self.Parent->GetB(Bx=Bx)
 
 self.centerline->GetVertexAttributeData,'s',ss
 self.base->GetVertexAttributeData,'C_IDX',c
 self.base->GetVertexAttributeData,'N_IDX',n_idx
 self.base->GetVertexAttributeData,'ex',ex
 self.base->GetVertexAttributeData,'ey',ey
 
 b2b0=self->b2b0(c)
 a=self.a/sqrt(b2b0)
 b=self.b/sqrt(b2b0)
 
 idx=array_indices(Bx,n_idx)
 r=[line[0,c]-idx[0,*],line[1,c]-idx[1,*],line[2,c]-idx[2,*]]
 x=fltarr(n_elements(c))
 y=fltarr(n_elements(c))
 for i=0,n_elements(c)-1 do begin
  x[i]=dot(r[*,i],ex[*,c[i]])
  y[i]=dot(r[*,i],ey[*,c[i]])
 end
 l=abs(ss[0]-ss[n_elements(ss)-1])
 s0=self.s0
 s=ss[c]
 success=execute('nr_nth='+self.nr_nth)
 if ~success then begin
  answ=dialog_message('Invalid radial distribution:'+STRING(10b)+self.nr_nth+STRING(10b)+'Check syntax!',/error)
  return
 end 
 success=execute('ns_nth='+self.ns_nth)
 if ~success then begin
  answ=dialog_message('Invalid radial distribution:'+STRING(10b)+self.nr_nth+STRING(10b)+'Check syntax!',/error)
  return
 end 
 n_nth=self.n_nth*nr_nth*ns_nth
 self.base->SetVertexAttributeData,'n_nth',n_nth
 self.base->SetVertexAttributeData,'nr_nth',nr_nth
END

Function gxFluxTube::get_nb_arr,s_arr=s_arr,ana=ana
 self.base->GetVertexAttributeData,'C_IDX',c_idx
 self.base->GetVertexAttributeData,'nr_nth',nr_nth
 if keyword_set(ana) then begin
   error=execute('p='+self.p_nth)
   error=execute('q='+self.q_nth)
   self.centerline->GetVertexAttributeData,'s',s
   l=delta(s)
   s0=self.s0
   s=s[c_idx]
   success=execute('ns_nth='+self.ns_nth)
   nb=nr_nth*ns_nth*self.n_nth
 endif else begin
   nb_arr=(self->integrate_f_arr(/pa,/energy,s_arr=s_arr))
   if ~isa(nb_arr) then return,0
     nb=nr_nth*nb_arr[c_idx,self.ftime_idx]
   endelse
 return,nb
End


pro gxFluxtube::UpdateDisplays,n_th=n_th,n_nth=n_nth,energy=energy,mu=mu,b2b0=b2B0,em=em,all=all
  self->SynchronizeDuplicateFields
  if keyword_set(B2B0) or keyword_set(all) then self->DisplayB2B0ratio
  if keyword_set(n_th) or keyword_set(all) then self->Draw_N_TH
  if keyword_set(n_nth) or keyword_set(all) then self->Draw_N_NTH
  if keyword_set(mu) or keyword_set(all) then self->UpdatePADistribution
  if keyword_set(energy) or keyword_set(n_nth) or keyword_set(all) then self->UpdateEnergyDistribution
  if keyword_set(em) or keyword_set(n_nth) or keyword_set(all) then self->Display_EM
end

pro gxFluxtube::SynchronizeDuplicateFields
 if ~widget_valid(self.wparent) then return
  base_arr='GXFLUXTUBE:'+['Gbase','THbase','NTHbase','PAbase','Ebase']
  self->GetProperty,fparms=fparms,n_th=n_th,T0=T0,n_nth=n_nth
  if isa(fparms) then begin
    fsz=size(fparms.f_arr)
    max_ftime_idx=fsz[0] ge 4? fsz[4]-1:0
    time_value=string(self.ftime_idx,fparms.t_arr[self.ftime_idx],format="('f_arr_time[',i0,']=',f0.2,' s')")
  endif else begin
    max_ftime_idx=0
    self.ftime_idx=0
    time_value='0'
    self.nb_arr=0
  endelse
    self.centerline->GetVertexAttributeData,'s',s
    l=delta(s)
    m=min(abs(s-self.s0),s0_idx)
    self.s0=s[s0_idx]
  for k=0,n_elements(base_arr)-1 do begin
   base=widget_info(self.wparent,find_by_uname=base_arr[k])
   if widget_valid(base) then begin
   
     ws02l=widget_info(base,find_by_uname='GXFLUXTUBE:s0/l')
     if widget_valid(ws02l) then widget_control,ws02l,set_value=self.s0/l
     
     ws0value=widget_info(base,find_by_uname='GXFLUXTUBE:s0value')
     if widget_valid(ws0value) then widget_control,ws0value,set_value=string(s0_idx,self.s0/l,format="('s0[',i0,']/l=',g0)")
     
     ws0=widget_info(base,find_by_uname='GXFLUXTUBE:s0')
     if widget_valid(ws0) then widget_control,ws0,set_value=s0_idx
     
     wsvalue=widget_info(base,find_by_uname='GXFLUXTUBE:svalue')
     if widget_valid(wsvalue) then  widget_control,wsvalue,set_value=string(self.spine_idx,s[self.spine_idx]/l,format="('s[',i0,']/l=',g0)")
     
     ws=widget_info(base,find_by_uname='GXFLUXTUBE:s')
     if widget_valid(ws) then widget_control,ws,set_value=self.spine_idx
     
     wftime_idx=widget_info(base,find_by_uname='GXFLUXTUBE:FTIME_IDX')
     if widget_valid(wftime_idx) then  widget_control,wftime_idx,set_slider_max=max_ftime_idx,set_value=self.ftime_idx,sensitive=1
     
     wftime=widget_info(base,find_by_uname='GXFLUXTUBE:FTIME')
     if widget_valid(wftime) then widget_control,wftime,set_value=time_value
     
     wnb_arr=widget_info(base,find_by_uname='GXFLUXTUBE:nb_arr')
     if widget_valid(wnb_arr) then widget_control,wnb_arr,set_value=self.nb_arr
     
     wn_th=widget_info(base,find_by_uname='GXFLUXTUBE:n_th')
     if widget_valid(wn_th) then widget_control,wn_th,set_value=self.n_th
     
     wn_nth=widget_info(base,find_by_uname='GXFLUXTUBE:n_nth')
     if widget_valid(wn_nth) then widget_control,wn_nth,set_value=self.n_nth
     
     wT0=widget_info(base,find_by_uname='GXFLUXTUBE:T0')
     if widget_valid(wT0) then widget_control,wT0,set_value=self.T0
     
   end
  end
end

pro gxFluxtube::Draw_N_NTH
  if ~widget_valid(self.wparent) then return
  wdraw=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:draw_nth')
  a=self.a
  b=self.b
  p=self.p_nth
  q=self.q_nth
  rad=self.nr_nth
  long=self.ns_nth
  s0=self.s0
  widget_control,wdraw,get_value=window
  wset,window
  tvlct,rgb_curr,/get
  pmulti=!p.multi
  loadct,39
  erase,window
  !p.multi=[0,2,1]
  nx=30
  self.centerline->GetVertexAttributedata,'s',s
  self.centerline->GetProperty,XCOORD_CONV=XCOORD_CONV
  self.base->GetVertexAttributeData,'cutoff',rho
  if n_elements(rho) eq 0 then rho=5*max([a,b])
  error=execute('p='+self.p_nth)
  error=execute('q='+self.q_nth)
  axis=[rho*(findgen(nx)/(nx-1))-rho/2,0]
  axis=axis[sort(axis)]
  x=axis
  y=0
  success=execute('nx='+self.nr_nth)
  if ~success then begin
    answ=dialog_message('Invalid radial distribution:'+STRING(10b)+self.nr_nth+STRING(10b)+'Check syntax!',/error)
    return
  end
  y=axis
  x=0
  success=execute('ny='+self.nr_nth)
  if ~success then begin
    answ=dialog_message('Invalid radial distribution:'+STRING(10b)+self.nr_nth+STRING(10b)+'Check syntax!',/error)
    return
  end
  l=delta(s)
  x=0
  y=0
  success=execute('ns_nth='+self.ns_nth)
  if ~success then begin
    answ=dialog_message('Invalid longitudinal distribution:'+STRING(10b)+self.ns_nth+STRING(10b)+'Check syntax!',/error)
    return
  end
  p={nx:nx,ny:ny,ns:ns_nth,x:axis/a,y:axis/b,s:s/l}
  xticks=3
  plot,p.x,p.nx,xmargin=[8,1],xticks=xticks,back=255,/nodata,color=0,/xsty,/ysty,xtitle='x/a; y/b',ytitle='n!Dr(x,y)',yrange=[0,1]
  oplot,p.x,p.nx,color=50,thick=4,linesty=1
  gx_plot_label,0.09,0.8,'n!Dr!N(x,0)',color=50,charthick=3,charsize=2
  oplot,p.y,p.ny,color=250,thick=4,linesty=2
  gx_plot_label,0.09,0.6,'n!Dr!N(0,y)',color=250,charthick=3,charsize=2
  
  norm=1/(4*!dpi)*(self.delta1-1)/(self.emin^(1-self.delta1)-self.emax^(1-self.delta1))*self.emin^(-self.delta1)
  ;n_b/(4*pi)*(del-1)/(E_min^(1-del)-E_max^(1-del))*E_min^(-del)
  ;f_arr=self->integrate_f_arr(/pa,/en,s_arr=s_arr)
  f_arr=self->integrate_f_arr(s_arr=s_arr)
  if isa(f_arr) then begin
   f_arr=f_arr[0,0,*,self.ftime_idx]
   plot,p.s,p.ns*self.n_nth*norm,xmargin=[8,1],xticks=xticks,color=0,$
        /xsty,/ysty,xtitle='s/l',ytitle='nb(s)',$
        yrange=minmax(p.ns*self.n_nth*norm+f_arr),thick=2
   oplot,p.s,p.ns*self.n_nth*norm,color=50,thick=3
   oplot,s_arr,f_arr,color=160,thick=3
   oplot,s_arr,p.ns*self.n_nth*norm+f_arr,color=250,thick=3
  endif else begin
    plot,p.s,p.ns*self.n_nth*norm,xmargin=xmargin,xticks=xticks,$
          color=0,/xsty,/ysty,xtitle='s/l',ytitle='nb(s)',thick=2
  endelse
  oplot,[1,1]*s0/l,!y.crange,color=0
  gx_plot_label,0.6,0.9,'n!Dtot!N',color=250,charthick=3,charsize=2
  gx_plot_label,0.6,0.7,'n!Dana!N',color=50,charthick=3,charsize=2
  gx_plot_label,0.6,0.5,'n!Darr!N',color=160,charthick=3,charsize=2
  !p.multi=pmulti
  tvlct,rgb_curr
end

function gxFluxtube::f_arr_norm, distfunc,e_arr,mu_arr
if N_PARAMS() ne 3 then return,0d
 s_arr=self->GetVertexData('s')
 s_arr=s_arr/delta(s_arr)
 f_arr=distfunc
 sz=size(f_arr)
 n_e=sz[1]
 n_mu=sz[2]
 n_s=sz[3]
 n_t=sz[4]
 f_arr_int=dblarr(n_e,n_s,n_t)
 for k=0,n_t-1 do begin
   for j=0,n_s-1 do begin
     for i=0,n_e-1 do begin
       f_arr_int[i,j,k]=2d0*!dpi*int_tabulated(mu_arr, f_arr[i, *,j,k], /double)
     endfor
   endfor
 endfor
 f_arr=temporary(f_arr_int)
 loge=alog(e_arr)
 f_arr_int=dblarr(n_s,n_t)
 for k=0,n_t-1 do begin
   for j=0,n_s-1 do begin
     f_arr_int[j,k]=int_tabulated(loge, f_arr[*,j,k]*e_arr, /double)
   endfor
 endfor
 f_arr=temporary(f_arr_int)
 return,max(f_arr)
end

Function gxFluxTube::integrate_f_arr,pa=pa,energy=energy,e_arr=e_arr,mu_arr=mu_arr,s_arr=s_arr,t_arr=t_arr
  if ~ptr_valid(self.fparms) then return,!null
  s_arr=self->GetVertexData('s')
  s_arr=s_arr/delta(s_arr)
  f_arr=double((*self.fparms).f_arr*self.nb_arr)
  mu_arr=double((*self.fparms).mu_arr)
  n_mu=n_elements(mu_arr)
  e_arr=double((*self.fparms).e_arr)
  n_E=n_elements(e_arr)
  s_arr0=(*self.fparms).s_arr
  n_s=n_elements(s_arr0)
  t_arr=(*self.fparms).t_arr
  n_t=n_elements(t_arr)
  dMu=mu_arr[1]-mu_arr[0]
  if keyword_set(pa) then begin
    ;ap integration
    f_arr_int=dblarr(n_e,n_s,n_t)
    for k=0,n_t-1 do begin
      for j=0,n_s-1 do begin
        for i=0,n_e-1 do begin
          f_arr_int[i,j,k]=2d0*!dpi*int_trapzd(mu_arr, f_arr[i, *,j,k])
        endfor
      endfor 
    endfor
    f_arr=temporary(f_arr_int)
  endif
  
  if keyword_set(energy) then begin
    loge=alog(e_arr)
    logf_arr=alog(f_arr)
    if keyword_set(pa) then begin
      f_arr_int=dblarr(n_s,n_t)
      for k=0,n_t-1 do begin
        for j=0,n_s-1 do begin
          f_arr_int[j,k]=int_trapzdLog(loge, logf_arr[*,j,k])
        endfor
      endfor
    endif else begin
      f_arr_int=dblarr(n_mu,n_s,n_t)
      for k=0,n_t-1 do begin
        for j=0,n_s-1 do begin
          for i=0,n_mu-1 do begin
           f_arr_int[i,j,k]=int_trapzdLog(loge, logf_arr[*,i,j,k])
          endfor
        endfor
      endfor
    endelse
    f_arr=temporary(f_arr_int)
  endif
  
  sz=size(f_arr)
  case sz[0] of
    4:begin
      sz[3]=n_elements(s_arr)
      f_arr_int=dblarr(sz[1:4])
      for i=0,sz[1]-1 do begin
        for j=0,sz[2]-1 do begin
          for k=0,sz[4]-1 do begin
           f_arr_int[i,j,*,k]=interpol(reform(f_arr[i,j,*,k]),s_arr0,s_arr)>0
          end
        endfor
      endfor
    end
    3:begin
        sz[2]=n_elements(s_arr)
        f_arr_int=dblarr(sz[1:3])
        for i=0,sz[1]-1 do begin
          for j=0,sz[3]-1 do begin
            f_arr_int[i,*,j]=interpol(reform(f_arr[i,*,j]),s_arr0,s_arr)>0
          endfor
        endfor
      end
    else: begin
            sz[1]=n_elements(s_arr)
            f_arr_int=dblarr(sz[1:2])
            for i=0,sz[2]-1 do begin
              f_arr_int[*,i]=interpol(reform(f_arr[*,i]),s_arr0,s_arr)>0
            endfor
           end
  endcase
  return,f_arr_int
end


Function gxFluxTube::colapse_f_arr,dimension_,axis=axis
 f_arr=!null
 if ~ptr_valid(self.fparms) then return,f_arr
 dimension=(size(dimension_,/tname) eq 'STRING')?strlowcase(dimension_):'none'
 f_arr=(*self.fparms).f_arr
 mu_arr=(*self.fparms).mu_arr
 e_arr=(*self.fparms).e_arr
 dMu=mu_arr[1]-mu_arr[0]
 loge=alog10(e_arr)
 dloge=loge[1]-loge[0]
 case dimension of
  'spine': begin 
             f_arr=2*!pi*total(f_arr,2)*dMu;mu integration
             axis=(*self.fparms).s_arr
           end  
  'mu': begin 
          f_arr=total(f_arr,3);spine integration
          axis=(*self.fparms).mu_arr
        end        
  else: begin
         f_arr=total(total(f_arr,2),2); mu & spine integration
         axis=(*self.fparms).e_arr
        end 
 endcase
 if dimension ne 'energy' then begin
  loge=alog10((*self.fparms).e_arr)
  dloge=loge[1]-loge[0]
  sz=size(f_arr)
  e_arr=array_replicate((*self.fparms).e_arr,sz[2])
  f_arr=alog(10.)*dloge*total(f_arr*e_arr,1)
 endif
 if (size(reform(f_arr)))[0] eq 0 then axis=!null
 f_arr*=self.nb_arr
 return,f_arr
end


PRO gxFluxTube::Update_Theta_c,c_idx,B2B0=B2B0
 widget_control,/hourglass
 if n_elements(c_idx) eq 0 then self.base->GetVertexAttributeData,'C_IDX',c_idx
 theta_c=self->Theta_c(c_idx,B2B0=B2B0)
 self.base->SetVertexAttributeData,'THETA_C',theta_c
END

PRO gxFluxTube::Update_Theta_b,c_idx,B2B0=B2B0
 widget_control,/hourglass
 if n_elements(c_idx) eq 0 then self.base->GetVertexAttributeData,'C_IDX',c_idx
 theta_b=self->Theta_b(c_idx,B2B0=B2B0)
 self.base->SetVertexAttributeData,'THETA_B',theta_b
END

PRO gxFluxTube::Update_dMu,c_idx,B2B0=B2B0
 widget_control,/hourglass
  if n_elements(c_idx) eq 0 then self.base->GetVertexAttributeData,'C_IDX',c_idx
 dMu=self->dMu(c_idx,B2B0=B2B0)
 self.base->SetVertexAttributeData,'dMu',dMU
END

PRO gxFluxTube::Update_A4,c_idx,B2B0=B2B0
 widget_control,/hourglass
  if n_elements(c_idx) eq 0 then self.base->GetVertexAttributeData,'C_IDX',c_idx
 a4=self->A4(c_idx,B2B0=B2B0)
 self.base->SetVertexAttributeData,'a4',a4
END

PRO gxFluxTube::SelectEnergyDistribution,index
 if n_elements(index) eq 0 then begin
  index=self.dist_e-1
  wEnergySelect=widget_info(self.wParent,Find_By_Uname='GXFLUXTUBE:E_Select')
  widget_control,wEnergySelect,SET_COMBOBOX_SELECT=index
 endif else begin
  self.dist_e=index+1
 end
 EBase=widget_info(self.wParent,Find_By_Uname='GXFLUXTUBE:Ebase')
 widget_control,EBase,Map=0
 ParmBase=widget_info(self.wParent,Find_By_Uname='GXFLUXTUBE:parm_e')
 ParmSubBase=widget_info(ParmBase,/child)
 if widget_valid(ParmSubBase) then widget_control,ParmSubBase,/destroy
 ParmSubBase=widget_base(ParmBase,/column)
 prefix='GXFLUXTUBE:'
 font=!defaults.font
 case self.dist_e of
  1:begin
     ;thermal (FFO)
     goto,thm
    end
  2:begin
      thm:
      ;thermal (THM)
      xtextsize=15
      ;xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_th=cw_objfield(base, UNAME=prefix+'n_th', LABEL='n0=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_th,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wT0=cw_objfield(base, UNAME=prefix+'T0', LABEL='T0=',$
        INCREMENT=1, $
        UNITS='K', $
        VALUE=self.T0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)  
      base=widget_base(ParmSubBase,/row)  
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)  
    end
  11:begin
      ;thermal (THM)
      xtextsize=xtextsize
     ; xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_th=cw_objfield(base, UNAME=prefix+'n_th', LABEL='n0=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_th,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wT0=cw_objfield(base, UNAME=prefix+'T0', LABEL='T0=',$
        INCREMENT=1, $
        UNITS='K', $
        VALUE=self.T0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)  
     ;single power law over kinetic energy (PLW)
     base=widget_base(ParmSubBase,/row)
      wn_nth=cw_objfield(base, UNAME=prefix+'n_nth', LABEL='nb=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_nth,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wDelta1=cw_objfield(base, UNAME=prefix+'Delta1', LABEL='delta=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta1,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)         
      base=widget_base(ParmSubBase,/row)  
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)  
    end 
  12:begin
       ;thermal (THM)
      xtextsize=xtextsize
      ;xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_th=cw_objfield(base, UNAME=prefix+'n_th', LABEL='n0=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_th,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wT0=cw_objfield(base, UNAME=prefix+'T0', LABEL='T0=',$
        INCREMENT=1, $
        UNITS='K', $
        VALUE=self.T0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)  
    ;double power law over kinetic energy (DPL)
      xtextsize=12
      ;xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_nth=cw_objfield(base, UNAME=prefix+'n_nth', LABEL='nb=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_nth,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont) 
      wDelta1=cw_objfield(base, UNAME=prefix+'Delta1', LABEL='delta1=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta1,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wDelta2=cw_objfield(base, UNAME=prefix+'Delta2', LABEL='delta2=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta2,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)   
      base=widget_base(ParmSubBase,/row)  
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)
      wEbreak=cw_objfield(base, UNAME=prefix+'Ebreak', LABEL='Ebreak=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.ebreak,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont) 
    end       
   3:begin  
      ;single power law over kinetic energy (PLW)
      xtextsize=xtextsize
      ;xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_nth=cw_objfield(base, UNAME=prefix+'n_nth', LABEL='nb=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_nth,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wDelta1=cw_objfield(base, UNAME=prefix+'Delta1', LABEL='delta=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta1,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)   
      base=widget_base(ParmSubBase,/row)  
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)  
    end
  4:begin
      ;double power law over kinetic energy (DPL)
      xtextsize=12
;      xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_nth=cw_objfield(base, UNAME=prefix+'n_nth', LABEL='nb=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_nth,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont) 
      wDelta1=cw_objfield(base, UNAME=prefix+'Delta1', LABEL='delta1=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta1,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wDelta2=cw_objfield(base, UNAME=prefix+'Delta2', LABEL='delta2=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta2,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)   
      base=widget_base(ParmSubBase,/row)  
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)
      wEbreak=cw_objfield(base, UNAME=prefix+'Ebreak', LABEL='Ebreak=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.ebreak,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
    end
  5:begin
      ;thermal/nonthermal over kinetic energy (TNT)
      xtextsize=12
     ; xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_th=cw_objfield(base, UNAME=prefix+'n_th', LABEL='n0=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_nth,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont) 
      wT0=cw_objfield(base, UNAME=prefix+'T0', LABEL='T0=',$
        INCREMENT=1, $
        UNITS='K', $
        VALUE=self.T0,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)   
      wDelta=cw_objfield(base, UNAME=prefix+'Delta1', LABEL='delta=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta1,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)     
      base=widget_base(ParmSubBase,/row)  
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wEps=cw_objfield(base, UNAME=prefix+'eps', LABEL='eps=',$
        INCREMENT=1, $
        UNITS='', $
        VALUE=self.eps,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
    end
  6:begin
      ;kappa (KAP)
       xtextsize=12
     ; xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_th=cw_objfield(base, UNAME=prefix+'n_th', LABEL='n0=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_nth,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont) 
      wT0=cw_objfield(base, UNAME=prefix+'T0', LABEL='T0=',$
        INCREMENT=1, $
        UNITS='K', $
        VALUE=self.T0,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)   
      wkappa=cw_objfield(base, UNAME=prefix+'kappa', LABEL='kappa=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.kappa,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)     
      base=widget_base(ParmSubBase,/row)  
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
    end
  7:begin  
      ;power law over the absolute value of momentum (PLP)
      xtextsize=xtextsize
     ; xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_nth=cw_objfield(base, UNAME=prefix+'n_nth', LABEL='nb=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_nth,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wDelta1=cw_objfield(base, UNAME=prefix+'Delta1', LABEL='delta=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta1,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont) 
      base=widget_base(ParmSubBase,/row) 
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)  
    end
  8:begin
      ;power law over the Lorentz factor (PLG)
      xtextsize=xtextsize
      ;xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_nth=cw_objfield(base, UNAME=prefix+'n_nth', LABEL='nb=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_nth,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wDelta1=cw_objfield(base, UNAME=prefix+'Delta1', LABEL='delta=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta1,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont) 
      base=widget_base(ParmSubBase,/row)    
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont) 
    end
  9:begin
      ;thermal/nonthermal over the absolute value of momentum (TNP)
      xtextsize=12
      ;xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_th=cw_objfield(base, UNAME=prefix+'n_th', LABEL='n0=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_nth,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont) 
      wT0=cw_objfield(base, UNAME=prefix+'T0', LABEL='T0=',$
        INCREMENT=1, $
        UNITS='K', $
        VALUE=self.T0,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)   
      wDelta=cw_objfield(base, UNAME=prefix+'Delta1', LABEL='delta=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta1,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)     
      base=widget_base(ParmSubBase,/row)  
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wEps=cw_objfield(base, UNAME=prefix+'eps', LABEL='eps=',$
        INCREMENT=1, $
        UNITS='', $
        VALUE=self.eps,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
    end
 10:begin
      ;thermal/nonthermal over the Lorentz factor (TNG).
      xtextsize=12
      ;xlabelsize=50
      ;lfont=font
      base=widget_base(ParmSubBase,/row)
      wn_th=cw_objfield(base, UNAME=prefix+'n_th', LABEL='n0=',$
        INCREMENT=1e7, $
        UNITS='cm^-3', $
        VALUE=self.n_nth,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont) 
      wT0=cw_objfield(base, UNAME=prefix+'T0', LABEL='T0=',$
        INCREMENT=1, $
        UNITS='K', $
        VALUE=self.T0,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)   
      wDelta=cw_objfield(base, UNAME=prefix+'Delta1', LABEL='delta=',$
        INCREMENT=0.1, $
        UNITS='', $
        VALUE=self.delta1,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)     
      base=widget_base(ParmSubBase,/row)  
      wEmin=cw_objfield(base, UNAME=prefix+'Emin', LABEL='Emin=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emin,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wEmax=cw_objfield(base, UNAME=prefix+'Emax', LABEL='Emax=',$
        INCREMENT=1, $
        UNITS='MeV', $
        VALUE=self.Emax,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
      wEps=cw_objfield(base, UNAME=prefix+'eps', LABEL='eps=',$
        INCREMENT=1, $
        UNITS='', $
        VALUE=self.eps,map=1,/frame,XTEXTSIZE=2+XTEXTSIZE/2,xlabelsize=xlabelsize,lfont=lfont)  
    end  
 else:
 endcase
 widget_control,EBase,Map=1
END

FUNCTION gxFluxTube::THM,E
 ;thermal (THM) 
 nth=self.n_th
 T=self.T0
 mc2=0.511 ;(MeV)
 kb=8.6174e-11 ;(MeV K-1)
 theta=kb*T/mc2
 gamma=E/mc2+1
 exp_arg=Alog(nth*(2/!pi)*gamma*sqrt(gamma^2-1)*(1/theta)*(1/Beselk(1/theta,2,/double)))-gamma/theta
 N_rel=exp(exp_arg)
 N_nonrel=nth*(2/!pi)*sqrt(gamma-1)*theta^(-1.5)*exp(-(gamma-1)/theta)
 return,(T gt 5e7)?N_rel:N_nonrel
END

FUNCTION gxFluxTube::PLW,E
 ;single power law over kinetic energy (PLW)
 nb=self.n_nth 
 delta=self.delta1
 E1=self.Emin
 E2=self.Emax
 return,nb*(delta-1)/(E1^(1-delta)-E2^(1-delta))*(E^(-delta))
END


FUNCTION gxFluxTube::DPL,E
 ;double power law over kinetic energy (DPL) 
 nb=self.n_nth
 delta1=self.delta1
 delta2=self.delta2
 E1=self.Emin
 E2=self.Emax 
 Ebr=self.ebreak
 A=nb*(delta1-1)/(E1^(1-delta1)-E2^(1-delta1))
 N=dindgen(n_elements(E))
 range1=where(E lt Ebr,count,comp=range2,ncomp=ncomp)
 if count gt 0 then N[range1]=A*E[range1]^(-delta1)
 if ncomp gt 0 then N[range2]=A*(Ebr^(delta2-delta1))*E[range2]^(-delta2)
 return,N
END

FUNCTION gxFluxTube::TNT,E
 ;thermal/nonthermal over kinetic energy (TNT)
 T=self.T0
 nth=self.n_th
 mc2=0.511 ;(MeV)
 eps=self.eps
 delta=self.delta1
 kb=8.6174e-11 ;(MeV K-1)
 theta=kb*T/mc2
 pth2=theta*(2.+ theta)       ;thermal momentum^2
 gamcr=sqrt(1.+pth2/eps)      ;Critical Lorenz-factor
 Ecr=(gamcr-1)*mc2            ;corresponding energy in MeV
 A=self->THM(Ecr)*Ecr^(delta)
 N=dblarr(n_elements(E))
 range1=where(E lt Ecr,count,comp=range2,ncomp=ncomp)
 if count gt 0 then N[range1]=self->THM(E[range1])
 if ncomp gt 0 then N[range2]=A*(E[range2]^(-delta))
 return,N
END

FUNCTION gxFluxTube::KAP,E
 ;kappa (KAP) 
 nth=self.n_th
 T=self.T0
 mc2=0.511 ;(MeV)
 kb=8.6174e-11 ;(MeV K-1)
 theta=kb*T/mc2
 gamma=E/mc2+1
 kappa=self.kappa
 N=gamma*sqrt(gamma^2-1)*(theta^(-1.5))*((1+(gamma-1)/(kappa-1.5)/theta)^(-kappa-1))
 N=nth*N/total(N)/(2*!pi)
 return,N
END

FUNCTION gxFluxtube::PLP,E
 ;power law over the absolute value of momentum (PLP)
 mc2=0.511
 nb=self.n_nth
 E1=self.Emin/mc2
 E2=self.Emax/mc2
 delta=self.delta1
 pmin=sqrt(E1*(2+E1))
 pmax=sqrt(E2*(2+E2))
 p=sqrt((E/mc2)*(2+(E/mc2)))
 A=(nb/(2*!pi))*(delta-3)/(pmin^(3-delta)-pmax^(3-delta))
 return,A/(p^delta)
END

FUNCTION gxFluxtube::PLG,E
 ;power law over the absolute value of momentum (PLP)
 mc2=0.511
 nb=self.n_nth
 E1=self.Emin
 E2=self.Emax
 delta=self.delta1
 gamma1=E1/mc2+1
 gamma2=E2/mc2+1
 gamma=E/mc2+1
 A=(nb/(2*!pi))*(delta-1)/(gamma1^(1-delta)-gamma2^(1-delta))
 return,A/(gamma^delta)
END

FUNCTION gxFluxTube::TNP,E
 ;thermal/nonthermal over the absolute value of momentum (TNP)
 T=self.T0
 nth=self.n_th
 mc2=0.511 ;(MeV)
 eps=self.eps
 delta=self.delta1
 kb=8.6174e-11 ;(MeV K-1)
 theta=kb*T/mc2
 pth2=theta*(2.+ theta)       ;thermal momentum^2
 gamcr=sqrt(1.+pth2/eps)      ;Critical Lorenz-factor
 pcr=sqrt(pth2/eps)           ;Critical momentum
 Ecr=(gamcr-1)*mc2            ;corresponding energy in MeV
 p=sqrt((E/mc2)*(2+(E/mc2)))  ;momentum
 A=self->THM(Ecr)*(pcr^delta)
 N=dblarr(n_elements(E))
 range1=where(E lt Ecr,count,comp=range2,ncomp=ncomp)
 if count gt 0 then N[range1]=self->THM(E[range1])
 if ncomp gt 0 then N[range2]=A*(p[range2]^(-delta))
 return,N
END

FUNCTION gxFluxTube::TNG,E
 ;thermal/nonthermal over the absolute value of momentum (TNP)
 T=self.T0
 nth=self.n_th
 mc2=0.511 ;(MeV)
 eps=self.eps
 delta=self.delta1
 kb=8.6174e-11 ;(MeV K-1)
 theta=kb*T/mc2
 pth2=theta*(2.+ theta)       ;thermal momentum^2
 gamcr=sqrt(1.+pth2/eps)      ;Critical Lorenz-factor
 ;pcr=sqrt(pth2/eps)           ;Critical momentum
 Ecr=(gamcr-1)*mc2            ;corresponding energy in MeV
 gamma=E/mc2+1
 A=self->THM(Ecr)*(gamcr^delta)
 N=dblarr(n_elements(E))
 range1=where(E lt Ecr,count,comp=range2,ncomp=ncomp)
 if count gt 0 then N[range1]=self->THM(E[range1])
 if ncomp gt 0 then N[range2]=A*(gamma[range2]^(-delta))
 return,N
END

FUNCTION gxFluxTube::TPL,E
 ;Thermal plus single power law over kinetic energy (TPL)

 ;thermal (THM) 
 nth=self.n_th
 T=self.T0
 mc2=0.511 ;(MeV)
 kb=8.6174e-11 ;(MeV K-1)
 theta=kb*T/mc2
 gamma=E/mc2+1
 exp_arg=Alog(nth*(2/!pi)*gamma*sqrt(gamma^2-1)*(1/theta)*(1/Beselk(1/theta,2,/double)))-gamma/theta
 N_rel=exp(exp_arg)
 N_nonrel=nth*(2/!pi)*sqrt(gamma-1)*theta^(-1.5)*exp(-(gamma-1)/theta)
 thm=(T gt 5e7)?N_rel:N_nonrel
 
 ;single power law over kinetic energy (PLW)
 nb=self.n_nth 
 delta=self.delta1
 E1=self.Emin
 E2=self.Emax
 pwl=nb*(delta-1)/(E1^(1-delta)-E2^(1-delta))*(E^(-delta))
 
 return,thm+pwl
 
 
END

FUNCTION gxFluxTube::TDP,E
 ;Thermal plus double power law over kinetic energy (TDP)

 ;thermal (THM) 
 nth=self.n_th
 T=self.T0
 mc2=0.511 ;(MeV)
 kb=8.6174e-11 ;(MeV K-1)
 theta=kb*T/mc2
 gamma=E/mc2+1
 exp_arg=Alog(nth*(2/!pi)*gamma*sqrt(gamma^2-1)*(1/theta)*(1/Beselk(1/theta,2,/double)))-gamma/theta
 N_rel=exp(exp_arg)
 N_nonrel=nth*(2/!pi)*sqrt(gamma-1)*theta^(-1.5)*exp(-(gamma-1)/theta)
 thm=(T gt 5e7)?N_rel:N_nonrel
 
  ;double power law over kinetic energy (DPL) 
 nb=self.n_nth
 delta1=self.delta1
 delta2=self.delta2
 E1=self.Emin
 E2=self.Emax 
 Ebr=self.ebreak
 A=nb*(delta1-1)/(E1^(1-delta1)-E2^(1-delta1))
 N=dindgen(n_elements(E))
 range1=where(E lt Ebr,count,comp=range2,ncomp=ncomp)
 if count gt 0 then N[range1]=A*E[range1]^(-delta1)
 if ncomp gt 0 then N[range2]=A*(Ebr^(delta2-delta1))*E[range2]^(-delta2)
 return,thm+N
END

PRO gxFluxTube::UpdateEnergyDistribution
 wdraw=widget_info(self.wParent,Find_By_Uname='GXFLUXTUBE:draw_e')
 if widget_valid(wdraw) then begin
 widget_control,wdraw,get_value=window
 wset,window
 pmulti=!p.multi
 !p.multi=0
 erase,255
   E1=self.emin
   E2=self.emax   
   dE=Alog10(E2/E1)/100
   E=10^(Alog10(E1)+findgen(101)*dE)
   case self.dist_e of
    1:begin
       ;thermal (FFO)  
        N=self->THM(E)
      end
    2:begin
       ;thermal (THM)  
        N=self->THM(E)
      end
    3:begin  
        ;single power law over kinetic energy (PLW)
        N=self->PLW(E)
      end
    4:begin
       ;double power law over kinetic energy (DPL)
       N=self->DPL(E) 
      end
    5:begin
        ;thermal/nonthermal over kinetic energy (TNT) 
       N=self->TNT(E)
      end
    6:begin
        ;kappa (KAP)
        N=self->KAP(E)
      end
    7:begin  
        ;power law over the absolute value of momentum (PLP)
        N=self->PLP(E)
      end
    8:begin
        ;power law over the Lorentz factor (PLG)
        N=self->PLG(E)
      end
    9:begin
        ;thermal/nonthermal over the absolute value of momentum (TNP)
        N=self->TNP(E)
      end
   10:begin
        ;thermal/nonthermal over the Lorentz factor (TNG).
        N=self->TNG(E)
      end  
   11:begin
        ;Thermal plus single power law over kinetic energy (TPL)
        N=self->TPL(E)
      end    
   12:begin
        ;Thermal plus double power law over kinetic energy (TDP)
        N=self->TDP(E)
      end       
   else:
   endcase
   if ptr_valid(self.fparms) then begin
     f_arr=self->integrate_f_arr(/pa,e_arr=e_arr)
     f_arr=f_arr[*,self.spine_idx,self.ftime_idx]
     plot,E,N,back=255,color=0,/ylog,/xlog,$
      xtitle='E(MeV)',ytitle='n(cm^-3)',/xsty,xticks=4,yticks=4,xmargin=[13,1],$
      xrange=minmax([e_arr,E]),yrange=minmax([N,f_arr]),/ysty
     oplot,e_arr,f_arr,color=50,thick=3,psym=-2
   endif else begin
     plot,E,N,back=255,color=0,/ylog,/xlog,xtitle='E(MeV)',ytitle='n(cm^-3)',/xsty,xticks=4,yticks=4,xmargin=[13,1]
   endelse
   !p.multi=pmulti
 endif
END

FUNCTION gxFluxTube::Bt
 self.centerline->GetVertexAttributeData,'B',B
 RETURN,sqrt(total(B^2,1))
END

FUNCTION gxFluxTube::B2B0,c_idx,s=s,b0=b0
 if n_elements(c_idx) eq 0 then begin
  self.centerline->GetVertexAttributeData,'s',s
  c_idx=lindgen(n_elements(s))
 end 
 B=self->Bt()
 if widget_valid(self.wparent) then begin
 ws0=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:s0')
 if widget_valid(ws0) then begin
  widget_control,ws0,get_value=i0
 endif else begin
  self.centerline->GetVertexAttributeData,'s',s
  m=min(abs(s-self.s0),i0)
 endelse
 endif else begin
  self.centerline->GetVertexAttributeData,'s',s
  m=min(abs(s-self.s0),i0)
 endelse
 B0=B[i0]
 B=B[c_idx]
 return,B/B0
END

FUNCTION gxFluxTube::Theta_c,c_idx,B2B0=B2B0
 if ~self.use_clg then return, replicate(self.theta_c0,n_elements(c_idx))
 if n_elements(B2B0) eq 0 then B2B0=self->B2B0(c_idx)
 theta_c=asin(sin(!dtor*self.theta_c0)*sqrt(B2B0))/!dtor
 bad=where(finite(theta_c) eq 0,nbad)
 if nbad gt 0 then theta_c[bad]=90
 return,theta_c
END

FUNCTION gxFluxTube::Theta_b,c_idx,B2B0=B2B0
 if ~self.use_clg then return, replicate(self.theta_b0,n_elements(c_idx))
 if n_elements(B2B0) eq 0 then B2B0=self->B2B0(c_idx)
 mu00=cos(!dtor*self.theta_b0)
 mu0=sqrt(1-(1-mu00^2)*B2B0)
 bad=where(finite(mu0) eq 0,nbad)
 if nbad gt 0 then mu0[bad]=0
 neg=where(mu0*mu00 lt 0,nneg)
 if nneg gt 0 then mu0[neg]=-mu0[neg]
 return,acos(mu0)/!dtor
END

FUNCTION gxFluxTube::dMu,c_idx,B2B0=B2B0
 if ~self.use_clg then return, replicate(self.dMu0,n_elements(c_idx))
 if n_elements(B2B0) eq 0 then B2B0=self->B2B0(c_idx)
 return,self.dMu0*sqrt(B2B0)
END

FUNCTION gxFluxTube::a4,c_idx,B2B0=B2B0
 if ~self.use_clg then return, replicate(self.a4_0,n_elements(c_idx))
 if n_elements(B2B0) eq 0 then B2B0=self->B2B0(c_idx)
 return,self.a4_0/B2B0
END

PRO gxFluxTube::SelectPADistribution,index
 if n_elements(index) eq 0 then begin
  index=self.dist_ang
  wPASelect=widget_info(self.wParent,Find_By_Uname='GXFLUXTUBE:PA_Select')
  widget_control,wPASelect,SET_COMBOBOX_SELECT=index
 endif else begin
  self.dist_ang=index
 end
 PABase=widget_info(self.wParent,Find_By_Uname='GXFLUXTUBE:PAbase')
 widget_control,PABase,Map=0
 ParmBase=widget_info(self.wParent,Find_By_Uname='GXFLUXTUBE:parm_pa')
 ParmSubBase=widget_info(ParmBase,/child)
 if widget_valid(ParmSubBase) then widget_control,ParmSubBase,/destroy
 ParmSubBase=widget_base(ParmBase,/column)
 prefix='GXFLUXTUBE:'
 font=!defaults.font
 case self.dist_ang of
  0:begin
    ;isotropic* (ISO)'
    end
  1:begin 
    ;isotropic (ISO)'
    end
  2:begin  
    ;exponential loss-cone (ELC)
    bt=self->Bt()
    xtextsize=5
    ;xlabelsize=150
    ;lfont=font
    base=widget_base(ParmSubBase,/row)
    wn_dMu0=cw_objfield(base, UNAME=prefix+'dMu0', LABEL='dMu(s0)=',$
    INCREMENT=0.1, $
    UNITS='', $
    VALUE=self.dMu0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
    wn_theta_c0=cw_objfield(base, UNAME=prefix+'theta_c0', LABEL='theta_c(s0)=',$
    INCREMENT=10, $
    UNITS=string(176b), $
    VALUE=self.theta_c0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
    base=widget_base(base,/nonexclusive)
    wselect_clg=widget_button(font=font,base, UNAME=prefix+'use_clg',value='Use B/B0 geometry')
    widget_control,wselect_clg,set_button=self.use_clg
    
    base=widget_base(ParmSubBase,/row)
    wn_dMu=cw_objfield(base, UNAME=prefix+'dMu', LABEL='dMu(s)=',$
    INCREMENT=0.1, $
    UNITS='', $
    VALUE=self.dMu0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont,sensitive=0)
    wn_theta_c=cw_objfield(base, UNAME=prefix+'theta_c', LABEL='theta_c(s)=',$
    INCREMENT=10, $
    UNITS=string(176b), $
    VALUE=self.theta_c0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont,sensitive=0)
    end
  3:begin
    ;Gaussian loss-cone (GLC) 
    xtextsize=5
    ;xlabelsize=150
    ;lfont=font
    base=widget_base(ParmSubBase,/row)
    wn_dMu0=cw_objfield(base, UNAME=prefix+'dMu0', LABEL='dMu(s0)=',$
    INCREMENT=0.1, $
    UNITS='', $
    VALUE=self.dMu0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
    wn_theta_c0=cw_objfield(base, UNAME=prefix+'theta_c0', LABEL='theta_c(s0)=',$
    INCREMENT=10, $
    UNITS=string(176b), $
    VALUE=self.theta_c0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
    base=widget_base(base,/nonexclusive)
    wselect_clg=widget_button(font=font,base, UNAME=prefix+'use_clg',value='Use B/B0 geometry')
    widget_control,wselect_clg,set_button=self.use_clg
    
    base=widget_base(ParmSubBase,/row)
    wn_dMu=cw_objfield(base, UNAME=prefix+'dMu', LABEL='dMu(s)=',$
    INCREMENT=0.1, $
    UNITS='', $
    VALUE=self.dMu0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont,sensitive=0)
    wn_theta_c=cw_objfield(base, UNAME=prefix+'theta_c', LABEL='theta_c(s)=',$
    INCREMENT=10, $
    UNITS=string(176b), $
    VALUE=self.theta_c0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont,sensitive=0)
    end
  4:begin
    ;Gaussian (GAU)
    xtextsize=5
    ;xlabelsize=150
    ;lfont=font
    base=widget_base(ParmSubBase,/row)
    wn_dMu0=cw_objfield(base, UNAME=prefix+'dMu0', LABEL='dMu(s0)=',$
    INCREMENT=0.1, $
    UNITS='', $
    VALUE=self.dMu0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
    wn_theta_b0=cw_objfield(base, UNAME=prefix+'theta_b0', LABEL='theta_b(s0)=',$
    INCREMENT=10, $
    UNITS=string(176b), $
    VALUE=self.theta_b0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
    base=widget_base(base,/nonexclusive)
    wselect_clg=widget_button(font=font,base, UNAME=prefix+'use_clg',value='Use B/B0 geometry')
    widget_control,wselect_clg,set_button=self.use_clg
    
    base=widget_base(ParmSubBase,/row)
    wn_dMu=cw_objfield(base, UNAME=prefix+'dMu', LABEL='dMu(s)=',$
    INCREMENT=0.1, $
    UNITS='', $
    VALUE=self.dMu0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont,sensitive=0)
    wn_theta_b=cw_objfield(base, UNAME=prefix+'theta_b', LABEL='theta_b(s)=',$
    INCREMENT=10, $
    UNITS=string(176b), $
    VALUE=self.theta_b0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont,sensitive=0)
    end
  5:begin  
    ;super-Gaussian (SGA)
    xtextsize=5
    ;xlabelsize=150
    ;lfont=!default.font
    base=widget_base(ParmSubBase,/row)
    wn_dMu0=cw_objfield(base, UNAME=prefix+'dMu0', LABEL='dMu(s0)=',$
    INCREMENT=0.1, $
    UNITS='', $
    VALUE=self.dMu0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
   
    wn_a_40=cw_objfield(base, UNAME=prefix+'a_40', LABEL='a_4(s0)=',$
    INCREMENT=0.1, $
    UNITS='', $
    VALUE=self.a4_0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)   
   
    wn_theta_b0=cw_objfield(base, UNAME=prefix+'theta_b0', LABEL='theta_b(s0)=',$
    INCREMENT=10, $
    UNITS=string(176b), $
    VALUE=self.theta_b0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont)
    base=widget_base(base,/nonexclusive)
    wselect_clg=widget_button(font=font,base, UNAME=prefix+'use_clg',value='Use B/B0 geometry')
    widget_control,wselect_clg,set_button=self.use_clg
    
    base=widget_base(ParmSubBase,/row)
    wn_dMu=cw_objfield(base, UNAME=prefix+'dMu', LABEL='dMu(s)=',$
    INCREMENT=0.1, $
    UNITS='', $
    VALUE=self.dMu0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont,sensitive=0)
   
    wn_a_4=cw_objfield(base, UNAME=prefix+'a_4', LABEL='a_4(s)=',$
    INCREMENT=0.1, $
    UNITS='', $
    VALUE=self.a4_0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont,sensitive=0)   
   
    wn_theta_b=cw_objfield(base, UNAME=prefix+'theta_b', LABEL='theta_b(s)=',$
    INCREMENT=10, $
    UNITS=string(176b), $
    VALUE=self.theta_b0,map=1,/frame,XTEXTSIZE=XTEXTSIZE,xlabelsize=xlabelsize,lfont=lfont,sensitive=0)
    end
 else:
 endcase
 widget_control,PABase,Map=1
END

PRO gxFluxtube::DisplayB2B0ratio
  self.centerline->GetVertexAttributeData,'B',Bvec
  self.centerline->GetVertexAttributeData,'s',s
  l=delta(s)
  B=dblarr(n_elements(s))
  for i=0,n_elements(B)-1 do B[i]=norm(Bvec[*,i])
  m=min(abs(self.s0-s),s0_idx)
  B0=B[s0_idx]
  B2B0=B/B0
  wdraw=widget_info(self.wparent,find_by_uname='GXFLUXTUBE:draw_clg')
  widget_control,wdraw,Get_Value=window
  wset,window
  pmulti=!p.multi
  !p.multi=[0,2,1]
    alpha=self.centerline->GetAlpha()
    plot,s/l,alpha,back=255,color=0,xtitle='s/l',ytitle='alpha',/xsty,thick=2
    oplot,[1,1]*s[self.centerindex]/l,!y.crange,color=0
    oplot,!x.crange,[0,0],color=0
    mom=moment(alpha,sdev=sdev)
    oplot,!x.crange,mom[[0,0]],color=10
    gx_plot_label,0.1,0.7,strcompress(string(mom[0],sdev,format="('!4a!3=',g10.3,'!9+!3',g10.3)")),color=0
    plot,s/l,B2B0,back=255,color=0,xtitle='s/l',ytitle='B/B!D0!N',/xsty,thick=2
    oplot,[1,1]*self.s0/l,!y.crange,color=0
    oplot,!x.crange,[1,1],color=0
  !p.multi=pmulti
END



PRO gxFluxTube::UpdatePADistribution
 wdraw=widget_info(self.wParent,Find_By_Uname='GXFLUXTUBE:draw_pa')
 if widget_valid(wdraw) then begin
 widget_control,wdraw,get_value=window
 wset,window
 erase,255
 pmulti=!p.multi
 mu1=-1.0
 mu2=1.0
 np=101
 deltamu=(mu2-mu1)/(np-1)
 mu=mu1+deltamu*findgen(np)
 c_idx=self.spine_idx
 case self.dist_ang of
  0:begin
    ;isotropic* (ISO)'
    !p.multi=0
    plot,[mu1,mu2],[0.5,0.5],back=255,color=0,xtitle='!4l!3',ytitle='g(!4l!3)',/xsty,/ysty,yrange=[0,1],thick=2
    end
  1:begin 
    ;isotropic (ISO)'
    !p.multi=0
    plot,[mu1,mu2],[0.5,0.5],back=255,color=0,xtitle='!4l!3',ytitle='g(!4l!3)',/xsty,/ysty,yrange=[0,1],thick=2
    end
  2:begin  
    ;exponential loss-cone (ELC)
     !p.multi=0
     g=mu*0
     dMu=(self->dMu(c_idx,B2B0=B2B0))[0]
     widget_control,widget_info(self.wParent,find_by_uname='GXFLUXTUBE:dMu'),set_value=floor(1000*dMu)/1000.
     theta_c=(self->theta_c(c_idx,B2B0=B2B0))[0]
     widget_control,widget_info(self.wParent,find_by_uname='GXFLUXTUBE:theta_c'),set_value=floor(theta_c*100)/100.
     muc=cos(!dtor*theta_c)
     range1=where(abs(mu) lt muc,nr1,comp=range2,ncomp=nr2)
     A=1/(2*(muc+dMu-dMu*exp((muc-1)/dmu)))
     if nr1 gt 0 then g[range1]=A
     if nr2 gt 0 then g[range2]=A*exp(-(abs(mu[range2])-muc)/dmu)
     plot,mu,g,back=255,color=0,xtitle='!4l!3',ytitle='g(!4l!3)',/xsty,thick=2,yrange=[0,max(g)>1]
    end
  3:begin
    ;Gaussian loss-cone (GLC) 
     !p.multi=0
     g=mu*0
     dMu=(self->dMu(c_idx,B2B0=B2B0))[0]
     widget_control,widget_info(self.wParent,find_by_uname='GXFLUXTUBE:dMu'),set_value=floor(1000*dMu)/1000.
     theta_c=(self->theta_c(c_idx,B2B0=B2B0))[0]
     widget_control,widget_info(self.wParent,find_by_uname='GXFLUXTUBE:theta_c'),set_value=floor(theta_c*100)/100.
     muc=cos(!dtor*theta_c)
     range1=where(abs(mu) lt muc,nr1,comp=range2,ncomp=nr2)
     A=1/(2*(muc+sqrt(!pi)*dmu*erf((1-muc)/dmu)))
     if nr1 gt 0 then g[range1]=A
     exp_arg=Alog(A)-((abs(mu[range2])-muc)/dmu)^2
     if nr2 gt 0 then g[range2]=exp(exp_arg)
     plot,mu,g,back=255,color=0,xtitle='!4l!3',ytitle='g(!4l!3)',/xsty,thick=2,yrange=[0,max(g)>1]
    end
  4:begin
    ;Gaussian (GAU)
     !p.multi=0
     dMu=(self->dMu(c_idx,B2B0=B2B0))[0]
     widget_control,widget_info(self.wParent,find_by_uname='GXFLUXTUBE:dMu'),set_value=floor(1000*dMu)/1000.
     theta_b=(self->theta_b(c_idx,B2B0=B2B0))[0]
     widget_control,widget_info(self.wParent,find_by_uname='GXFLUXTUBE:theta_b'),set_value=floor(theta_b*100)/100.
     muc=cos(!dtor*theta_b)
     mu0=cos(!dtor*self.theta_b0)
     A=1/(sqrt(!pi)*(erf((1-mu0)/dmu)+erf((1+mu0)/dmu))/2)
     g=A*exp(-((mu-mu0)/dmu)^2)
     plot,mu,g,back=255,color=0,xtitle='!4l!3',ytitle='g(!4l!3)',/xsty,thick=2,yrange=[0,max(g)>1]
    
    end
  5:begin  
    ;super-Gaussian (SGA)
    !p.multi=0
     dMu=(self->dMu(c_idx,B2B0=B2B0))[0]
     widget_control,widget_info(self.wParent,find_by_uname='GXFLUXTUBE:dMu'),set_value=floor(1000*dMu)/1000.
     theta_b=(self->theta_b(c_idx,B2B0=B2B0))[0]
     widget_control,widget_info(self.wParent,find_by_uname='GXFLUXTUBE:theta_b'),set_value=floor(theta_b*100)/100.
     a4=(self->a4(c_idx,B2B0=B2B0))[0]
     widget_control,widget_info(self.wParent,find_by_uname='GXFLUXTUBE:a_4'),set_value=floor(a4*100)/100.
     mu0=cos(!dtor*self.theta_b0)
     g=exp(-((mu-mu0)^2+a4*(mu-mu0)^4)/dmu^2)
     g=g/total(g)
     plot,mu,g,back=255,color=0,xtitle='!4l!3',ytitle='g(!4l!3)',/xsty,thick=2,yrange=[0,max(g)>1]
    end
 else:
 endcase
 if ptr_valid(self.fparms) then begin
  fparms=self->interpolate_fparms()
  f_arr=self->integrate_f_arr(/energy,mu_arr=mu_arr)
  f_arr0=f_arr[*,c_idx,self.ftime_idx]
  oplot,mu_arr,f_arr0/INT_TABULATED( mu_arr,f_arr0 ),psym=-2,color=50,thick=3
 endif
  
 !p.multi=pmulti
 end
END

PRO gxFluxTube::CLEANUP
if obj_valid(self.centerline) then begin
 self.centerline->SetProperty,center=0,lock=0
 self.centerline->GetProperty,parent=oModel
 oModel->RequestVolumeUpdate, /newID
end
 self->IDLgrModel::CLEANUP
END

PRO gxFluxTube::GetProperty,wParent=wParent,a=a,b=b,phi=phi,nrho=nrho,nphi=nphi,centerline=centerline,centerindex=centerindex,lock=lock,$
                             p_nth=p_nth,nr_nth=nr_nth,q_nth=q_nth,ns_nth=ns_nth,n_nth=n_nth,$
                             p_th=p_th,q_th=q_th,nr_th=nr_th,nz_th=nz_th,n_th=n_th,T0=T0,$
                            dist_e=dist_e,dist_ang=dist_ang,eps=eps,kappa=kappa,emin=emin,emax=emax,ebreak=ebreak,$
                            delta1=delta1,delta2=delta2,theta_c0=theta_c0,theta_b0=theta_b0,dMu0=dMu0,parm_a4_0=parm_a4_0,use_clg=use_clg,s0=s0,$
                            centerbase=centerbase,fkey=fkey,fparms=fparms,$
                            ftime_idx=ftime_idx,spine_idx=spine_idx,nb_arr=nb_arr,_ref_extra=extra
 wParent=self.wParent
 centerline=self.centerline
 centerindex=self.centerindex
 a=self.a
 b=self.b
 phi=self.phi/!dtor
 nrho=self.nrho
 nphi=self.nphi
 lock=self.lock
 p_nth=double(str2arr(strmid(self.p_nth,1,strlen(self.p_nth)-2)))
 nr_nth=self.nr_nth
 q_nth=double(str2arr(strmid(self.q_nth,1,strlen(self.q_nth)-2)))
 ns_nth=self.ns_nth
 n_nth=self.n_nth
 T0=self.T0
 n_th=self.n_th
 nr_th=self.nr_th
 nz_th=self.nz_th
 p_th=double(str2arr(strmid(self.p_th,1,strlen(self.p_th)-2)))
 q_th=double(str2arr(strmid(self.q_th,1,strlen(self.q_th)-2)))
 dist_e=self.dist_e
 dist_ang=self.dist_ang
 eps=self.eps
 kappa=self.kappa
 emin=self.emin
 emax=self.emax
 ebreak=self.ebreak
 delta1=self.delta1
 delta2=self.delta2
 theta_c0=self.theta_c0
 theta_b0=self.theta_b0
 dMu0=self.dMu0
 parm_a4_0=self.a4_0
 use_clg=self.use_clg
 s0=self.s0
 fkey=self.fkey
 ftime_idx=self.ftime_idx
 nb_arr=self.nb_arr
 spine_idx=self.spine_idx
 fparms=ptr_valid(self.fparms)?*(self.fparms):!null
 centerbase=self->GetByName('Base')
 self->IDLgrModel::GetProperty,_extra=extra
END

PRO gxFLUXTUBE::upload_fparms,filename
  catch, error_status
  if error_status ne 0 then begin
      message, !error_state.msg,/cont
      return
  endif
  if ~file_exist(filename) then begin
    filename=dialog_pickfile(filter=['*.sav'],$
    DEFAULT_EXTENSION='sav',$
    /read,/must_exist,$
    title='Please select an IDL sav file to upload a fluxtube numerical solution')
  endif
  restore,filename
   self.nb_arr=1
   s_arr=exist(s)?s:z
   bad=where(distfunc eq 0,count,comp=good)
   if count gt 0 then begin
    distfunc[bad]=min(distfunc[good],/nan)
   endif
   fparms={f_arr:distfunc/self.nb_arr,e_arr:e,mu_arr:mu,s_arr:s_arr,t_arr:t}
   ptr_free,self.fparms
   self.fparms=ptr_new(fparms)
   spine_f_arr=self->integrate_f_arr(/en,/pa)
   self.nb_arr=max(spine_f_arr)
   fparms={f_arr:distfunc/self.nb_arr,e_arr:e,mu_arr:mu,s_arr:s_arr,t_arr:t}
   ptr_free,self.fparms
   self.fparms=ptr_new(fparms)
END

PRO gxFLUXTUBE::remove_fparms
  catch, error_status
  if error_status ne 0 then begin
    message, !error_state.msg,/cont
    return
  endif
  ptr_free,self.fparms
  self.nb_arr=0
  self.time_idx=0
END

PRO gxFLUXTUBE::export_spine_parms
  catch, error_status
  if error_status ne 0 then begin
    message, !error_state.msg,/cont
    return
  endif
  self->ExportSpineParms
END

FUNCTION gxFLUXTUBE::interpolate_fparms,time_idx=time_idx
  default,time_idx,self.ftime_idx
  if ~ptr_valid(self.fparms) then return,!null
  fparms=(*self.fparms)
  sz=size(fparms.f_arr)
  spine=self->GetVertexData('s')
  spine=spine/delta(spine)
  f_arr=fltarr(sz[1],sz[2],n_elements(spine))
  for i=0,sz[1]-1 do begin
    for j=0,sz[2]-1 do begin
        f_arr[i,j,*]=interpol(fparms.f_arr[i,j,*,time_idx],fparms.s_arr,spine)>0
    endfor
  endfor
  fparms=rep_tag_value(fparms,f_arr,'f_arr')
  fparms=rem_tag(fparms,'s_arr')
  fparms=add_tag(fparms,fparms.t_arr[time_idx],'time')
  fparms=rem_tag(fparms,'t_arr')
  fparms.f_arr*=self.nb_arr
  return,fparms
END

PRO gxFLUXTUBE::ExportSpineParms
  filename=dialog_pickfile(filter=['*.sav'],$
    DEFAULT_EXTENSION='sav',$
    /write,$
  Title='Please select an IDL sav filename, to export fluxtube spine parameters')
  if filename ne '' then begin
    n_th=self->spine_n_th(s=s,l=l,/onepervox)
    b=self->spine_b(s=s,l=l,/onepervox)
    s=s/l
    l=l*gx_rsun(unit='cm')
    save,s,b,n_th,l,file=filename
  end
END

PRO gxFluxTube::SetProperty,wParent=wParent,centerindex=centerindex,a=a,b=b,phi=phi,nrho=nrho,nphi=nphi,lock=lock,$
                            p_nth=p_nth,nr_nth=nr_nth,q_nth=q_nth,ns_nth=ns_nth,n_nth=n_nth, $
                            p_th=p_th,q_th=q_th,nr_th=nr_th,nz_th=nz_th,n_th=n_th,T0=T0,$
                            dist_e=dist_e,dist_ang=dist_ang,eps=eps,kappa=kappa,emin=emin,emax=emax,ebreak=ebreak,$
                            delta1=delta1,delta2=delta2,theta_c0=theta_c0,theta_b0=theta_b0,dMu0=dMu0,$
                            parm_a4_0=parm_a4_0,use_clg=use_clg,s0=s0,fkey=fkey,$
                            ftime_idx=ftime_idx,spine_idx=spine_idx,nb_arr=nb_arr,_extra=_extra
 if exist(wParent) then self.wParent=wParent
 if exist(centerindex) then self.centerindex=centerindex
 if exist(nrho) then self.nrho=nrho
 if exist(nphi) then self.nphi=nphi
 if exist(lock) then begin
 self.lock=lock
 self.centerline->SetProperty,lock=lock
 end
 if exist(p_nth) then self.p_nth=strcompress('['+arr2str(p_nth)+']')
 if exist(nr_nth) then self.nr_nth=nr_nth
 if exist(q_nth) then self.q_nth=strcompress('['+arr2str(q_nth)+']')
 if exist(ns_nth) then self.ns_nth=ns_nth
 if exist(n_nth) then self.n_nth=n_nth
 if exist(p_th) then self.p_th=strcompress('['+arr2str(p_th)+']')
 if exist(nr_th) then self.nr_th=nr_th
 if exist(q_th) then self.q_th=strcompress('['+arr2str(q_th)+']')
 if exist(nz_th) then self.nz_th=nz_th
 if exist(a) then self.a=a
 if exist(b) then self.b=b
 if exist(phi) then self.phi=phi*!dtor
 if exist(n_th) then self.n_th=n_th
 if exist(T0) then self.T0=T0
 if exist(dist_e) then self.dist_e=dist_e
 if exist(dist_ang) then self.dist_ang=dist_ang
 if exist(eps) then self.eps=eps
 if exist(kappa) then self.kappa=kappa
 if exist(emin) then self.emin=emin
 if exist(emax) then self.emax=emax
 if exist(ebreak) then self.ebreak=ebreak
 if exist(delta1) then self.delta1=delta1
 if exist(delta2) then self.delta2=delta2
 if exist(theta_c0) then self.theta_c0=theta_c0
 if exist(theta_b0) then self.theta_b0=theta_b0
 if exist(dMu0) then self.dMu0=dMu0
 if exist(parm_a4_0) then self.a4_0=parm_a4_0
 if exist(use_clg) then self.use_clg=use_clg
 if exist(s0) then self.s0=s0
 if exist(fkey) then self.fkey=fkey
 if exist(ftime_idx) then self.ftime_idx=ftime_idx
 if exist(nb_arr) then self.nb_arr=nb_arr
 if exist(spine_idx) then self.spine_idx=spine_idx
 self->IDLgrModel::SetProperty,_extra=_extra
END

PRO gxFluxTube__define
self={gxFluxTube, inherits IDLgrModel, wParent:0l,$
centerindex:0l,a:0d,b:0d,phi:0d,n:[0d,0,0],ex:[0d,0,0],ey:[0d,0,0],nphi:0l,nrho:0l,centerline:obj_new(),base:obj_new(),lock:0l,$
p_nth:'',nr_nth:'',q_nth:'',ns_nth:'',p_th:'',q_th:'',nr_th:'',nz_th:'',n_th:0.0,T0:0.0,n_nth:0.0,$
dist_e:0,dist_ang:0,eps:0.0,kappa:0.0,emin:0.0,emax:0.0,ebreak:0.0,delta1:0.0,delta2:0.0,theta_c0:0.0,$
theta_b0:0.0,dMu0:0.0,a4_0:0.0,use_clg:0b,s0:0.0,fkey:0,ftime_idx:0l,spine_idx:0l,nb_arr:0d,fparms:ptr_new()} 
END