function gxCorona::Init,R=R,size=size,_extra=_extra
compile_opt hidden
 catch, error_stat
 if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return, 0
 end
 default,R,1.5
 default,size,[64,64,64]
 self.R=R
 self.size=size
 dr=2*self.R/self.size
 result=self->gxComponent::Init(_extra=_extra)
 self.n0=1e8
 self.T0=1e6
 self.chromo_n=1e13
 self.chromo_T=3500
 self.chromo_h=0
 self.n_th='n0*exp(-h/R/(6.7576e-8)/T0)'
 self.p='[0]'
 self.dist_e=1
 self.kappa=4
 self.emin=0.001
 self.emax=10
 data=fltarr(self.size[0],self.size[1],self.size[2])
 sz=size(data)
 xcoord_conv=[0,dr[0]]
 ycoord_conv=[0,dr[1]]
 zcoord_conv=[0,dr[2]]

 self.volume=obj_new('gxVolume',data,xcoord_conv=[0,dr[0]],ycoord_conv=[0,dr[1]],zcoord_conv=[0,dr[2]],zbuffer=1,_extra=_extra,/interpolate,/ZERO_OPACITY_SKIP)
 self.volume->SetProperty,_extra=_extra
 self->Reset
 self->Translate,-self.R,-self.R,-self.R
 rgb_curr=bytarr(3,256)
 tvlct,rgb_curr,/get
 loadct,39
 rgb=bytarr(3,256)
 tvlct,rgb,/get
 self.volume->SetProperty,rgb_table0=rgb
 tvlct,rgb_curr
 return,result
end

function gxCorona::GetVolume
;p is expressed in self's box fractional index coordinates
return,self.volume
end

function gxCorona::Size
 self->GetProperty,data0=data0
 return,size(data0)
end

pro  gxCorona::SetProperty,n0=n0,T0=T0,p=p,n_th=n_th,wParent=wParent,emin=emin,emax=emax,dist_e=dist_e,kappa=kappa,$
                           chromo_n=chromo_n,chromo_T=chromo_T,chromo_h=chromo_h,chromo_view=chromo_view,blend=blend,ignore=ignore,_extra=_extra
 if exist(chromo_n) then self.chromo_n=chromo_n
 if exist(chromo_T) then self.chromo_T=chromo_T
 if exist(chromo_h) then self.chromo_h=chromo_h
 if exist(chromo_view) then self.chromo_view=chromo_view
 if exist(n0) then self.n0=n0
 if exist(T0) then self.T0=T0
 if exist(p) then self.p=strcompress('['+arr2str(p)+']')
 if exist(n_th) then self.n_th=n_th
 if exist(wParent) then self.wParent=wParent
 if exist(dist_e) then self.dist_e=dist_e
 if exist(emin) then self.emin=emin
 if exist(emax) then self.emax=emax
 if exist(kappa) then self.kappa=kappa
 if exist(blend) then self.blend=blend
 if exist(ignore) then self.ignore=ignore
 self.volume->SetProperty,_extra=_extra
 self->gxComponent::SetProperty,_extra=_extra
end

pro gxCorona::GetProperty,n0=n0,T0=T0,p=p,n_th=n_th,wParent=wParent,emin=emin,emax=emax,dist_e=dist_e,kappa=kappa,$
                          chromo_n=chromo_n,chromo_T=chromo_T,chromo_h=chromo_h,chromo_view=chromo_view,blend=blend,ignore=ignore,_ref_extra=extra
 
 chromo_n=self.chromo_n
 chromo_T=self.chromo_T
 chromo_h=self.chromo_h
 chromo_view=self.chromo_view
 n0=self.n0
 T0=self.T0
 p=double(str2arr(strmid(self.p,1,strlen(self.p)-2)))
 n_th=self.n_th
 wParent=self.wParent
 emin=self.emin
 emax=self.emax
 dist_e=self.dist_e
 kappa=self.kappa
 blend=self.blend
 ignore=self.ignore
 self.volume->GetProperty,_extra=extra
 self->gxComponent::GetProperty,_extra=extra
end

function gxCorona::GetDensity,radius,h=h,n0=n0,T0=T0,chromo_n=chromo_n,chromo_T=chromo_T,chromo_h=chromo_h,chromo_view=chromo_view,blend=blend,ignore=ignore
 chromo_n=self.chromo_n
 chromo_T=self.chromo_T
 chromo_h=self.chromo_h
 chromo_view=self.chromo_view
 blend=self.blend
 ignore=self.ignore
 R=1
 n0=self.n0
 T0=self.T0
 hh=radius-R
 in=where(hh lt 0,count,comp=comp,ncomp=ncomp)
 h=hh[comp]
 error=execute('p='+self.p)
 success=execute('n='+self.n_th)
 n_th=hh*0
 if ncomp gt 0 then n_th[comp]=n
 h=hh
 if count then n_th[in]=0
 if ~success then begin
  answ=dialog_message('Invalid density distribution:'+STRING(10b)+n_th+STRING(10b)+'Check syntax!',/error)
  return,h*0
 end
 return,n_th
end

FUNCTION gxCorona::THM,E
 ;thermal (THM)
 nth=self.n0
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

FUNCTION gxCorona::KAP,E
 ;kappa (KAP)
 nth=self.n0
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

pro gxCorona::UpdateVolume,_extra=_extra
 widget_control,/hourglass
 R=1
 self->GetProperty,xcoord_conv=xcoord_conv,data0=data0,p=p,n_th=n_th,n0=n0,T0=T0
 sz=size(data0)
 radius=findgen(sz[1])*xcoord_conv[1]/2
 n_th=self->GetDensity(radius,h=h)
 good=where(h ge 0)
 n_th=n_th(good)
 h=h[good]
 if widget_valid(self.wParent) then begin
  wDraw=widget_info(self.wParent,find_by_uname='GXCORONA:draw')
  widget_control,wdraw,get_value=window
  wset,window
  rgb_curr=bytarr(3,256)
  tvlct,rgb_curr,/get
  pmulti=!p.multi
  !p.multi=[0,2,1]
  loadct,0,/silent
  erase,255
  plot,h,n_th,color=0,back=255,ytitle='n0(cm^-3)',xtitle='h/R',xmargin=[13,3]
  E1=self.emin
  E2=self.emax
  dE=Alog10(E2/E1)/100
  E=10^(Alog10(E1)+findgen(101)*dE)
  if (self.dist_e eq 1 or self.dist_e eq 2) then  N=self->THM(E) else  N=self->KAP(E)
  plot,E,N,back=255,color=0,/ylog,/xlog,xtitle='E(MeV)',ytitle='n(cm^-3)',/xsty,xticks=4,yticks=4,xmargin=[13,3]
  tvlct,rgb_curr
  !p.multi=pmulti
end
  (self.Parent)->RequestVolumeUpdate,_extra=_extra
end


pro gxCorona__define
 struct_hide,{gxCorona,inherits gxComponent,wParent:0l,volume:obj_new(),n0:0d,T0:0d,kappa:0.0,emin:0.0,emax:0.0,dist_e:0,p:'',n_th:'',R:0d,size:[0l,0l,0l],$
 chromo_n:0d,chromo_T:0d,chromo_h:0d,chromo_view:0b,blend:0,ignore:0}
end