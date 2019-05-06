pro gs_transfer_slice_arr_ana,parms,rowdata,path,parmin,datain,info=info

 if n_elements(path) eq 0 then begin
  dirpath=file_dirname((ROUTINE_INFO('gs_transfer_slice_arr_ana',/source)).path,/mark)
  path=dirpath+'gs_transfer_slice_arr_ana.dll'
 end
 
 if arg_present(info) then begin
    if n_elements(parms) gt 0 then dummy=temporary(parms)
    if n_elements(info) eq 0 then begin
      result=call_external(path,'GET_PARMS',/F_VALUE,/unload )
      openr,lun,'parms.txt',/get,error=error
      if error eq 1 then return
      line=''
      WHILE ~ EOF(lun) DO BEGIN 
         READF, lun, line 
         info=strsplit(line,';',/extract)
         if n_elements(info) eq 3 then info=[info,'']
         if n_elements(parms) eq 0 then begin
         parms={name:strcompress(info[0],/rem),value:float(info[1]),unit:strcompress(info[2],/rem),hint:info[3]}
         endif else begin
         parms=[parms,{name:strcompress(info[0],/rem),value:float(info[1]),unit:strcompress(info[2],/rem),hint:info[3]}]
         end
      ENDWHILE
      free_lun,lun
    endif else parms=info.parms  
    Nvox=5L
    Npix=1L
    Nparms=n_elements(parms)
    N_dat=lonarr(5) 
    N_dat(0)=Npix
    N_dat(1)=Nvox
    N_dat(2)=20;N_Earr
    N_dat(3)=41;N_Muarr
    N_dat(4)=Nparms;N_Parm
    f_sVP=dblarr(N_dat(0),N_dat(1),N_dat(2),N_dat(3))
    E=dblarr(N_dat(2))
    mu_s=dblarr(N_dat(3))
    N_dat(4)=n_elements(parms);N_Parm
    dummy_parmin=dblarr(Nparms,Nvox,Npix)
    dummy_parmin[*]=1d0*parms.value#(dblarr(nvox)+1)
    dummy_datain=dblarr(7,parms[18].value,Npix)
    test_call=call_external(path, 'GET_MW_SLICE', N_dat, dummy_parmin,  E, mu_s, f_sVP,dummy_datain,/d_value, /unload)
    info={parms:parms,$
          pixdim:[parms[18].value,2,3],$
          spectrum:{x:{axis:reform(dummy_datain[0,*]),label:'Frequency',unit:'GHz'},$
                    y:{label:['LCP','RCP','[RCP+LCP]','[RCP-LCP]','[R-L]/[R+L]','T_LCP','T_RCP','T_I','T_V'],unit:['sfu','sfu','sfu','sfu','%','K','K','K','K']}},$
          channels:['Exact Coupling', 'Weak Coupling', 'Strong Coupling']}                 
    return
 end
   sz=size(rowdata,/dim)
   Npix=sz[0]
   Nfreq=sz[1]
   Npol=sz[2]
   Ncouplings=sz[3]
   sz=size(parms,/dim)
   Npix=sz[0] 
   Nvox=sz[1]  
   Nparms=sz[2]
   if n_elements(datain) eq 0 then datain=dblarr(7,Nfreq,Npix)
   if n_elements(parmin) eq 0 then parmin=dblarr(Nparms,Nvox,Npix)
   for i=0, npix-1 do begin
    parmin[*,*,i]=transpose(parms[i,*,*])
   end
    N_dat=lonarr(5)
    N_dat(0)=Npix
    N_dat(1)=Nvox
    N_dat(2)=1;N_Earr
    N_dat(3)=1;N_Muarr
    N_dat(4)=34L;N_Parm
    E=dblarr(N_dat(2))
    mu_s=dblarr(N_dat(3))
    f_sVP=dblarr(N_dat(0),N_dat(1),N_dat(2),N_dat(3))
  ;test_call=call_external(lib, 'GET_MW_SLICE', N_dat, parmin, E, mu_s, f_sVP(*,*,*,*), s_slice,/d_value, /unload)
   test_call=call_external(path, 'GET_MW_SLICE', N_dat, parmin,  E, mu_s, f_sVP,datain,/d_value, /unload)
   rowdata[*,*,0,0]=transpose(datain[5,*,*]);eL
   rowdata[*,*,1,0]=transpose(datain[6,*,*]);eR
   rowdata[*,*,0,1]=transpose(datain[1,*,*]);wL
   rowdata[*,*,1,1]=transpose(datain[2,*,*]);wR
   rowdata[*,*,0,2]=transpose(datain[3,*,*]);sL
   rowdata[*,*,1,2]=transpose(datain[4,*,*]);sR
end