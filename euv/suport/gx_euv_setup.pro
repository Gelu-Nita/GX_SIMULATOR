pro gx_euv_setup, instrument=instrument, idx=idx, info=info

default, idx, { parms:  {dr:0,t0:1,n0:2,q:3,l:4,voxid:5,nhi:6,trf:7},$
                nparms: {npix:0,nvox:1,nchan:2,usedem:3,addtr:4,applytrf:5,demavg:6,evenorm:7,chiantifix:8},$
                rparms: {ds:0,nhi0:1,relabund:2}, $
                sparms: {instr:0,responsedate:1,ebtel:2 }}
    
if ~arg_present(info) then return
    
if n_elements(info) eq 0 then begin
  default,instrument,'AIA'
  timenow=AIA_BP_UTC2DATE_STRING(ANYTIM2UTC(RELTIME(/now), /ccsds))
  
  parms=Replicate({Name:'unused',Value:0d, Unit:'', Hint:''},n_tags(idx.parms))

  parms[idx.parms.dr]    ={Name:'dR',      Value:0.600d+09,   Unit:'cm',      Hint:'Source/voxel Depth'}
  parms[idx.parms.t0]    ={Name:'T_0',     Value:0.200d+08,   Unit:'K',       Hint:'Plasma Temperature'}
  parms[idx.parms.n0]    ={Name:'n_0',     Value:0.500d+10,   Unit:'cm^{-3}', Hint:'Thermal e density'}
  parms[idx.parms.q]     ={Name:'Q',       Value:0d,          Unit:'',        Hint:'Heating rate'}
  parms[idx.parms.l]     ={Name:'Length',  Value:0d,          Unit:'cm',      Hint:'Half length of the associated fieldline'}
  parms[idx.parms.voxid] ={Name:'VoxelID', Value:0d,          Unit:'0/1/2',   Hint:'chromo/TR/corona'}
  parms[idx.parms.nhi]   ={Name:'n_hi',    Value:0d,          Unit:'cm^{-3}', Hint:'Neutral Hydrogen density'}
  parms[idx.parms.trf]   ={Name:'TRfactor',Value:0d,          Unit:'' ,       Hint:'TR factor'}

  nparms=replicate({name:'unused',value:0l,unit:'',user:0,hint:''},n_tags(idx.nparms))

  nparms[idx.nparms.npix]        ={name:'N_pix',         value:0l, unit:'(int)', user:0, hint:'Number of pixels'}
  nparms[idx.nparms.nvox]        ={name:'N_vox',         value:0l, unit:'(int)', user:0, hint:'Number of voxels'}
  nparms[idx.nparms.nchan]       ={name:'N_chan',        value:0l, unit:'(int)', user:0, hint:'Number of channels'}
  nparms[idx.nparms.usedem]      ={name:'UseDEM',        value:1l, unit:'(int)', user:0, hint:'Use DEM'}
  nparms[idx.nparms.addtr]       ={name:'AddTR',         value:1l, unit:'(int)', user:0, hint:'Add TR Contribution'}
  nparms[idx.nparms.applytrf]    ={name:'ApplyTRfactor', value:1l, unit:'(int)', user:0, hint:'Apply TR Factor'}
  nparms[idx.nparms.demavg]      ={name:'DEMavg',        value:0l, unit:'(int)', user:0, hint:'DEM Interpolation Method'}
  nparms[idx.nparms.evenorm]     ={name:'EVEnorm',       value:1l, unit:'(int)', user:1, hint:'Perform EVE normalization'}
  nparms[idx.nparms.chiantifix]  ={name:'CHIANTIfix',    value:1l, unit:'(int)', user:1, hint:'Apply CHIANTI correction'}
  if strupcase(instrument) ne 'AIA' then nparms=nparms[0:idx.nparms.demavg]
  
  rparms=replicate({name:'unused',value:0d,unit:'',user:0,hint:''},n_tags(idx.rparms))
   
  rparms[idx.rparms.ds]           ={name:'dS',                  value:0d, unit:'(cm^2)',  user:0, hint:'Source pixel/area'}
  rparms[idx.rparms.nhi0]         ={name:'n_hi0',               value:1d, unit:'cm^{-3}', user:1, hint:'Neutral H density coronal cutoff'}
  rparms[idx.rparms.relabund]     ={name:'relative_abundance',  value:1d, unit:'',        user:1, hint:'Relative to coronal abundance for Chianti'}
  
  sparms=replicate({name:'unused',value:'',unit:'',user:0,hint:''},n_tags(idx.sparms))
  
  sparms[idx.sparms.instr] ={name:'Instrument',       value:instrument, unit:'', user:0, hint:''}
  sparms[idx.sparms.responsedate] ={name:'response_date',       value:timenow, unit:'(UTC)', user:0, hint:'yyyymmdd_hhmmss'}
  sparms[idx.sparms.ebtel] ={name:'ebtel', value:file_basename(gx_ebtel_path()), unit:'', user:0, hint:'filename base'}
  
  case strupcase(instrument) of
    'TRACE':begin
      rgb_path='trace_rgb.sav'
      restore,gx_findfile('trace_response.sav')
      response=rep_tag_value(response,timenow,'date')
    end
    'SXT':begin
      rgb_path='sxt_rgb.sav'
      restore,gx_findfile('sxt_response.sav')
      response=rep_tag_value(response,timenow,'date')
      response.channels= str_replace(strupcase(response.channels),'GAL','A')
    end
    'SOLO-FSI':begin
           rgb_path='eui_rgb.sav'
           response=rep_tag_value(gx_get_eui_response(timenow,/fsi),timenow,'date')
          end
    'SOLO-HRI':begin
            rgb_path='eui_rgb.sav'
            response=rep_tag_value(gx_get_eui_response(RELTIME(/now),/hri),timenow,'date')
          end 
    'STEREO-A':begin
            rgb_path='euvi_rgb.sav'
            response=rep_tag_value(gx_get_euvi_response(RELTIME(/now),/a),timenow,'date')
          end
    'STEREO-B':begin
            rgb_path='euvi_rgb.sav'
            response=rep_tag_value(gx_get_euvi_response(RELTIME(/now),/b),timenow,'date')
          end     
    'AIA2':begin
      rgb_path='aia_rgb.sav'
      response=rep_tag_value(gx_get_aia_response(RELTIME(/now)),timenow,'date')
     end          
    else: begin
      rgb_path='aia_rgb.sav'
      restore,gx_findfile('aia_response.sav')
    end
  endcase

  nchan=n_elements(response.channels)
  nparms[idx.nparms.nchan].value=nchan
  w=fltarr(nchan)
  for i=0,nchan-1 do w[i]=fix(strmid(response.channels[i],1))
  info={parms:parms,$
    nparms:nparms,$
    rparms:rparms,$
    sparms:sparms,$
    pixdim:[nchan],$
    spectrum:{x:{axis:w,label:'Wavelength',unit:'A'},$
    y:{label:'I',unit:'counts/s/pix'}}}
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    goto,skip_rgb
  end
  restore,gx_findfile(rgb_path)
  skip_rgb:
  if n_elements(rgb) ne 0 then info=create_struct(info, 'rgb',rgb)
 endif
end