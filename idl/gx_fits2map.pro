;+
; NAME:
;    gx_fits2map
; PURPOSE:
;    GX FITS to MAP conversion routine
; CATEGORY:
;    GX_Simulator
; CALLING SEQUENCE:
;     gx_fits2map,filename,map,no_reformat=no_reformat
; INPUTS:
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;
; ROUTINES CALLED:
;  mrdfits
;  fits2map
;  wimagr_index2map
;
; OUTPUTS:
; COMMENTS:
; Intended to replace obsolete ovsa2map
; SIDE EFFECTS:
; RESTRICTIONS:
;  If FITS header indicates a known instrument, th corresponding fits rading routine is called. 
;  Foer all other cases, the general purpose fits2map is called
; MODIFICATION HISTORY:
;     Written 03-Jan-2017 Gelu M. Nita
;
pro gx_fits2map,filename,map,reform=reform
  if n_elements(filename) eq 0 then filename=dialog_pickfile(filter='*.f*')
  if ~file_exist(filename) then return
  break_file, filename, disk_log, dir, instr_name, ext
  instr_name=strupcase(strmid(instr_name,0,3))
  data=mrdfits(filename,0,header,/silent)
 case strupcase(instr_name) of
     'AIA': instr='SDO'
     'HMI': instr='SDO'
     'IFA': instr='NORH'
     'IFS': instr='NORH'
     'IFZ': instr='NORH'
      else: begin
              instr=strcompress(fxpar(header,'INSTRUME'),/rem)
              if instr eq '0' then instr=strcompress(fxpar(header,'TELESCOP'),/rem)
            end
  endcase
  case instr of
    'OVSA':BEGIN
      catch, error_stat
      if error_stat ne 0 then begin
        catch, /cancel
        MESSAGE, /INFO, !ERROR_STATE.MSG
        err:
        info={npol:1,nfreq:1,ntim:1,offset:[0.0,0.]}
        index={header:header}
        goto, skip
      end
      info=mrdfits(filename,'info',/silent)
      if size(info,/tname) eq 'STRUCT' then begin
        index=mrdfits(filename,'index',/silent)
        skip:
        map=wimagr_index2map(data,index,info)
        if keyword_set(reform) then map_arr=reform(map_arr,info.npol,info.nfreq,info.ntim)
        header=index.header
      endif else goto,err
    END
    'EOVSA': vla_fits2map, filename, map
    'RHESSI':hsi_fits2map,filename,map
    'NORH': begin
             index=norh_img2idx(header)
             norh_index2map,index,data,map
             case instr_name of
              'IFA':map.ID='NORH I 17 GHz'
              'IFS':map.ID='NORH V 17 GHz'
              'IFZ':map.ID='NORH I 34 GHz'
              else:
             endcase 
            end
    'SDO': begin
              if !version.os_family eq 'Windows' then noshell=1 else noshell=0
              read_sdo, filename, index0, data0,noshell=noshell,/uncomp_delete,/comp_delete
              aia_prep, index0, data0, oindex, odata
              index2map, oindex, odata, map
           end
    else:fits2map,filename,map,header=header
  endcase
end