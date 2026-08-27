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
;  gx_map_add_beam_from_fits
;
; OUTPUTS:
; COMMENTS:
; Intended to replace obsolete ovsa2map.
; After instrument-specific conversion, standard FITS beam keywords
; (BMAJ/BMIN/BPA) are copied onto the map when present and not already set
; (see gx_map_add_beam_from_fits.pro). All HDUs are scanned if the primary
; header lacks BMAJ/BMIN. EOVSA maps that already carry bmaj_bmin_bpa are
; left unchanged.
; SIDE EFFECTS:
; RESTRICTIONS:
;  If FITS header indicates a known instrument, th corresponding fits rading routine is called. 
;  Foer all other cases, the general purpose fits2map is called
; MODIFICATION HISTORY:
;     Written 03-Jan-2017 Gelu M. Nita
;     21-Aug-2026 Recover BMAJ/BMIN/BPA from FITS header when present;
;                 detect EOVSA from filename/extension; use fitsio shared lib
;
pro gx_fits2map,filename,map,reform=reform,header=header,loud=loud,_extra=_extra
  if n_elements(filename) eq 0 then filename=dialog_pickfile(filter='*.f*')
  if ~file_exist(filename) then return
  break_file, filename, disk_log, dir, fname, ext
  name3=strupcase(strmid(fname,0,3))
  data=mrdfits(filename,0,header,/silent)
  ; Keep a copy of the primary header for beam recovery if later paths replace HEADER
  beam_header = header
  case name3 of
     'AIA': begin
              ; Full AIA level products have INSTRUME; time-averaged / custom
              ; cutouts often do not — those must use generic fits2map.
              inst=strcompress(fxpar(header,'INSTRUME'),/rem)
              if inst eq '0' or inst eq '' then $
                inst=strcompress(fxpar(header,'TELESCOP'),/rem)
              if (strupcase(inst) eq 'SDO') or (strpos(strupcase(inst),'AIA') ge 0) then $
                instr='SDO' else instr=''
            end
     'HMI': begin
              inst=strcompress(fxpar(header,'INSTRUME'),/rem)
              if inst eq '0' or inst eq '' then $
                inst=strcompress(fxpar(header,'TELESCOP'),/rem)
              if (strupcase(inst) eq 'SDO') or (strpos(strupcase(inst),'HMI') ge 0) then $
                instr='SDO' else instr=''
            end
     'IFA': begin & instr='NORH' & instr_name=name3 & end
     'IFS': begin & instr='NORH' & instr_name=name3 & end
     'IFZ': begin & instr='NORH' & instr_name=name3 & end
     'EOV': instr='EOVSA'
      else: begin
              instr=strcompress(fxpar(header,'INSTRUME'),/rem)
              if instr eq '0' then instr=strcompress(fxpar(header,'TELESCOP'),/rem)
              ; Empty primary HDU (common for tile-compressed radio FITS): peek extensions
              if instr eq '0' or instr eq '' then begin
                catch, err_peek
                if err_peek ne 0 then begin
                  catch, /cancel
                endif else begin
                  fits_open, filename, fcb
                  for ih = 1, fcb.nextend do begin
                    fits_read, fcb, junk, hdu_hdr, /header_only, exten_no=ih
                    instr=strcompress(fxpar(hdu_hdr,'INSTRUME'),/rem)
                    if instr eq '0' or instr eq '' then $
                      instr=strcompress(fxpar(hdu_hdr,'TELESCOP'),/rem)
                    if instr ne '0' and instr ne '' then begin
                      header = hdu_hdr
                      beam_header = hdu_hdr
                      break
                    endif
                  endfor
                  fits_close, fcb
                  catch, /cancel
                endelse
              endif
              ; basename heuristics for EOVSA products named eovsa.*
              if (instr eq '0' or instr eq '') and strpos(strupcase(fname),'EOVSA') eq 0 then instr='EOVSA'
            end
  endcase
  if n_elements(instr_name) eq 0 then instr_name=name3
  ; Fall through to generic fits2map when instrument unknown / empty
  if instr eq '0' then instr=''
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
        if size(header,/tname) eq 'STRING' then beam_header = header
      endif else goto,err
    END
    'EOVSA': vla_fits2map, filename, map,_extra=_extra
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
              catch, err_sdo
              if err_sdo ne 0 then begin
                catch, /cancel
                if keyword_set(loud) then $
                  message, 'SDO/AIA prep failed ('+!error_state.msg+'); falling back to fits2map', /info
                goto, fits2map_generic
              endif
              if !version.os_family eq 'Windows' then noshell=1 else noshell=0
              read_sdo, filename, index0, data0,noshell=noshell,/uncomp_delete,/comp_delete
              aia_prep, index0, data0, oindex, odata
              index2map, oindex, odata, map
              ; prefer prepared index for any later keyword recovery
              header=oindex
              catch, /cancel
           end
    else:begin
           fits2map_generic:
           fits2map,filename,map,header=header
           if size(header,/tname) eq 'STRING' then beam_header = header
           ;this fix is needed because fits2map ignores these header keys, assigning instead 
           ;the RSUN, B0, L0 parameters corresponding to the Earth View perspective
           if size(map,/tname) eq 'STRUCT' then begin
             rsun_obs=fxpar(header,'RSUN_OBS')
             if rsun_obs ne 0 then add_prop,map,rsun=rsun_obs,/replace
             b0_obs=fxpar(header,'HGLT_OBS')
             if b0_obs ne 0 then add_prop,map,b0=b0_obs,/replace
             l0_obs=fxpar(header,'HGLN_OBS')
             if l0_obs ne 0 then add_prop,map,l0=l0_obs,/replace
           endif else if keyword_set(loud) then $
             message, 'fits2map failed for '+file_basename(filename), /info
           ;end fits2map fix
         end  
  endcase

  ; Recover standard radio beam keywords when present and not already on the map.
  ; Try current HEADER, then the stashed primary/beam header, then every other HDU.
  if size(map, /tname) eq 'STRUCT' then begin
    has_beam = tag_exist(map, 'a_beam') or tag_exist(map, 'b_beam') $
      or tag_exist(map, 'bmaj') or tag_exist(map, 'bmin') $
      or tag_exist(map, 'bmaj_bmin_bpa')
    if ~has_beam then begin
      gx_fits2map_try_beam, map, header, status=st, loud=loud
      if ~st then gx_fits2map_try_beam, map, beam_header, status=st, loud=loud
      if ~st then begin
        catch, err_hdu
        if err_hdu ne 0 then begin
          catch, /cancel
        endif else begin
          fits_open, filename, fcb
          for ih = 0, fcb.nextend do begin
            if st then break
            fits_read, fcb, junk, hdu_hdr, /header_only, exten_no=ih
            gx_fits2map_try_beam, map, hdu_hdr, status=st, loud=loud
          endfor
          fits_close, fcb
          catch, /cancel
        endelse
      endif
      if keyword_set(loud) and ~st then $
        message, 'no BMAJ/BMIN found in any HDU of '+file_basename(filename), /info
    endif else if keyword_set(loud) then $
      message, 'beam already on map (e.g. EOVSA bmaj_bmin_bpa)', /info
  endif else if keyword_set(loud) then $
    message, 'no map produced for '+file_basename(filename), /info
end
