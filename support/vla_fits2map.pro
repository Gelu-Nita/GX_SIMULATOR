;+
; Project     : VLA 
;
; Name        : VLA_FITS2MAP
;
; Purpose     : Convert a (temporal) array of 2-, 3-, or 4-D VLA fits file to
;               a map or 1-, 2-, or 3-D array of maps
;
; Category    :
;
; Syntax      : vla_fits2map, files, vla_maps
;
; Dependencies: vla_readfits.pro 
; Inputs      : files = a 2-, 3-, or 4-D VLA fits file or a temporal array of VLA fits files
;               the input fits files are organized in X, Y, frequency, and polarization
;
; Output      : hsi_map = array of maps.  If 3-D array of maps, organization is frequency, polarization, 
;               time
;
; Keywords    : nosep - If set, combine frequency, time, and polarization dimensions, i.e. return
;                 array dimensioned [nx,ny,nfreq*ntime*npol] instead of [nx,ny,nfreq,npol,ntime] 
;
; History     : Written by Bin Chen (bin.chen@njit.edu) based on hsi_fits2map.pro by P. T. Gallagher
;
; Modifications:
;           16-Feb-2016, Bin Chen - set roll_angle to zero for two-dimensional maps. Otherwise it takes -p_angle 
;           25-Apr-2025, Gelu Nita- replaced vla_readfits by read_sdo, which seems to work where vla_readfits fails
; Contact     : bin.chen@njit.edu
;-

pro vla_fits2map, files, map,_extra=_extra

;vla_readfits,files,index,data,/silent
read_sdo,files,index,data,/silent
n_dim = size( data, /n_dim )
;map stokes values in FITS to actual keys, reference: Greisen & Calabretta 2002, A&A, 395, 1061
stokesvals=[1,2,3,4,-1,-2,-3,-4,-5,-6,-7,-8]
stokeskeys=['I','Q','U','V','RR','LL','RL','LR','XX','YY','XY','YX']
case 1 of
    n_dim eq 2: begin
        index2map, index, data, map
        ;determine the frequency axis
        if (tag_exist(index,'crval3') and index.ctype3 eq 'FREQ') then begin 
            if tag_exist(index,'cunit3') and index.cunit3 eq 'Hz' then begin
                freq=index.crval3/1e9
                frequnit='GHz'
            endif
        endif
        if (tag_exist(index,'crval4') and index.ctype4 eq 'FREQ') then begin
            if tag_exist(index,'cunit4') and index.cunit4 eq 'Hz' then begin
                freq=index.crval4/1e9
                frequnit='GHz'
            endif
        endif
        ;determine the stokes axis
        if (tag_exist(index,'crval3') and index.ctype3 eq 'STOKES') then $
            stokesnum=index.crval3
        if (tag_exist(index,'crval4') and index.ctype4 eq 'STOKES') then $
            stokesnum=index.crval4
        ;check if the stokes value exist in the dictionary
        sind=where(stokesvals eq stokesnum,nsind)
        if nsind eq 1 then begin
            stokes=stokeskeys[sind[0]]
        endif else begin
            print, 'error when finding the stokes parameter!'
            return
        endelse
        ; get beam info (in arcsec)
        if  tag_exist(index,'bmaj') and $
            tag_exist(index,'bmin') and $
            tag_exist(index,'bpa') then begin
            bmaj=index.bmaj*3600.
            bmin=index.bmin*3600.
            bpa=index.bpa
            beam = string(bmaj, bmin, bpa, format='(F6.2,", ",F6.2,", ",F5.1)')
        endif


        add_prop, map, id = index.telescop 
        add_prop, map, freq = freq
        add_prop, map, frequnit = 'GHz' 
        add_prop, map, stokes = stokes
        add_prop, map, bmaj_bmin_bpa = beam
        map.roll_angle=0.
        map.id = index.telescop + ' ' + stokes + ' ' + strtrim(string(freq,format='(f6.3)'),2) + ' '+frequnit 
    end

    n_dim eq 3: begin
        ; determine the frequency axis
        index0=index[0]
        if (tag_exist(index0,'crval3') and index0.ctype3 eq 'FREQ') then begin 
            if tag_exist(index0,'cunit3') and index.cunit3 eq 'Hz' then begin
                freqs=index0.crval3/1e9+findgen(index0.naxis3)*index0.cdelt3/1e9
                frequnit='GHz'
            endif
        endif
        if (tag_exist(index0,'crval4') and index0.ctype4 eq 'FREQ') then begin 
            if tag_exist(index0,'cunit4') and index.cunit4 eq 'Hz' then begin
                freqs=index0.crval4/1e9+findgen(index0.naxis4)*index0.cdelt4/1e9
                frequnit='GHz'
            endif
        endif
        nfreqs = n_elements(freqs)
        ; determine the time axis
        time = index0.date_obs
        ; determine the stokes axis
        if (tag_exist(index0,'crval3') and index0.ctype3 eq 'STOKES') then $
            stokesnum=index0.crval3
        if (tag_exist(index0,'crval4') and index0.ctype4 eq 'STOKES') then $
            stokesnum=index0.crval4
        ;check if the stokes value exist in the dictionary
        sind=where(stokesvals eq stokesnum,nsind)
        if nsind eq 1 then begin
            stokes=stokeskeys[sind[0]]
        endif else begin
            print, 'error when finding the stokes parameter!'
            return
        endelse

        for i = 0, nfreqs - 1 do begin
             freq = freqs[i]
             ;compute xcen and ycen
             ind=index[i]
             get_fits_cen,ind,xcen,ycen
             dummy = make_map( data( *, *, i ), $
                       xc     = xcen, $
                       yc     = ycen, $
                       dx     = ind.cdelt1, $
                       dy     = ind.cdelt2, $
                       time   = anytim(time,/vms), $
                       id     = index.telescop + ' ' + stokes + ' ' + strtrim(string(freq,format='(f6.3)'),2) + frequnit,$ 
                       dur    = ind.exptime, $
                       freq  = freq , $
                       frequnit = 'GHz', $
                       xunits = 'arcsec', $
                       yunits = 'arcsec', $
                       stokes = stokes)

             if ( i eq 0 )  then begin
                map = replicate(dummy, nfreqs)
             endif else begin
                map[i] = dummy
             endelse
         endfor
       end

    n_dim eq 4: begin ;the third and fourth axes are frequency and stokes 
        ; determine the frequency axis
        index0=index[0,0]
        if (tag_exist(index0,'crval3') and index0.ctype3 eq 'FREQ') then begin 
            if tag_exist(index0,'cunit3') and index.cunit3 eq 'Hz' then begin
                freqs=index0.crval3/1e9+findgen(index0.naxis3)*index0.cdelt3/1e9
                frequnit='GHz'
            endif
        endif
        if (tag_exist(index0,'crval4') and index0.ctype4 eq 'FREQ') then begin 
            if tag_exist(index0,'cunit4') and index.cunit4 eq 'Hz' then begin
                freqs=index0.crval4/1e9+findgen(index0.naxis4)*index0.cdelt4/1e9
                frequnit='GHz'
            endif
        endif
        nfreqs = n_elements(freqs)
        ; determine the time axis
        time = index[0,0].date_obs
        ;determine the stokes axis
        if (tag_exist(index0,'crval3') and index0.ctype3 eq 'STOKES') then $
            stokesnums=index0.crval3+indgen(index0.naxis3)*index0.cdelt3
        if (tag_exist(index0,'crval4') and index0.ctype4 eq 'STOKES') then $
            stokesnums=index0.crval4+indgen(index0.naxis4)*index0.cdelt4
        nstokes=n_elements(stokesnums)
        stokes_=strarr(nstokes)
        ;check if the stokes value exist in the dictionary
        for s=0,nstokes-1 do begin
            stokesnum=stokesnums[s]
            sind=where(stokesvals eq stokesnum,nsind)
            if nsind eq 1 then begin
                stokes_[s]=stokeskeys[sind[0]]
            endif else begin
                print, 'error when finding the stokes parameter!'
                return
            endelse
        endfor
        
        ; Make map for all freqs and stokes
        for i = 0, nfreqs - 1 do begin
            freq = freqs[i]
            for j = 0, nstokes - 1 do begin
                 stokes = stokes_[j]
                 ind=index[i,j]
                 get_fits_cen,ind,xcen,ycen
                 dummy = make_map( data( *, *, i, j), $
                           xc     = xcen, $
                           yc     = ycen, $
                           dx     = ind.cdelt1, $
                           dy     = ind.cdelt2, $
                           time   = anytim(time,/vms), $
                           id     = ind.telescop + ' ' + stokes + ' '+ strtrim(string(freq,format='(f6.3)'),2) + frequnit , $
                           dur    = ind.exptime, $
                           freq  = freq , $
                           frequnit = frequnit, $
                           xunits = 'arcsec', $
                           yunits = 'arcsec',$
                           stokes = stokes)
                 if ( i eq 0 and j eq 0)  then begin
                    map = replicate(dummy, nfreqs, nstokes)
                 endif else begin
                    map[i,j] = dummy
                 endelse
            endfor
        endfor
    end

    n_dim eq 5: begin ;the 3rd, 4th, and 5th axes are frequency, stokes, and time 
        ; determine the frequency axis
        index0=index[0,0,0]
        if (tag_exist(index0,'crval3') and index0.ctype3 eq 'FREQ') then begin 
            if tag_exist(index0,'cunit3') and index.cunit3 eq 'Hz' then begin
                freqs=index0.crval3/1e9+findgen(index0.naxis3)*index0.cdelt3/1e9
                frequnit='GHz'
            endif
        endif
        if (tag_exist(index0,'crval4') and index0.ctype4 eq 'FREQ') then begin 
            if tag_exist(index0,'cunit4') and index.cunit4 eq 'Hz' then begin
                freqs=index0.crval4/1e9+findgen(index0.naxis4)*index0.cdelt4/1e9
                frequnit='GHz'
            endif
        endif
        nfreqs = n_elements(freqs)
        ; determine the time axis
        times = reform(index[0,0,*].date_obs)
        timesecs = anytim(times)
        ntimes = n_elements(times)
        ;determine the stokes axis
        if (tag_exist(index0,'crval3') and index0.ctype3 eq 'STOKES') then $
            stokesnums=index0.crval3+indgen(index0.naxis3)*index0.cdelt3
        if (tag_exist(index0,'crval4') and index0.ctype4 eq 'STOKES') then $
            stokesnums=index0.crval4+indgen(index0.naxis4)*index0.cdelt4
        nstokes=n_elements(stokesnums)
        stokes_=strarr(nstokes)
        ;check if the stokes value exist in the dictionary
        for s=0,nstokes-1 do begin
            stokesnum=stokesnums[s]
            sind=where(stokesvals eq stokesnum,nsind)
            if nsind eq 1 then begin
                stokes_[s]=stokeskeys[sind[0]]
            endif else begin
                print, 'error when finding the stokes parameter!'
                return
            endelse
        endfor

        ; Make map for all times, frequencies, and polarizations
        for i = 0, nfreqs-1 do begin
            freq=freqs[i]
            for j = 0, nstokes - 1 do begin
                stokes=stokes_[j]
                for k = 0, ntimes - 1 do begin
                     time=times[k]
                     ind=index[i,j,k]
                     get_fits_cen,ind,xcen,ycen
                     dummy = make_map( data( *, *, i, j, k), $
                               xc     = xcen, $
                               yc     = ycen, $
                               dx     = ind.cdelt1, $
                               dy     = ind.cdelt2, $
                               time   = anytim(time,/vms), $
                               id     = ind.telescop + ' ' + stokes + ' '+ strtrim(string(freq,format='(f6.3)'),2) + frequnit , $
                               dur    = ind.exptime, $
                               freq  = freq , $
                               frequnit = 'GHz', $
                               xunits = 'arcsec', $
                               yunits = 'arcsec',$
                               stokes = stokes)
                     if ( i eq 0 and j eq 0 and k eq 0)  then begin
                        map = replicate(dummy, nfreqs, nstokes, ntimes)
                     endif else begin
                        map[i,j,k] = dummy
                     endelse
                endfor
            endfor
        endfor
    end
endcase

end
