;+
; Name: vla_readfits
; 
; Purpose: Read image data from a VLA fits file from CASA output (via vla_prep.py)
;
; Calling sequence:
;   vla_readfits, fitsfiles, index, [data, /silent, /quiet]
; 
; Restrictions:
;   if the input is multiple fits files, they should have the same dimensions. 
;   multiple input fits files are assumed to be at different times
;
; Outputs:
;   DATA: 3-d to 5-d data array -- x, y, [freq, pol, time]
;   INDEX: 1-d to 3-d structure array -- [freq, pol, time]
; 
pro vla_readfits, files0, index, data, $
    silent=silent, quiet=quiet, nodata=nodata, $
    outsize=outsize,strtemplate=strtemplate,$
    nocomment=nocomment,nohistory=nohistory,ext=ext

files=files0
nf=n_elements(files)
quiet=keyword_set(quiet)
silent=quiet or keyword_set(silent)

IF not file_exist(files[0]) THEN BEGIN
    gzp=strpos(files,'.gz')
    wgz=where(gzp GT 0,ngz,complement=wnogz)
    IF gzp[0] GT 0 THEN BEGIN
        IF not silent THEN message,files[0]+' not found; trying without .gz',/info
    files[wgz]=strmid(files[wgz],0,gzp[0]) 
    ENDIF ELSE BEGIN
    ; try with .gz
        IF not silent THEN message,files[0]+' not found; trying with .gz',/info
        files[wnogz]=files[wnogz]+'.gz'
    ENDELSE
ENDIF
        
; ---------------------- define the template structure ----------------
if not data_chk(strtemplate,/struct) then begin
    head=headfits(files(0),ext=ext)     ; pretty fast header-only read
    if (keyword_set(all_keywords)) then begin
    for i=1l,nf-1 do begin
        head2 = headfits(files(i),ext=ext)
        ss1 = where_arr( strmid(head2, 0, 8), strmid(head, 0, 8), /map_ss)
        ss2 = where(ss1 eq -1, nss2)    ;where head2 is not in head
        if (nss2 ne 0) then begin
        head = [head, head2(ss2)]
        end
    end
    end
    strtemplate=fitshead2struct(head,add_standard=add_standard, nofill=nofill,$
                               wcs=wcs, SILENT=silent, _extra=_extra)
endif

; --------------------- read all headers first -------------------------
info=replicate(strtemplate,nf)

ccnts=lonarr(nf)
hcnts=lonarr(nf)

comments='' & history=''
good=-1

for i=0l,nf-1 do begin

    IF not file_exist(files[i]) THEN BEGIN
        message,files[i]+' not found; skipping',/info
    help,i
    wait,1
        goto,next
    ENDIF
    
   head=headfits(files(i),ext=ext)           ; read header-only
   alls=lonarr(n_elements(head))+1               ; header map
   nonnull=strlen(strtrim(head,2)) ne 0          ; non-null map

;  ---------- seperate COMMENT and HISTORY records -------------
   coms=(strpos(head,'COMMENT') eq 0)        ; comment-only map
   hiss=(strpos(head,'HISTORY') eq 0)            ; history-only map
   comss=where(coms ,ccnt) & comss(0)=comss(0)>0  ; where COMMENT
   hisss=where(hiss,hcnt) & hisss(0)=hisss(0)>0  ; where HISTORY
   if not keyword_set(nocomments) then $
    comments=[temporary(comments),head(comss)]    ; append->output
   if not keyword_set(nohistory) then $
    history =[temporary(history), head(hisss)]    ; append->output
   ccnts(i)=ccnt & hcnts(i)=hcnt                 ; counter/pointer
;  ----------------------------------------------------------------

   head=head(where(alls and nonnull and $      
            (1-(coms*keyword_set(comsep))) and $ ; strip COMMENTS? (future use)
            (1-(hiss*keyword_set(hissep)))))     ; strip HISTORY?  (future use)

   if(total(strpos(head,'COMMENT') eq 0) eq 0) then $
    fxaddpar,head,'COMMENT','' ; force at least a blank COMMENT line (CED)

   if(total(strpos(head,'HISTORY') eq 0) eq 0) then $
    fxaddpar,head,'HISTORY','' ; force at least a blank HISTORY line (CED)

;  header->structure
   fits_interp,head,outstr,instruc=strtemplate, _extra=_extra ; convert to structure
   ;date_obs is intepreted wrong in fits_interp, use fitshead2struct output to fix it
   strtmp=fitshead2struct(head,add_standard=add_standard, nofill=nofill,$
                               wcs=wcs, SILENT=1, _extra=_extra)
   outstr.date_obs=strtmp.date_obs
   if tag_exist(outstr,'wcs_struct') then $
     outstr.wcs_struct = fitshead2wcs(outstr,_extra=_extra)
   info(i)=outstr   
   IF good[0] EQ -1 THEN good=i ELSE good=[good,i]
   next:
endfor

comments=comments(where(strlen(comments) gt 0) >0) ; Kill blank lines (CED)
history = history(where(strlen(history ) gt 0) >0) ; Kill blank lines (CED)

; --------------------------------------------------------------------

; -------------- Account for non-existent files ----------------------
info=info[good]
files=files0[good]
nf=n_elements(good)
; --------------------------------------------------------------------

; read primary header
dat=readfits(files[0],ind,noscale=noscale,silent=silent)
; check the data dimension of each file
sz=size(dat)

;force output to have time, frequency, and pol axes
if nf gt 1 then begin
    if sz[0] eq 2 then index=replicate(info[0],1,1,nf) 
    if sz[0] eq 3 then begin
        if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') $
            then index=replicate(info[0],sz[3],1,nf) ;data 3rd axis is frequency
        if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') $
            then index=replicate(info[0],1,sz[3],nf) ;data 3rd axis is pol, mov to after frequency 
    endif
    if sz[0] eq 4 then begin
        if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then $
            index=replicate(info[0],sz[3],sz[4],nf) ;freq is the 3rd axis
        if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then $
            index=replicate(info[0],sz[4],sz[3],nf) ;pol is the 3rd axis
    endif
    if sz[0] gt 4 then begin
        print, 'Currently the procedure only handles up to 4-dimension input fits'
        return
    endif
endif else begin
    if sz[0] eq 2 then index=info[0]
    if sz[0] eq 3 then begin
        if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then $
            index=replicate(info[0],sz[3],1)
        if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then $
            index=replicate(info[0],1,sz[3])
    endif
    if sz[0] eq 4 then begin
        if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then $
            index=replicate(info[0],sz[3],sz[4])
        if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then $
            index=replicate(info[0],sz[4],sz[3])
    endif
    if sz[0] gt 4 then begin
        print, 'Currently the procedure only handles up to 4-dimension input fits'
        return
    endif
endelse

; --------- read/populate the cube unless /nodata set -----------
nodata=keyword_set(nodata) or n_params() lt 3
if not nodata then begin                ; "not nodata" is bad grammer
;  ----- determine size of output array -----
    case n_elements(outsize) of
        0: outxy=[max(gt_tagval(info[0],/naxis1)),max(gt_tagval(info[0],/naxis2))]
        1: outxy=replicate(outsize,2)
        2: outxy=outsize
        else: outxy=outsize(0:1)          ; should not happen
    endcase
                                ; get representative image data type
                                ; Force reading of header to treat
                                ; UINT types correctly
    if nf gt 1 then begin
        if sz[0] eq 2 then data=make_array(outxy(0), outxy(1), 1, 1, nf, type=data_chk(dat,/type)) 
        if sz[0] eq 3 then begin
            if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then $
                data=make_array(outxy(0), outxy(1), sz[3], 1, nf, type=data_chk(dat,/type))
            if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then $
                data=make_array(outxy(0), outxy(1), 1, sz[3], nf,type=data_chk(dat,/type))
        endif
        if sz[0] eq 4 then begin
            if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then $
                data=make_array(outxy(0), outxy(1), sz[3], sz[4], nf, type=data_chk(dat,/type))
            if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then $
                data=make_array(outxy(0), outxy(1), sz[4], sz[3], nf, type=data_chk(dat,/type))
        endif
    endif else begin
        if sz[0] eq 2 then data=make_array(outxy(0), outxy(1), type=data_chk(dat,/type)) 
        if sz[0] eq 3 then begin
            if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then $
                data=make_array(outxy(0), outxy(1), sz[3], 1, type=data_chk(dat,/type))
            if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then $
                data=make_array(outxy(0), outxy(1), 1, sz[3], type=data_chk(dat,/type))
        endif
        if sz[0] eq 4 then begin
            if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then $
                data=make_array(outxy(0), outxy(1), sz[3], sz[4], type=data_chk(dat,/type))
            if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then $
                data=make_array(outxy(0), outxy(1), sz[4], sz[3], type=data_chk(dat,/type))
        endif
    endelse
;  ----------------------

;  loop though all files, read data and insert into output array
    if nf gt 1 then begin
       for i=0,nf-1 do begin
          dat0=readfits(files[i], ind0, noscale=noscale,silent=silent); read 2D, 3D, or 4D files
          if sz[0] eq 2 then begin
              index[i]=info[i]     ;
              data[*,*,0,0,i]=temporary(dat0)
          endif
          if sz[0] eq 3 then begin
              if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then begin 
                  index[*,0,i]=info[i]     ; insert file axis after frequency axis
                  data[*,*,*,0,i]=temporary(dat0)
              endif
              if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then begin
                  index[0,*,i]=info[i]
                  data[*,*,0,*,i]=temporary(dat0)
              endif
          endif
          if sz[0] eq 4 then begin
              if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then begin
                  index[*,*,i]=info[i]
                  data[*,*,*,*,i]=temporary(dat0)
              endif
              if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then begin
                  index[*,*,i]=info[i]
                  data[*,*,*,*,i]=temporary(transpose(dat0,[0,1,3,2]))
              endif
          endif
       endfor
    endif else begin
          dat0=readfits(files, ind0, noscale=noscale,silent=silent)
          if sz[0] eq 2 then data=temporary(dat0)
          if sz[0] eq 3 then begin
              if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then $
                  data[*,*,*,0]=temporary(dat0)
              if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then $ 
                  data[*,*,0,*]=temporary(dat0)
          endif
          if sz[0] eq 4 then begin
              if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then $ 
                  data[*,*,*,*]=temporary(dat0)
              if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then $ 
                  data[*,*,*,*]=temporary(transpose(dat0,[0,1,3,2]))
          endif
    endelse
endif else begin
    if nf gt 1 then begin
       for i=0,nf-1 do begin
          dat0=readfits(files[i], ind0, noscale=noscale,silent=silent); read 2D, 3D or 4D files
          if sz[0] eq 2 then index[0,0,i]=temporary(ind0)     ;
          if sz[0] eq 3 then begin
              if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'FREQ') then $ 
                    index[*,0,i]=temporary(ind0)
              if (tag_exist(info[0],'naxis3') and info[0].ctype3 eq 'STOKES') then $ 
                    index[0,*,i]=temporary(ind0)
          endif
          if sz[0] eq 4 then index[*,*,i]=temporary(ind0)
       endfor
    endif
endelse
    
end

