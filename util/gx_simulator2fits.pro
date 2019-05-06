; For now, GXL file has no date, time, or solar information (needs R_sun, RA, Dec)
; Switch CUBE, if set, causes output to be a data cube (3D fits file), otherwise
; each image results in a separate file.
PRO gx_simulator2fits,gxlfile,outfile,ra=ra,dec=dec,cube=cube

   default,ra,0.0
   default,dec,15.0
   ; Read .GXL file
   ;default,gxlfile,'GX_Simulator.log'
   IF ~file_exist(gxlfile) THEN BEGIN
      gxlfile=dialog_pickfile(Title='Please choose a GX Simulator log file to upload',filter='*.gxl')
      IF ~file_exist(gxlfile) THEN return
   END
   IF (n_elements(outfile) EQ 0) THEN BEGIN
      outfile = dialog_pickfile(Title='Please provide an output FITS file name',filter='*.fits',default_extension='fits')
      IF (outfile EQ '') THEN return
   ENDIF

   rec=MULTI_RESTORE(lun,file=gxlfile, header=header,/new,/verb)
   if strpos(header.renderer,'Xray.pro') ne -1 then xray = 1 else xray = 0
   stat=fstat(lun)
   ny=(stat.size-stat.cur_ptr)/n_tags(rec,/data)
;   parmdim=size(rec.parms,/dim)
;   parmdim=[parmdim[0],ny,parmdim[1:*]]
   datadim=size(rec.data,/dim)
   datadim[1]=ny
   row=lonarr(ny)
;   parms=make_array(parmdim,/float)
   data=make_array(datadim,/float)
   if xray then begin
      ; X-ray data have only three dimensions
      FOR i=0,ny-1 DO BEGIN
         rec=MULTI_RESTORE(lun,file=gxlfile)
         row=rec.row
         IF row LT ny THEN BEGIN
;            parms[*,row,*]=rec.parms
            data[*,row,*]=rec.data
         END
      END
   endif else begin
      ; Radio data have five dimensions (pol + mode-coupling)
      FOR i=0,ny-1 DO BEGIN
         rec=MULTI_RESTORE(lun,file=gxlfile)
         row=rec.row
         IF row LT ny THEN BEGIN
;            parms[*,row,*,*,*]=rec.parms
            data[*,row,*,*,*]=rec.data
         END
      END
   endelse   
   close,lun
   
   if xray then begin
      rdata = temporary(data)
   endif else begin
      ; Limit data to four dimensions (uses exact mode coupling)
      rdata = data[*,*,*,0,0]
      ldata = data[*,*,*,1,0]
      data = 0  ; Release memory
   endelse
   nf = n_elements(rdata[0,0,*])  ; Number of frequencies/energies
   ; Create FITS header
   if (keyword_set(cube)) then begin
      ; Case of multifrequency datacube
      fxhmake,h,rdata 
   endif else begin
      ; Case of single frequency
      fxhmake,h,rdata[*,*,1]
      fxaddpar,h,'NAXIS',3  ; Add a third axis (with only one entry)
      fxaddpar,h,'NAXIS3',1
   endelse
;   fxhmake,h,rdata
   fxaddpar,h,'OBJECT','SUN     ',' Source name'
   fxaddpar,h,'TELESCOP','Simulation'
   fxaddpar,h,'INSTRUME',''
   fxaddpar,h,'ORIGIN','GX Simulator'
   fxaddpar,h,'DATE-OBS','',' Obs start date YYYY-MM-DD'   ; Will be the date from GXL header, when available
   ; fxaddpar,h,'TIME-OBS','',' Obs start time HH:MM:SS.SSS'   ; Will be the time from GXL header, when available
   caldat,systime(/jul),mo,da,yr
   datestr = string(yr,mo,da,format='(I4,"-",I2.2,"-",I2.2)')
   fxaddpar,h,'DATE-MAP',datestr,' Last processing date YYYY-MM-DD'
   fxaddpar,h,'BSCALE',1,' REAL = TAPE * BSCALE + BZERO',format='E20.9'
   fxaddpar,h,'BZERO',0,format='E20.9'
   fxaddpar,h,'BUNIT','',' Units of flux' ; Will be the units from GXL header
   fxaddpar,h,'EPOCH',2000,format='E20.9'
   fxaddpar,h,'DATAMAX',max(rdata,/nan),format='E20.9'
   fxaddpar,h,'DATAMIN',min(rdata,/nan),format='E20.9'
   ; Axis 1, RA
   fxaddpar,h,'CTYPE1','RA---SIN'
   fxaddpar,h,'CRVAL1',0.0,' Degrees',format='E20.9'
   r_sun = 960 ; Nominal size of Sun in arcsec -- will be updated if/when gxl header has it
   rx = r_sun*(header.xrange[1]-header.xrange[0])/header.nx
   fxaddpar,h,'CDELT1',-rx/3600.,' Degrees per pixel',format='E20.9'
   fxaddpar,h,'CRPIX1',header.nx/2.,format='E20.9'
   fxaddpar,h,'CROTA1',0.0,format='E20.9'
   ; Axis 2, DEC
   fxaddpar,h,'CTYPE2','DEC--SIN'
   fxaddpar,h,'CRVAL2',15.0,' Degrees',format='E20.9'
   ry = r_sun*(header.yrange[1]-header.yrange[0])/header.ny
   fxaddpar,h,'CDELT2',ry/3600.,' Degrees per pixel',format='E20.9'
   fxaddpar,h,'CRPIX2',header.ny/2.,format='E20.9'
   fxaddpar,h,'CROTA2',0.0,format='E20.9'
   ; Axis 3, FREQ
   ; NB: This is NOT correct--we should instead use a frequency table, but
   ;     I am not sure that Miriad does the right thing with a data cube
   ;     with a frequency table...
   if xray then begin
      i = where(header.info.parms.name EQ 'Eph_min')
      fmin = header.info.parms[i].value  ; Start energy
      fxaddpar,h,'CTYPE3','ENERGY'
      fxaddpar,h,'CRVAL3',fmin,' keV',format='E20.9'
      i = where(header.info.parms.name EQ 'dEph') 
      df = header.info.parms[i].value
      flist = 10^(alog10(fmin) + findgen(nf)*df)  ; List of energies (logarithmically spaced)
   endif else begin
      i = where(header.info.parms.name EQ 'f_min')
      fmin = header.info.parms[i].value  ; Start frequency
      fxaddpar,h,'CTYPE3','FREQ'
      fxaddpar,h,'CRVAL3',fmin,' Hz',format='E20.9'
      i = where(header.info.parms.name EQ 'df')
      df = header.info.parms[i].value
      flist = 10^(alog10(fmin) + findgen(nf)*df)  ; List of frequencies (logarithmically spaced)
   endelse
   if (keyword_set(cube)) then begin
      ; If we are making a datacube, it is not possible to have a logarithmically
      ; spaced set of frequencies.  As a kludge, we will assume a linearly spaced
      ; set, spaced by (fmax-fmin)/nf
      fmax = flist(nf-1)
      deltaf = (fmax-fmin)/nf  ; Linear spacing
      if xray then fxaddpar,h,'CDELT3',df,' Log(keV)',format='E20.9' else fxaddpar,h,'CDELT3',deltaf,' Hz',format='E20.9'
   endif else begin
      ; Case of individual files.  CDELT3 does not matter in this case
      ; Just use 100 MHz
      fxaddpar,h,'CDELT3',1.e8,' Hz',format='E20.9'
   endelse
   fxaddpar,h,'CRPIX3',1.0,format='E20.9'
   fxaddpar,h,'CROTA3',0.0,format='E20.9'
   ; Axis 4, STOKES
   ; FITS Stokes Code Parameter Definition
   ;   1 I
   ;   2 Q
   ;   3 U
   ;   4 V
   ;  -1 RR
   ;  -2 LL
   ;  -3 RL
   ;  -4 LR
   ;  -5 XX
   ;  -6 YY
   ;  -7 XY
   ;  -8 YX
   if not xray then begin
      fxaddpar,h,'CTYPE4','STOKES'
      fxaddpar,h,'CRVAL4',-1.0,' RR',format='E20.9'
      fxaddpar,h,'CDELT4',1.0,format='E20.9'
      fxaddpar,h,'CRPIX4',1.0,format='E20.9'
      fxaddpar,h,'CROTA4',0.0,format='E20.9'
   endif
   fxaddpar,h,'HISTORY','Data Created by GX_SIMULATOR2FITS'
   
   dir = file_dirname(outfile,/mark)
   base = file_basename(outfile)
   if (keyword_set(cube)) then begin
      ; Write R file
      if xray then fxwrite,dir+'X'+base,h,rdata else fxwrite,dir+'r'+base,h,rdata
      if not xray then begin
         fxaddpar,h,'DATAMAX',max(ldata,/nan),format='E20.9'
         fxaddpar,h,'DATAMIN',min(ldata,/nan),format='E20.9'
         fxaddpar,h,'CRVAL4',-2.0,' LL',format='E20.9'
         ; Write L file
         fxwrite,dir+'l'+base,h,ldata
      endif
   endif else begin
      ; Write set of R files
      for i = 0, nf-1 do begin
         fxaddpar,h,'DATAMAX',max(rdata[*,*,i],/nan),format='E20.9'
         fxaddpar,h,'DATAMIN',min(rdata[*,*,i],/nan),format='E20.9'
         fxaddpar,h,'CRVAL3',flist[i],format='E20.9'
         if xray then begin
            fxwrite,dir+'X'+strcompress(strtrim(string(i+1,format='(I4)'),1)+base,/remove_all),h,rdata[*,*,i]
         endif else begin
            fxwrite,dir+'r'+strcompress(strtrim(string(i+1,format='(I4)'),1)+base,/remove_all),h,rdata[*,*,i]
         endelse
      endfor
      if not xray then begin
         ; Write set of L files
         fxaddpar,h,'CRVAL4',-2.0,' LL',format='E20.9'
         for i = 0, nf-1 do begin
            fxaddpar,h,'DATAMAX',max(ldata[*,*,i],/nan),format='E20.9'
            fxaddpar,h,'DATAMIN',min(ldata[*,*,i],/nan),format='E20.9'
            fxaddpar,h,'CRVAL3',flist[i],format='E20.9'
            fxwrite,dir+'l'+strcompress(strtrim(string(i+1,format='(I4)'),1)+base,/remove_all),h,ldata[*,*,i]
         endfor
      endif
   endelse
   
   return
END
