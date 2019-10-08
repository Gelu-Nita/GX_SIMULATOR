;+
; :Description: sfq_disambig
;    Resolves 180-ambiguity of transverse magnetic field measurements.
;    Both partial  and full disk magnetograms are supported.  The data is automaticly processed in parts in case of large field of view
;    It is better to execute this code under 64 bit operating system with a lot of RAM.
;    This is of importance in case of large magnetograms.
;    To disambiguate SDO/HMI magnetogram in full resolution you need at least 4 Gb of RAM.
;    The default parameter set is optimized for Hinode SOT/SP magnetograms
;    While processing of HMI or SOLIS magnetograms, please, set "hmi" or "solis" keywords respectively.
;    
;    
;
; :Params:
;    bx_: in, out, required, type=fltarr(nx,ny)
;     Bx transversal component of the magnetic field (in - ambiguous, out - disambiguated). 
;    by_: in, out, required, type=fltarr(nx,ny)
;     By transversal component of the magnetic field (in - ambiguous, out - disambiguated). 
;    bz_: in, required, type=fltarr(nx,ny)
;     LOS component of the magnetic field
;    apos: in, required, type=fltarr(4)
;     magnetogram position in arcsec from disc center ([left-bottom X, left-bottom Y,right-top X,right-top Y])
;    Rsun: in,required, type=float
;     Solar radius in arcseconds
;
; :Keywords:
;    solis: in, optional
;     Set this keyword to use the parameter set optimized for solis magnetograms
;    hmi: in, optional
;     Set this keyword to use the parameter set optimized for hmi magnetograms
;    silent: in, optional
;     Set this keyword to toggle on silent mode
;     
;  :Examples:
;  sfq_disambig,bx,by,bz,pos,Rsun
; 
;  :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;
;When you publish your work using our code, please provide a reference to
;our paper "Very Fast and Accurate Azimuth Disambiguation of Vector Magnetograms"
;Rudenko, G. V. And Anfinogentov, S. A.:2014, Solar Physics, Volume 289, Issue 5, pp.1499-1516 

;-
pro sfq_disambig,bx_,by_,bz_,apos,Rsun,solis=solis,hmi=hmi,silent=silent, acute = acute
  time=systime(/sec)
  s=get_str_mag(bx_,by_,bz_,apos,Rsun)
  if  (((s.pos(2)-s.pos(0)) lt .5*s.qs.r) or (n_elements(bx_) lt (1024l * 1024l))) then begin
    s=sfq_frame(s,solis=solis,hmi=hmi,silent=silent, acute = acute)
    bx_=s.t1&by_=s.t2
    if not keyword_set(silent) then message,'Full SFQ disambiguation complete in '+strcompress(systime(/sec)-time,/remove)+' seconds',/info
    return
  endif
  pot=pex_bl(s,silent=silent)
  s=sfq_step1(s,pot, acute = acute)
  by=s.t1
  if n_elements(solis) le 0 then solis=0
  if keyword_set(hmi) then solis=1
  if solis ne 0 then solis=1
  if solis eq 1 then sfq_clean,by,bz,/mode,silent=silent else sfq_clean,by,bz,silent=silent
  s.t1=by
  s.t2=bz
  bx_=by&by_=bz
  if not keyword_set(silent) then message,'Full SFQ disambiguation complete in '+strcompress(systime(/sec)-time,/remove)+' seconds',/info
end
