;+
; NAME:
;     DEFPARMS
; PURPOSE:
;     Routine to set various "system variables."  These are like global
;     parameters, and once set, are visible to any program in the current
;     IDL session.  If this routine is called a second time, and the
;     parameters are already defined, it returns quietly.
; CATEGORY:
;     OVRO APC ANALYSIS
; CALLING SEQUENCE:
;     defparms
; INPUTS:
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
; ROUTINES CALLED:
;     os_family
; OUTPUTS:
; COMMENTS:
; SIDE EFFECTS:
;     Numerous system variables are set.
; RESTRICTIONS:
; MODIFICATION HISTORY:
;     Written 24-Sep-1997 by Dale Gary
;     18-Jan-1998  DG
;       Added scan and segment code system variables
;     10-Mar-1998  DG
;       Changed Windows default directories so that DBDIR is C:\STD\
;       and WORKDIR is C:\WORKING\.
;     16-Mar-1998  DG
;       Added !T_GSCAN, !T_FSCAN for Gain and Frequency calibration
;       scan types.
;     21-Mar-1998  DG
;       Added PRFILE entry in !DEFAULTS system variable, to keep track
;       of which preferences file is in effect.  Removed warning modal
;       dialog when no preferences file is found.
;     01-Nov-1998  DG
;       Added total power calibration segment
;     04-Mar-1999  DG
;       Added DLASCAN, DLCAL scan types, and MONITOR segment type
;     22-Jul-1999  DG
;       Added NCODRZERO scan type, and TPCAL, AMPCAL, and PHZCAL segment types
;     13-Nov-1999  DG
;       Added INDEX and EOF segments types
;     17-Nov-1999  DG
;       Added GPARM and TPUPD segment types
;     24-Nov-1999  DG
;       Added HSKEEP scan type
;     29-Dec-1999  DG
;       Added file/scan type !T_PTSCN (point scan)
;     11-Mar-2000  DG
;       Changed CPCLOG segment type to DILOGHIST (dialog history)
;     16-Apr-2000  DG
;       Fixed bug in definition of !DEFAULTS.CMDDIR (trailing slash is needed)
;     15-Aug-2000  DG
;       Changed pickfile() to dialog_pickfile().
;     05-Janb-2004 GN
;       Changed the default helpdir to c:\ssw\radio\ovsa\doc
;     15-Mar-2005  DG
;       Added SATOBS scan type.
;-

pro gx_defparms
  ;This is a local gx_simulator copy of the OVSA tree defparms routine

   ro = 1   ; Specifies read-only
   rw = 0   ; Specifies read-write (i.e., changeable)

   ; Array of environment variable names
   envarr = ['DATADIR','DBDIR','WORKDIR','HELPDIR','EPHEMDIR','WEBDIR','CMDDIR','STDFONT']

   ; Definition of {defaults} structure

   junk = {defaults,      $
              datadir:'', $  ; Directory containing data (.ARC) files
                dbdir:'', $  ; Directory containing parameter files
              workdir:'', $  ; Directory for output of working files
              helpdir:'', $  ; Directory containing the help files
             ephemdir:'', $  ; Directory containing ephemeris info
               webdir:'', $  ; Directory for web-related output
               cmddir:'', $  ; Dir containing non-IDL .BAT and .EXE files
                 font:'', $  ; Standard font to use for fixed-width text
               prfile:'',ebtel:'',ebtel_ss:''}

   ; First check whether this file has already been run.  If so, it
   ; resets the !DEFAULTS system variable to its default values and
   ; returns quietly without doing anything more.

   defsysv,'!DEFAULTS',EXISTS=exists

   ; Set everything to default values
   case !version.os_family of
   'unix':    defsysv,'!DEFAULTS',{defaults, $
               '/cdrom/', '/tmp/', './', '$IDL_DIR/help/', './ephem/',$
               './web/','./std/','','None','',''},rw
   'Windows': defsysv,'!DEFAULTS',{defaults, $
               'C:\working\','C:\parm\','C:\working\','C:\ssw\radio\ovsa\doc\',$
               'C:\ephem\','c:\web\','C:\std\','lucida console*14','None','',''},rw
    else: defsysv,'!DEFAULTS',{defaults, $
               '/cdrom/', '/tmp/', './', '$IDL_DIR/help/', './ephem/',$
               './web/','./std/','','None','',''},rw
   endcase

   ; Overwrite defaults for any entries that have environment variable set
   for i = 0, n_elements(envarr)-1 do begin
      env = getenv(envarr[i])
      if (env ne '') then !DEFAULTS.(i) = env
   endfor

   ; If !DEFAULTS already existed, return
   if (exists) then return

   ; Turn off math error messages (typically generated due to use of NaNs)
   !Except = 0

   ro = 1   ; Specifies read-only
   rw = 0   ; Specifies read-write (i.e., changeable)

   ; Try to find Preferences file in standard locations (for now, just
   ; the current directory)

   prfiles = findfile('*.prf',count=n)

   CASE n OF
      0: filename = 'default.prf'
      1: filename = prfiles(0)
      ELSE: BEGIN
         instr = ['More than one preferences (.prf) file was found.',$
                  'Please select the one you wish to use, or [Cancel]',$
                  'To use the default values.']
         ans = widget_message(instr,/info,/cancel)
         if(ans eq 'Cancel') then filename = 'default.prf' $
                             else filename = dialog_pickfile(filter='*.prf')
      END
   ENDCASE

   ; A Preferences file name was supplied, so try to open and read it

   if (filename ne 'default.prf') then begin
      err_stat = 0
      CATCH,err_stat
      if (err_stat eq 0) then begin
         str = ''
         openr,lun,/get_lun,filename
         readf,lun,str     & datadir = str
         readf,lun,str     & dbdir = str
         readf,lun,str     & workdir = str
         readf,lun,str     & helpdir = str
         readf,lun,str     & ephemdir = str
         readf,lun,str     & webdir = str
         readf,lun,str     & cmddir = str
         readf,lun,str     & font = str
         readf,lun,str     & ebtel = str
         readf,lun,str     & ebtel_ss = str

;         ; Use OS_FAMILY() routine to determine whether this is a
;         ; UNIX machine.  This is used chiefly to determine when
;         ; bytes must be swapped.
;         case os_family() of
;         'unix': begin
;               defsysv,'!UNIX',1,ro
;            end
;         'Windows': begin
;               defsysv,'!UNIX',0,ro
;            end
;         'vms': begin
;               defsysv,'!UNIX',0,ro
;            end
;         endcase
;
;         if (strupcase(os_family()) eq 'UNIX') then begin
;            defsysv,'!UNIX',1,ro
;         endif else begin
;            defsysv,'!UNIX',0,ro
;         endelse

         prfile = filename
         defsysv,'!DEFAULTS',{defaults, datadir, $
                                        dbdir,   $
                                        workdir, $
                                        helpdir, $
                                        ephemdir, $
                                        webdir, $
                                        cmddir, font, prfile,ebtel,ebtel_ss }
         close,lun
         free_lun,lun
         CATCH,/CANCEL
      endif else begin
         filename = 'default.prf'
         CATCH,/CANCEL
      endelse

   endif

   ; No preferences file was opened, so will use default preferences

;   if (filename eq 'default.prf') then !DEFAULTS.prfile = 'None'

      ; Find out whether this is a unix machine and set default
      ; preferences accordingly

      ; Removed this line 21-Mar-1998, to avoid interruption of startup.
      ;ans = widget_message(['Error opening Preferences file.',$
      ;                      'The DEFAULT values will be used.'],/info)
;      case os_family() of
;      'unix': begin
;            defsysv,'!UNIX',1,ro
;            defsysv,'!DEFAULTS', $
;              {defaults, '/cdrom/', '/tmp/', './', '$IDL_DIR/help/', 'None'},rw
;         end
;      'Windows': begin
;            ; Set standard defaults for APC at OVRO.
;            defsysv,'!UNIX',0,ro
;            defsysv,'!DEFAULTS', $
;              {defaults, 'C:\working', 'C:\std\', 'C:\working\', $
;                         'C:\rsi\idl50\help\', 'None'},rw
;         end
;      'vms': begin
;            defsysv,'!UNIX',0,ro
;            defsysv,'!DEFAULTS',$
;              {defaults,'DATA:','TEMP:','','HELP:', 'None'},rw
;         end
;      endcase

;   endif

   ; Set file/scan/segment types.  These are (readonly) system variables so
   ; that these definitions can be used in LAUNCHER, GETTASKS, and any other
   ; routines where needed, yet can be defined in this single location.

   ; The way this scheme works is:

   ; All file types have bit 0 (LSB) set, and there can be any number of
   ; file types, specified by having any bits higher than 2 set

   ; All scan types have bit 1 set, and there can be any number of
   ; scan types, specified by setting any bits higher than 2.

   ; Thus, any type that, when masked with !T_FILE, gives non-zero,
   ; is a file.  Bit 2 is reserved for future types (e.g. segments)

   ; Files
   defsysv, '!T_FILE',  "01,ro         ; A generic file
   defsysv, '!T_CFILE', "11,ro         ; A closed (collapsed) file
   defsysv, '!T_OFILE', "21,ro         ; An expanded (open) file

   ; Scans
   defsysv, '!T_SCAN' ,  "02,ro        ; A generic scan
   defsysv, '!T_CSCAN',  "12,ro        ; A closed (collapsed) scan
   defsysv, '!T_OSCAN',  "22,ro        ; An expanded (open) scan
   defsysv, '!T_GSCAN',  "42,ro        ; A Gain Calibration scan
   defsysv, '!T_FSCAN', "102,ro        ; A Frequency Calibration scan
   defsysv, '!T_SSCAN', "202,ro        ; A Solar Data scan
   defsysv, '!T_ASCAN', "402,ro        ; An Antenna Calibration scan
   defsysv, '!T_PSCAN',"1002,ro        ; A Phase Cal Data scan
   defsysv, '!T_TPSCN',"2002,ro        ; A Total Power Cal scan
   defsysv, '!T_CRSCN',"4002,ro        ; A Ctr Cal scan
   defsysv, '!T_DSCAN',"10002,ro       ; A Delay Center scan
   defsysv, '!T_PTSCN',"20002,ro       ; A Point scan
   defsysv, '!T_PUSCN',"40002,ro       ; A Peakup scan


   ; Segments
   defsysv, '!T_SEGM'  , "04,ro        ; A generic segment

   ; Array of types:
   defsysv, '!TYPES', ["01,"11,"21,"02,"12,"22,"42,"102,"202,"402,"1002,"2002,"4002,"04,"10002,"20002,"40002]

   ; Set scan and segment code types:
   defsysv, '!SCAN', {types, test:1, solar:2, fcal:3, gcal:4, $
      pcal:5, refapcal:6, reftpcal:7, pntcal:8, dlascan: 9, dlcal:10, $
      ncdiag:11, peakup:12, limpoint:13, squintcal:14, lunarcal:15, point:16, $
      ncodrzero:17, ctrcal:18, hskeep:19, satobs:20},ro
   defsysv, '!SEGM', {codes, header:1, config:2, obseq:3, traj:4, raparm:5, $
       diloghist:6, data:7, eos:8, ephem:9, monitor:10, encoder:11, scrdump:12, $
       tpcal:13, ampcal:14, phzcal:15, index:16, eof:17, gparm:18, tpupd:19, $
       dailyphz:20}

return
end
