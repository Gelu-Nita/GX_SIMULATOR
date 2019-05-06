;+
; NAME:
;     gx_progmeter
; PURPOSE:
;     A widget that displays a progress meter with a color bar
;     that grows horizontally to indicate what percentage of a task has
;     been completed.  The percentage is also written as text.  The
;     window title can be set, and an optional cancel button with
;     settable text may be shown.
; CATEGORY:
;     OVRO SPAN UTILITY
; CALLING SEQUENCE:
;     id     = gx_progmeter(/INIT,[GROUP=group][,LABEL=label]$
;                         [,BUTTONTEXT=buttontext])
;     status = gx_progmeter(id,value)
;     status = gx_progmeter(id,/DESTROY)
; INPUTS:
;     id	the widget ID returned by a previous, initializing
;                  call to gx_progmeter (that used the /INIT switch).
;                  This input is ignored if INIT keyword is set.
;     value	the new value to set the widget to, ranging from
;                  0 to 1 (0=0% complete, 1=100% complete).
;                  This input is ignored if INIT keyword is set.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;     init      a keyword switch that governs which of two calling
;                  sequences is being used.  If set, a new widget is
;                  created.  If not set, an existing widget is to be
;                  updated.
;     destroy	a keyword switch that destroys the widget created by
;                  a previous call to gx_progmeter (that used the /INIT switch)
;     group     the top base id of the calling program, so that the widget
;                  will be destroyed if the calling program exits.
;                  This input is ignored if INIT keyword is not set.
;     label     an optional title for the progress meter window.  If
;                  omitted, the title "Percent Complete" is used.
;                  This input is ignored if INIT keyword is not set.
;     buttontext
;               an optional text for a button, such as a cancel button
;                  to interrupt the process whose progress is being shown.
;                  If omitted, no button is present.  If set and the user
;                  clicks on the button an event is generated.  To detect
;                  the event, call WIDGET_EVENT as discussed below.
;                  This input is ignored if INIT keyword is not set.
;     colorbar  an optional color for bar other than white
; ROUTINES CALLED:
; OUTPUTS:
;     id	the widget id of the compound widget (only when INIT
;                  keyword is set).
;     status	the status of the cancel button (only when INIT keyword
;                  is not set).  If the cancel button has been pressed,
;                  status='Cancel' and if not an empty string ('') is returned.
;                  If an error in calling sequence occurs (ID or VALUE
;                  is not supplied) then status='Cancel' also.  NB: If
;                  the widget is initialize without a button, status
;                  will always be an empty string ('')
; COMMENTS:
;     To use the routine, call it with the /INIT switch to create the
;     widget, then call it repeatedly without the /INIT switch to update
;     it.  When done with the widget, it should be destroyed.  Here is
;     an example:
;
;          id = gx_progmeter(/INIT,label='Progress Meter',button='Abort')
;          val = 0
;          for i = 0, n do begin
;             <Do something useful here>
;             val = i/(1.0*n)      ; Fraction of loop completed
;             if (gx_progmeter(id,val) eq 'Cancel') then goto,escape
;          endfor
;          escape: status = gx_progmeter(id,/DESTROY)
;
; SIDE EFFECTS:
; RESTRICTIONS:
;     The progress meter widget must be explicitly destroyed.
; MODIFICATION HISTORY:
;     Written 1-May-2017 by GN
;-

;-----------------------------------------------------------------
function gx_progmeter,id,value,INIT=init,DESTROY=destroy,GROUP=group,$
  LABEL=label,BUTTONTEXT=buttontext,color=color,$
  INPUT_TEXT=input_text
  COMPILE_OPT hidden
  COMMON managed, ids, $    ; IDs of widgets being managed
    names, $  ; and their names
    modalList ; list of active modal widgets
  if keyword_set(init) then begin
    if n_elements(names) gt 0 then top_idx=where(names eq 'gx_simulator',count) else count=0
    if count gt 0 then begin
      if widget_valid(ids[top_idx[0]]) then begin
        wStatusBar=widget_info(ids[top_idx[0]],find_by_uname='StatusBar')
        widget_control,widget_info(wStatusBar,/child),get_uvalue=oStatusBar
        oStatusBar->Start,action=label
      end else wStatusBar=0
    endif else wStatusBar=0
    return,wStatusBar
  endif
  
  if ~widget_valid(id) then begin
    if n_elements(names) gt 0 then top_idx=where(names eq 'gx_simulator',count) else count=0
    if count gt 0 then begin
      id=widget_info(ids[top_idx[0]],find_by_uname='StatusBar')     
    endif else id=0
  endif
  
  if widget_valid(id) then begin
     widget_control,widget_info(id,/child),get_uvalue=oStatusBar
     
     if keyword_set(init) then begin
      oStatusBar->Start,action=label
      return,id
     endif
    
    if keyword_set(destroy) then begin
      oStatusBar->Erase
      return,'Cancel'
    endif
   
    if n_elements(value) ne 0 then widget_control,id,set_value=value
    return,oStatusBar->AbortButton()
  endif else return,keyword_set(init)?id:''
end