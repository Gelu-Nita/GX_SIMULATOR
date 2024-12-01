FUNCTION cw_strarr_event, event
 return,event
END

FUNCTION cw_strarr_get_value, id
 childs= widget_info(id, /all)
 data=[]
 for k=0,n_elements(childs)-1 do begin
  widget_control,widget_info(childs[k],/child), GET_VALUE=value
  data=[data,value]
 endfor
 return,data
END

PRO cw_strarr_set_value, id, value
 childs= widget_info(id, /all)
 for k=0,n_elements(childs)-1 do widget_control,widget_info(childs[k],/child), SET_VALUE=value[k]
END


FUNCTION cw_strarr, parent, value=value, labels=labels, editable=editable,xtextsize=xtextsize,xlabelsize=xlabelsize,_extra=_extra
  ; Validate inputs
  IF N_ELEMENTS(value) NE N_ELEMENTS(labels) THEN $
    MESSAGE, 'The number of values and labels must match.'

  IF N_ELEMENTS(editable) NE N_ELEMENTS(value) THEN $
    MESSAGE, 'The number of editable flags must match the number of values.'

  ; Create idbase widget
  base = widget_base(parent, /Column, /align_left,_extra=_extra,$
  EVENT_FUNC='CW_STRARR_EVENT',FUNC_GET_VALUE='CW_STRARR_GET_VALUE', PRO_SET_VALUE='CW_STRARR_SET_VALUE')
  
 
  ; Create widgets for each value
  FOR i = 0, N_ELEMENTS(value) - 1 DO BEGIN
    row = widget_base(base, /Row, /align_left)
    text_field = widget_text(row, VALUE=value[i], UVALUE={index: i},editable=editable[i],sensitive=editable[i],xsize=xtextsize)
    label = widget_label(row, VALUE=labels[i],xsize=xlabelsize)
  ENDFOR

  ; Attach the data structure to the base widget
  widget_control, base, SET_VALUE=value

  RETURN, base
END

pro example_cw_strarr_event, event
 widget_control,widget_info(widget_info(event.id,/parent),/child),get_value=value & print,value
 case widget_info(event.id,/uname) of
  'Close': widget_control, get_tlb(event.id),/destroy
  else:
 endcase
END


PRO example_cw_strarr
  ; Sample values, labels, and editable flags
  values = ['Value 1', 'Value 2', 'Value 3']
  labels = ['Label 1', 'Label 2', 'Label 3']
  editable = [1B, 0B, 1B]

  ; Create the base widget application
  tlb = widget_base(/Column, TITLE='CW_STRARR Example')

  ; Add the custom compound widget
  cw = cw_strarr(tlb, value=value, labels=labels, editable=editable)

  ; Add a button to close the application
  close_button = widget_button(tlb, VALUE='Close', UVALUE=tlb, UNAME='Close',/Align_Center)

  ; Realize the widgets
  widget_control, tlb, /Realize

  ; Enter the event loop
  XMANAGER, 'example_cw_strarr', tlb, /NO_BLOCK
END
