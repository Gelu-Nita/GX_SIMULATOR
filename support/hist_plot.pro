
;+
; NAME:
;     hist_plot
; PURPOSE:
;     To plot a histogram
; CATEGORY:
;     OVSA utils
; CALLING SEQUENCE:
;       hist_plot,x,y,_extra=_extra
; INPUTS:
;
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;
; ROUTINES
; OUTPUTS:
; COMMENTS:
; SIDE EFFECTS:
;
; RESTRICTIONS:
; MODIFICATION HISTORY:
;     Written 12-Jan-2006 by Gelu Nita
;
;-
function first,x
 return,x[0]
end

function last,x
 return,x[n_elements(x)-1]
end

pro hist_plot,x,y,xlog=xlog,yrange=yrange,over=over,fill=fill,_extra=extra
 if not keyword_set(yrange) then yrange=[0,1.1*max(y)]
 if keyword_set(xlog) then begin
  k=x[1]/x[0]
  startx=x
  endx=k*x
 endif else begin
  startx=x
  endx=x+x[1]-x[0]
 endelse
 if not keyword_set(over) then begin
  plot,startx,y,xlog=xlog,yrange=yrange,/ysty,/xsty,_extra=extra,/nodata,xrange=[first(startx),last(endx)]
 end
 xpoly=startx[0]
 ypoly=yrange[0]
 for i=0,n_elements(x)-1 do begin
  if keyword_set(fill) then begin
   xpoly=[xpoly,startx[i],endx[i]]
   ypoly=[ypoly,y[i],y[i]]
  endif else begin 
   oplot,startx[i]*[1,1],[yrange[0],y[i]],_extra=extra
   oplot,endx[i]*[1,1],[yrange[0],y[i]],_extra=extra
   oplot,[startx[i],endx[i]],y[i]*[1,1],_extra=extra
  endelse
 end
 if keyword_set(fill) then  polyfill,[first(xpoly),xpoly,last(xpoly),first(xpoly)],[yrange[0],ypoly,yrange[0],yrange[0]],_extra=extra
end