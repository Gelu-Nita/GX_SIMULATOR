pro gx_plot_label,xnorm,ynorm,label,ylog=ylog,xlog=xlog,xrange=xrange,yrange=yrange,_extra=_extra
default,xrange,!x.crange
default,yrange,!y.crange
x=min(xrange)+(max(xrange,min=min)-min)*xnorm
y=min(yrange)+(max(yrange,min=min)-min)*ynorm
if keyword_set(ylog) then y=10^y
if keyword_set(xlog) then x=10^x
xyouts,x,y,label,_extra=_extra
end