gxpath=getenv('SSW')+path_sep()+'packages'+path_sep()+'gx_simulator'
print,'Adding '+gxpath+' to the IDL path...'
add_path,gxpath,/expand
ebtel=gx_ebtel_path(gx_ebtel_path())
ebtel_ss=gx_ebtel_path(gx_ebtel_path(/ss),/ss)