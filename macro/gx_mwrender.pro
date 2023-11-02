function gx_mwrender,model,renderer,gxcube=gxcube,map=map,flux=flux,_extra=_extra
 gxcube=gx_render(model,renderer,_extra=_extra)
 if ~isa(gxcube) then goto,skip
 map=keyword_set(flux)?gx_mwcube2sfumaps(gxcube,map):gx_mwcube2tbmaps(gxcube,map)
 return,map
 skip:
 return,isa(map)?map:!null
end