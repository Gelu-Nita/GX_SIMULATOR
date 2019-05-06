pro gx_bzrefmap,box
rb0p=(get_rb0p(box.date))
xy=hel2arcmin(box.lat,box.lon,date=box.date,rsun=rb0p[0],b0=rb0p[1],p=rb0p[2])*60
rsun=rb0p[0]
map_struct=make_map(box.bz[*,*,0],xc=xy[0],yc=xy[1],time=box.date,dx=box.dr[0]*rsun,dy=box.dr[1]*rsun)
map_struct=create_struct(map_struct,'rsun',rsun)
map=obj_new('map')
map->set,0,map=map_struct
box=create_struct(box,'refmaps',ptr_new([map]))
end