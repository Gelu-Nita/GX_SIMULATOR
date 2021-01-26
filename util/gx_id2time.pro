function gx_id2time,id
 if size(id,/tname) ne 'STRING' then return,!null
 time=(strsplit(id,'.',/extract))[2]
 return,strjoin([strmid(time,0,4),strmid(time,4,2),strmid(time,6,2)],'-')+' '+$
        strjoin([strmid(strmid(time,9),0,2),strmid(strmid(time,9),2,2),strmid(strmid(time,9),4,2)],':')
end