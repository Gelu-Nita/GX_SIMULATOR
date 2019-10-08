;+
; :Description: example_hinode
;     SFQ disambiguation example.
;     
;
;
;
;
;  :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
pro example_hinode
  ;restore ambiguous data
  restore,'hinode.sav'
  ;retrieving  data dimensions
  nx=(size(bx))[1]
  ny=(size(by))[2]
  ;open window in Memory
  window,xsize=nx,ysize=ny
  ;writing ambiguous magnetograms to png files
  
  tvscl,bx
  write_png,'hinode_ambigbx.png',tvrd(true=1)
  tvscl,by
  write_png,'hinode_ambigby.png',tvrd(true=1)
  ;SFQ disambiguation---------
   sfq_disambig,bx,by,bz,pos,Rsun;,/silent
  ;writing disambiguation results to png files--
  tvscl,bx
  write_png,'hinode_disambigbx.png',tvrd(true=1)
  tvscl,by
  write_png,'hinode_disambigby.png',tvrd(true=1)
  ;----------------
end