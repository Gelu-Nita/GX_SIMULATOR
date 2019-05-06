;+
  ; :Description:
  ;    Replaces the magnetic field in the box with the potential extrapolation.
  ;    Field at the lower level is not affected.
  ;
  ; :Params:
  ;    box  - GX-simulator compatible box structure, to hold the modified potential field solution 
  ;             that may be subsequenty used to perform NLFFF reconstruction 
  ;    pbox - GX-simulator compatible box structure to hold the potential field solution
  ;
  ;
  ; :Author: Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
  ; GN: Added pbox keywod to return, optionally, the full potential solution from fft_pot_open
  ;-
pro gx_box_make_potential_field, box,pbox

  sz = size(box.bx)
  bz0 = box.bz[*,*,0]
  field = fft_pot_open(bz0, sz[3])
  
  box.bx[*,*,1:*] = field.bx[*,*,1:*]
  box.by[*,*,1:*] = field.by[*,*,1:*]
  box.bz[*,*,1:*] = field.bz[*,*,1:*]

  expr = stregex(box.id,'(.+)\.([A-Z]+)',/subexpr,/extract)  
  box.id = expr[1] + '.BND'
  if arg_present(pbox) then begin
    pbox=box
    pbox.bx=field.bx
    pbox.by=field.by
    pbox.bz=field.bz
    pbox.id = expr[1] + '.POT'
  endif

end