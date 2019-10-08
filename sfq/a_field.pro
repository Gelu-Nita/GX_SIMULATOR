;+
; :Description: a_field
;    This is a part of SFQ disambiguation code
;    Don't call this routine directly
;
;
; :Author: George rudenko(rud@iszf.irk.ru) and Sergey Anfinogentov (anfinogentov@iszf.irk.ru)
;-
function a_field,x0,x1,x2,set=s0,get=get
common com_a_field,s
if n_elements(get) gt 0 then if n_elements(s) gt 0 then return,s
if n_elements(s0) gt 0 then begin
s=s0
if n_elements(x0) le 0 then return,1
endif
if (where(tag_names(s.a) eq 'PROC'))(0) eq -1 then proc='b_spl' else proc=s.proc
if (where(tag_names(s.a) eq 'HALF'))(0) ne -1 then half=s.half
if (where(tag_names(s.a) eq 'VEC'))(0) ne -1 then vec=s.vec else vec=1
nt=1
if (where(tag_names(s.a) eq 'T1'))(0) ne -1 then nt=nt+1
if (where(tag_names(s.a) eq 'T2'))(0) ne -1 then nt=nt+1
if nt ne 3 then vec=0
if n_elements(half) gt 0 then proc='u_int'
a={x0:x0,x1:x1,x2:x2,qs:s.b.qs}
b={qs:s.a.qs}
u=SOL_crd(a,b,/crd)
pos=s.a.pos
t0=x0*0
if nt gt 1 then t1=t0
if nt gt 2 then t2=t0
if n_elements(pos) eq 4 then begin
  if strlowcase(s.a.qs.type) eq 'sbox' then ind =where( ((u.x0-pos(0))*(u.x0-pos(2)) le 0.) and ((u.x1-pos(1))*(u.x1-pos(3)) le 0.)) else $
  ind =where( ((u.x1-pos(0))*(u.x1-pos(2)) le 0.) and ((u.x2-pos(1))*(u.x2-pos(3)) le 0.))
  if (where(tag_names(u) eq 'VISIBLE'))(0) ne -1 then begin
    ind1=u.visible
    if (ind1(0) ne -1) and (ind(0) ne -1) then begin
      tmp=t0
      tmp(ind1)=1
      tmp(ind)=tmp(ind)+1
      ind=where(temporary(tmp) eq 2)
    endif
  endif
  if ind(0) ne -1 then begin
    
    if proc eq 'u_int' then begin
      if n_elements(half) le 0 then begin
        if (strlowcase(s.a.qs.type) eq 'box') or (strlowcase(s.a.qs.type) eq 'sbox') then begin
          t0(ind)= U_interpolate(u.x0(ind),u.x1(ind),POS=pos,arr=s.a.t0)
          if nt gt 1 then t1(ind)= U_interpolate(u.x0(ind),u.x1(ind),POS=pos,arr=s.a.t1)
          if nt gt 2 then t2(ind)= U_interpolate(u.x0(ind),u.x1(ind),POS=pos,arr=s.a.t2)
        endif else begin
          t0(ind)= U_interpolate(u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t0)
          if nt gt 1 then t1(ind)= U_interpolate(u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t1)
          if nt gt 2 then t2(ind)= U_interpolate(u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t2)
        endelse
      
      endif else begin
        if (strlowcase(s.a.qs.type) eq 'box') or (strlowcase(s.a.qs.type) eq 'sbox') then begin
          t0(ind)= U_interpolate(u.x0(ind),u.x1(ind),POS=pos,arr=s.a.t0,half=half)
          if nt gt 1 then t1(ind)= U_interpolate(u.x0(ind),u.x1(ind),POS=pos,arr=s.a.t1,half=half)
          if nt gt 2 then t2(ind)= U_interpolate(u.x0(ind),u.x1(ind),POS=pos,arr=s.a.t2,half=half)
        endif else begin
          t0(ind)= U_interpolate(u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t0,half=half)
          if nt gt 1 then t1(ind)= U_interpolate(u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t1,half=half)
          if nt gt 2 then t2(ind)= U_interpolate(u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t2,half=half)
      
        endelse
      
      
      endelse
    endif else begin
      if (strlowcase(s.a.qs.type) eq 'box') or (strlowcase(s.a.qs.type) eq 'sbox') then begin
        t0(ind)= b_spl(u.x0(ind),u.x1(ind),POS=pos,arr=s.a.t0)
        if nt gt 1 then t1(ind)= b_spl(u.x0(ind),u.x1(ind),POS=pos,arr=s.a.t1)
        if nt gt 2 then t2(ind)= b_spl(u.x0(ind),u.x1(ind),POS=pos,arr=s.a.t2)
      endif else begin
        t0(ind)= b_spl(u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t0)
        if nt gt 1 then t1(ind)= b_spl(u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t1)
        if nt gt 2 then t2(ind)= b_spl(u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t2)
      endelse
    endelse
    if vec then begin
      a1={x0:x0(ind),x1:x1(ind),x2:x2(ind),t0:t0(ind),t1:t1(ind),t2:t2(ind),qs:s.b.qs,ts:s.a.ts}
      b1={qs:s.b.qs,ts:s.b.ts}
      u1=SOL_crd(a1,b1,/fld)
      t0(ind)=u1.t0
      t1(ind)=u1.t1
      t2(ind)=u1.t2
    endif
    if (nt eq 3) and vec then return,{x0:x0,x1:x1,x2:x2,t0:t0,t1:t1,t2:t2,qs:s.b.qs,ts:s.b.ts}
    if (nt eq 3) and (vec ne 1) then return,{x0:x0,x1:x1,x2:x2,t0:t0,t1:t1,t2:t2,qs:s.b.qs}
    if (nt eq 2) then return,{x0:x0,x1:x1,x2:x2,t0:t0,t1:t1,qs:s.b.qs}
    if (nt eq 1) then return,{x0:x0,x1:x1,x2:x2,t0:t0,qs:s.b.qs,err:0}
endif else return,{err:1}
endif
;_________

ind =where( ((u.x1-pos(1))*(u.x1-pos(4)) le 0.) and ((u.x2-pos(2))*(u.x2-pos(5)) le 0.) and ((u.x0-pos(0))*(u.x0-pos(3)) le 0.))
if ind(0) ne -1 then begin
  
  if proc eq 'u_int' then begin
    t0(ind)= U_interpolate(u.x0(ind),u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t0)
    if nt gt 1 then t1(ind)= U_interpolate(u.x0(ind),u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t1)
    if nt gt 2 then t2(ind)= U_interpolate(u.x0(ind),u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t2)
  endif else begin
    t0(ind)= b_spl(u.x0(ind),u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t0)
    if nt gt 1 then t1(ind)= b_spl(u.x0(ind),u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t1)
    if nt gt 2 then t2(ind)= b_spl(u.x0(ind),u.x1(ind),u.x2(ind),POS=pos,arr=s.a.t2)
  endelse
  if vec then begin
    a1={x0:x0(ind),x1:x1(ind),x2:x2(ind),t0:t0(ind),t1:t1(ind),x2:t2(ind),qs:s.b.qs,ts:s.a.ts}
    b1={qs:s.b.qs,ts:s.b.ts}
    u1=SOL_crd(a,b,/fld)
    t0(ind)=u1.t0
    t1(ind)=u1.t1
    t2(ind)=u1.t2
  endif
  if (nt eq 3) and vec then return,{x0:x0,x1:x1,x2:x2,t0:t0,t1:t1,t2:t2,qs:s.b.qs,ts:s.b.ts}
  if (nt eq 3) and (vec ne 1) then return,{x0:x0,x1:x1,x2:x2,t0:t0,t1:t1,t2:t2,qs:s.b.qs}
  if (nt eq 2) then return,{x0:x0,x1:x1,x2:x2,t0:t0,t1:t1,qs:s.b.qs}
  if (nt eq 1) then return,{x0:x0,x1:x1,x2:x2,t0:t0,qs:s.b.qs,err:0}
endif else return,{err:1}

end
