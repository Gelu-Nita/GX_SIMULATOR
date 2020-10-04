pro dem_interpolate,n,t,dem,ddm,path=path,logtdem=logtdem,dem_run=dem_run,ddm_run=ddm_run,qrun=qrun,lrun=lrun,qarr=qarr,$
                    larr=larr,tr=tr,avgdem=avgdem,ss=ss,duration=duration,method=method,info=info,expert=expert,$
                    use_dem=use_dem,has_ddm=has_ddm
  if keyword_set(info) then goto,getinfo
  if n_elements(logtdem) eq 0 or n_elements(dem_run) eq 0 or n_elements(qrun) eq 0 or n_elements(lrun) eq 0 then begin
    if n_elements(path) eq 0 then path=gx_ebtel_path(ss=ss)
    restore,path,/verb
    has_ddm=(n_elements(ddm_cor_run) eq n_elements(dem_cor_run)) and (n_elements(ddm_cor_run) eq n_elements(dem_cor_run))
    use_ddm=keyword_set(has_ddm) and ~keyword_set(use_dem)
    if keyword_set(tr) then begin
      dem_run=dem_tr_run 
      if use_ddm then ddm_run=ddm_tr_run
    endif else begin
      dem_run=dem_cor_run
      if use_ddm then ddm_run=ddm_cor_run
    endelse
  end  
  if n_elements(larr) eq 0 then larr=0
  if n_elements(qarr) eq 0 then qarr=0
  t0=systime(/s)
  sz=size(qrun)
  x=reform(lrun[0,*])
  xloc=[VALUE_LOCATE( x, larr )]
  dlogt=logtdem[1]-logtdem[0]
  tdem=(10.^logtdem)
  sqrtdem=tdem^2
  ntdem=n_elements(tdem)
  ones=replicate(1,ntdem)
  n=fltarr(n_elements(larr))
  T=fltarr(n_elements(larr))
  use_ddm=keyword_set(has_ddm) and ~keyword_set(use_dem)
  if arg_present(dem) then dem=fltarr(n_elements(logtdem), n_elements(larr)) 
  if arg_present(ddm) and keyword_set(use_ddm)then ddm=fltarr(n_elements(logtdem), n_elements(larr)) 
  getinfo:
  default,avgdem,0
  case avgdem of
    1:begin
      method='Nearest Neighbor'+(keyword_set(expert)?' (loop)':'')
      if keyword_set(info) then return
      xmm=[-1,n_elements(x)-1]
      ymm=[-1,n_elements(qrun[*,0])-1]
      for k=0l, n_elements(larr)-1 do begin
        i1=xloc[k]
        if i1 eq xmm[0] or i1 eq xmm[1] then goto,skip1
        l=larr[k]
        q=qarr[k]
        j1=VALUE_LOCATE(qrun[*,i1], q)
        if j1 eq ymm[0] or j1 eq ymm[1] then goto,skip1
        j3=VALUE_LOCATE(qrun[*,i1+1], q)
        if j3 eq ymm[0] or j3 eq ymm[1] then goto,skip1

        ;          Corner DEMs listed here for reference
        ;          a1=dem_run[*,j1,i1]
        ;          a2=dem_run[*,j1+1,i1]
        ;          a3=dem_run[*,j3,i1+1]
        ;          a4=dem_run[*,j3+1,i1+1]

        dx1=(dx2=abs(x[i1]-l))
        dx3=(dx4=abs(x[i1+1]-l))
        dy1=abs(qrun[j1,i1]-q)
        dy2=abs(qrun[j1+1,i1]-q)
        dy3=abs(qrun[j3,i1+1]-q)
        dy4=abs(qrun[j3+1,i1+1]-q)

        if dx1 lt dx2 then begin
          if dy1 lt dy2 then begin
            dem_k=dem_run[*,j1,i1]
            if use_ddm then ddm_k=ddm_run[*,j1,i1]
          endif else begin
            dem_k=dem_run[*,j1+1,i1]
            if use_ddm then ddm_k=ddm_run[*,j1+1,i1]
          endelse
        endif else begin
          if dy3 lt dy4 then begin
            dem_k=dem_run[*,j3,i1+1]
            if use_ddm then ddm_k=ddm_run[*,j3,i1+1]
          endif else begin
            dem_k=dem_run[*,j3+1,i1+1]
            if use_ddm then ddm_k=ddm_run[*,j3+1,i1+1]
          endelse
        endelse
        if use_ddm then begin
          N[k]=alog(10.)*dlogt*total(ddm_k*tdem,/double)
          T[k]=total(ddm_k*sqrtdem,/double)/total(ddm_k*tdem,/double)
        endif else begin
          N[k]=sqrt(alog(10.)*dlogt*total(dem_k*tdem,/double))
          T[k]=total(dem_k*sqrtdem,/double)/total(dem_k*tdem,/double)
        endelse
        if arg_present(dem) then dem[*,k]=dem_k
        if arg_present(ddm) and use_ddm then ddm[*,k]=ddm_k
        skip1:
      endfor
    end 
    2:begin
      method='4-Neighbors Mean'+(keyword_set(expert)?' (loop)':'')
      if keyword_set(info) then return
      xmm=[-1,n_elements(x)-1]
      ymm=[-1,n_elements(qrun[*,0])-1]
      for k=0l, n_elements(larr)-1 do begin
        i1=xloc[k]
        if i1 eq xmm[0] or i1 eq xmm[1] then goto,skip2
        l=larr[k]
        q=qarr[k]
        j1=VALUE_LOCATE(qrun[*,i1], q)
        if j1 eq ymm[0] or j1 eq ymm[1] then goto,skip2
        j3=VALUE_LOCATE(qrun[*,i1+1], q)
        if j3 eq ymm[0] or j3 eq ymm[1] then goto,skip2

        ;          Corner DEMs listed here for reference
        ;          a1=dem_run[*,j1,i1]
        ;          a2=dem_run[*,j1+1,i1]
        ;          a3=dem_run[*,j3,i1+1]
        ;          a4=dem_run[*,j3+1,i1+1]

        dem_k=(dem_run[*,j1,i1]+dem_run[*,j1+1,i1]+dem_run[*,j3,i1+1]+dem_run[*,j3+1,i1+1])/4
        if use_ddm then begin 
          ddm_k=(ddm_run[*,j1,i1]+ddm_run[*,j1+1,i1]+ddm_run[*,j3,i1+1]+ddm_run[*,j3+1,i1+1])/4
          N[k]=alog(10.)*dlogt*total(ddm_k*tdem,/double)
          T[k]=total(ddm_k*sqrtdem,/double)/total(ddm_k*tdem,/double)
        endif else begin  
          N[k]=sqrt(alog(10.)*dlogt*total(dem_k*tdem,/double))
          T[k]=total(dem_k*sqrtdem,/double)/total(dem_k*tdem,/double)
        end
        if arg_present(dem) then dem[*,k]=dem_k
        if arg_present(ddm) and use_ddm then ddm[*,k]=ddm_k
        skip2:
      endfor
    end
    3:begin
      method='4-Neighbors Weighted Mean'+(keyword_set(expert)?' (vect.)':'')
      if keyword_set(info) then return
      xhist=histogram([xloc],loc=iloc,r=r)
      for k=0, n_elements(iloc)-1 do begin
        if (r[k] ne r[k+1]) and (iloc[k] gt -1) and (iloc[k] lt sz[2]-1) then begin
          i=[R[R[k] : R[k+1]-1]]
          qr1=qrun[*,iloc[k]]
          jloc1=value_locate(qr1,qarr[i])
          qr2=qrun[*,iloc[k]+1]
          jloc2=value_locate(qr2,qarr[i])
          good=where(jloc1 gt -1 and jloc1 lt sz[1]-1 and jloc2 gt -1 and jloc2 lt sz[1]-1,count)
          if count gt 0 then begin
            i=i[good]
            jloc1=jloc1[good]
            jloc2=jloc2[good]
            dem_i=dem_run[*,jloc1,iloc[k]]
            a2=dem_run[*,jloc1+1,iloc[k]]
            a3=dem_run[*,jloc2,iloc[k]+1]
            a4=dem_run[*,jloc2+1,iloc[k]+1]
            if use_ddm then begin
              ddm_i=ddm_run[*,jloc1,iloc[k]]
              b2=ddm_run[*,jloc1+1,iloc[k]]
              b3=ddm_run[*,jloc2,iloc[k]+1]
              b4=ddm_run[*,jloc2+1,iloc[k]+1]
            endif

            x1=(lrun[jloc1,xloc[i]])
            x3=(lrun[jloc2,xloc[i]+1])
            y1=qrun[jloc1,xloc[i]]
            y2=qrun[jloc1+1,xloc[i]]
            y3=qrun[jloc2,xloc[i]+1]
            y4=qrun[jloc2+1,xloc[i]+1]

            dx1=abs(x1-larr[i])
            dx3=abs(x3-larr[i])
            dy1=abs(y1-qarr[i])
            dy2=abs(y2-qarr[i])
            dy3=abs(y3-qarr[i])
            dy4=abs(y4-qarr[i])

            c1=dx3/((dy1+dy2)*(dx1+dx3))
            c2=dx1/((dy3+dy4)*(dx1+dx3))

            w1=(c1*dy2)
            w2=(c1*dy1)
            w3=(c2*dy4)
            w4=(c2*dy3)
            for c=0,count-1 do begin
              dem_i[*,c]=dem_i[*,c]*w1[c]+a2[*,c]*w2[c]+a3[*,c]*w3[c]+a4[*,c]*w4[c]
              if use_ddm then ddm_i[*,c]=ddm_i[*,c]*w1[c]+b2[*,c]*w2[c]+b3[*,c]*w3[c]+b4[*,c]*w4[c]
            endfor
            if count eq 1 then begin
              dem_i=reform(dem_i,ntdem,count)
              if use_ddm then ddm_i=reform(ddm_i,ntdem,count)
            endif
            if use_ddm then begin
              narr=(ddm_i##tdem)
              N[i]=alog(10.)*dlogt*narr
              T[i]=ddm_i##sqrtdem/narr
            endif else begin
              n2arr=(dem_i##tdem)
              N[i]=sqrt(alog(10.)*dlogt*n2arr)
              T[i]=dem_i##sqrtdem/n2arr
            end
            if arg_present(dem) then dem[*,i]=dem_i
            if arg_present(ddm) and use_ddm then ddm[*,i]=ddm_i
          endif
        endif
      endfor
    end
    
    4:begin
      method='Nearest Index Neighbor'+(keyword_set(expert)?' (loop)':'')
      if keyword_set(info) then return
      xmm=[-1,n_elements(x)-1]
      ymm=[-1,n_elements(qrun[*,0])-1]
      for k=0l, n_elements(larr)-1 do begin
        i1=xloc[k]
        if i1 eq xmm[0] or i1 eq xmm[1] then goto,skip4
        l=larr[k]
        q=qarr[k]
        j1=VALUE_LOCATE(qrun[*,i1], q)
        if j1 eq ymm[0] or j1 eq ymm[1] then goto,skip4
        dem_k=dem_run[*,j1,i1]
        if use_ddm then begin
          ddm_k=ddm_run[*,j1,i1]
          N[k]=alog(10.)*dlogt*total(ddm_k*tdem,/double)
          T[k]=total(ddm_k*sqrtdem,/double)/total(ddm_k*tdem,/double)
        endif else begin
          N[k]=sqrt(alog(10.)*dlogt*total(dem_k*tdem,/double))
          T[k]=total(dem_k*sqrtdem,/double)/total(dem_k*tdem,/double)
        end
        if arg_present(dem) then dem[*,k]=dem_k
        if arg_present(ddm) and use_ddm then ddm[*,k]=ddm_k
        skip4:
      endfor
    end
             
    5:begin
        method='4-Neighbors Mean'+(keyword_set(expert)?' (vect.)':'')
        if keyword_set(info) then return
        xhist=histogram([xloc],loc=iloc,r=r)
        for k=0, n_elements(iloc)-1 do begin
          if (r[k] ne r[k+1]) and (iloc[k] gt -1) and (iloc[k] lt sz[2]-1) then begin
            i=[R[R[k] : R[k+1]-1]]
            qr1=qrun[*,iloc[k]]
            jloc1=value_locate(qr1,qarr[i])
            qr2=qrun[*,iloc[k]+1]
            jloc2=value_locate(qr2,qarr[i])
            good=where(jloc1 gt -1 and jloc1 lt sz[1]-1 and jloc2 gt -1 and jloc2 lt sz[1]-1,count) 
            if count gt 0 then begin
              i=i[good]
              jloc1=jloc1[good]
              jloc2=jloc2[good]
              
              dem_i=dem_run[*,jloc1,iloc[k]]
              a2=dem_run[*,jloc1+1,iloc[k]]
              a3=dem_run[*,jloc2,iloc[k]+1]
              a4=dem_run[*,jloc2+1,iloc[k]+1]
              dem_i=(dem_i+a2+a3+a4)/4
              if count eq 1 then dem_i=reform(dem_i,ntdem,count)
              
              if use_ddm then begin
                ddm_i=ddm_run[*,jloc1,iloc[k]]
                b2=ddm_run[*,jloc1+1,iloc[k]]
                b3=ddm_run[*,jloc2,iloc[k]+1]
                b4=ddm_run[*,jloc2+1,iloc[k]+1]
                ddm_i=(ddm_i+b2+b3+b4)/4
                if count eq 1 then ddm_i=reform(ddm_i,ntdem,count)
                narr=(ddm_i##tdem)
                N[i]=alog(10.)*dlogt*narr
                T[i]=ddm_i##sqrtdem/narr
              endif else begin
                n2arr=(dem_i##tdem)
                N[i]=sqrt(alog(10.)*dlogt*n2arr)
                T[i]=dem_i##sqrtdem/n2arr 
              endelse
              
              if arg_present(dem) then dem[*,i]=dem_i
              if arg_present(ddm) and use_ddm then ddm[*,i]=ddm_i
            endif
          endif
         endfor   
      end
    
      6:begin
        ;this was the method used before 26 March 2020,
        method='4-Neighbors Weighted Mean'+(keyword_set(expert)?' (loop)':'')
        if keyword_set(info) then return
        xmm=[-1,n_elements(x)-1]
        ymm=[-1,n_elements(qrun[*,0])-1]
        for k=0l, n_elements(larr)-1 do begin
          i1=xloc[k]
          if i1 eq xmm[0] or i1 eq xmm[1] then goto,skip3
          l=larr[k]
          q=qarr[k]
          j1=VALUE_LOCATE(qrun[*,i1], q)
          if j1 eq ymm[0] or j1 eq ymm[1] then goto,skip3
          j3=VALUE_LOCATE(qrun[*,i1+1], q)
          if j3 eq ymm[0] or j3 eq ymm[1] then goto,skip3
          
          a1=dem_run[*,j1,i1]
          a2=dem_run[*,j1+1,i1]
          a3=dem_run[*,j3,i1+1]
          a4=dem_run[*,j3+1,i1+1]
          dx1=(dx2=abs(x[i1]-l))
          dx3=(dx4=abs(x[i1+1]-l))
          dy1=abs(qrun[j1,i1]-q)
          dy2=abs(qrun[j1+1,i1]-q)
          dy3=abs(qrun[j3,i1+1]-q)
          dy4=abs(qrun[j3+1,i1+1]-q)
          a12=(a1*dy2+a2*dy1)/(dy1+dy2)
          a34=(a3*dy4+a4*dy3)/(dy3+dy4)
          dem_k=(a12*dx3+a34*dx1)/(dx1+dx3)
          
          if use_ddm then begin
            b1=ddm_run[*,j1,i1]
            b2=ddm_run[*,j1+1,i1]
            b3=ddm_run[*,j3,i1+1]
            b4=ddm_run[*,j3+1,i1+1]
            dx1=(dx2=abs(x[i1]-l))
            dx3=(dx4=abs(x[i1+1]-l))
            dy1=abs(qrun[j1,i1]-q)
            dy2=abs(qrun[j1+1,i1]-q)
            dy3=abs(qrun[j3,i1+1]-q)
            dy4=abs(qrun[j3+1,i1+1]-q)
            b12=(b1*dy2+b2*dy1)/(dy1+dy2)
            b34=(b3*dy4+b4*dy3)/(dy3+dy4)
            ddm_k=(b12*dx3+b34*dx1)/(dx1+dx3)
            N[k]=alog(10.)*dlogt*total(ddm_k*tdem,/double)
            T[k]=total(ddm_k*sqrtdem,/double)/total(ddm_k*tdem,/double)
          endif else begin
            N[k]=sqrt(alog(10.)*dlogt*total(dem_k*tdem,/double))
            T[k]=total(dem_k*sqrtdem,/double)/total(dem_k*tdem,/double)
          endelse
          if arg_present(dem) then dem[*,k]=dem_k
          if arg_present(ddm) and use_ddm then ddm[*,k]=ddm_k
          skip3:
        endfor
      end
      else:begin
        method='Nearest Index Neighbor'+(keyword_set(expert)?' (vect.)':'')
        if keyword_set(info) then return
        xhist=histogram([xloc],loc=iloc,r=r)
        for k=0, n_elements(iloc)-1 do begin
          if (r[k] ne r[k+1]) and (iloc[k] gt -1) and (iloc[k] lt sz[2]-1) then begin
            i=[R[R[k] : R[k+1]-1]]
            qr=qrun[*,iloc[k]]
            jloc=value_locate(qr,qarr[i])
            good=where(jloc gt -1 and jloc lt sz[1]-1,count)
            if count gt 0 then begin
              i=i[good]
              jloc=jloc[good]
              dem_i=dem_run[*,jloc,iloc[k]]
              if use_ddm then ddm_i=ddm_run[*,jloc,iloc[k]]
              if count eq 1 then begin 
                dem_i=reform(dem_i,ntdem,count)
                if use_ddm then ddm_i=reform(ddm_i,ntdem,count)
              end 
              if use_ddm then begin
                narr=(ddm_i##tdem)
                N[i]=alog(10.)*dlogt*narr
                T[i]=ddm_i##sqrtdem/narr
              endif else begin
                n2arr=(dem_i##tdem)
                N[i]=sqrt(alog(10.)*dlogt*n2arr)
                T[i]=dem_i##sqrtdem/n2arr
              endelse
              if arg_present(dem) then dem[*,i]=dem_i
              if arg_present(ddm) and use_ddm then ddm[*,i]=ddm_i
            endif
          endif
        endfor
      end
  endcase
duration=systime(/s)-t0
end