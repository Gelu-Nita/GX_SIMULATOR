function gx_chmp2grid, result,best=best
if ~isa(result) then begin
  message,'No input structure provided!',/info
  return,!null
endif
compile_opt idl2
 
 ;----------------------------------------------------------------------------
 a0=result.a
 b0=result.b
 
 objMetricsArr=objarr(n_elements(result),2)
 q_best=dblarr(n_elements(result),2)
 modFilesArr=strarr(n_elements(result),2)
 objMetricsArr[*,0]=result.RES_BEST_METRICS
 objMetricsArr[*,1]=result.CHI_BEST_METRICS
 modFilesArr[*,0]=result.res2_best_file
 modFilesArr[*,1]=result.chi2_best_file
 q_best[*,0]=result.Q_RES2_BEST
 q_best[*,1]=result.Q_CHI2_BEST
 
 a=a0[uniq(a0,sort(a0))]
 b=b0[uniq(b0,sort(b0))]
 chi2_img=(res2_img=(res_img=(chi_img=(q0_img=dblarr(n_elements(a),n_elements(b),2)*!values.d_nan))))
 obj_img=objarr(n_elements(a),n_elements(b),2)
 for k=0,1 do begin
   nmod=n_elements(result)
   R=(npix=(q0=(res=(res2=fltarr(nmod)))))
   chi2=res2
   chi=chi2
   modidx=0  
   smaxm=0
   d_res=1 
   q0=q_best[*,k]
  for i=0,nmod-1 do begin
   obj_metrics=objMetricsArr[i,k]
   
  ;      0 MAP:R ALLIGNED [-5.15,-0.74] (R= 0.98)GX Tb_I 17 GHz q=0.00266136*(B/q[1])^0/(L/q[2])^1.1
  ;      1 REF REMAPPED REFMAP NORH I 17 GHz
  ;      2 SDEV REMAPPED SDEV NORH I 17 GHz
  ;      3 ROI:NPIX ROI MASK (839/10000 pixels)
  ;      4 ROI:RES RESIDUAL (7815.44)
  ;      5 ROI:RES_NORM NORMALIZED RESIDUAL (-0.00154924)
  ;      6 ROI:RES2 SQUARED RESIDUAL (1.03101e+009)
  ;      7 ROI:RES2_NORM NORMALIZED SQUARED RESIDUAL (0.0960858)
  ;      8 ROI:CHI CHI (-0.00991784)
  ;      9 ROI:CHI2 CHI2 (63.5342)
  ;     metrics structure:
  ;       R - Pearson correlation coefficient
  ;       res_img= data_model - data_obs
  ;       res= total(res_img[mask_pix])
  ;       res_img_norm=res_img/data_obs
  ;       res_norm=total(res_img_norm[mask_pix])/n_mask_pix
  ;       res2_img=res_img^2
  ;       res2=total(res2_img[mask_pix])-res^2/n_mask_pix
  ;       res2_img_norm=res_img_norm^2
  ;       res2_norm=total(res2_img_norm[mask_pix])-res_norm^2
  ;       chi_img=res_img/data_sdev
  ;       chi=total(chi_img[mask_pix])/n_mask_pix
  ;       chi2_img=chi_img^2
  ;       chi2=total(chi2_img[mask_pix])/(n_mask_pix-n_free)-chi^2
      modI=obj_metrics->get(0,/map)
      R[i]=modI.roi_metrics      
      obsI=obj_metrics->get(1,/map)
      dx=obsI.xc-obsI.orig_xc
      dy=obsI.yc-obsI.orig_yc
      obsIsdev=obj_metrics->get(2,/map)        
  ;       0 MAP:R
  ;       1 REF
  ;       2 SDEV
  ;       3 ROI:NPIX
  ;       4 ROI:RES
  ;       5 ROI:RES_NORM
  ;       6 ROI:RES2
  ;       7 ROI:RES2_NORM
  ;       8 ROI:CHI
  ;       9 ROI:CHI2

      mod_dS=modI.dx*modI.dy
      npix[i]=obj_metrics->get(3,/roi_metrics)
      
      RES_NORM_MAP=obj_metrics->get(5,/map)
      res[i]=res_norm_map.roi_metrics
      
;      bad=where(RES_NORM_MAP.data eq 1,nbad,ncomp=ncomp)
;      if nbad gt 0 then RES_NORM_MAP.data[bad]=0
      
      RES2_MAP=obj_metrics->get(7,/map)
      res2[i]=RES2_MAP.roi_metrics
      
      CHI_MAP=obj_metrics->get(8,/map)
      chi[i]=CHI_MAP.roi_metrics
              
      CHI2_MAP=obj_metrics->get(9,/map)
      chi2[i]=CHI2_MAP.roi_metrics
     endfor

     for l=0,nmod-1 do begin
      ii=where(a0[l] eq a)
      jj=where(b0[l] eq b)
      res2_img[ii,jj,k]=res2[l]
      chi2_img[ii,jj,k]=chi2[l]
      res_img[ii,jj,k]=res[l]
      chi_img[ii,jj,k]=chi[l]
      q0_img[ii,jj,k]=q0[l]
      obj_img[ii,jj,k]=ObjMetricsArr[l,k]
     endfor
 end
 a_arr=a
 b_arr=b
 q_chi2_best=(q_res2_best=(chi2_best=(res2_best=reform(dblarr(n_elements(a),n_elements(b)),n_elements(a),n_elements(b)))))
 obj_chi2_best=(obj_res2_best=reform(objarr(n_elements(a),n_elements(b)),n_elements(a),n_elements(b)))
 for i=0,n_elements(a)-1 do begin
  for j=0,n_elements(b) -1 do begin
    res2_best[i,j]=min(reform(res2_img[i,j,*]),imin)
    q_res2_best[i,j]=q0_img[i,j,imin]
    obj_res2_best[i,j]=obj_img[i,j,imin]
    chi2_best[i,j]=min(reform(chi2_img[i,j,*]),jmin)
    q_chi2_best[i,j]=q0_img[i,j,jmin]
    obj_chi2_best[i,j]=obj_img[i,j,jmin]
  endfor
 end
 best_res2=min(res2_best,idx_res2)
 best_chi2=min(chi2_best,idx_chi2)
 if keyword_set(best) then begin
  idx_res2=array_indices(obj_res2_best,idx_res2)
  idx_chi2=array_indices(obj_chi2_best,idx_chi2)
  grid={a:a_arr[idx_res2[0]],b:b_arr[idx_res2[1]],idx_res2_best:idx_res2,idx_chi2_best:idx_chi2,$
  obj_res2_best:obj_res2_best[idx_res2],$
  res2_best:res2_best[idx_res2],q_res2_best:q_res2_best[idx_res2],$
  obj_chi2_best:obj_chi2_best[idx_chi2],$
  chi2_best:chi2_best[idx_chi2],q_chi2_best:q_chi2_best[idx_chi2]}
 endif else begin     
 na=n_elements(a)
 nb=n_elements(b)
 grid={a:a_arr,b:b_arr,idx_res2_best:idx_res2,idx_chi2_best:idx_chi2,$
  obj_res2_best:reform(obj_res2_best,na,nb),obj_chi2_best:reform(obj_chi2_best,na,nb),$
  res2_best:reform(res2_best,na,nb),q_res2_best:reform(q_res2_best,na,nb),$
  chi2_best:reform(chi2_best,na,nb),q_chi2_best:reform(q_chi2_best,na,nb)}
 endelse
 return,grid     
end

