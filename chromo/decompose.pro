function decompose, mag, cont
;uses CLOSEST function
;mag and cont are 2D image arrays

;cutoff_qs=get_cont_qs(mag, cont)
mag_qs=10  ;10 Gauss for QS
thr_plage=3 ;MF in plage is thr_plage times stronger than QS
sub=where(abs(mag) lt mag_qs,count)
cutoff_qs=total(cont[sub])/count
print, cutoff_qs, count


;all pxls in FOV including sunspots
pdf = HISTOGRAM(cont, LOCATIONS=xbin)
pdf = HISTOGRAM(cont(sub), nbins=n_elements(cont), LOCATIONS=xbin)
cutoff_b=xbin[CLOSEST(cdf,0.75)]
cutoff_f=xbin[CLOSEST(cdf,0.97)]

;exclude sunspots
sub=where(cont gt cutoff_qs*0.9,count)
pdf = HISTOGRAM(cont(sub), nbins=n_elements(cont), LOCATIONS=xbin)
cdf = TOTAL(pdf, /CUMULATIVE) / count
cutoff_b=xbin[CLOSEST(cdf,0.75)]
cutoff_f=xbin[CLOSEST(cdf,0.97)]
print, cutoff_b, cutoff_f

;creating decomposition mask
s=size(cont)
model_mask=intarr(s(1),s(2))
model_mask(*,*)=0
absmag=abs(mag)

;umbra
sub=where(cont le 0.65*cutoff_qs, n_umbra)
print, 'umbra: nelem= ',n_umbra,' abs(B) range: ',min(absmag(sub)), max(absmag(sub))
model_mask(sub)=7

;penumbra
sub=where(cont gt 0.65*cutoff_qs and cont le 0.9*cutoff_qs, n_penumbra)
print, 'penumbra: nelem= ',n_penumbra,' abs(B) range: ',min(absmag(sub)), max(absmag(sub))
model_mask(sub)=6



;enhanced NW
sub=where(cont gt cutoff_f and cont le 1.19*cutoff_qs, n_enw)
if n_enw ne 0 then begin 
model_mask(sub)=3
print, 'eNW: nelem= ',n_enw,' abs(B) range: ',min(absmag(sub)), max(absmag(sub))
end

;NW lane
sub=where(cont gt cutoff_b and cont le cutoff_f, n_nw)
if n_nw ne 0 then begin 
model_mask(sub)=2
print, 'NW: nelem= ',n_nw,' abs(B) range: ',min(absmag(sub)), max(absmag(sub))
end

;IN
sub=where(cont gt 0.9*cutoff_qs and cont le cutoff_b, n_in)
if n_in ne 0 then begin 
model_mask(sub)=1
print, 'IN: nelem= ',n_in,' abs(B) range: ',min(absmag(sub)), max(absmag(sub))
end


;plage
;sub=where(cont gt 1.19*cutoff_qs and cont le 1.43*cutoff_qs, n_plage)
sub=where(cont gt 0.95*cutoff_qs and cont le cutoff_f and abs(mag) gt thr_plage*mag_qs, n_plage)
if n_plage ne 0 then begin
  model_mask(sub)=4
  print, 'plage: nelem= ',n_plage,' abs(B) range: ',min(absmag(sub)), max(absmag(sub))
end

;facula
;sub=where(cont gt 1.43*cutoff_qs and cont le 1.8*cutoff_qs, n_facula)
sub=where(cont gt 1.01*cutoff_qs and abs(mag) gt thr_plage*mag_qs, n_facula)
if n_facula ne 0 then begin
  model_mask(sub)=5
  print, 'facula: nelem= ',n_facula,' abs(B) range: ',min(absmag(sub)), max(absmag(sub))
end


n_tot=n_in+n_nw+n_enw+n_plage+n_facula+n_penumbra+n_umbra
print, n_tot
print, n_elements(cont)

return,model_mask
end