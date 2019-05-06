PRO xray_cross_section, ee, Eph, Xray_CS
; mean atomic number
z=1.18

cs=fltarr(N_elements(Eph),N_elements(EE))

print,'Calculating cross-sections ........................ '

for i=0,N_elements(eph)-1 do begin
    for j=i,n_elements(ee)-1 do begin
    cs(i,j)=bremss_cross(ee(j),eph(i),Z,/NoElec)
    ;cs(i,j)=bremss_cross(ee(j),eph(i),Z,/NoElec)*511.
    ;Brm_BremCross, ee, eph(i), z, cross
    ;cs(i,*)=cross
    end
;print,'Photon energy =',eph(i)
;message, 'Photon energy calculated....'
end
print,'calculated .....................................OK'

;Jph=cs##(Fe*De)

Xray_CS=CS

END