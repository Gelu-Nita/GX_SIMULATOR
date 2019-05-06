function bremss_cross,Eel,Eph,Z,NoElw=NoElw,NoElec=NoElec
;
;  Program to calculate the differential electron-ion Bremsstrahlung cross
;  section according to equation 3BN of Koch & Motz 1959, Rev. Mod. Phys.,
;  31,920.
;
;  INPUTS:
;    Eel - Incident electron kinetic energy (keV) - scalar
;
;    Eph - Photon energy (keV) - scalar
;
;    Z   - Atomic number of target
;
;  OUTPUTS:
;    Bremss_cross - The differential electron-ion bremsstrahlung cross section, in cm^2/keV
;
;  KEYWORDS:
;    NoElw - Do not apply the Elwert correction for collisional energy loss
;            It is not expected that this keyword be set, except in "diagnostic"
;            situations where one wishes, for example, to compare the forms of
;            different cross-sections.
;    NoElec - Do not add the electron-electron cross-section.  If this keyword
;            is not set, then the routine will return the total cross-section for
;            a neutral plasma composed of ions and/or atoms of atomic number Z.
;            If the keyword _is_ set, then the routine returns the electron-ion
;            cross-section only.  For photon energies below ~100 keV, the difference
;            is of order 1% or less.
;
;  HISTORY:
;    25-Jul-2003 CMJ Written.
;    28-Jul-2003 CMJ Corrected beta typo and added Emslie fudge to give
;                    finite values at the high frequency limit.
;    29-Jul-2003 CMJ Changed from procedure to function call
;    29-Jul-2003 AGE Added documentation comments and changed return for
;                    Eph > Eel from warning message to zero value
;    31-Jul-2003 AGE Added documentation and changed some variables
;                    (e.g., ro^2 -> ro2) to speed up the program a little
;    28-Aug-2003 AGE Added electron-electron function call and keyword
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;     Check inputs for valid configuration
;
; make sure sufficient data has been specified
if n_params() lt 3 then begin
   print,'Warning: insufficient specification of input values: '
   print,'cross = Cross_3BN( Eel, Eph, Z, /NoElw, /NoElec)'
   retall
endif

; make sure the user has not requested zero photon or zero electron energy
if eph eq 0. then message,'Photon energy must be non-zero positive value'
if eel eq 0. then message,'Electron energy must be non-zero positive value'

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Cross=0.d0

if eph le eel then begin                 ; compute (non-zero) cross-section

; physical constants
mec2 = 510.9989d00                       ; Electron rest mass (keV)
ro2 = 7.941d-26                          ; classical e- radius squared (cm^2)
alpha=1.d0/137.036d0                     ; fine structure constant

; rescale to m_ec^2 units
k=Eph/mec2                               ; photon energy
Eo = Eel/mec2 + 1.                       ; initial e- total energy

; fix to handle high frequency (Eph = Eel) limit - avoids attempted evaluation
; of indeterminate quantities by setting Eph to a slightly smaller value

if (1.d0 + k) eq Eo then k = k/ 1.00001

; set up intermediate quantities
E = Eo - k                               ; final e- total energy (m_ec^2 units)
po = sqrt(Eo^2. - 1.d00)                 ; initial e- momentum (m_ec units)
p = sqrt(E^2. - 1.d00)                   ; final e- momentum (m_ec units)
L = 2.d00*alog((Eo*E + po*p -1.d00)/k)   ; auxiliary quantity
epso = alog((Eo + po)/(Eo - po))     ; auxiliary quantity
eps = alog((E + p)/(E - p))      ; auxiliary quantity

; Now, evaluate the cross-section (equation 3BN of Koch & Motz) in cm^2/m_ec^2
; units
Cross3bn = Z^2 * ro2 * p *alpha/(k * po) $
        * (4.d00/3.d00 - 2.*Eo*E*(p^2. + po^2.)/(p^2.*po^2.) + epso*E/po^3. $
        + eps*Eo/p^3. - eps*epso/(po*p) + L*((8.d00/3.d00)*Eo*E/(po*p) $
        + k^2.*(Eo^2.*E^2.+po^2.*p^2.)/(po^3.*p^3.) + k/(2.*po*p) $
        * ( (Eo*E+po^2.)/po^3.*epso - (Eo*E+p^2.)/p^3.*eps $
        + 2.*k*Eo*E/(p^2.*po^2.) ) ) )

; apply Elwert Coulomb correction unless "noelw" keyword has been set
if not keyword_set(noelw) then begin     ; apply Elwert correction
   betao = po / Eo
   beta = p / E
   Elw_corr = betao*(1.d00-exp(-2.d00*!pi*Z*alpha/betao)) / $
             (beta*(1.d00-exp(-2.d00*!pi*Z*alpha/beta)))
   Cross3bn = Cross3bn * Elw_corr
endif

; Divide by electron rest mass to get cross section in cm^2/keV units

Cross = Cross3bn / mec2

if not keyword_set(noelec) then cross=cross + Z*ee_bremcross(Eel,Eph)

; add electron-electron cross-section

endif

return, Cross

end
