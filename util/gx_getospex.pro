;+
; Name: gx_getospex
; 
; Purpose: Function to read an OSPEX results FITS file and return a structure containing photon
;   flux spectra, and corresponding times and energies suitable for import into GX_SIMULATOR. The OSPEX 
;   results file is written in the OSPEX package and contains the results of fitting solar models to 
;   observed data from a selected instrument.
;   Optionally writes a save file containing structure.
;   
; Calling sequence:  ref = gx_getospex(infile=infile [,outfile=outfile])
;  
; Sample call:  ref = gx_getospex(infile='ospex_results_30_oct_2017.fits', outfile='ospex_30_oct_2017.ref'
; 
; Input Keywords:
;  infile - name of OSPEX results FITS file (if not absolute path, assumes it's in current directory) (Required)
;  outfile - name of output save file to save ref structure in (Optional)
;  
; Output:  Returns structure with these tags:
;   t - midpoints of time bins in anytim format (seconds since 01-Jan-1979)
;   x - midoints of energy bins in keV
;   y - photon flux spectrum dimensioned [nenergy, 1, ntime] in photons / s / cm^2 / keV
;   
; Written: Kim Tolbert 05-Jun-2018
; Modifications:
; 
;-

function gx_getospex, infile=infile, outfile=outfile

if ~file_exist(infile) then begin
  message, /cont, 'Input file does not exist: ' + infile + ' Aborting.'
  return, -1
endif

  s = spex_read_fit_results(infile)
  ti = get_edges(s.spex_summ_time_interval, /mean)
  nt = n_elements(ti)
  en = get_edges(s.spex_summ_energy, /mean)
  nen = n_elements(en)
  flux = s.spex_summ_ph_model
  ref = { t: reform(ti, nt), x: en, y: reform(flux, nen, 1, nt) }

  if keyword_set(outfile) then save, file=outfile, ref

  return, ref

end