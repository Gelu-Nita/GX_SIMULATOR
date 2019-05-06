# BASEMAP2FITS
## Description
Saves the result of the [PREPARE_BASEMAPS](prepare_basemaps.md) function into fits files.
## Params
* *basemap* - Structure ruturned by the PREPARE_BASEMAPS function
* *name_out* - String identifying the output files.
            E.g., if it is specified as name_out='AR001',
              the procedure appends the suffixes and extensions and writes
              the files:
  * 'AR001.Br.fits' - the radial component,
  * 'AR001.Bt.fits' - the latitudinal component,
  * 'AR001.Bp.fits' - the longitudinal component,
  * 'AR001.Ic.fits' - continuum intensity.
