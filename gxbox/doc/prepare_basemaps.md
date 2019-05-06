# PREPARE_BASEMAPS 
## Description
Generate magnetic field maps in Cylindrical Equal Area (CEA) or Top View projection from SDO/HMI magnetograms

## Output
Returns a structure containing the following fields:
* *Bp*  - longitudinal component of the magnetic field
* *Bt*  - latitudinal component of the magnetic field
* *Br*  - radial component of the magnetic field
* *Ic*  - Continuum intensity
* *WCS* - WCS structure describing the projection

## Params
* *file_field* - file name of the FIELD fits file
* *file_inclination* - file name of the INCLINATION fits file
* *file_azimuth* - file name of the AZIMUTH fits file
* *file_disambig* - file name of the DISAMBIGuation fits file
* *file_continuum* - file name of the continuum with removed limb darkening fits file
* *center_arcsec* - center of the patch to be mapped
* *size_pix* - size of the resulting CEA map in pixels
* *dx_km* - spatial resolution of the resulting  map in kilometers
## Keywords
* *WCS* - (optional input), WCS structure, describing the projection where to map field to
* *Carrington* - set this if the center of the patch is given as longitude and latitude (degrees) in carrington coordinate system
* *Top* - set this keyword to create the "Top view" map
