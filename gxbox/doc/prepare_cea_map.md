# PREPARE_CEA_MAP(obsolete)
## Description
Generate magnetic field maps in Cylindrical Equal Area (CEA) projection from SDO/HMI magnetograms
## Calling sequence

```idl
 res = prepare_cea_map(file_field, file_inclination, file_azimuth, file_disambig, center, sz, dx[, wcs = wcs][, /carrington)
```
## Returning value
Returns a structure containing the following fields
* *WCS* - WCS structure containing information about the map position and projection
* *Bp* - PHI component of the magnetic field
    * *Bt* - THETA component of the magnetic field
    * *Br* - RADIAL magnetic field component

## Parameters
*   *file_field* - file name of the FIELD fits file
*   *file_inclination* - file name of the INCLINATION fits file
     *   *file_azimuth* - file name of the AZIMUTH fits file
     *   *file_disambig* - file name of the DISAMBIGuation fits file
     *   *center_arcsec* - 2 element array, center of the patch to be mapped into CEA projection
     *   *size_pix* - 2 element array, size of the resulting CEA map in pixels
     *   *dx_deg* - spatial resolution of the resulting CEA map in heliographic degrees


## Keywords

- WCS - (optional input), WCS structure, describing the CEA projection where to map field to
- Carrington - set this keyword if the centre of the patch is given in degrees as Carrington longitude and latitude
