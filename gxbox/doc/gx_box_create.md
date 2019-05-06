
 # GX_BOX_CREATE
 ## Description
 Creates the GX-simulator compatible box structure from SDO/HMI full disk fits files

 ## Output
   Returns GX-simulator compatible box structure
 ## Parameters
  * *file_field* - file name of the FIELD fits file
  * *file_inclination* - file name of the INCLINATION fits file
  * *file_azimuth* - file name of the AZIMUTH fits file
  * *file_disambig* - file name of the DISAMBIGuation fits file
  * *file_continuum* - file name of the continuum with removed limb darkening fits file
  * *center_arcsec* - center of the patch to be mapped into the base of the box in arcseconds
  * *size_pixx* - [nx, ny, nz] size of the resulting box in voxels
  * *dx_km* - voxel size in kilometers
 ## Keywords
  * *carrington* - set this keyword if the box center is given as carrington longitude and latitude in degrees
  * *CEA* - set this keyword to use the CEA projection for the base of the box
  * *TOP* - set this keyword to use the TOP VIEW projection for the base of the box
