
 # GX_BOX_DOWNLOAD_HMI_DATA
 ## Description
Downloads from JSOC all SDO/HMI files required to produce GX-simulator box. The function will return a structure containing the names of downloaded files.


 ## Parameters
* `time` - time of the magnetogram (any format recognised by `ANYTIM` function from SSW)
* `tmp_dir` - path to the directory, where FITS files will be saved (default: current directory)
 ## 