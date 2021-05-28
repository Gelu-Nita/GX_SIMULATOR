# Step-by-step instructions for creating *GX Simulator* models

## An automatic pipeline routine

The `../ssw/packages/gx_simulator/demo/gx_fov2box` subfolder includes a  general purpose routine `gx_fov2box` that integrates a series of individual building block routines included in the `../ssw/packages/gx_simulator/gxbox` subfolder  and described its associated `../ssw/packages/gx_simulator/gxbox/demo` subfolder. 

The prototype of this IDL procedure is :

```idl
pro gxbox_creation_full,time, center_arcsec=center_arcsec, size_pix=size_pix, dx_km=dx_km, out_dir=out_dir, tmp_dir=tmp_dir, empty_box_only=empty_box_only, save_empty_box=save_empty_box, potential_only=potential_only, save_potential=save_potential, save_bounds=save_bounds, use_potential=use_potential, nlfff_only=nlfff_only, generic_only=generic_only, save_gxm=save_gxm, _extra=_extra
```

The `create_box_20160220.pro` routine included in the same folder calls  the`gx_fov2box` procedure using a series of hard-coded options that, if kept unchanged,  should generate the  sequence of actions described below.

###1. Downloading required HMI and optional AIA images

#### Routines called

[gx_box_download_hmi_data](../gxbox/doc/gx_box_download_hmi_data.md)

[gx_box_download_aia_data](../gxbox/doc/gx_box_download_aia_data.md)

####Required input

`time` -Any of the time string formats compatible with the `anytim` SSW  function, in this example '2016-02-20 17:00:00'

####Optional keyword parameters

`tmp_dir=path`- User defined directory in which a 'jsoc_cache' subfolder is created to store data to be downloaded from the JSOC server. If this path is not explicitly defined by the user, the operating system's temporary directory is used. If the user defined `tmp_dir` does not yet exist, it is automatically created.

`/euv` - Se this keyword to request AIA EUV (94A, 131A, 175A, 193A, 2011A, 304A, and 223A) images closest in time

`/uv` - Se this keyword to request AIA UV (1600A and 1700A) images closest in time

```markdown
NOTE: Requested data is download is attempted only if they do not already exist in the "..\tmp_dir\jsoc_cache\" repository 
```

Running  `create_box_20160220.pro`  with its hard-coded parameters should download and save the following fits  files.

```idl
;HMI data
'C:\jsoc_cache\2016-02-20\hmi.B_720s.20160220_170000_TAI.azimuth.fits' 
'C:\jsoc_cache\2016-02-20\hmi.B_720s.20160220_170000_TAI.disambig.fits'
'C:\jsoc_cache\2016-02-20\hmi.B_720s.20160220_170000_TAI.field.fits' 
'C:\jsoc_cache\2016-02-20\hmi.B_720s.20160220_170000_TAI.inclination.fits'
'C:\jsoc_cache\2016-02-20\hmi.Ic_noLimbDark_720s.20160220_170000_TAI.continuum.fits'
'C:\jsoc_cache\2016-02-20\hmi.M_720s.20160220_170000_TAI.magnetogram.fits'
;AIA EUV data
'C:\jsoc_cache\2016-02-20\aia.lev1_euv_12s.2016-02-20T165948Z.image.94.fits'
'C:\jsoc_cache\2016-02-20\aia.lev1_euv_12s.2016-02-20T165948Z.image.131.fits'
'C:\jsoc_cache\2016-02-20\aia.lev1_euv_12s.2016-02-20T165948Z.image.171.fits'
'C:\jsoc_cache\2016-02-20\aia.lev1_euv_12s.2016-02-20T165948Z.image.193.fits'
'C:\jsoc_cache\2016-02-20\aia.lev1_euv_12s.2016-02-20T165948Z.image.211.fits'
'C:\jsoc_cache\2016-02-20\aia.lev1_euv_12s.2016-02-20T165948Z.image.304.fits'
'C:\jsoc_cache\2016-02-20\aia.lev1_euv_12s.2016-02-20T165948Z.image.335.fits'
;AIA UV data
'C:\jsoc_cache\2016-02-20\aia.lev1_uv_24s.2016-02-20T165948Z.image.1600.fits'
'C:\jsoc_cache\2016-02-20\aia.lev1_uv_24s.2016-02-20T165948Z.image.1700.fits'

```

###2. Creating a *GX Simulator* compatible model box IDL structure

####Routine called

[gx_box_create](../gxbox/doc/gx_box_create.md)

####Required input parameters

`center_arcsec=[x,y]`- A 2-element array indicating the heliocentric coordinates in arcseconds corresponding to the center of the model box.

```markdown
NOTE: Set the \carrington keyword to indicate that the box center is provaided as Carrington coordinates, longitude and latitude in degrees.
```

`size_pix=[nx, ny, nz]`- A 3-element array indicating the number of volume elements (voxels) of the extrapolation box  in each Cartesian direction.

`dx_km=dx_km` - The voxel size in km, the same in each Cartesian direction. 

####Optional input or keyword parameters

`/cea`- Set this keyword to request Cylindrical Equal Area (CEA) projection to be used to project image data on the base of the Cartesian extrapolation box . 

`/top` - Set tis keyword to request TOP view projection to be used to project image data on the base of the Cartesian extrapolation box . 

```markdown
NOTE: CEA projection is used by default if no projection keyword is explicitely set
```

`out_dir=path`- User defined directory in which the output models must be saved. If this path is not explicitly defined by the user, the operating system's temporary directory is used. If the user defined `out_dir` does not yet exist, it is automatically created.

`/save_empty_box`- Set this keyword to save an intermediary box structure not yet filled with any magnetic field model

`/empty_box_only`- Set this keyword to stop the box creation pipeline after the empty box is created.  Setting this keyword automatically save the empty box, even if  the `/save_empty_box` is not explicitly set.

`_extra=_extra` - The IDL `_extra` keyword mechanism can be used to pass any additional keywords understood by the [gx_box_create](../gxbox/doc/gx_box_create.md) routine.

####Output data

The `create_box_20160220.pro` example should create at this stage of its execution  an output file named

```idl
'C:\gx_models\2016-02-20\hmi.M_720s.20160220_165811.E34N4CR.CEA.NONE.sav'
```
Such `*NONE.sav` file is a standard IDL `.sav` file that can be restored into a *GX Simulator* box structure that contains the geometrical information needed to place the model on the solar disk, the base CEA- or TOP View-projected vector magnetic field maps needed to perform potential or NLFFF extrapolations, as well as, subject of JSOC availability, the corresponding LOS HMI and user-selected AIA maps closest in time to the base magnetic field maps. The `NONE` filename suffix indicates that the magnetic field tags `Bx` , `Bx`, `Bz` are zero-filled except for the base layers from which the extrapolations are yet to be performed in the subsequent steps.

````markdown
Note: The users who prefer to produce their own magnetic field reconstruction may populate such empty .NONE.sav structures outside the provided production pipeline and subsequently import them in the GX Simulator framework.
````

 ### 3. Creating  a Potential field model

####Output data 

This stage of the model box creation pipeline replaces the field of the previously created empty box with a potential magnetic field solution obtained based on on the vertical magnetic component stored in the empty magnetic field cube. 

In addition to this **potential box** structure, the pipeline also produces a **bound box** structure, which is identical with the *potential box* structure except for the bottom box layer, in which the *Bx* and *By*  potential solutions are replaced by the *observed* transverse vector components. The **bound box**  structure is to be used as an input for the next stage of the model production pipeline, as described below.

#### Routine called

[gx_box_make_potential_field](../gxbox/doc/gx_box_make_potential_field.md)

#### Optional input or keyword parameters

`/save_potential` -Set this keyword to save the **potential box** structure to a default `*.POT.sav` file.  The `create_box_20160220.pro` example should create at this stage a file named

```idl
'C:\gx_models\2016-02-20\hmi.M_720s.20160220_165811.E34N4CR.CEA.POT.sav'
```
`/save_bounds` - Set this keyword to to save the **bound box** structure to a default `*BND.sav` file.  The `create_box_20160220.pro` example should create at this stage a file named

```idl
'C:\gx_models\2016-02-20\hmi.M_720s.20160220_165811.E34N4CR.CEA.BND.sav'
```
`/potential_only` - Set this keyword to stop the box creation pipeline at this point. 

```markdown
NOTE: If /potential_only keyword is set, the potential box structure is automatically saved to disk, even if the /save_potential keyword is not explicitely set. However, the /save_bounds keyword must be exlicitely set to also save the bound box structure.
```

`/use_potential`- **Set this keyword to skip the next stage of building a NLFFF model and continue with the potential box solution instead.** 

```markdown
NOTE: In the current implementation, if the operation system is not Windows, the NLFFF reconstruction stage is skipped regardless the use of the /use_potential keyword because the NLFFF optimization is implemented as a call to an external code that runs only on Windows OS. 
```

### 4. Creating a NLFFF field model

#### Routine called

[gx_box_make_nlfff_wwas_field]()

This is an IDL wrapper to external call of Weighted Wiegelmann NLFFF Field Reconstruction Method library. The external code is specifically compiled for being used on Windows OS only, (c) Alexey G. Stupishin, Saint Petersburg State University, 2017.

The NLFFF model box is **automatically** saved to disk after its completion. The `create_box_20160220.pro` example should create at this stage a file named

```idl
'C:\gx_models\2016-02-20\hmi.M_720s.20160220_165811.E34N4CR.CEA.NAS.sav'
```

The default  `.NAS.sav` filename extension indicates that the file contains and IDL structure representing the output of the **N**LFFF reconstruction method implemented by **A**lexey **S**tupishin.

#### Optional keywords

`/nlfff_only` - Set this keyword to stop the production pipeline after this step.

### 5. Adding coronal heating parameters to the magnetic field model

As described bin [Nita et al. 2018](https://doi.org/10.3847/1538-4357/aaa4bf), the current version of  the *GX Simulator* may add on top of the magnetic model a coronal heating model parametrized for each volume element in terms of the length *L* of a magnetic field intersecting it and the averaged total magnetic field *B* computed along such field line. At this stage of the model production pipeline, these parameters are computed for every voxel in the volume. 

```markdown
NOTE: Depending on the box size and the computer capablilities, this stage may be the most time consuming step of the model creation pipeline.
```

The output of this model production stage is automatically saved to a file name having the default extension `.GEN.sav`, which indicates that the file contains a **generic magnetic and coronal model**.  

On a Windows system, the `create_box_20160220.pro` example should create at this stage a file named

```idl
'C:\gx_models\2016-02-20\hmi.M_720s.20160220_165811.E34N4CR.CEA.NAS.GEN.sav'
```

On Linux/Unix or MAC systems the generic model is saved under the name 

```idl
'C:\gx_models\2016-02-20\hmi.M_720s.20160220_165811.E34N4CR.CEA.POT.GEN.sav'
```

which indicates that a **potential** magnetic field model has been passed to this stage by skipping the NLFFF optimization stage.

####Optional keywords

`/save_gxm` - Set this keyword to request saving the IDL box structure as an **gx_model** IDL object. If created, such **gx_model** files have the default extension  **.gxm**, instead of the  **.sav** extension.

```markdown
NOTE: The production pipeline generates such object to take advantage of its methods for computing the B and L parameters but, by default, destroys it after transferring its volume properties to the box structure. The /save_gxm keyword is provided for just for the convenience f an user that prefers to open such model directly in the GX Simulator tool, instead of importing and converting the IDL structure to the object representation internally used by the simulator.
```

`/generic_only` - Set this keyword to stop the production pipeline after this stage.

###7. Adding a chromosphere model to the bottom of the coronal model

As described bin [Nita et al. 2018](https://doi.org/10.3847/1538-4357/aaa4bf), the bottom layers of the uniformly spaced magnetic field model,, from photosphere up to the transition region height,  may be replaced by a non-uniform height chromosphere model fully determined by the input *magnetic* and *continuum*  intensity maps.

The output of this final stage of the model production pipeline is saved to disk using the default `CHR.sav` filename extension.

On a Windows system, the `create_box_20160220.pro` example should create at this stage a file named

```idl
'C:\gx_models\2016-02-20\hmi.M_720s.20160220_165811.E34N4CR.CEA.NAS.CHR.sav'
```

On Linux/Unix or MAC systems the generic model is saved under the name 

```idl
'C:\gx_models\2016-02-20\hmi.M_720s.20160220_165811.E34N4CR.CEA.POT.CHR.sav'
```

which indicates that a **potential** magnetic field model has been passed to this stage by skipping the NLFFF optimization stage.

```markdown
NOTE: The .CHR.sav (or .CHR.gxm) models are created and were fully tested by Nita et al. 2018 for the purpose of synthetizing radio and EUV emision from active region. Although flare modeling is in princople possible using this type of models, the users should be aware that some incompatibilities may exists.
We recommend at this time the use of .GEN., .NAS., or .POT. models for flare modeling, which use an unresolved chromospheric slab instead of the more sophisticated non-uniform CHR model. 
```

