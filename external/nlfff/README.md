# Magnetic-Field library

See [Licence](https://github.com/Alexey-Stupishin/Magnetic-Field_Library/blob/master/LICENCE.md)

[doi:10.5281/zenodo.3896222](https://zenodo.org/record/3896222)

The repository contains:
* sources of Magnetic Field Library (C++11)
* Windows .dll and Linux .so libraries
* projects for build under Windows and Linux
* wrappers for IDL calls

## Dependencies
IDL wrappers are designed to be used as subpackage of [GX-simulator](https://github.com/Gelu-Nita/GX_SIMULATOR) package, which included in [Solar Soft](http://www.lmsal.com/solarsoft/sswdoc/sswdoc_jtop.html) environment.

This repository contains submodule [common](https://github.com/Alexey-Stupishin/Common). Working with submodules can be found [here](https://git-scm.com/book/en/v2/Git-Tools-Submodules).

## Routines
* _/binaries/WWNLFFFReconstruction.dll_ - Calculation library for Windows
* _/binaries/WWNLFFFReconstruction.so_ - Calculation library for Linux (tested at Ubuntu 20.04.4 LTS, Linux 5.4.0-121-generic, x86-64)
* _/binaries/WWNLFFFReconstruction_x86-64.so_ - Calculation library for Linux (Mac Intel chip, x86-64)
* _/binaries/WWNLFFFReconstruction_arm64.so_ - Calculation library for Linux (Mac M1 chip, arm64)
* _/idl/gx_box_calculate_lines.pro_ - IDL wrapper to calculate megnetic field lines (see comments in the function)
* _/idl/gx_box_make_nlfff_wwas_field.pro_ - IDL wrapper to reconstruct non-linear force-free magnetic field (NLFFF) by T.Wiegelmann (weighted) method  (see comments in the function)
* _/idl/gx_box_field_library_version.pro_ - IDL wrapper to get library version info  (see comments in the function)
* _/sources_ - source codes (C++11)
* _/Windows_ - MSVC project for Windows (Visual Studio 2015)
* _/Linux_ - makefile (g++) for Linux

For NLFFF Weghted method please refer to:
Wiegelmann, T. Optimization code with weighting function for the reconstruction of coronal magnetic fields. _Solar Phys_., 2004, __219__, 87–108. ([doi:10.1023/B:SOLA.0000021799.39465.36](https://link.springer.com/article/10.1023/B:SOLA.0000021799.39465.36), [ADS:2004SoPh..219...87W](https://ui.adsabs.harvard.edu/abs/2004SoPh..219...87W/abstract), [arXiv:0802.0124](https://arxiv.org/abs/0802.0124))

Some proves and using of this library can be found in:

Fleishman, G., Anfinogentov, S., Loukitcheva, M., Mysh'yakov, I., Stupishin, A. Casting the Coronal Magnetic Field Reconstruction Tools in 3D Using the MHD Bifrost Model. _ApJ_, 2017, __839__, 30 ([doi:10.3847/1538-4357/aa6840](https://iopscience.iop.org/article/10.3847/1538-4357/aa6840), [ADS:2017ApJ...839...30F](https://ui.adsabs.harvard.edu/abs/2017ApJ...839...30F/abstract), [arXiv:1703.06360](https://arxiv.org/abs/1703.06360))

Stupishin, A., Kaltman, T., Bogod, V., Yasnov, L. Modeling of Solar Atmosphere Parameters Above Sunspots Using RATAN-600 Microwave Observations. _Solar Phys_, 2018, __293__, 13 ([doi:10.1007/s11207-017-1228-7](https://link.springer.com/article/10.1007/s11207-017-1228-7), [ADS:2018SoPh..293...13S](https://ui.adsabs.harvard.edu/abs/2018SoPh..293...13S/abstract))

Anfinogentov, S., Stupishin, A., Mysh’yakov, I., Fleishman, G. Record-breaking Coronal Magnetic Field in Solar Active Region 12673. _ApJL_, 2019, __880__, L29 ([doi:10.3847/2041-8213/ab3042](https://iopscience.iop.org/article/10.3847/2041-8213/ab3042), [ADS:2019ApJ...880L..29A](https://ui.adsabs.harvard.edu/abs/2019ApJ...880L..29A/abstract), [arXiv:1907.06398](https://arxiv.org/abs/1907.06398))

Fleishman, G., Mysh’yakov, I., Stupishin, A., Loukitcheva, M., Anfinogentov, S. Force-free Field Reconstructions Enhanced by Chromospheric Magnetic Field Data. _ApJ_, 2019, __870__, 101 ([doi:10.3847/1538-4357/aaf384](https://iopscience.iop.org/article/10.3847/1538-4357/aaf384), [ADS:2019ApJ...870..101F](https://ui.adsabs.harvard.edu/abs/2019ApJ...870..101F/abstract), [arXiv:1811.02093](https://arxiv.org/abs/1811.02093))

Fleishman, G., Anfinogentov, Stupishin, A., Kuznetsov, A., Nita, G. Coronal Heating Law Constrained by Microwave Gyroresonant Emission. _ApJ_, 2021, __909__, 89 ([doi:10.3847/1538-4357/abdab](https://iopscience.iop.org/article/10.3847/1538-4357/abdab1), [ADS:2021ApJ...909...89F](https://ui.adsabs.harvard.edu/abs/2021ApJ...909...89F/abstract), [arXiv:2101.03651](https://arxiv.org/abs/2101.03651))

## Update History
* 16 June 2020 - 1st release (v 2.1.20.428), [doi:10.5281/zenodo.3896223](https://zenodo.org/record/3896223#.Y13LRHZBxJQ)
* 04 October 2020 (v 2.1.20.1004, rev.363): _weight_bound_size_ key added to _gx_box_make_nlfff_wwas_field.pro_, see comment inside
* 25 January 2021 (v 2.2.21.125, rev.384): bug fixed (_extra parameters could prevent NLFFF in some cases)
* 18 February 2021 (v 2.3.21.217, rev.392):
	* bug fixed (it was crash when calculated with seeds)
	* improvement (all lines calculated, even if seed below chromo_level); but only part above chromo_level is stored
	* comment in _gx_box_calculate_lines.pro_ utility corrected, so that is less confusing
* 16 October 2022 (v 2.4.22.1016, rev.625):
	* small bug for short low loops fixed
	* small lines wrapper correction
	* Linux version started (at separate repository)
* 30 October 2022 (v 3.4.22.1025, rev.626): 
	* Major structure changes 
	* Significant code reorganization for multiplatforming, change implementation to C++11 standard
	* Linux version built
