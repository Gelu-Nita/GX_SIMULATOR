# GX_SIMULATOR
GX_Simulator is an interactive IDL widget application intended to provide a flexible tool that allows the user to generate spatially resolved radio and/or X-ray spectra. The object-based architecture of this application provides full interaction with local 3D magnetic field extrapolation models that may be embedded in a global coronal model. By the use of various mouse tools provided, the user is allowed to explore the magnetic connectivity of the model by generating magnetic field lines originating in user-specified volume voxels. Such lines may be selected to create magnetic flux tubes, which are further populated with user-defined analytical thermal/non thermal particle distribution models. By default, the application integrates IDL callable DLL and Shared libraries containing fast GS emission codes developed in FORTRAN and C++ based on the newly developed Fleishman–Kuznetsov approximation, and IDL X-ray codes developed by Eduard Kontar. However, the interactive interface allows interchanging these default libraries with any user-defined IDL or external callable codes designed to solve the radiation transfer equation in the same or other wavelength ranges of interest.

# Here are the steps needed to download the latest version of the package, including the linked external submodule dependencies

#### If not already installed on your system, download and install git for command line

https://git-scm.com/

#### Launch the Git Bash terminal

#### CD to the location where you want to copy the GX_Simulator package and issue the following sequence of commands

```bash
git clone https://github.com/Gelu-Nita/GX_SIMULATOR
cd GX_Simulator
git submodule update --init --recursive --remote
```

#### Delete the content of your SSW/packages/GX_Simulator folder and move there the content of the cloned GitHub GX_simulator folder created in the previous step 

