# GX_SIMULATOR
GX_Simulator is an interactive IDL widget application intended to provide a flexible tool that allows the user to generate spatially resolved radio and/or X-ray spectra. The object-based architecture of this application provides full interaction with local 3D magnetic field extrapolation models that may be embedded in a global coronal model. By the use of various mouse tools provided, the user is allowed to explore the magnetic connectivity of the model by generating magnetic field lines originating in user-specified volume voxels. Such lines may be selected to create magnetic flux tubes, which are further populated with user-defined analytical thermal/non thermal particle distribution models. By default, the application integrates IDL callable DLL and Shared libraries containing fast GS emission codes developed in FORTRAN and C++ based on the newly developed Fleishman–Kuznetsov approximation, and IDL X-ray codes developed by Eduard Kontar. However, the interactive interface allows interchanging these default libraries with any user-defined IDL or external callable codes designed to solve the radiation transfer equation in the same or other wavelength ranges of interest.

# Here are the steps needed to download the latest version of the package, including the linked external submodule dependencies

#### If not already installed on your system, download and install git for command line

https://git-scm.com/

#### Launch the Git Bash terminal

#### CD to your SSW/packages/ installation folder and issue the following sequence of commands

```bash
rm -rf gx_simulator
git clone https://github.com/Gelu-Nita/GX_SIMULATOR gx_simulator
cd gx_simulator
git submodule update --init --recursive --remote
```

#### Add GX_SIMULATOR to your SSW_INSTR list by editig the appropriate SSWIDL script

## Additional actions required on Mac systems
- [ ] if not already installed, install libomp via homebrew: 

```bash 
brew install libomp 
```

- [ ] add to your shell startup script (.cshrc) this instruction:

```idl
setenv,'CPATH=/opt/homebrew/opt/libomp/include/'
```
- [ ] if already existent, remove the previous version of RenderIrregular.so 
  
```bash 
rm /Users/#your name#/gx_binaries/RenderIrregular.so
```
- [ ] start an IDL session

```bash
sswidl
``` 
- [ ] verify that GX_SIMULATOR ia ready to be launched:

```idl
print, gx_libpath('grid')
```
- [ ] if succesfull, the console should display:

```idl
/Users/#your name#/gx_binaries/RenderIrregular.so
```

 


