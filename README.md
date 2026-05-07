# GX_SIMULATOR
GX_Simulator is an interactive IDL widget application intended to provide a flexible tool that allows the user to generate spatially resolved radio and/or X-ray spectra. The object-based architecture of this application provides full interaction with local 3D magnetic field extrapolation models that may be embedded in a global coronal model. By the use of various mouse tools provided, the user is allowed to explore the magnetic connectivity of the model by generating magnetic field lines originating in user-specified volume voxels. Such lines may be selected to create magnetic flux tubes, which are further populated with user-defined analytical thermal/non thermal particle distribution models. By default, the application integrates IDL callable DLL and Shared libraries containing fast GS emission codes developed in FORTRAN and C++ based on the newly developed Fleishman–Kuznetsov approximation, and IDL X-ray codes developed by Eduard Kontar. However, the interactive interface allows interchanging these default libraries with any user-defined IDL or external callable codes designed to solve the radiation transfer equation in the same or other wavelength ranges of interest.

## Software prerequisites needed to ensure full GX Simulator functionaliy:
- [ ] All OS platforms: IDL 8.2 or later
- [ ] All OS platforms: SSWIDL installed from https://www.lmsal.com/solarsoft/ with the following list of required instruments included: ontology vso sdo aia hessi chianti hxt spex xray norh gx_simulator
- [ ] Linux/Mac platforms: gcc 4.8 or later

# Here are the steps needed to download the package, including the linked external submodule dependencies

#### If not already installed on your system, download and install git for command line

https://git-scm.com/

#### Launch the Git Bash terminal

#### CD to your SSW/packages/ installation folder and issue the following sequence of commands

```bash
git clone https://github.com/Gelu-Nita/GX_SIMULATOR gx_simulator
cd gx_simulator
git submodule update --init --recursive
```

This checks out the submodule commits recorded by the GX_SIMULATOR repository. That is the recommended normal installation path because it makes the checkout reproducible.

If you want your local copy to follow the latest commits from each configured submodule branch, you can run:

```bash
git submodule update --init --recursive --remote
```

After using `--remote`, `git status` may show modified submodules. That is not necessarily an error. It means your local clone has moved one or more submodules to newer commits than the submodule pointers currently recorded upstream in GX_SIMULATOR. Normal users do not need to commit or push those pointer changes.

##### Developer/maintainer submodule synchronization

Committing updated submodule pointers is a repository maintenance operation. Only developers with write access to GX_SIMULATOR and the relevant submodule repositories should run the synchronization helper.

To update all submodules recursively and commit any changed nested and top-level submodule pointers locally:

```bash
bash tools/update-submodules-and-commit.sh
```

To also push the created submodule commits first, then push the top-level GX_SIMULATOR commit:

```bash
bash tools/update-submodules-and-commit.sh --push
```

If you only want GX_SIMULATOR to record updated top-level submodule pointers, and do not have write access to nested submodule repositories owned by others, use:

```bash
bash tools/update-submodules-and-commit.sh --top-level-only --dry-run
bash tools/update-submodules-and-commit.sh --top-level-only --push
```

This mode does not create commits inside submodule repositories. It updates only GX_SIMULATOR's direct submodules to their configured remote branches, restores nested submodules to the commits recorded by their parent repositories, and records top-level GX_SIMULATOR pointers to commits that are already available from the submodules' remotes.

For a report-only maintainer check, use:

```bash
bash tools/update-submodules-and-commit.sh --dry-run
```

Dry-run mode still runs `git submodule update --init --recursive --remote`, then reports the commits and pushes it would make without creating commits or pushing. The helper refuses to commit in a detached-HEAD submodule and refuses to create parent commits that point to submodule commits not available from a remote branch unless `--push` is used.

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

 
