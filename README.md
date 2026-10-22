# GX_SIMULATOR
GX_Simulator is an interactive IDL widget application intended to provide a flexible tool that allows the user to generate spatially resolved radio and/or X-ray spectra. The object-based architecture of this application provides full interaction with local 3D magnetic field extrapolation models that may be embedded in a global coronal model. By the use of various mouse tools provided, the user is allowed to explore the magnetic connectivity of the model by generating magnetic field lines originating in user-specified volume voxels. Such lines may be selected to create magnetic flux tubes, which are further populated with user-defined analytical thermal/non thermal particle distribution models. By default, the application integrates IDL callable DLL and Shared libraries containing fast GS emission codes developed in FORTRAN and C++ based on the newly developed Fleishman–Kuznetsov approximation, and IDL X-ray codes developed by Eduard Kontar. However, the interactive interface allows interchanging these default libraries with any user-defined IDL or external callable codes designed to solve the radiation transfer equation in the same or other wavelength ranges of interest.

## Documentation and citation

For a full description of GX Simulator methods and capabilities, please see the open-access ApJS paper:

[Nita et al. 2023, ApJS, 267, 6](https://iopscience.iop.org/article/10.3847/1538-4365/acd343)

The GUI **Help** button opens the GitHub-rendered README for the installed release tag (`v` + [`VERSION`](VERSION)), falling back to `master`, then to the local `README.md` file if you are offline. Installation, version checks, and developer notes continue below.

## Software prerequisites needed to ensure full GX Simulator functionaliy:
- [ ] All OS platforms: IDL 8.2 or later
- [ ] All OS platforms: SSWIDL installed from https://www.lmsal.com/solarsoft/ with the following list of required instruments included: ontology vso sdo aia hessi chianti hxt spex xray norh gx_simulator
- [ ] Linux/Mac platforms: gcc 4.8 or later

# Here are the steps needed to download the package, including the linked external submodule dependencies

GX Simulator records a specific commit for each external library (and those libraries record their own nested commits, such as AMaFiL inside `external/nlfff`). That recorded set is what a GX Simulator **release** was tested with. Ordinary users should install and update that vetted set. Only maintainers or advanced users should overwrite it with newer upstream submodule tips.

#### If not already installed on your system, download and install git for command line

https://git-scm.com/

#### Launch the Git Bash terminal

#### CD to your SSW/packages/ installation folder and issue the following sequence of commands

```bash
git clone --recurse-submodules https://github.com/Gelu-Nita/GX_SIMULATOR gx_simulator
cd gx_simulator
```

Equivalent two-step form (same result: the commits recorded by this GX Simulator tree, including nested submodules):

```bash
git clone https://github.com/Gelu-Nita/GX_SIMULATOR gx_simulator
cd gx_simulator
git submodule update --init --recursive
```

Do **not** add `--remote` here. `--remote` ignores the commits this release recorded and checks out the current tips of the submodule branches instead.

To install a frozen published release instead of the current `master` tip, check out that tag before initializing submodules (example: `v4.4.2`; use the latest `v*` tag you want):

```bash
git clone https://github.com/Gelu-Nita/GX_SIMULATOR gx_simulator
cd gx_simulator
git checkout v4.4.2
git submodule update --init --recursive
```

#### Updating an existing installation (most users)

```bash
cd gx_simulator
git checkout master
git pull
git submodule update --init --recursive
```

`git pull` alone updates GX Simulator source but does not refresh submodule checkouts. The `git submodule update --init --recursive` step (still **without** `--remote`) moves each library to the commits the newly pulled GX Simulator release recorded.

On Linux/Mac, an older compiled library in `~/gx_binaries` is reused until you rebuild it. After a library submodule change, either delete the stale `*.so` under `~/gx_binaries` or request a rebuild, for example:

```idl
print, gx_libpath('nlfff', /update)
print, gx_libpath('grid', /update)
```

#### Advanced: track live upstream submodule tips (unvetted)

This overwrites the GX Simulator release pins with whatever the submodule owners have published on the branches listed in `.gitmodules`. Those commits have not necessarily been tested with this GX Simulator version. Do not commit the resulting dirty submodule pointers unless you are maintaining GX_SIMULATOR.

```bash
git submodule update --init --recursive --remote
```

After this command, `git status` may show modified submodules. That is expected in this advanced mode.

##### Developer/maintainer submodule synchronization

Committing updated submodule pointers is how a GX Simulator release absorbs a newly vetted upstream library (for example a new `nlfff` / AMaFiL tip). After you pin and tag, ordinary users who follow the install/update steps above receive that set without `--remote`. Only developers with write access to GX_SIMULATOR and the relevant submodule repositories should run the synchronization helper.

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

##### Developer/maintainer version bump and release

The displayed version is read from the committed [`VERSION`](VERSION) file (not from a live GitHub query). For a tagged checkout and for About / title bar / `gx_simulator,/version` to match the release, bump `VERSION` in the **same commit you tag**.

```bash
# From the gx_simulator repo root, on an up-to-date master branch:
echo 4.2.0 > VERSION
git add VERSION
git commit -m "Release 4.2.0"
git tag v4.2.0
git push origin master --tags
```

Notes:

- Use a SemVer-style string in `VERSION` **without** a leading `v` (e.g. `4.2.0`). The git tag should be `v` plus that same string (e.g. `v4.2.0`).
- After a `v*` tag is pushed, [`.github/workflows/sync-version.yml`](.github/workflows/sync-version.yml) updates `VERSION` on `master` if it does not already match the tag. That keeps `master` aligned, but SSW users who check out the tag itself still need `VERSION` correct **on the tagged commit** — hence bump-before-tag above.
- Do not rely on editing the About dialog text by hand; it calls `gx_version()`.

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

## Checking the installed version

The package version is stored in the top-level [`VERSION`](VERSION) file and is shown in several places:

- **Window title bar** — e.g. `GX SIMULATOR 4.1.0` or `GX SIMULATOR 4.1.0 (Expert)`
- **About dialog** — toolbar About button
- **IDL command line** — from an `sswidl` session:

```idl
gx_simulator, /version
print, gx_version()
```

## CHMP EBTEL heating-parameter search

Interactive GUI: `gx_chmp`. Batch API: `gx_search4bestq`.

Documentation for image vs spectrum modes, `spec_weights=`, reference FITS/directories, GUI `_extra` validation, and workdir/`gxchmp.ini` defaults: [doc/CHMP_search.md](doc/CHMP_search.md).
