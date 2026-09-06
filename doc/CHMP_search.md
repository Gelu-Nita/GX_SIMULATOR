# CHMP EBTEL parameter search (`gx_chmp` / `gx_search4bestq`)

Interactive GUI: `gx_chmp`. Batch API: `gx_search4bestq` → `gx_processmodels_ebtel`.

## Launch and paths

```idl
cd, '/path/to/workdir'   ; repositories default under this directory
gx_chmp                  ; restore ./gxchmp.ini if present
gx_chmp, /fresh          ; ignore ini; use curdir() defaults
```

| Path | Default (no ini / invalid path) |
|------|----------------------------------|
| Model maps | `./modDir` |
| PostScript | `./psDir` |
| Temporary | `./tmpDir` |

Settings are saved to `curdir()/gxchmp.ini` on GUI exit. No personal absolute paths are hard-coded.

Renderer / EBTEL table defaults come from `gx_findfile` under the GX Simulator package (e.g. `aia.pro` for EUV, `grffdemtransfer.pro` for Unix MW).

## Reference data

`refdatapath` may be:

- one `.sav` or FITS (`.fits` / `.fts` / `.fit`) file, or
- a **directory** of those files (multi-channel / multi-frequency set).

Two snapshot formats stay on **Method B** (independent-pixel SDEV):

- Format-3 `{maps:[mean,sdev], a_beam, b_beam, ...}` (e.g. `prepare_ref` output)
- A single 2-D FITS / map (placeholder SDEV if none is present)

**Time-series cubes** (new): if the path is `RMAPS` / a map array with \(M\ge 2\) frames, or a directory of 2-D maps that share `CHAN`/`FREQ` at different times, CHMP attaches the cube. Do **not** point `refdatapath` at lev1 JSOC trees (no `aia_prep` inside CHMP). Beam keywords are still required when headers lack `BMAJ`/`A_BEAM`:

```text
a_beam=1.5, b_beam=1.5, phi_beam=0
```

### `sdev_method` (uncertainty)

| `sdev_method` | Spectrum + cube | Image + cube | Snapshot `[mean,sdev]` |
|---------------|-----------------|--------------|------------------------|
| `'auto'` (default) | Method A: \(s_F\) of the ROI light curve (\(M-1\), not SEM) | Method B: remapped per-pixel sample \(\sigma\) | Method B (unchanged quadrature) |
| `'A'` | Force Method A | Refused | Error (no cube) |
| `'B'` | Force Method B on remapped cube \(\sigma\) | Method B | Method B |

Method A uses the **live** `mask=` / `apply2` ROI at each Q (default `apply2=3` can change with the model). `gx_fov_integral_map` Method B quadrature is unchanged.

GUI:

- file picker: `*.sav`, `*.fits`, `*.fts`, `*.fit`
- directory picker: folder of mixed `.sav` / FITS refs
- pass `sdev_method='A'` or `'B'` in `_extra` (no extra widget). The PSF/ref line shows `cube M=` when a cube is loaded.

Loader: `gx_ref2chmp`. Averaged AIA FITS often lack `BMAJ`/`BMIN`; pass beam overrides in `_extra` (or they are applied when loading FITS/directories).

FOV / resolution import dialogs accept `*.sav` and `*.map` (Motif filter: `*.sav *.map`).

## Search modes

Default is **image**. Keyword is **`search_mode=`** (not `mode=`).

### Image mode (`search_mode='image'` or omitted)

- Requires a **scalar** `chan=` (EUV / CHAN refs) or `freq=` (MW / FREQ refs) when the reference set has more than one axis.
- Vector `chan=` / `freq=` is refused.
- `spec_weights=` is refused.

Example (AIA 94 Å):

```idl
result = gx_search4bestq(..., renderer=aia_pro, refdatapath=refdir, $
  chan=94, a_beam=1.5, b_beam=1.5, phi_beam=0, ...)
```

### Spectrum mode (`search_mode='spectrum'`)

- Always loads the **full** reference set from `refdatapath`.
- Channel inclusion / soft weighting: **`spec_weights=` only** (omit → weight 1 on every axis).
- `w <= 0` excludes a point from RES²/CHI²; `w > 0` is in the search subset.
- Top-level `chan=`, `freq=`, `spec_chan=`, `spec_freq=` are **refused**.
- Needs at least two reference axes.
- MW synthesis list still comes from `_extra.freqlist` when needed.

Example (all AIA channels, drop 171):

```idl
result = gx_search4bestq(..., search_mode='spectrum', $
  spec_weights=[1,1,0,1,1,1], a_beam=1.5, b_beam=1.5, phi_beam=0, ...)
```

`mask=` / `levels=` select the **spatial ROI**. They do not select spectral channels.

## GUI `_extra` field

Text is validated before search and when editing `_extra` (same rules as `gx_search4bestq`).

| Mode | Valid in `_extra` | Invalid |
|------|-------------------|---------|
| spectrum | `search_mode='spectrum'`, `spec_weights=[...]`, `sdev_method=`, beam / `freqlist` / mask extras | `chan=`, `freq=` |
| image (default) | scalar `chan=` or `freq=`, beam extras, `sdev_method='B'`/`'auto'` | `spec_weights=`, `sdev_method='A'` |

The **Convolving PSF parameters** line is read-only: it displays beam tags after refs load. Beam **inputs** belong in `_extra` (or FITS headers).

Optional auto-fill when `_extra` is empty and refs are FITS/directories: beam keywords only. The GUI does **not** invent `chan=` / `freq=` / `search_mode=`.

Task scripts include `_extra` keywords. Preview requires at least one row in the Best Models Search Queue.

## Metrics and plots

- Image: per-pixel map metrics (`gx_metrics_image` / `gx_metrics_map`).
- Spectrum: ROI-integrated `S_obs` / `S_mod` / `S_sdev` via `gx_maps2spectrum` and `gx_metrics_spectrum` (`weights=` optional; used by CHMP as `spec_weights`).
- After a successful search, **Best of Bests.ps** is written by default (`plot_best=1`) without rewriting cell PS (`/bob_only`).
- Per-cell `set_a*b*_final.ps` are written during the search by the shared cell plotter (image and spectrum: Q metrics, optional spectrum page, then Data | Model | (D−M)/(D+M) maps). One channel fills the first row of the 3×3 page; several channels use one column per channel.
- Replot everything from a saved result (create `psDir` if needed):

```idl
gx_plotbestchmpmodels_ebtel, result            ; all cells, then Best of Bests if n>1
gx_plotbestchmpmodels_ebtel, result, /bob_only ; Best of Bests only (n>1)
gx_plotbestchmpmodels_ebtel, result, plot_best=0
gx_plotbestchmpmodels_ebtel, result, /overwrite, /debug  ; neighborhood Qs (best few)
```

`psDir` omitted uses `result.psDir`. `/overwrite` skips the confirm dialog. `/debug` adds the ~6 best RES² / CHI² Q samples (maps from `spec_allmetrics` or `modDir`; never fakes Method A `S_sdev` from the map SDEV layer).

To show one spectrum channel with legacy map plotters / GUI:

```idl
r1 = gx_result_select_channel(result, chan=171)   ; or index=/freq=
```

## Related routines

| Routine | Role |
|---------|------|
| `gx_ref2chmp` / `gx_ref2chmp_one` | Load CHMP refs (snapshots or time cubes) |
| `gx_maps2spectrum` | ROI integrals; Method A/B via `sdev_method=` |
| `gx_ref_select_axis` | Select / sort by FREQ or CHAN |
| `gx_processmodels_ebtel` | Q search + metrics for one `(a,b)` |
| `gx_metrics_spectrum` | Spectral RES² / CHI² (`weights=` optional) |
| `gx_plotbestchmpmodels_ebtel` | Top-level plot/replot: all cells, then Best of Bests if `n>1`. `/bob_only`, `plot_best=0`, `/overwrite`, `/debug`. Alias `gx_plotbestmwmodels_ebtel` |
| `gx_plot_chmp_cell` | One cell PS (metrics, optional spectrum, 3×3 maps) |
| `gx_plot_chmp_spectrum` / `gx_plot_chmp_chanmaps` / `gx_plot_chmp_qsearch` | Shared page helpers |
