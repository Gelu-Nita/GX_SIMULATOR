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

GUI:

- file picker: `*.sav`, `*.fits`, `*.fts`, `*.fit`
- directory picker: folder of mixed `.sav` / FITS refs

Loader: `gx_ref2chmp`. Averaged AIA FITS often lack `BMAJ`/`BMIN`; pass beam overrides in `_extra` (or they are applied when loading FITS/directories):

```text
a_beam=1.5, b_beam=1.5, phi_beam=0
```

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
| spectrum | `search_mode='spectrum'`, `spec_weights=[...]`, beam / `freqlist` / mask extras | `chan=`, `freq=` |
| image (default) | scalar `chan=` or `freq=`, beam extras | `spec_weights=` |

The **Convolving PSF parameters** line is read-only: it displays beam tags after refs load. Beam **inputs** belong in `_extra` (or FITS headers).

Optional auto-fill when `_extra` is empty and refs are FITS/directories: beam keywords only. The GUI does **not** invent `chan=` / `freq=` / `search_mode=`.

Task scripts include `_extra` keywords. Preview requires at least one row in the Best Models Search Queue.

## Metrics and plots

- Image: per-pixel map metrics (`gx_metrics_image` / `gx_metrics_map`).
- Spectrum: ROI-integrated `S_obs` / `S_mod` / `S_sdev` via `gx_maps2spectrum` and `gx_metrics_spectrum` (`weights=` optional; used by CHMP as `spec_weights`).
- After a successful search, **Best of Bests.ps** is written by default (`plot_best=1`).
- Per-cell `set_a*b*_final.ps` are written during the search; Best of Bests does **not** rewrite them unless `/replot_final` (or `gx_replot_chmp_finalps`).

To show one spectrum channel with legacy map plotters / GUI:

```idl
r1 = gx_result_select_channel(result, chan=171)   ; or index=/freq=
```

## Related routines

| Routine | Role |
|---------|------|
| `gx_ref2chmp` / `gx_ref2chmp_one` | Load CHMP refs |
| `gx_ref_select_axis` | Select / sort by FREQ or CHAN |
| `gx_processmodels_ebtel` | Q search + metrics for one `(a,b)` |
| `gx_metrics_spectrum` | Spectral RES² / CHI² (`weights=` optional) |
| `gx_plotbestmwmodels_ebtel` | Best of Bests (+ optional `/replot_final`) |
| `gx_plot_chmp_spectrum` / `gx_plot_chmp_chanmaps` / `gx_plot_chmp_qsearch` | Spectrum-mode PS helpers |
