# Radio flaring renderers

Active MW (gyrosynchrotron) wrappers in this folder:

| Wrapper | Library (`gx_libpath`) | Notes |
|---------|------------------------|-------|
| `mw_transfer_arr.pro` (`MW_TRANSFER_ARR`) | `mwtransferarr` | Current Kuznetsov path (array DFs); prefer this |
| `mw_transfer_los.pro` (`MW_Transfer_LOS`) | `mwtransferlos` | LOS variant |
| `mw_transfer_gpu.pro` (`MW_Transfer_GPU`) | `mwtransfergpu` | GPU variant |
| `gs_transfer.pro` / `gs_transfer_dp.pro` | legacy GS trees | Older GS wrappers |

## Retired

**`mw_transfer.pro` (`MW_Transfer`) → `external/mw/mwtransfer`** was removed to reduce renderer confusion.
It was a vendored older slice API superseded by `MW_TRANSFER_ARR`.

To recover from git history (example):

```bash
git log --oneline -- userslib/radio_flaring/mw_transfer.pro
git checkout <commit-before-removal> -- userslib/radio_flaring/mw_transfer.pro external/mw/mwtransfer
```
