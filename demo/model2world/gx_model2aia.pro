;+
; NAME:
;   gx_model2aia
;
; PURPOSE:
;   Displays a 3D GX Simulator model in the context of a corresponding
;   AIA (Atmospheric Imaging Assembly) reference image, with optional
;   magnetic field line overlays.
;
; SYNTAX:
;   gx_model2aia, model
;
; USAGE:
;   gx_model2aia, model, tlb=tlb
;     ; Initial call to restore a model, create the interactive display widget,
;     ; and generate the plots for the default AIA channel (2 = 171 Å).
;
;   gx_model2aia, model, tlb=tlb, chan=chan
;     ; To change the default AIA channel and update the plots accordingly.
;
;   gx_model2aia, model, tlb=tlb, /newlines
;     ; To remove existing and create new magnetic field lines and update the plots.
;
;   gx_model2aia, model, tlb=tlb, save2png=1, pngfile='output.png'
;     ; To save the plot to a PNG file instead of displaying it interactively.
;     ; If pngfile is not specified, a default name 'gx_model2aia_output.png' is used.
;
; KEYWORDS:
;   tlb        - Optional widget top-level base for GUI display. If undefined, a new viewer is created.
;   xsize      - Width of the display window in pixels (default: 800).
;   ysize      - Height of the display window in pixels (default: 800).
;   chan       - AIA channel index (0–5); default: 2 (typically AIA 171 Å).
;   newlines   - If set, removes existing and adds new magnetic field lines.
;   save2png   - If set, the plot will be saved to a PNG file instead of just displaying.
;   pngfile    - Filename for the PNG output (only used if save2png is set).
;   _EXTRA     - Extra keywords passed to the plot_map routine.
;
; NOTES:
;   - The routine expects the model object to contain valid reference maps.
;   - Requires gx_read, gx_model2world, plot_map, and related utilities.
;
; MODIFICATION HISTORY:
;   Written by Gelu Nita. Header and enhancements by ChatGPT, 2025-04-17.
;-

pro gx_model2aia, model, tlb=tlb, xsize=xsize, ysize=ysize, chan=chan, newlines=newlines, $
  save2png=save2png, pngfile=pngfile, ct=ct,_extra=_extra

  ;-- Set default display parameters
  default, xsize, 800
  default, ysize, 800
  default, chan, 2

  ;-- Load model if not provided or invalid
  if ~obj_valid(model) then model = gx_read()

  ;-- If requested, generate a new set of magnetic field lines
  if keyword_set(newlines) then begin
    model->RemoveBlines
    sz = model->Size()
    model->AddBLines, x0=sz[1]/2., y0=sz[2]/2., z0=0, $
      dx=10, dy=10, dz=1, $
      nx=sz[1]/10, ny=sz[2]/10, nz=1
  endif

  ;-- Reset model viewer position
  model->ResetPosition

  ;-- Launch viewer if not already displayed
  if ~widget_valid(tlb) then begin
    xobjview, model, tlb=tlb, xsize=xsize, ysize=ysize
  endif else begin
    xobjview, refresh=tlb
  endelse

  ;-- Extract reference maps from model and select AIA maps only
  refmaps = *(model->Refmaps())
  aia_maps = obj_new('map')
  for k = 0, refmaps->get(/count) - 1 do begin
    if strmid(refmaps->get(k, /id), 0, 3) eq 'AIA' then $
      aia_maps->set, aia_maps->get(/count), map=refmaps->get(k, /map)
  endfor

  ;-- Get selected AIA channel map
  ref = aia_maps->get(chan, /map)
  obj_destroy, aia_maps

  ;-- Open display window and prepare RGB LUTs
  window, 0, xsize=xsize, ysize=ysize
  restore, gx_findfile('AIA_rgb.sav')  ; Load pre-defined AIA color tables
  tvlct, rgb0, /get                    ; Save current color table
  tvlct, rgb[*, *, chan < 6]           ; Set LUT for selected AIA channel
  gx_rgb_white2black                  ; Adjust white background if needed

  ;-- Plot the selected AIA map
  plot_map, ref, _extra=_extra

  ;-- Overlay model field lines transformed to reference map coordinates
  if n_elements(ct) then ct=39
  loadct, ct
  gx_model2world, model, /lines, ref=ref,_extra=_extra

  ;-- Save to PNG if requested
  if keyword_set(save2png) then begin
    if n_elements(pngfile) eq 0 then pngfile = 'gx_model2aia_output.png'
    write_png, pngfile, tvrd(/true)
    print, 'Saved plot to ', pngfile
  endif

  ;-- Restore original color table
  tvlct, rgb0

end
