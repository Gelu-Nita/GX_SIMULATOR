# GX_BOX_MAKE_POTENTIAL_FIELD
## Description
Replaces the magnetic field in the box with the potential extrapolation.
Field at the lower level is not affected.

## Parameters
  * `box` - GX-simulator compatible box structure, to hold the modified potential field solution with the observed field preserved at the lower boundary that may be subsequenty used to perform NLFFF reconstruction
  * `pbox` - GX-simulator compatible box structure to hold the potential field solution in the whole box (including lower boundary)
