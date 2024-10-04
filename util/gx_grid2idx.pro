FUNCTION gx_grid2idx, indices, dims
  ;+
  ; NAME:
  ;    gx_grid2idx
  ;
  ; PURPOSE:
  ;    Converts multi-dimensional array indices into a single linear index
  ;    for use in row-major ordered arrays.
  ;
  ; CATEGORY:
  ;    Array manipulation
  ;
  ; CALLING SEQUENCE:
  ;    result = gx_grid2idx(indices, dims)
  ;
  ; INPUTS:
  ;    indices: A vector containing the multi-dimensional indices (e.g., [i1, i2, i3]).
  ;    dims:    A vector specifying the dimensions of the array (e.g., [d1, d2, d3]).
  ;
  ; OUTPUTS:
  ;    A scalar representing the corresponding 1D index.
  ;
  ; EXAMPLE:
  ;    array_dims = [4, 5, 6]   ; Dimensions of the 3D array
  ;    multi_index = [2, 3, 1]  ; Multi-dimensional indices
  ;
  ;    linear_index = gx_grid2idx(multi_index, array_dims)
  ;    PRINT, '1D index: ', linear_index
  ;
  ; RESTRICTIONS:
  ;    The number of elements in 'indices' must match the number of elements in 'dims'.
  ;
  ; MODIFICATION HISTORY:
  ;    Written by: ChatGPT, October 2024
  ;-

  ; Ensure the number of indices matches the number of dimensions
  IF N_ELEMENTS(indices) NE N_ELEMENTS(dims) THEN $
    MESSAGE, 'Number of indices must match the number of dimensions.'

  ; Compute the linear index using row-major order
  linear_index = 0
  factor = 1
  FOR i = 0, N_ELEMENTS(indices) - 1 DO BEGIN
    linear_index = linear_index + indices[i] * factor
    factor = factor * dims[i]
  ENDFOR

  RETURN, linear_index
END
