function gx_getEBTELparms,gx_key,a=a,b=b,q0=q0,parms=parms,formula=formula
  default,GX_Key,'q=[ 0.000415, 100.00000, 1.0000000e+009, 1, 0.75] & q0=q[0] & q=q0*(B/q[1])^q[3]*(L/q[2])^q[4] & NTDEM= 1 & NTSSDEM= 0'
  keys=strsplit(gx_key,'&',/extract)
  if n_elements(keys) lt 3 then return,keys
  e=execute(keys[0])
  e=execute(keys[1])
  formula=keys[2]
  q0=q[0]
  a=q[3]
  b=q[4]
  parms=q
  return,keys
end