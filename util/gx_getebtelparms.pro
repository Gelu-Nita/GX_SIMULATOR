function gx_getEBTELparms,gx_key,a,b,q,parms=parms,formula=formula
  ;this routine extracct key parameters from a GX map produced using an EBTEL coronal heating model
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;a key is arbitrarily defined here for testing purposes
  default,gx_key,'q=[ 0.0014100000, 100.00000, 1.0000000e+009, 0.00000000, 0.00000000] & q0=q[0] & q=q0*(B/q[1])^0*(L/q[2])^3 & NTDEM= 1 & NTSSDEM= 0'
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++
  keys=strsplit(gx_key,'&',/extract)
  if n_elements(keys) lt 3 then return,keys
  e=execute(keys[0])
  e=execute(keys[1])
  formula=keys[2]
  e=execute(str_replace(str_replace(str_replace(str_replace(keys[2],'q=','a=alog10('),'q0*',''),'L/q[2]','1.'),'B/q[1]','10.')+')')
  e=execute(str_replace(str_replace(str_replace(str_replace(keys[2],'q=','b=alog10('),'q0*',''),'B/q[1]','1.'),'L/q[2]','10.')+')')
  q=q0
  return,keys
end