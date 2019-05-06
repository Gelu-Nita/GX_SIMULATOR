pro gxGetBridgeVar,bridge,task,vars
vars=(strsplit(str_replace(task,'=',','),',',/extract))[1:*]
vars=vars[uniq(vars)]
for i=0,n_elements(vars)-1 do begin
   CATCH, Error_status
   IF Error_status NE 0 THEN BEGIN
      goto,skipvar
      CATCH, /CANCEL
   ENDIF
  var=bridge->GetVar(vars[i])
 (SCOPE_VARFETCH(vars[i],/ENTER,LEVEL=-1)) = (SCOPE_VARFETCH('var',LEVEL=0))
 skipvar:
end
end