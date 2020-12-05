

function oaa_str_replace, str, pattern, replace

   pos = STRPOS(str, pattern)
   if pos eq -1 then return, str

   s2 = STRMID(str,0,pos)+replace+STRMID(str,pos+STRLEN(pattern))
   return, s2

end
