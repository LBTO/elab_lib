
; Returns the keyword value in the passed FITS header. Returns "" if not value is found
;
; hdr: (strarr) FITS header to search
; keyword: name of the keyword to search ('HIEARCH' extensions will be automatically searched)
;
; returns: keyword value or ""

function aoget_fits_keyword, hdr, keyword

len = strlen(keyword)

value = ""
for i=0,n_elements(hdr)-1 do begin
    if (strcmp(strmid(hdr[i],0,len),keyword, /FOLD) eq 1) then begin
        pos = strpos( hdr[i], "=")
        value = strtrim( strmid(hdr[i],pos+2), 2)
        if (strcmp( strmid(value, 0, /REVERSE), "/") eq 1) then value = strtrim( strmid(value, 0, strlen(value)-1), 2)
        ; get rid of comment (starting with " /" and having string after it
        pos_comment = strpos(value, ' /', /REVERSE_SEARCH)
        if pos_comment ne -1 then value = strtrim( strmid(value,0,pos_comment), 2)
        break
    endif
endfor
;print,'Before:',value

if value ne "" then begin
    if ((strcmp( strmid(value,0,1), "'") eq 1) and $
       (strcmp( strmid(value,0,/REVERSE), "'") eq 1)) then begin
            value = strmid( value, 1, strlen(value)-2)
    endif
endif
;print,'After: ',value

if ((value eq "") and (strcmp(strmid(keyword, 0, 9), 'HIERARCH ', /FOLD) eq 0)) then return, aoget_fits_keyword( hdr, 'HIERARCH '+keyword) $
else begin
;print,value
 return, value
endelse

end


