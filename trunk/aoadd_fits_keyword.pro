
; 
; add a keyword to a FITS header.
; Much more restricted than sxaddpar, but support HIERARCH
;

function aoadd_fits_keyword, hdr, name, value, comment
    
    nentries=n_elements(hdr)
    
    endline = 'END' +string(replicate(32b,77))     ;END line
    
    line = ''
    if (strlen(name) gt 8) or ( strpos(name, '.') ne -1 ) then line += 'HIERARCH '
    line += name
    line += ' = '

    apost = "'"             ;quote a quote
    type = size(value)      ;get type of value parameter
    if type[0] ne 0 then message,'Keyword Value (third parameter) must be scalar'

    case type[1] of         ;which type?
    7:  begin
        upval = strupcase(value)      ;force upper case.
        if (upval eq 'T') or (upval eq 'F') then begin
            line += upval  ;insert logical value.
        endif else begin              ;other string?
            line += apost + value + apost
        endelse                                          ;to at least 8 chars
        end
    5:  BEGIN
        IF (N_ELEMENTS(format) EQ 1) THEN $             ; use format keyword
            v = string(value, FORMAT='('+strupcase(format)+')') ELSE v = STRING(value, FORMAT='(G19.12)')
            line += v 
        END

    else:  begin
        if (N_elements(format) eq 1) then $            ;use format keyword
            v = string(value, FORMAT='('+strupcase(format)+')' ) else $
            v = strtrim(strupcase(value),2)      
                                      ;convert to string, default format
        line += v
        end
    endcase

    if n_elements(comment) ne 0 then line += ' /' + comment
    line += string(replicate(32b, 80-strlen(line))) 
    return, [hdr[0:nentries-2],line, hdr[173]]
end
