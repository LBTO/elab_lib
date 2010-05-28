
; mad_correct_bad_pixel from Carmelo


;this function is usefull when n is even (pari)
function coordinate,length,DOUBLE=double

if (length mod 2)  eq 0 then begin
       n = fix(length)
       if keyword_set(DOUBLE) then x = dindgen(n)-n/2 +.5 else  x = findgen(n)-n/2 +.5
endif else begin
       n = fix(length)
       if keyword_set(DOUBLE) then x = dindgen(n)-n/2 else x = findgen(n)-n/2
endelse

return,x
end

pro coo2d,n,xx,yy
xx= rebin(coordinate(n),n,n) & yy=transpose(xx)
return
end


function mad_correct_bad_pixel, mask_t, im_t_orig, SIDEARRAY=sidearray

    im_t = im_t_orig
    if keyword_set(SIDEARRAY) then begin
        ns = sidearray
        ns = ns + 1 - (ns mod 2)
    endif else ns = 5
    ns = round(ns)
    nns = round(ns*ns/2)
    ;xsize = sizex
    ;ysize = sizey
    xsize = (size(im_t,/DIM))[0]
    ysize = (size(im_t,/DIM))[1]
    coo2d,ns,xp,yp & xp = reform(xp,ns^2) & yp = reform(yp,ns^2)
    xp = [xp[0:nns-1],xp[nns+1:ns^2-1]]
    yp = [yp[0:nns-1],yp[nns+1:ns^2-1]]
    indexbad = WHERE(mask_t eq 0,count)
    ;im_t[indexbad] = median(im_t[in])
    for j = 0L,count -1L do begin
       xin = indexbad[j] mod xsize
       yin = floor(float(indexbad[j])/float(xsize))
       array = (im_t[(xin-ns/2) >0:(xin+ns/2) < (xsize-1),(yin-ns/2) >0:(yin+ns/2)< (ysize-1)])
       m_array = (mask_t[(xin-ns/2) >0:(xin+ns/2) < (xsize-1),(yin-ns/2) >0:(yin+ns/2)< (ysize-1)])

       if n_elements(array) eq ns^2 and (max(array)-min(array)) gt 0 then begin
            array = reform(array,ns^2)
            m_array = reform(m_array,ns^2)
            array = [array[0:nns-1],array[nns+1:ns^2-1]]
            m_array = [m_array[0:nns-1],m_array[nns+1:ns^2-1]]
            ink = where(m_array gt 0.7,count)
            if count eq N_elements(m_array) then begin
               val = (min_curve_surf(array,xp,yp,NX=ns,NY=ns))[nns]
               val_m = (min_curve_surf(m_array,xp,yp,NX=ns,NY=ns))[nns]
               if finite(val) eq 1 then begin
                im_t[indexbad[j]] = val
                mask_t[indexbad[j]] = val_m
               endif else begin
                im_t[indexbad[j]] = median(array)
                mask_t[indexbad[j]] = median(m_array)
               endelse

            endif else begin
                if count ne 0 and FLOAT(N_ELEMENTS(INK))/FLOAT(N_ELEMENTS(array)) gt 0.35 then begin
                   val = (min_curve_surf(array[ink],xp[ink],yp[ink],NX=ns,NY=ns))[nns]
                   val_m = (min_curve_surf(m_array,xp,yp,NX=ns,NY=ns))[nns]
                   if finite(val) eq 1 then begin
                       im_t[indexbad[j]] = val
                       mask_t[indexbad[j]] = val_m
                   endif else begin
                       im_t[indexbad[j]] = 0
                       mask_t[indexbad[j]] = 0
                   endelse
                endif else begin
                    im_t[indexbad[j]] = 0
                    mask_t[indexbad[j]] = 0
                endelse
            endelse
       endif else begin
        ;m_array = [m_array[0:nns-1],m_array[nns+1:ns^2-1]]
        ink = where(m_array  gt 0.7,count)
        if count eq 0 then begin
            im_t[indexbad[j]] = 0
            mask_t[indexbad[j]] = 0
        endif else begin
            im_t[indexbad[j]] = median(array[ink])
            mask_t[indexbad[j]] = median(m_array[ink])
       endelse
       if finite(im_t[indexbad[j]]) eq 0 then begin
        im_t[indexbad[j]] = 0
        mask_t[indexbad[j]] = 0
       endif
       if mask_t[indexbad[j]] le 0.7 then begin
        im_t[indexbad[j]] = 0
        mask_t[indexbad[j]] = 0
       endif
     endelse
   endfor
return, im_t
end
