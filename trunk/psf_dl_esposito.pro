
;+
;
; lambda=1.6d-6 ;;; filtro H 1.6 micron
; irtc_sampling = 0.01 ;;;; arcsec/px
;-
function psf_dl_esposito, lambda, irtc_sampling
    D = 8.22 ;;; diametro di LBT in m
    np_psf = 5000  ;;; punti della psf_difflim
    psf_difflim = fltarr(np_psf, np_psf)
    for i=0L,np_psf-1 do begin
        for j=0L,np_psf-1 do begin
        teta1 = sqrt((1.*i-np_psf/2)^2+(1.*J-np_psf/2)^2)*irtc_sampling/10.
        psf_difflim(i,j) = airy(lambda, D, teta1)
        endfor
    endfor
    return, psf_difflim
end



