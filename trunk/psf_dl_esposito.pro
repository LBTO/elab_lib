
;+
;
; lambda=1.6d-6 ;;; filtro H 1.6 micron
; irtc_sampling = 0.01 ;;;; arcsec/px
;-
function psf_dl_esposito, lambda, irtc_sampling, oc=oc,  Dpup=Dpup
	if not keyword_set(oc) then oc=0.
    if not keyword_set(Dpup) then Dpup = 8.222 ;;; diametro di LBT in m
    np_psf = 5000  ;;; punti della psf_difflim

    x = rebin( findgen(np_psf), np_psf, np_psf)
    y = transpose(x)

    tetanorm = sqrt((1.*x-np_psf/2)^2+(1.*y-np_psf/2)^2)*irtc_sampling/10. *(4.848d-6/(lambda/float(Dpup)))
    psf_difflim = float( psf_dl( tetanorm, OBS=oc, PEAK_NORM=1))

    return, psf_difflim
end



