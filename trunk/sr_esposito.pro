;+
;
;-
; from Simone's calc_sr1 17 nov 09
;function sr_esposito, ima_bs, psf_difflim, lambda, irtc_sampling, plot=plot, errmsg = errmsg, FIX_BG = FIX_BG
function sr_esposito, ima_bs, psf_difflim, plot=plot, errmsg = errmsg, FIX_BG = FIX_BG

npx = n_elements(ima_bs(*,0))
npy = n_elements(ima_bs(0,*))

;ima_bs[319,255]= 0 ;;; set the bad pixel to zero (DONE before using badpixelmap!!!)

;Remove the borders of the image
npr = 10	;number of pixels to remove from the edges
ima_bs[0:npr-1,*] = 0
ima_bs[npx-npr:*,*] = 0
ima_bs[*,0:npr] = 0
ima_bs[*,npy-npr:*] = 0
max_ima = max(ima_bs,h)

if n_elements(plot) ne 0 then print, 'Imax, x, y', max_ima,h/npx,h-h/npx*npx
ima_fit = gauss2dfit(ima_bs,coeff)
;print,'Gaussian fit:',coeff
xc = round(coeff(4))
yc = round(coeff(5))

;;;;;;;; questo non va bene perche ci posono essere punti a zero dentro la cornice
;a = 20 ;;; larghezza della cornice usata per il calcolo del background
;ima_dummy = ima_bs
;ima_dummy[a:npx-a-1,a:npy-a-1] = 0.0
;ind0= where(ima_dummy EQ 0.0,count0)
;npbg = float(1.0*npx*npy)-count0
;new_bg = total(ima_dummy)/ npbg
;fixbg=0
;counter=0

ac = 10	;;; larghezza della striscia usata per il calcolo del background
ima_dummy = fltarr(npx,npy)
ima_dummy[npr:npx-npr-1,npr:npr+ac] = 1.
ima_dummy[npr:npx-npr-1,npy-npr-ac:npy-npr-1] = 1.
ima_dummy[npr:npr+ac,npr+ac:npy-npr-ac-1] = 1.
ima_dummy[npx-npr-ac:npx-npr-1,npr+ac:npy-npr-ac-1] = 1.
ind1 = where(ima_dummy, npbg)
new_bg = total(ima_bs[ind1])/ npbg
fixbg=0
counter=0

;side = min([xc,npx-xc,yc,npy-yc],h)
side = min([xc,npx-xc,yc,npy-yc]-npr)	;do not take into account image corner set to zero!!
if side lt 0 then begin
	errmsg = 'Center of PSF estimation failed'
	message, errmsg, /info
	return, 0
endif
stepsize=4 ; even number
side10 = fix(side)/stepsize*stepsize
nside = side10/stepsize * 2 -1

if nside lt 1 then begin
	errmsg = 'Center of PSF estimation error'
   	message, errmsg, /info
   	return, 0
endif

repeat begin
	new_bg += fixbg
   	flux = fltarr(nside)
   	side_size= fltarr(nside)
   	for i= 1, nside do begin
    	side_size(i-1) = stepsize*i
      	ima_side = ima_bs(xc-(stepsize/2)*i:xc+(stepsize/2)*i-1,yc-(stepsize/2)*i:yc+(stepsize/2)*i-1)
		;flux(i-1) = total(ima_side-replicate(new_bg,10*i,10*i)) ;ma non c'e bisogno di fare replicate!!
	  	flux[i-1] = total(ima_side-new_bg) ; flux in squares of 10,20,30..10*nside pixels. Background dynamically corrected.
        ;print, 'side '+strtrim(side_size(i-1),2)+'  average_flux:',flux[i-1]/ n_elements(ima_side) 
   	endfor
	if not keyword_set(FIX_BG) then break

	dflux = flux-shift(flux,1)
    ssize = side_size^2
    dssize = ssize-shift(ssize,1)
    aaa = dflux / dssize  ; flux derivative counts/px^2. aaa[0] is not good
    aaa_len = n_elements(aaa)  ; TODO aaa_len === nside
    if aaa_len lt 5 then break  ; do not correct bg if nside lt 5 (aaa[0] is not good) 
    fixbg = mean(aaa[aaa_len-4:aaa_len-1])
    if keyword_set(plot) then print,'Background modified by ',fixbg,' counts'
	counter += 1
endrep until (abs(fixbg) lt 0.001) or (counter gt 10)

if counter gt 10 then begin
   	errmsg = 'Background correction failed'
   	message, errmsg, /info
   	;return, 0
endif

;Resampling of diff-limited PSF.
;NOTE: the PSF image is oversampled by a factor 10 (output of psf_dl_esposito).
np_psf = n_elements(psf_difflim[0,*])
dx = 5-abs(coeff(4)-round(coeff(4)))*10
dy = 5-abs(coeff(5)-round(coeff(5)))*10
psf_difflim = rebin(shift(psf_difflim,dx,dy),np_psf/10,np_psf/10)*1e2
dl_flux = total(psf_difflim)
psf_difflim = psf_difflim/dl_flux*max(flux)
;max_dl = max(psf_difflim)

sr = max(ima_bs-new_bg)/max(psf_difflim)
if keyword_set(plot) then print, 'Minimum SR value (obtained with flux max)', sr
sr_side = sr*max(flux)/flux
if n_elements(plot) ne 0 then begin
    !p.multi=[0,1,3]
    plot, psf_difflim[250-side10/2:250+side10/2-1,250], charsize=2
    oplot, ima_bs[xc-side10/2:xc+side10/2-1,yc]-new_bg, thick=1.5
    oplot, psf_difflim[250-side10/2:250+side10/2-1,250]*sr,thick=1.5, color = 200L
    plot_io, ima_bs[xc-side10/2:xc+side10/2-1,yc]-new_bg + 40, psym =4, charsize=2
    oplot, psf_difflim[250-side10/2:250+side10/2-1,250]+40, thick=1.5
    oplot, ima_fit[xc-side10/2:xc+side10/2-1,yc]-new_bg + 40, color=200L
    plot_io, side_size, sr_side, charsize=2, /xstyle, yrange=[0.1,1.2], /ystyle
    oplot, side_size, flux/max(flux), color=200
    ;print, side_size, sr_side
    !p.multi=0
    window, 1
    plot, side_size, flux/max(flux), /yst, psym=-4
endif

; Test per problemi sul background
n_points=5
threshold = 0.001
if n_elements(flux) gt n_points then begin
   test = flux/max(flux)
;   print, test
   ss = (test-shift(test,1))[1:*]
   mm = mean(ss[n_elements(ss)-n_points:*])
   if abs(mm) gt threshold then errmsg = 'SR may be in error'
   if keyword_set(plot) then print, mm
endif else begin
   errmsg = 'Could not test SR quality'
endelse
if n_elements(errmsg) ne 0 then message, errmsg, /info

;print, ' bg ' , new_bg
;stop
return, sr
end


; 20110211_123412 = 0.0057 (forte sottostima)
; 20110211_123445 = 0.00025 (buona)
; 20110211_123843 = 0.0033 (sovrastima)
; 20110211_123916 = 0.0036 (sottostima)
