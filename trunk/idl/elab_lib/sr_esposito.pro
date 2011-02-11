; from Simone's calc_sr1 17 nov 09
function sr_esposito, ima_bs, psf_difflim, lambda, irtc_sampling, plot=plot, errmsg = errmsg, FIX_BG = FIX_BG

ima_bs(319,255)= 0 ;;; set the bad pixel to zero
ima_bs[0:9,*] = 0
ima_bs[310:*,*] = 0
ima_bs[*,0:9] = 0
ima_bs[*,246:*] = 0
max_ima = max(ima_bs,h)

if n_elements(plot) ne 0 then print, 'Imax, x, y', max_ima,h/320,h-h/320*320
ima_fit = gauss2dfit(double(ima_bs),coeff)
xc = round(coeff(4))
yc = round(coeff(5))

npx = n_elements(ima_bs(*,0))
npy = n_elements(ima_bs(0,*))

a = 20 ;;; larghezza della cornice usata per il calcolo del background
ima_dummy = ima_bs
ima_dummy(a:npx-a-1,a:npy-a-1) = 0.0
ind0= where(ima_dummy EQ 0.0,count0)
npbg = float(1.0*npx*npy)-count0
new_bg = total(ima_dummy)/ npbg
fixbg=0
counter=0

repeat begin
   new_bg += fixbg
   side = min([xc,320-xc,yc,256-yc],h)
   side10 = fix(side)/10*10
   nside = side10/10 * 2 -1
   flux = fltarr(nside)
   side_size= fltarr(nside)
   for i= 1, nside do begin
      side_size(i-1) = 10*i
      ima_side = ima_bs(xc-5*i:xc+5*i-1,yc-5*i:yc+5*i-1)
      flux(i-1) = total(ima_side-replicate(new_bg,10*i,10*i))
   endfor

   if keyword_set(FIX_BG) then begin

      dflux = flux-shift(flux,1)
      ssize = side_size^2
      dssize = ssize-shift(ssize,1)
      aaa = dflux / dssize
      aaa_len = n_elements(aaa)
      fixbg = mean(aaa[aaa_len-4:aaa_len-1])
      print,'Background modified by ',fixbg,' counts'
   endif

counter += 1
endrep until (abs(fixbg) lt 0.001) or (counter gt 10)

if counter gt 10 then begin
    errmsg = 'Background correction failed'
    print, errmsg
    return, 0
endif
 

;irtc_sampling = 0.01 ;;;; arcsec
D = 8.22 ;;; diametro di LBT in m
np_psf = 5000  ;;; punti della psf_difflim

dx = 5-abs(coeff(4)-round(coeff(4)))*10
dy = 5-abs(coeff(5)-round(coeff(5)))*10
psf_difflim = rebin(shift(psf_difflim,dx,dy),np_psf/10,np_psf/10)*1e2
dl_flux = total(psf_difflim)
psf_difflim = psf_difflim/dl_flux*max(flux)
max_dl = max(psf_difflim)

sr = max(ima_bs-new_bg)/max(psf_difflim)
if n_elements(plot) ne 0 then print, 'Minimum SR value (obtained with flux max)', sr
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
    !p.multi=0
endif

; Test per problemi sul background
errmsg=''
n_points=5
; 20110211_123412 = 0.0057 (forte sottostima)
; 20110211_123445 = 0.00025 (buona)
; 20110211_123843 = 0.0033 (sovrastima)
; 20110211_123916 = 0.0036 (sottostima)

threshold = 0.001
if n_elements(flux) gt n_points then begin
   test = flux/max(flux)
;   print, test
   ss = (test-shift(test,1))[1:*]
   mm = mean(ss[n_elements(ss)-n_points:*])
   if abs(mm) gt threshold then errmsg = 'SR may be in error' 
   print,mm
endif else begin
   errmsg = 'Cannot test SR quality'
endelse
;stop
return, sr
end



