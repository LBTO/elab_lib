;function airy, lambda, D, teta
;;; lambda in micron
;;; D Diametro in m
;;; teta fuoriasse in arcsec
;x = !pi*(teta*4.848)/(lambda/D)
;if x EQ 0.0 then begin
;  val = 1
;endif else begin
;  val = (2*beselj(x,1,/DOUBLE)/x)^2
;endelse
;return, val
;end

;function psf_dl, irtc_sampling
;    ;irtc_sampling = 0.01 ;;;; arcsec/px
;    lambda = 1.6 ;;; filtro H 1.6 micron
;    D = 8.25 ;;; diametro di LBT in m
;    np_psf = 5000  ;;; punti della psf_dl
;    psf_dl = fltarr(np_psf, np_psf)
;    for i=0,np_psf-1 do begin
;        for j=0,np_psf-1 do begin
;        teta1 = sqrt((1.*i-np_psf/2)^2+(1.*J-np_psf/2)^2)*irtc_sampling/10.
;        psf_dl(i,j) = airy(lambda, D, teta1)
;        endfor
;    endfor
;    return, psf_dl
;end

; from Simone's calc_sr1 17 nov 09
function sr_esposito, ima_bs, psf_difflim, plot=plot
;irtc_pix = 0.01 ;;; arcsec
;data_dir = 'C:\simone_work\FLAO\calcolo psf\Data_20091117_114815\'
;bg_file = data_dir+'20091117_114104.fits_cube.fits'
;ima_file = data_dir+'irtc.fits'
;bg = readfits(bg_file)
;nbg = n_elements(bg(0,0,*))
;ima = readfits(ima_file)
;nima = n_elements(ima(0,0,*))

;ima1 = total(float(ima),3)/nima
;bg1 = total(float(bg),3)/nbg
;ima_bs = ima1-bg1
ima_bs(319,255)= 0 ;;; set the bad pixel to zero
max_ima = max(ima_bs,h)
if n_elements(plot) ne 0 then print, 'Imax, x, y', max_ima,h/320,h-h/320*320
ima_fit = gauss2dfit(double(ima_bs),coeff)
xc = round(coeff(4))
yc = round(coeff(5))
new_bg = coeff(0)

npx = n_elements(ima_bs(*,0))
npy = n_elements(ima_bs(0,*))

a = 20 ;;; larghezza della cornice usata per il calcolo del background
ima_dummy = ima_bs
ima_dummy(a:npx-a-1,a:npy-a-1) = 0.0
ind0= where(ima_dummy EQ 0.0,count0)
npbg = float(1.0*npx*npy)-count0
new_bg = total(ima_dummy)/ npbg
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
irtc_sampling = 0.01 ;;;; arcsec
lambda = 1.6 ;;; filtro H 1.6 micron
D = 8.25 ;;; diametro di LBT in m
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
    plot_io, side_size, sr_side, charsize=2, /xstyle, yrange=[0.01,1.2]
    oplot, side_size, flux/max(flux), color=200
    !p.multi=0
endif

return, sr
end


