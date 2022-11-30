; +
;
;   check_pupil_IM
;
;   procedure to check the X and Y signals on the sub-aperture
;   to verify the index of valid sub-aperture, also known as pupil
;
;   cloop_TN    = string of TN for Optical Loop Data acquired
;                 with pupil to be checked 
;                 example '20191109_040213'
;   combined_IM = string of full path      
;                 'adsec_calib/M2C/KL_v20/RECs/Intmat_20181215_201757.fits'     
;   side        = (optional) 'SX' or 'DX' depending on the telescope side               
;                            this is not required on the WFS/ADSEC workstations
;
;   authors Enrico Pinna    enrico.pinna@inaf.it
;           Guido Agapito   guido.agapito@inaf.it
;
; -

pro check_pupil_IM, cloop_TN, combined_IM, side=side

; initialize the elab_lib to get access to pupils and remap functions
if n_elements(side) eq 0 then begin
    ao_init, /white
endif else begin
    if side EQ 'SX' then ao_init, /left, /white
    if side EQ 'DX' then ao_init, /right, /white
endelse

ee=getaoelab(cloop_TN)

; restore the interaction matrix (combined signals)
im=readfits(ao_datadir()+path_sep()+combined_IM)

; computes the RMS
rms_im = fltarr((size(im,/dim))[1])
for i=0,(size(im,/dim))[1]-1 do rms_im[i] = rms(im[*,i])

; normalize the RMS
rms_im = rms_im/mean(rms_im)

; remap the slopes
sl2d = (ee->slopes())->slopes2d(slopevec=rms_im)

; empty slope vector to have a reference of valid sub-apertures
sl0 = (ee->slopes())->slopes2d(slopevec=fltarr(2484)+0.1)-0.1

; select the index where the IM is greater than 0
rms_im_good = rms_im[where(rms_im gt 0, count_good)]

; Soring X and Y slopes in 2 different variables
rms_im_good_x = rms_im_good[0:n_elements(rms_im_good)-2:2]
rms_im_good_y = rms_im_good[1:n_elements(rms_im_good)-1:2]

; max normalized RMS
rms_im_good_max = rms_im_good_x*0.
for i=0,n_elements(rms_im_good_max)-1 do $
  rms_im_good_max[i] = max([rms_im_good_x[i],rms_im_good_y[i]])

; elements with normalized RMS lower or equal to 0.5
idx_xy_le05 = where(rms_im_good_x le 0.5 and rms_im_good_y le 0.5, count_xy_le05)
print, 'SA below 0.5 of norm. RMS: ', strtrim(count_xy_le05,2)
print, 'over: ', strtrim(count_good/2,2), ' elements'

; display the slopes RMS
window, 0, xs=1200, ys=450
loadct, 34
image_show, /as, /sh, (sl2d+sl0)[120:239,0:59]
; histogram display

window, 1, xs=900, ys=675
plothist, rms_im_good_max, bin=0.1, $
  tit='!17', xtit='norm. slope RMS on sub-aperture', ytit='occurences', $
  xgridstyle=1, ygridstyle=1, xticklen=1, yticklen=1, charsize=3, $
  fcolor=255l;, xmargin=[12, 3], ymargin=[12, 2]

end
