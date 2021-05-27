;**************** ON THE WFS MACHINE or Merry*****
; 20181101 NCPA OFF
R086_S13 = ['20181101_024723','20181101_024920','20181101_025019','20181101_025641','20181101_025722','20181101_025803','20181101_030044','20181101_030125','20181101_030205']
; FeII SR =75% seeing >1.0''
R090_S08 = ['20181101_040121', '20181101_040213','20181101_040318', '20181101_040402']
; H2 SR 87% S0.8"
R090_S10 = ['20181101_033919', '20181102_051425', '20181102_051556', '20181102_051721', '20181102_051804', '20181102_052655']
R120_S10 = ['20181101_043633', '20181101_043733', '20181101_043837', '20181220_125431', '20181220_125550', '20181220_130045', '20181220_132046']

;----------------------------------------
; 20190405 NCPA OFF
R086_S12 = ['20190406_044801','20190406_044845','20190406_045054','20190406_045229','20190406_045259','20190406_045345']
R112_S12 = ['20190406_061225','20190406_061332','20190406_061411','20190406_062306','20190406_062351','20190406_062432',$
  '20190406_062547','20190406_062632','20190406_062711','20190406_062858']
R129_S11 = ['20190406_065640','20190406_065727','20190406_065850','20190406_065946','20190406_070025','20190406_070158',$
  '20190406_070250','20190406_070800','20190406_070845','20190406_070924','20190406_071019']
R143_S12 = ['20190406_075636','20190406_080320','20190406_080409','20190406_080502']
; 20190407 NCPA OFF
R080_S27 = ['20190408_044818','20190408_044842','20190408_044917']
R080_S14 = ['20190408_052741','20190408_052826','20190408_052856','20190408_052930']
R080_S15 = ['20190408_055154','20190408_055230','20190408_055258','20190408_055528','20190408_055632','20190408_055705',$
  '20190408_060346','20190408_060426','20190408_060518','20190408_060755','20190408_060830']
R080_S13 = ['20190408_062424','20190408_062500','20190408_062531','20190408_062743','20190408_062821','20190408_062855',$
  '20190408_063034','20190408_063105']
; 20190407 NCPA ON
R080_S14N = ['20190408_070434','20190408_070508','20190408_070538','20190408_070813','20190408_070842','20190408_070912',$
  '20190408_070947','20190408_071014','20190408_071044']
; 20190408 NCPA OFF
R090_S09 = ['20190409_030235','20190409_030318','20190409_030346','20190409_030446','20190409_030911','20190409_030946',$
  '20190409_031016','20190409_031132','20190409_031235','20190409_031306','20190409_031341']
; 20190408, NCPA OFF, MGM On
R090_S10M = ['20190409_033055','20190409_033150','20190409_033247','20190409_033351','20190409_033905','20190409_034025',$
  '20190409_034153','20190409_034243','20190409_034447','20190409_035036']


; performances
set = [$
  R090_S08, $
  ; R090_S10, $
  ; R080_S27,$
  R080_S14,$
  ; R080_S15,$
  R080_S13,$
  R080_S14N,$
  R086_S12,$
  R090_S09,$
  R090_S10M,$
  R112_S12,$
  R120_S10,$
  R129_S11,$
  R143_S12 $
  ]
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TNs table

dir = '/data6/plantet/Data/SOUL/'

;dataset_to_tab, set, /left, filename = dir+'tmp'
restore,dir+'tmp.sav'

index = where(tab.dimm eq 1 and finite(tab.mag) and tab.haspsf eq 1)

seeings = tab.seeing[index]
mags = tab.mag[index]
tns = tab.tn[index]

tab_see = [0.7:1.7:0.1]
tab_mag = [9:15:1]

nsee = n_elements(tab_see)-1
nmags = n_elements(tab_mag)-1

tab_tns = strarr(nmags,nsee)

for i = 0,nsee-1 do begin
  for j = 0,nmags-1 do begin
    cur_index = where(seeings ge tab_see[i] and seeings lt tab_see[i+1] and $
      mags ge tab_mag[j] and mags lt tab_mag[j+1])
    if total(cur_index) ne -1 then tab_tns[j,i] = tns[cur_index[0]]
  endfor
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PSF table

loadct,3, rgb_table = rgb_table
npix = 70.
fullframe = 0
savefits = 0

tab_psf = fltarr(npix*nmags,npix*nsee)

xs = nmags*npix
ys = nsee*npix

window, xs = xs, ys = ys, /free

for i = 0,nsee-1 do begin
  for j = 0,nmags-1 do begin
    if tab_tns[j,i] ne '' then begin
      ee = getaoelab(tab_tns[j,i])
      
      sci_camera = cur_ee->luci())
      if NOT obj_valid(sci_camera) then sci_camera = cur_ee->lmircam()

      cur_psf = sci_camera->longexposure(fullframe = fullframe)
      
      psf_disp = bytscl(congrid(alog(cur_psf > 1),npix,npix))
      psf_rgb = bytarr(3,npix,npix)
      for k = 0,2 do psf_rgb[k,*,*] = rgb_table[psf_disp[*],k]
      tv, psf_rgb, /true, i*nmags+j
      x = .01+j/float(nmags)
      y = (nsee - i)/float(nsee)-.02
      xyouts, x, y, 'seeing '+string(.5*(tab_see[i]+tab_see[i+1]),format = '(F0.2)'), color = 'FFFFFF'x,/normal
      xyouts, x, y-.015, ' mag '+string(.5*(tab_mag[j]+tab_mag[j+1]),format = '(F0.1)'), color = 'FFFFFF'x,/normal
      
      tab_psf[j*npix:(j+1)*npix-1,i*npix:(i+1)*npix-1] = congrid(cur_psf,npix,npix)
      
      if keyword_set(savefits) then begin
        if keyword_set(fullframe) then tag_full = '_fullframe' else tag_full = ''
        tag_seeing = '_seeing'+string(.5*(tab_see[i]+tab_see[i+1]),format = '(F0.2)')
        tag_mag = '_mag'+string(.5*(tab_mag[j]+tab_mag[j+1]),format = '(F0.1)')
        file = 'SOUL_onsky_psf'+tag_seeing+tag_mag+tag_full
        writefits,dir+file+'.fits',cur_psf
      endif
      
    endif
  endfor
endfor

end
