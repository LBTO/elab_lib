pro lbti_pupils, sys = sys

;sys: System to be checked with on-sky footprints. sys = 'LBTISX' or sys = 'LUCI1'.

;Directories are for Arcetri servers. To be updated to make it compatible with LBT servers.

;LBTI pupils
dir = '/home/elab/LBT_data_left/wfs_calib_LBTISX/ocam2/LUTs/'
;bin1
p1a = readfits(dir+'mode2/20200103_074805/pup1.fits')
p1b = readfits(dir+'mode2/20200103_074805/pup2.fits')
p1c = readfits(dir+'mode2/20200103_074805/pup3.fits')
p1d = readfits(dir+'mode2/20200103_074805/pup4.fits')

;bin2
p2a = readfits(dir+'mode3/20181222_065045/pup1.fits')
p2b = readfits(dir+'mode3/20181222_065045/pup2.fits')
p2c = readfits(dir+'mode3/20181222_065045/pup3.fits')
p2d = readfits(dir+'mode3/20181222_065045/pup4.fits')

;bin3
p3a = readfits(dir+'mode4/20201221_205729/pup1.fits')
p3b = readfits(dir+'mode4/20201221_205729/pup2.fits')
p3c = readfits(dir+'mode4/20201221_205729/pup3.fits')
p3d = readfits(dir+'mode4/20201221_205729/pup4.fits')

;bin4
p4a = readfits(dir+'mode5/20180719_040754/pup1.fits')
p4b = readfits(dir+'mode5/20180719_040754/pup2.fits')
p4c = readfits(dir+'mode5/20180719_040754/pup3.fits')
p4d = readfits(dir+'mode5/20180719_040754/pup4.fits')

;old LBTI pupils
dir = '/home/elab/LBT_data_left/wfs_calib_LBTISX/ocam2/LUTs/'
;bin1
p1a_old = readfits(dir+'mode2/20180920_101624/pup1.fits')
p1b_old = readfits(dir+'mode2/20180920_101624/pup2.fits')
p1c_old = readfits(dir+'mode2/20180920_101624/pup3.fits')
p1d_old = readfits(dir+'mode2/20180920_101624/pup4.fits')

;LUCI pupils
dir = '/home/elab/LBT_data_left/wfs_calib_W1SOUL/ocam2/LUTs/'
;bin1
p1a_ref = readfits(dir+'mode2/20210210_205151/pup1.fits')
p1b_ref = readfits(dir+'mode2/20210210_205151/pup2.fits')
p1c_ref = readfits(dir+'mode2/20210210_205151/pup3.fits')
p1d_ref = readfits(dir+'mode2/20210210_205151/pup4.fits')

;bin2
p2a_ref = readfits(dir+'mode3/20210127_180255/pup1.fits')
p2b_ref = readfits(dir+'mode3/20210127_180255/pup2.fits')
p2c_ref = readfits(dir+'mode3/20210127_180255/pup3.fits')
p2d_ref = readfits(dir+'mode3/20210127_180255/pup4.fits')

;bin3
p3a_ref = readfits(dir+'mode4/20210219_221915/pup1.fits')
p3b_ref = readfits(dir+'mode4/20210219_221915/pup2.fits')
p3c_ref = readfits(dir+'mode4/20210219_221915/pup3.fits')
p3d_ref = readfits(dir+'mode4/20210219_221915/pup4.fits')

;bin4
p4a_ref = readfits(dir+'mode5/20200916_173857/pup1.fits')
p4b_ref = readfits(dir+'mode5/20200916_173857/pup2.fits')
p4c_ref = readfits(dir+'mode5/20200916_173857/pup3.fits')
p4d_ref = readfits(dir+'mode5/20200916_173857/pup4.fits')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Pupil diff

;bin1
np = 120
lbti_pup1 = fltarr(np*2,np*2)
lbti_pup1_old = fltarr(np*2,np*2)
luci_pup1 = fltarr(np*2,np*2)
lbti_pup1[[p1a,p1b,p1c,p1d]] = 1
lbti_pup1_old[[p1a_old,p1b_old,p1c_old,p1d_old]] = 1
luci_pup1[[p1a_ref,p1b_ref,p1c_ref,p1d_ref]] = 1

c_lbti_pup1 = [total(total(lbti_pup1,2)*findgen(np*2)), $
  total(total(lbti_pup1,1)*findgen(np*2))]/total(lbti_pup1)
c_lbti_pup1_old = [total(total(lbti_pup1_old,2)*findgen(np*2)), $
  total(total(lbti_pup1_old,1)*findgen(np*2))]/total(lbti_pup1_old)
c_luci_pup1 = [total(total(luci_pup1,2)*findgen(np*2)), $
    total(total(luci_pup1,1)*findgen(np*2))]/total(luci_pup1)

center_lbti1 = (round(c_lbti_pup1) > np/2) < 3*np/2
center_lbti1_old = (round(c_lbti_pup1_old) > np/2) < 3*np/2
center_luci1 = (round(c_luci_pup1) > np/2) < 3*np/2

lbti_pup1 = lbti_pup1[center_lbti1[0]-np/2:center_lbti1[0]+np/2-1,center_lbti1[1]-np/2:center_lbti1[1]+np/2-1]
lbti_pup1_old = lbti_pup1_old[center_lbti1_old[0]-np/2:center_lbti1_old[0]+np/2-1,center_lbti1_old[1]-np/2:center_lbti1_old[1]+np/2-1]
luci_pup1 = luci_pup1[center_luci1[0]-np/2:center_luci1[0]+np/2-1,center_luci1[1]-np/2:center_luci1[1]+np/2-1]

diff_pup1 = lbti_pup1-luci_pup1
diff_pup1_old = lbti_pup1-lbti_pup1_old
diff_pup1b = lbti_pup1-shift(luci_pup1,0,1)
diff_pup1b_old = lbti_pup1-shift(lbti_pup1_old,0,1)

window,0, xs = 360*3,ys=120*3, title = 'Bin 1' & tvscl, congrid([lbti_pup1,luci_pup1,diff_pup1],360*3,120*3)
xyouts, 60*3-20, 120*3-30,'LBTI', color = 255, charsize = 2, charthick = 2,/device
xyouts, 180*3-20, 120*3-30,'LUCI', color = 255, charsize = 2, charthick = 2,/device
xyouts, 300*3-20, 120*3-30,'DIFF', color = 255, charsize = 2, charthick = 2,/device

window,4, xs = 360*3,ys=120*3, title = 'Bin 1 old' & tvscl, congrid([lbti_pup1,lbti_pup1_old,diff_pup1_old],360*3,120*3)
xyouts, 60*3-20, 120*3-30,'LBTI', color = 255, charsize = 2, charthick = 2,/device
xyouts, 180*3-20, 120*3-30,'LBTI old', color = 255, charsize = 2, charthick = 2,/device
xyouts, 300*3-20, 120*3-30,'DIFF', color = 255, charsize = 2, charthick = 2,/device

;bin2
np = 60
lbti_pup2 = fltarr(np*2,np*2)
luci_pup2 = fltarr(np*2,np*2)
lbti_pup2[[p2a,p2b,p2c,p2d]] = 1
luci_pup2[[p2a_ref,p2b_ref,p2c_ref,p2d_ref]] = 1

c_lbti_pup2 = [total(total(lbti_pup2,2)*findgen(np*2)), $
  total(total(lbti_pup2,1)*findgen(np*2))]/total(lbti_pup2)
c_luci_pup2 = [total(total(luci_pup2,2)*findgen(np*2)), $
  total(total(luci_pup2,1)*findgen(np*2))]/total(luci_pup2)

center_lbti2 = (round(c_lbti_pup2) > np/2) < 3*np/2
center_luci2 = (round(c_luci_pup2) > np/2) < 3*np/2

lbti_pup2 = lbti_pup2[center_lbti2[0]-np/2:center_lbti2[0]+np/2-1,center_lbti2[1]-np/2:center_lbti2[1]+np/2-1]
luci_pup2 = luci_pup2[center_luci2[0]-np/2:center_luci2[0]+np/2-1,center_luci2[1]-np/2:center_luci2[1]+np/2-1]
diff_pup2 = lbti_pup2-luci_pup2

diff_pup2b = lbti_pup2-shift(luci_pup2,0,1)

window,1, xs = 360*3,ys=120*3, title = 'Bin 2' & tvscl, congrid([lbti_pup2,luci_pup2,diff_pup2],360*3,120*3)
xyouts, 60*3-20, 120*3-30,'LBTI', color = 255, charsize = 2, charthick = 2,/device
xyouts, 180*3-20, 120*3-30,'LUCI', color = 255, charsize = 2, charthick = 2,/device
xyouts, 300*3-20, 120*3-30,'DIFF', color = 255, charsize = 2, charthick = 2,/device

;bin3
np = 40
lbti_pup3 = fltarr(np*2,np*2)
luci_pup3 = fltarr(np*2,np*2)
lbti_pup3[[p3a,p3b,p3c,p3d]] = 1
luci_pup3[[p3a_ref,p3b_ref,p3c_ref,p3d_ref]] = 1

c_lbti_pup3 = [total(total(lbti_pup3,2)*findgen(np*2)), $
  total(total(lbti_pup3,1)*findgen(np*2))]/total(lbti_pup3)
c_luci_pup3 = [total(total(luci_pup3,2)*findgen(np*2)), $
  total(total(luci_pup3,1)*findgen(np*2))]/total(luci_pup3)

center_lbti3 = (round(c_lbti_pup3) > np/2) < 3*np/2
center_luci3 = (round(c_luci_pup3) > np/2) < 3*np/2

lbti_pup3 = lbti_pup3[center_lbti3[0]-np/2:center_lbti3[0]+np/2-1,center_lbti3[1]-np/2:center_lbti3[1]+np/2-1]
luci_pup3 = luci_pup3[center_luci3[0]-np/2:center_luci3[0]+np/2-1,center_luci3[1]-np/2:center_luci3[1]+np/2-1]
diff_pup3 = lbti_pup3-luci_pup3

diff_pup3b = lbti_pup3-shift(luci_pup3,0,1)

window,2, xs = 360*3,ys=120*3, title = 'Bin 3' & tvscl, congrid([lbti_pup3,luci_pup3,diff_pup3],360*3,120*3)
xyouts, 60*3-20, 120*3-30,'LBTI', color = 255, charsize = 2, charthick = 2,/device
xyouts, 180*3-20, 120*3-30,'LUCI', color = 255, charsize = 2, charthick = 2,/device
xyouts, 300*3-20, 120*3-30,'DIFF', color = 255, charsize = 2, charthick = 2,/device


;bin4
np = 30
lbti_pup4 = fltarr(np*2,np*2)
luci_pup4 = fltarr(np*2,np*2)
lbti_pup4[[p4a,p4b,p4c,p4d]] = 1
luci_pup4[[p4a_ref,p4b_ref,p4c_ref,p4d_ref]] = 1

c_lbti_pup4 = [total(total(lbti_pup4,2)*findgen(np*2)), $
  total(total(lbti_pup4,1)*findgen(np*2))]/total(lbti_pup4)
c_luci_pup4 = [total(total(luci_pup4,2)*findgen(np*2)), $
  total(total(luci_pup4,1)*findgen(np*2))]/total(luci_pup4)

center_lbti4 = (round(c_lbti_pup4) > np/2) < 3*np/2
center_luci4 = (round(c_luci_pup4) > np/2) < 3*np/2

lbti_pup4 = lbti_pup4[center_lbti4[0]-np/2:center_lbti4[0]+np/2-1,center_lbti4[1]-np/2:center_lbti4[1]+np/2-1]
luci_pup4 = luci_pup4[center_luci4[0]-np/2:center_luci4[0]+np/2-1,center_luci4[1]-np/2:center_luci4[1]+np/2-1]
diff_pup4 = lbti_pup4-luci_pup4

diff_pup4b = lbti_pup4-shift(luci_pup4,0,1)

window,3, xs = 360*3,ys=120*3, title = 'Bin 4' & tvscl, congrid([lbti_pup4,luci_pup4,diff_pup4],360*3,120*3)
xyouts, 60*3-20, 120*3-30,'LBTI', color = 255, charsize = 2, charthick = 2,/device
xyouts, 180*3-20, 120*3-30,'LUCI', color = 255, charsize = 2, charthick = 2,/device
xyouts, 300*3-20, 120*3-30,'DIFF', color = 255, charsize = 2, charthick = 2,/device

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;On-sky TNs to check footprints

;LBTI-SX set
if sys eq 'LBTISX' then begin
  set = obj_new('aodataset', from = '20190224_040435', to = '20190224_121044')
  set1 = set->where('wfs_status.camera.binning','eq',1)
  set4 = set->where('wfs_status.camera.binning','eq',4)
  pup_bin1 = lbti_pup1
  pup_bin4 = lbti_pup4
  center1 = center_lbti1
  center4 = center_lbti4
  thresh1 = 8.
  thresh4 = 8.
endif
if sys eq 'LUCI1' then begin
  set1 = obj_new('aodataset', from = '20200707_090207', to = '20200707_090618')
  set4 = obj_new('aodataset', from = '20191109_070419', to = '20191109_071135')
  pup_bin1 = luci_pup1
  pup_bin4 = luci_pup4
  center1 = center_luci1
  center4 = center_luci4
  thresh1 = 8.
  thresh4 = 3.
endif

ndata1 = n_elements(set1->tracknums())
ndata4 = n_elements(set4->tracknums())

ptr1 = set1->value('frames.frames(/dark)')
ptr4 = set4->value('frames.frames(/dark)')

if sys eq 'LBTISX' then begin
  frames1_avg = mean(*ptr1[0],dim=3)/ndata1
  frames4_avg = mean(*ptr4[0],dim=3)/ndata4
  for i = 1, ndata1-1 do frames1_avg += mean(*ptr1[i],dim=3)/ndata1
  for i = 1, ndata4-1 do frames4_avg += mean(*ptr4[i],dim=3)/ndata4
endif else begin
  frames1_avg = mean(mean(ptr1,dim=3),dim=3)
  frames4_avg = mean(mean(ptr4,dim=3),dim=3)
endelse

np1 = 120
np4 = 30

frames1_avg  = frames1_avg[center1[0]-np1/2:center1[0]+np1/2-1,*]
frames4_avg  = frames4_avg[center4[0]-np4/2:center4[0]+np4/2-1,center4[1]-np4/2:center4[1]+np4/2-1]

frames1_avg2 = frames1_avg gt thresh1*median(frames1_avg)
frames4_avg2 = frames4_avg gt thresh4*median(frames4_avg)

window,5, xs = 480*3,ys=120*3, title = 'Bin 1 on sky'
tvscl, congrid(frames1_avg,120*3,120*3),0
tvscl, congrid(frames1_avg2,120*3,120*3),1
tvscl, congrid(pup_bin1,120*3,120*3),2
tvscl, congrid(frames1_avg2-pup_bin1,120*3,120*3),3
xyouts, 60*3-65, 120*3-30,'On-sky average', color = 255, charsize = 2, charthick = 2,/device
xyouts, 180*3-65, 120*3-30,'Approx. footprint', color = 255, charsize = 2, charthick = 2,/device
xyouts, 300*3-25, 120*3-30,'Pupils', color = 255, charsize = 2, charthick = 2,/device
xyouts, 420*3-15, 120*3-30,'Diff', color = 255, charsize = 2, charthick = 2,/device

window,6, xs = 480*3,ys=120*3, title = 'Bin 4 on sky'
tvscl, congrid(frames4_avg,120*3,120*3),0
tvscl, congrid(frames4_avg2,120*3,120*3),1
tvscl, congrid(pup_bin4,120*3,120*3),2
tvscl, congrid(frames4_avg2-pup_bin4,120*3,120*3),3
xyouts, 60*3-65, 120*3-30,'On-sky average', color = 255, charsize = 2, charthick = 2,/device
xyouts, 180*3-65, 120*3-30,'Approx. footprint', color = 255, charsize = 2, charthick = 2,/device
xyouts, 300*3-25, 120*3-30,'Pupils', color = 255, charsize = 2, charthick = 2,/device
xyouts, 420*3-15, 120*3-30,'Diff', color = 255, charsize = 2, charthick = 2,/device

end