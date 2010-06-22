;pro batch_sr_vs_mag

set3_bin1_h_0529 = obj_new('aodataset', from='20100529_045651', to='20100529_051205')
sr3_bin1_h_0529 = set3_bin1_h_0529->value('irtc.sr_se')
set3_bin1_h_0601 = obj_new('aodataset', from='20100601_034910', to='20100601_035123')
sr3_bin1_h_0601 = set3_bin1_h_0601->value('irtc.sr_se')
set3_bin1_h_0603 = obj_new('aodataset', from='20100603_035522', to='20100603_043715')
sr3_bin1_h_0603 = set3_bin1_h_0603->value('irtc.sr_se')

;set3_bin1_h->merge, set3_bin1_h_bis 


set93_bin1_h_0531 = obj_new('aodataset', from='20100531_054115', to='20100531_062602')
sr93_bin1_h_0531 = set93_bin1_h_0531->value('irtc.sr_se')

set93_bin1_j_0531 = obj_new('aodataset', from='20100531_062934', to='20100531_064130')
sr93_bin1_j_0531 = set93_bin1_j_0531->value('irtc.sr_se')

set93_bin2_h_0531 = obj_new('aodataset', from='20100531_071124', to='20100531_072850')
sr93_bin2_h_0531 = set93_bin2_h_0531->value('irtc.sr_se')
set93_bin2_h_0603 = obj_new('aodataset', from='20100603_051742', to='20100603_052149')
sr93_bin2_h_0603 = set93_bin2_h_0603->value('irtc.sr_se')

set63_bin2_h_0531 = obj_new('aodataset', from='20100531_075915', to='20100531_081826')
sr63_bin2_h_0531 = set63_bin2_h_0531->value('irtc.sr_se')

set111_bin2_h_0531 = obj_new('aodataset', from='20100531_085035', to='20100531_091302')
sr111_bin2_h_0531 = set111_bin2_h_0531->value('irtc.sr_se')

set111_bin3_h_0531 = obj_new('aodataset', from='20100531_093542', to='20100531_093859')
sr111_bin3_h_0531 = set111_bin3_h_0531->value('irtc.sr_se')

set122_bin2_h_0603 = obj_new('aodataset', from='20100603_060640', to='20100603_061105')
sr122_bin2_h_0603 = set122_bin2_h_0603->value('irtc.sr_se')

set122_bin3_h_0601 = obj_new('aodataset', from='20100601_065349', to='20100601_065609')
sr122_bin3_h_0601 = set122_bin3_h_0601->value('irtc.sr_se')

set122_bin4_h_0601 = obj_new('aodataset', from='20100601_071105', to='20100601_072414')
sr122_bin4_h_0601 = set122_bin4_h_0601->value('irtc.sr_se')

set84_bin3_h_0601 = obj_new('aodataset', from='20100601_082644', to='20100601_083541')
sr84_bin3_h_0601 = set84_bin3_h_0601->value('irtc.sr_se')

set84_bin2_h_0601 = obj_new('aodataset', from='20100601_085057', to='20100601_090118')
sr84_bin2_h_0601 = set84_bin2_h_0601->value('irtc.sr_se')

set74_bin4_h_0601 = obj_new('aodataset', from='20100601_101108', to='20100601_101332')
sr74_bin4_h_0601 = set74_bin4_h_0601->value('irtc.sr_se')

set151_bin2_h_0603 = obj_new('aodataset', from='20100603_084308', to='20100603_084638')
sr151_bin2_h_0603 = set151_bin2_h_0603->value('irtc.sr_se')

set162_bin1_h_0604 = obj_new('aodataset', from='20100604_033658', to='20100604_034533')
sr162_bin1_h_0604 = set162_bin1_h_0604->value('irtc.sr_se')

set72_bin2_h_0604 = obj_new('aodataset', from='20100604_054156', to='20100604_054348')
sr72_bin2_h_0604 = set72_bin2_h_0604->value('irtc.sr_se')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

mag_simul = findgen(8)+8.5
sr_base = [76., 73, 61, 59, 51, 33, 23, 8.8, 3.2] - 0.75 ; 50% loss due to M2 coating

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

mag3_cat = 4.9
mag93_cat = 7.1
mag63_cat = 9.4
mag162_cat = 9.5
mag111_cat = 11.4
mag122_cat = 11.5
mag151_cat = 12.5
mag72_cat = 13.5
mag84_cat = 13.6
mag74_cat = 14.4


mag_cat = [mag3_cat,  mag93_cat, mag63_cat, mag162_cat, mag111_cat, mag122_cat, mag151_cat, $
           mag72_cat, mag84_cat, mag74_cat] 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


plot, [4,15], [0,100], /nodata, xra=[4,15], yra=[0,100], charsize=2, symsi=2, thick=2, $
    xtitle='R mag', ytitle='SR'
oplot, mag_simul, sr_base, psym=-1, symsi=2, thick=2
oplot, [mag3_cat],   [median(sr3_bin1_h_0529)]*100, psym=-4, col=255L, symsi=2, thick=2
oplot, [mag3_cat],   [median(sr3_bin1_h_0601)]*100, psym=-4, col=255L, symsi=2, thick=2
oplot, [mag3_cat],   [median(sr3_bin1_h_0603)]*100, psym=-4, col=255L, symsi=2, thick=2
oplot, [mag93_cat],  [median(sr93_bin1_h_0531)]*100, psym=-4, col=255L, symsi=2, thick=2
oplot, [mag93_cat],  [median(sr93_bin2_h_0531)]*100, psym=-2, col=255L, symsi=2, thick=2
oplot, [mag93_cat],  [median(sr93_bin2_h_0603)]*100, psym=-2, col=255L, symsi=2, thick=2
oplot, [mag63_cat],  [median(sr63_bin2_h_0531)]*100, psym=-2, col=255L, symsi=2, thick=2
oplot, [mag162_cat], [median(sr162_bin1_h_0604)]*100, psym=-4, col=255L, symsi=2, thick=2
oplot, [mag111_cat], [median(sr111_bin2_h_0531)]*100, psym=-2, col=255L, symsi=2, thick=2
oplot, [mag111_cat], [median(sr111_bin3_h_0531)]*100, psym=-1, col=255L, symsi=2, thick=2
oplot, [mag122_cat], [median(sr122_bin2_h_0603)]*100, psym=-2, col=255L, symsi=2, thick=2
oplot, [mag122_cat], [median(sr122_bin3_h_0601)]*100, psym=-1, col=255L, symsi=2, thick=2
oplot, [mag122_cat], [median(sr122_bin4_h_0601)]*100, psym=-6, col=255L, symsi=2, thick=2
oplot, [mag151_cat], [median(sr151_bin2_h_0603)]*100, psym=-2, col=255L, symsi=2, thick=2
oplot, [mag72_cat],  [median(sr72_bin2_h_0604)]*100,  psym=-2, col=255L, symsi=2, thick=2
oplot, [mag84_cat],  [median(sr84_bin3_h_0601)]*100,  psym=-1, col=255L, symsi=2, thick=2
oplot, [mag84_cat],  [median(sr84_bin2_h_0601)]*100,  psym=-2, col=255L, symsi=2, thick=2
oplot, [mag74_cat],  [median(sr74_bin4_h_0601)]*100,  psym=-6, col=255L, symsi=2, thick=2
oplot, [mag151_cat], [median(sr151_bin2_h_0603)]*100, psym=-2, col=255L, symsi=2, thick=2
legend,['bin 1','bin 2','bin 3', 'bin 4'], psym=[4,2,1,6], charsi=2, /top, /right
write_jpeg, filepath(root=ao_elabdir(), 'sr_vs_mag.jpeg'), tvrd(true=3), true=3      

end

