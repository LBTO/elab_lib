pro analyzeXYstages

setXY = obj_new('aodataset', from='20111012_063000', to='20111012_065900')

imas =  setXY->value('pisces.longexposure(/full)')
imassum = total(imas, 3)

starpos =  setXY->value('pisces.star_position_px(0)')
starpos[*,14]=[743, 546] 

image_show, /as, /log, imassum>1
oplot, starpos[0,*], starpos[1,*], psym=4, symsi=2
write_jpeg, 'offsetXYpisces.jpeg', tvrd(true=3), true=3

for i =0, n_elements(starpos[0,*])-1 do starpos[*,i] = pisces_distortion(starpos[*,i])

dpisces = (starpos-rebin(starpos[*,0],2,setXY->count())) * 0.01924 
dpisces=dpisces[[1,0],*]
dpisces[1,*] *=-1

; corregere distorsione di pisces
dsky = dpisces  ; arcsec

; stages position
stages =  setXY->value('wfs_status.stages')
orig = stages[*,0]

deltastages = 1./0.6 * (stages-rebin(orig, 3, setXY->count()))[0:1,*] ; arcsec

plot, deltastages[0,*], deltastages[1,*], psym=4, color=0, back='ffffff'x, $
    symsi=2, xtit='x-stage (mm)', ytit='y-stage (mm)', thick=2, charsi=2
write_jpeg, 'offsetXYstages.jpeg', tvrd(true=3), true=3

err = dsky-deltastages
plot, err[0,*], err[1,*], /iso, psym=4, xra=[-0.2,0.2], xtit='dx [arcsec]', ytit='dy [arcsec]', charsi=2, thick=2 
write_jpeg, 'offsetXYerror.jpeg', tvrd(true=3), true=3

end


