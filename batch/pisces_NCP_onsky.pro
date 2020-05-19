
set = obj_new('aodataset', from='20110612_064412', to='20110612_064749')
sr21=fltarr(21)
for i=0,20 do sr21[i]= (((getaoelab(set->get(pos=i)))->pisces())->star_sr())[0]
xra = (findgen(7)/6 - 0.5 ) * 80 ; nm
plot, xra, sr21[0:6], ytitle='SR [%]', charsi=1.5, title=' NCP on sky measurement ', xtitle='Amplitude [nm]'
oplot, xra, sr21[7:13], col='0000ff'x
oplot, xra, sr21[14:20], col='00ff00'x
legend, ['Z4','Z5', 'Z6'], col=['0000ff'x, 'ffffff'x, '00ff00'x], lines=[0,0,0]


