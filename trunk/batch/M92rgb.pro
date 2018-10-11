set = obj_new('aodataset', from='20110613_075027', to='20110613_081933') ;M92 K lungo
imas=set->value('pisces.longexposure(/full)') & nimas = n_elements(imas[0,0,*])
off = fltarr(2,nimas) & for i=0,nimas-1 do off[*,i]=measure_images_shift(imas[*,*,0], imas[*,*,i])
sum=fltarr(1024,1024) & for i =0,nimas-1 do begin sum+=shift(imas[*,*,i], -off[0,i], -off[1,i]) & endfor & sum/=nimas
sumklungo=sum


;set = obj_new('aodataset', from='20110611_072023', to='20110611_073822') 
set = obj_new('aodataset', from='20110613_095236', to='20110613_100615') ;M92 J lungo
imas=set->value('pisces.longexposure(/full)') & nimas = n_elements(imas[0,0,*])
off = fltarr(2,nimas) & for i=0,nimas-1 do off[*,i]=measure_images_shift(imas[*,*,0], imas[*,*,i])
sum=fltarr(1024,1024) & for i =0,nimas-1 do begin sum+=shift(imas[*,*,i], -off[0,i], -off[1,i]) & endfor & sum/=nimas
sumjlungo=sum


kl = alog10(sumklungo+10>5)
;jl = alog10(rot(sumjlungo+10>5,90))
jl = alog10(sumjlungo+50>10)
off = measure_images_shift( kl,jl )
jls = shift(jl, -off[0], -off[1])

kl *= 255/max(kl)
jls *= 255/max(jls)

ima = fltarr(3,1024,1024) & ima[0,*,*]=kl & ima[2,*,*]=jls & ima[1,*,*]=0.5*(jls+kl) 

tv, ima, true=1

;;;;;;;;;;;;;;;;;;;;;;;; corto ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

set = obj_new('aodataset', from='20110613_094121', to='20110613_094121') ; M92 K corto
imas=set->value('pisces.longexposure(/full)') & nimas = n_elements(imas[0,0,*])
off = fltarr(2,nimas) & for i=0,nimas-1 do off[*,i]=measure_images_shift(imas[*,*,0], imas[*,*,i])
sum=fltarr(1024,1024) & for i =0,nimas-1 do begin sum+=shift(imas[*,*,i], -off[0,i], -off[1,i]) & endfor & sum/=nimas
sumkcorto=sum


set = obj_new('aodataset', from='20110613_095151', to='20110613_095151') ;M92 J corto
imas=set->value('pisces.longexposure(/full)') & nimas = n_elements(imas[0,0,*])
off = fltarr(2,nimas) & for i=0,nimas-1 do off[*,i]=measure_images_shift(imas[*,*,0], imas[*,*,i])
sum=fltarr(1024,1024) & for i =0,nimas-1 do begin sum+=shift(imas[*,*,i], -off[0,i], -off[1,i]) & endfor & sum/=nimas
sumjcorto=sum

kc = alog10(sumKcorto>.5)
jc = alog10(sumjcorto-1>.5)

off = measure_images_shift( kc,jc )
jcs = shift(jc, -off[0], -off[1])

kc *= 255/max(kc)
jcs *= 255/max(jcs)

imac = fltarr(3,1024,1024) & imac[0,*,*]=kc & imac[2,*,*]=jcs & imac[1,*,*]=0.5*(jcs+kc) 

tv, imac>0, true=1

off = measure_images_shift( reform(ima[0,*,*]),reform(imac[0,*,*]) )
imalc = 0.5*ima + 0.5*shift(imac, 0, -off[0], -off[1])

tv, imalc>0, true=1


;;;;;;;;;;;;;;;; open loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

set = obj_new('aodataset', from='20110613_100834', to='20110613_100834') ;M92 J corto Open Loop
imas=set->value('pisces.longexposure(/full)') & nimas = n_elements(imas[0,0,*])
off = fltarr(2,nimas) & for i=0,nimas-1 do off[*,i]=measure_images_shift(imas[*,*,0], imas[*,*,i])
sum=fltarr(1024,1024) & for i =0,nimas-1 do begin sum+=shift(imas[*,*,i], -off[0,i], -off[1,i]) & endfor & sum/=nimas
sumJcortoOL = sum

jcol = alog10(sumJcortoOL>1)

set = obj_new('aodataset', from='20110613_100929', to='20110613_100929') ;M92 J lungo Open Loop
imas=set->value('pisces.longexposure(/full)') & nimas = n_elements(imas[0,0,*])
off = fltarr(2,nimas) & for i=0,nimas-1 do off[*,i]=measure_images_shift(imas[*,*,0], imas[*,*,i])
sum=fltarr(1024,1024) & for i =0,nimas-1 do begin sum+=shift(imas[*,*,i], -off[0,i], -off[1,i]) & endfor & sum/=nimas
sumJlungoOL = sum

jlol = alog10(sumJlungoOL+100>50)

off = measure_images_shift( jcol,jlol )
jlols = shift(jlol, -off[0], -off[1])

jcol *= 255/max(jcol)
jlols *= 255/max(jlols)

imaol = fltarr(3,1024,1024) & imaol[0,*,*]=jcol & imaol[2,*,*]=jlols & imaol[1,*,*]=0.5*(jcol+jlols) 

tv, imaol>0, true=1





