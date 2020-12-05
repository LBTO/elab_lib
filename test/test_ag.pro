
pro testag

ao_init, /left
ee = getaoelab('20201001_021730')
ag = ee.ag()
nsteps = ag.nsteps()

tt = fltarr(nsteps)
ho1 = fltarr(nsteps)
ho2 = fltarr(nsteps)
errmsg = strarr(nsteps)

for i=0,nsteps-1 do begin
   step = ag.step(i+1)
   step.recalc, GAINTT=GAINTT, GAINHO1=GAINHO1, GAINHO2=GAINHO2, ERRMSG=ERRMSG_

   tt[i] = GAINTT
   ho1[i] = GAINHO1
   ho2[i] = GAINHO2
   errmsg[i] = ERRMSG_
endfor


for i=0, nsteps-1 do begin
   print, i+1, tt[i], ho1[i], ho2[i], ' ', errmsg[i]
endfor

end



