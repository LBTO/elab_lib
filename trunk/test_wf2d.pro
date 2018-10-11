
pro test_wf2d, tn
;ee = getaoelab('20101126_075157')

xx=(tn->modeshapes())->xx()
yy=(tn->modeshapes())->yy()
triangulate, xx, yy, tr, b

; residual wf map
rmwfmat=(tn->residual_modes())->wfmat()
reswfmap=fltarr(128,128,1000)
for i=0,999 do begin reswfmap[*,*,i]=trigrid(xx,yy,reform(rmwfmat[i,*]), tr, nx=128, ny=128)
rmwfmat=0

; actuator position wf map
poswfmat=(tn->modalpositions())->wfmat()
poswfmap=fltarr(128,128,1000)
for i=0,999 do begin poswfmap[*,*,i]=trigrid(xx,yy,reform(poswfmat[i,*]), tr, nx=128, ny=128)
poswfmat=0

; input wf map
olwfmat=(tn->olmodes())->wfmat()
olwfmap=fltarr(128,128,1000)
for i=0,999 do begin olwfmap[*,*,i]=trigrid(xx,yy,reform(olwfmat[i,*]), tr, nx=128, ny=128)
olwfmat=0

; display
window, xsi=768, ysi=256
for i=0,999 do tvscl, rebin( [reswfmap[*,*,i]*20, poswfmap[*,*,i]-poswfmap[*,*,0],olwfmap[*,*,i]],768,256,/sam)

