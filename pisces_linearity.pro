pro pisces_linearity
    setlin = obj_new('aodataset', from='20110605_010541', to='20110605_010929')
    imas =  setlin->value('pisces.rawImage')
    npoints=n_elements(imas[0,0,*])
    med = fltarr(npoints)
    for i =0,npoints-1 do med[i]=median(imas[256:767,256:767,i])
    t = setlin->value('pisces.exptime')
    res = linfit(t[0:3],med[0:3]) ; fit median value in the region that we suppose linear
    y = t*res[1]+res[0]
    
    lin=((y-med)/y)*100
    
    

    plot, y, med, psym=-4, thick=2, xtit='Counts', ytit='Counts', charsi=2, subtitle='Pisces linearity 20110605_010500-011100', $
         xst=8, yst=8, xmargin=[12,12], ymargin=[6,6], background='ffffff'x, color='00000'x
    axis, xaxis=1, xrange=((!x.crange)-res[0])/res[1], xtitle='exposure time [s]',xst=1, charsi=2, col=0
    axis, yaxis=1, yrange=minmax(lin), ytitle='Linearity [%]', yst=1, charsi=2, col=0
    oplot, y,y, col=255L, thick=2
    oplot, y, (lin - min(lin))/ (max(lin)-min(lin)) * (max(!y.crange)-min(!Y.crange)),  thick=2, linest=2, psym=-4, col=0

    write_jpeg, 'pisces_linearity.jpg',tvrd(true=3), true=3, quality=95

end
