
pro PlotModalVariance, el1, el2, el3, el4, el5, el6
    nobj = n_params()
    xminmax = [1e300,1e-300]
    yminmax = [1e300,1e-300]
    switch nobj of
        6: begin
            data6y = (el6->residual_modes() )->variance()
            data6x = indgen( (el6->modal_rec())->nmodes() ) + 1
            xminmax[0] = min(data6x) < xminmax[0] & xminmax[1] = max(data6x) > xminmax[1]
            yminmax[0] = min(data6y) < yminmax[0] & yminmax[1] = max(data6y) > yminmax[1]
           end
        5: begin
            data5y = (el5->residual_modes() )->variance()
            data5x = indgen( (el5->modal_rec())->nmodes() ) + 1
            xminmax[0] = min(data5x) < xminmax[0] & xminmax[1] = max(data5x) > xminmax[1]
            yminmax[0] = min(data5y) < yminmax[0] & yminmax[1] = max(data5y) > yminmax[1]
           end
        4: begin
            data4y = (el4->residual_modes() )->variance()
            data4x = indgen( (el4->modal_rec())->nmodes() ) + 1
            xminmax[0] = min(data4x) < xminmax[0] & xminmax[1] = max(data4x) > xminmax[1]
            yminmax[0] = min(data4y) < yminmax[0] & yminmax[1] = max(data4y) > yminmax[1]
           end
        3: begin
            data3y = (el3->residual_modes() )->variance()
            data3x = indgen( (el3->modal_rec())->nmodes() ) + 1
            xminmax[0] = min(data3x) < xminmax[0] & xminmax[1] = max(data3x) > xminmax[1]
            yminmax[0] = min(data3y) < yminmax[0] & yminmax[1] = max(data3y) > yminmax[1]
           end
        2: begin
            data2y = (el2->residual_modes() )->variance()
            data2x = indgen( (el2->modal_rec())->nmodes() ) + 1
            xminmax[0] = min(data2x) < xminmax[0] & xminmax[1] = max(data2x) > xminmax[1]
            yminmax[0] = min(data2y) < yminmax[0] & yminmax[1] = max(data2y) > yminmax[1]
           end
        1: begin
            data1y = (el1->residual_modes() )->variance()
            data1x = indgen( (el1->modal_rec())->nmodes() ) + 1
            xminmax[0] = min(data1x) < xminmax[0] & xminmax[1] = max(data1x) > xminmax[1]
            yminmax[0] = min(data1y) < yminmax[0] & yminmax[1] = max(data1y) > yminmax[1]
           end
    endswitch

    cols = comp_colors(nobj)
    plot_oo, data1y, data1x, /nodata, xra=xminmax ,yra=yminmax, title='Modal variance', $
        xtitle='mode index', ytitle='modal variance [AU]'
    switch nobj of
        6: oplot, data6x, data6y, col=cols[5]
        5: oplot, data5x, data5y, col=cols[4]
        4: oplot, data4x, data4y, col=cols[3]
        3: oplot, data3x, data3y, col=cols[2]
        2: oplot, data2x, data2y, col=cols[1]
        1: oplot, data1x, data1y, col=cols[0]
    endswitch

end


pro setupplot, p_old=p_old
    p_old = !p
    !p.color=0
    !p.background='ffffffff'xul
    !P.THICK = 2.0
    !X.THICK = 2.0
    !Y.THICK = 2.0
    !P.CHARSIZE = 2.0
    !P.CHARTHICK = 1.2
    !P.FONT = 1 
end

