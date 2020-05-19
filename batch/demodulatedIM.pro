function demodulatedIM, set, plot=plot

; use first TN to determine common parameters
ee=getaoelab(set->get(pos=0))
    
; in SOUL the slopes become 2848
;n_slopes = 1600
n_slopes = (ee->slopes())->Nslopes()

idxslo = where(total((ee->slopes())->slopes(),1) ne 0., nslopes)
;nslopes = (ee->slopes())->nspectra()
nmodes = max(set->value('disturb.sin_mode'))+1
im = fltarr(nmodes, n_slopes)

for i = 0, set->count()-1 do begin

    ee = getaoelab(set->get(pos=i))
    
    nsinmodes = (ee->disturb())->nsinmodes()
    for j=0, nsinmodes-1 do begin
        modeno = (ee->disturb())->sin_mode(j)
        ; demodule (need slopes and disturb) 
        f0 = (ee->disturb())->sin_freq(j)
        eps = 0.001
        pp = (ee->slopes())->phase( (indgen(n_slopes))[idxslo], from=f0-eps, to=f0+eps, /average) * !CONST.RtoD
        ;plot, pp, psym=4

        ; determine phase shift between disturb and slopes
        ; we know that phase shift has to be positive and small
        idx = where(pp lt 0, cnt)
        pptmp = pp
        if cnt gt 0 then pptmp[idx] += 180  ;pptmp > 0
        ;sfasa = median(pptmp) ; sfasa > 0
        binsize = 0.5
        histo=histogram(pptmp, binsize=binsize)
        dum=max(histo, idx)
        sfasa=idx[0]*binsize
        print, 'mode '+strtrim(modeno,2)+' : phase shift '+strtrim(sfasa*!CONST.DtoR,2)
        ; use phase shift to determine the sign of slopes  
        ; maledetti angoli!
        if sfasa gt 90 then  $
            idx =where( ( pp gt sfasa-90) or ( pp lt sfasa-270 ), cnt) $
        else  $
            idx =where( ( pp gt sfasa-90) or ( pp lt sfasa+90 ), cnt) 

        sign = intarr(nslopes)+1
        if cnt gt 0 then sign[idx]=-1

        ; retrieve slopes from power and sign
        slopevector = sqrt((ee->slopes())->power((indgen(n_slopes))[idxslo], from=f0-0.001, to=f0+0.001)) * sign

        ; normalize for disturb_amplitude 
        slopevector /= (ee->disturb())->mode_amp(j)
        im[modeno,0:nslopes-1] = slopevector
        
        if keyword_set(plot) then begin
            
            plot, pp, psym=4
            oplot, fltarr(n_elements(pp))+sfasa-90, col='00ff00'x
            oplot, fltarr(n_elements(pp))+sfasa+90, col='00ff00'x
            oplot, fltarr(n_elements(pp))+sfasa-270, col='00ff00'x
            wait, 0.5
            ;indpup = ((ee->wfs_status())->pupils())->indpup()
            ;frame=reform_slopes2d(im[i,*], indpup)
            ;image_show, /as, /sh, frame
        endif

    endfor
endfor

return, im

end

function reform_slopes2d, imvect, indpup
	sensorSideX = ((ee->wfs_status())->camera())->sensorSideX()
	sensorSideY = ((ee->wfs_status())->camera())->sensorSideY()
    frame=fltarr (sensorSideX,sensorSideY)
    nsubap = n_elements(indpup[*,0])
    frame[indpup[*,0]] = imvect[ 0:nsubap*2-1:2]
    frame[indpup[*,1]] = imvect[ 1:nsubap*2-1:2]
    return, frame
end

pro test_demodulatedIM
;set = obj_new('aodataset', from='20111011_185137', to='20111011_185619')
set = obj_new('aodataset', from='20111015_121032', to='20111015_121742')
im = demodulatedIM(set)
indpup = (((getaoelab(set->get(pos=0)))->wfs_status())->pupils())->indpup()

for i=0, set->count()-1 do begin
    frame=reform_slopes2d(im[i,*], indpup)
    image_show, /as, /sh, frame
    wait,1
endfor
end
