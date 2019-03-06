; +
; inputs:
;   dist_file           file name of the disturbance file
;   adsec_save_file     [optional - option 1] file name of adsec structure
;   m2c_file            [optional - option 1] file name of m2c matrix
;   adsec_save_file     [optional - option 2] AO tracking number
; keywords:
;   display             display forces histogram
; -

function forces_from_dist, dist_file, adsec_save_file=adsec_save_file, m2c_file=m2c_file, trck=trck, display=display

    atmoDist = readfits(dist_file)

    if n_elements(trck) ne 0 then begin

        ee = getaoelab(trck)
        
        ff_matrix = ((ee->adsec_status())->struct_adsec()).ff_matrix
        act_w_cl = ((ee->adsec_status())->struct_adsec()).act_w_cl

        m2c = (ee->control())->m2c()

        c2m_temp = (ee->control())->c2m()
        c2m = m2c*0.
        idxGood = where(total(abs(m2c),2) gt 0,countGood)
        c2m[*,idxGood] = c2m_temp 

    endif else begin

        m2c = readfits(m2c_file)
        restore, adsec_save_file

        idxGood = where(total(abs(m2c),2) gt 0,countGood)
        c2m_temp = pseudo_invert(helm2c[idxGood,*])
        
        c2m = m2c*0.
        c2m[*,idxGood] = c2m_temp

        if n_elements(adsec_save) gt 0 then begin
            ff_matrix = adsec_save.ff_matrix
            act_w_cl = adsec_save.act_w_cl
        endif
        if n_elements(myadsec) gt 0 then begin
            ff_matrix = myadsec.ff_matrix
            act_w_cl = myadsec.act_w_cl
        endif

    endelse
    
    forceDist = ff_matrix##atmoDist

    atmoModes = c2m##atmoDist
    actFromModes = m2c##atmoModes
    forceDistKL = ff_matrix##actFromModes

    output = {forceDist:forceDist, forceDistKL:forceDistKL}
    
    if keyword_set(display) then begin
        res = histogram(abs(forceDist),binsize=0.01,locations=xx)
        resKL = histogram(abs(forceDistKL),binsize=0.01,locations=xxKL)
        plot_io, xx, res, thick=2, psym=10, xra=minmax([xx,xxKL]), yra=minmax([res,resKL]), /xst, /yst, $
              charsize=2, tit='histogram', xtit='force (N)', ytit='occurence'
        oplot, xxKL, resKL, col=255l, thick=2, psym=10
    
        if ROUTINE_FILEPATH("AL_LEGEND") ne '' then $
            al_legend, ['disturbance','dist. proj. on KL'], color=[-1l,255l], linest=[0,0], thick=2, /left, /bottom, charsize=2 $
                else legend, ['disturbance','dist. proj. on KL'], color=[-1l,255l], linest=[0,0], thick=2, /left, /bottom, charsize=2

    endif

    return, output

end

;dist_file = '/aodata/lbtdata/adsecsx/adsec_calib/CMD/disturb/dist_flao1_ts4_atm_s2.4_L040.0_v15.0_ovfreq1700.00_sd3892.fits'
;adsec_save_file = '/aodata/lbtdata/adsecsx/adsec_data/20180128/Data_20180128_043815/adsec.sav'
;m2c_file = '/aodata/lbtdata/adsecsx/adsec_calib/M2C/KL_v20/m2c.fits'
