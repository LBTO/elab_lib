function forces_from_dist, dist_file, adsec_save_file, m2c_file

    atmoDist = readfits(dist_file)
    m2c = readfits(m2c_file)
    restore, adsec_save_file

    idxGood = where(total(abs(m2c),2) gt 0,countGood)
    c2m_temp = pseudo_invert(m2c[idxGood,*])
    
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

    forceDist = ff_matrix##atmoDist

    atmoModes = c2m##atmoDist
    actFromModes = m2c##atmoModes
    forceKL = ff_matrix##actFromModes

    output = {forceDist:forceDist, forceDistKL:forceDistKL}

end

;dist_file = '/aodata/lbtdata/adsecsx/adsec_calib/CMD/disturb/dist_flao1_ts4_atm_s2.4_L040.0_v15.0_ovfreq1700.00_sd3892.fits'
;adsec_save_file = '/aodata/lbtdata/adsecsx/adsec_data/20180128/Data_20180128_043815/adsec.sav'
;m2c_file = '/aodata/lbtdata/adsecsx/adsec_calib/M2C/KL_v20/m2c.fits'
