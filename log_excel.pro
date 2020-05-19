pro log_excel, data, rec = rec, filename = filename, lbt = lbt, left = left, right = right, seeing = seeing

  if keyword_set(lbt) then dir = '/local/aomeas/' else begin
    if keyword_set(left) then dir = '/home/elab/LBT_data_left/'
    if keyword_set(right) then dir = '/home/elab/LBT_data_right/'
  endelse

  if size(data,/type) eq 11 then begin
    if obj_class(data) eq 'AOELAB' then tns = data->tracknum()
    if obj_class(data) eq 'AODATASET' then tns = data->tracknums()
  endif else tns = data

  if tns[0] eq '' then begin
    print, 'No valid TN'
    return
  endif
  nfiles = n_elements(tns)

  openw,unit,filename+'.txt',/get_lun
  tab = string(9B)

  for i = 0, nfiles-1 do begin
    if i eq 0 then printf, unit, 'TN'+tab+'Mag'+tab+'Freq (Hz)'+tab+'Bin'+tab+'Nmodes'+tab+'Seeing DIMM/disturb (arcsec)'+tab+ $
      'Seeing OL (arcsec)'+tab+'Nsubap'+tab+'Gain TT'+tab+'Gain MO'+tab+'Gain HO'+tab+'Recmat'+tab+'EMgain'+tab+ $
      'Nph/subap'+tab+'has PSF?'+tab+'SR'+tab+'SR from slopes'+tab+'FWHM (mas)'+tab+'Trans'+tab+'Filter'+tab+'Dark time (s)'+tab+'Exp. time (s)'+tab+ $
      'Rotator angle (degrees)'+tab+'RA (hours)'+tab+'DEC (degrees)'+tab+'Mode'

    cur_sr = -1
    cur_sr_fromslopes = -1
    cur_fwhm = -1
    cur_gains = fltarr(3)-1
    cur_filter = '******'
    cur_mag = 1e99
    cur_freq = -1
    cur_nmodes = -1
    cur_seeing = -1
    cur_seeingol = -1
    cur_nsubap = -1
    cur_trans = -1
    cur_nph = -1
    cur_bin = -1
    cur_recmat = '******'
    cur_mode = '******'
    cur_emgain = -1
    cur_hasPSF = 0
    cur_darktime = !values.f_infinity
    cur_exptime = -1
    cur_rot = 0
    cur_ra = 0
    cur_dec = 0

    if file_test(dir+'adsec_data/'+strmid(tns[i],0,8)+'/Data_'+tns[i]+'/adsec.sav') or $
      file_test(dir+'adsec_data/'+strmid(tns[i],0,8)+'/Data_'+tns[i]+'/wfs.fits') then begin
      cur_ee = getaoelab(tns[i],rec=rec)
      if obj_valid(cur_ee) then begin
        if obj_valid(cur_ee->luci()) then begin
          cur_haspsf = 1
          cur_exptime = (cur_ee->luci())->nframes()*(cur_ee->luci())->exptime()

          darkname = (cur_ee->luci())->dark_fname()
          if darkname ne '' then begin
            tn_dark = strmid(darkname,29,15,/reverse)
            ;time TN
            year_im = double(strmid(tns[i],0,4))
            month_im = double(strmid(tns[i],4,2))
            day_im = double(strmid(tns[i],6,2))
            hour_im = double(strmid(tns[i],9,2))
            minutes_im = double(strmid(tns[i],11,2))
            seconds_im = double(strmid(tns[i],13,2))
            time_im = julday(month_im, day_im, year_im)*24.*3600.+(hour_im-12)*3600.+minutes_im*60.+seconds_im
            ;time dark
            year_dark = double(strmid(tn_dark,0,4))
            month_dark = double(strmid(tn_dark,4,2))
            day_dark = double(strmid(tn_dark,6,2))
            hour_dark = double(strmid(tn_dark,9,2))
            minutes_dark = double(strmid(tn_dark,11,2))
            seconds_dark = double(strmid(tn_dark,13,2))
            time_dark = julday(month_dark, day_dark, year_dark)*24.*3600.+(hour_dark-12)*3600.+minutes_dark*60.+seconds_dark

            cur_darktime = time_im-time_dark
          endif

          cur_fwhm = ((cur_ee->luci())->star_fwhm())[0]*1e3

          if keyword_set((cur_ee->luci())->filter_name()) then cur_filter = (cur_ee->luci())->filter_name()
          psf_dl_fname = filepath( root=ao_elabdir(), 'psf_dl_'+strtrim(round((cur_ee->luci())->lambda()*1e9),2)+'_scale'+ $
            strtrim(round((cur_ee->luci())->pixelscale()*1e3),2)+'_oc'+strtrim(round((cur_ee->luci())->oc()*1e3),2)+'.sav')
          if file_test(psf_dl_fname) then sr_tmp = (cur_ee->luci())->sr_se() else sr_tmp = 0
          if not (sr_tmp gt 1 or sr_tmp lt 0) then cur_sr = sr_tmp
        endif
        if keyword_set(seeing) then cur_sr_fromslopes = sr_from_slopes(cur_ee, obj_valid(cur_ee->luci()) ? $
          (cur_ee->luci())->lambda()*1e9 : 1650.,/fitting, seeing = seeing) else begin
          if obj_valid(cur_ee->tel()) then if finite((cur_ee->tel())->dimm_seeing()) then $
            cur_sr_fromslopes = sr_from_slopes(cur_ee, obj_valid(cur_ee->luci()) ? $
            (cur_ee->luci())->lambda()*1e9 : 1650.,/fitting)
        endelse
        if obj_valid(cur_ee->wfs_status()) then begin
          if obj_valid((cur_ee->wfs_status())->camera()) then begin
            if obj_class((cur_ee->wfs_status())->camera()) ne 'AOCCD39' then $
              cur_emgain = ((cur_ee->wfs_status())->camera())->emGain()
            cur_freq = ((cur_ee->wfs_status())->camera())->framerate()
            cur_bin = ((cur_ee->wfs_status())->camera())->binning()
            if obj_valid((cur_ee->wfs_status())->filtw1()) then begin
              cur_trans = ((cur_ee->wfs_status())->filtw1())->transmissivity()
              if finite(cur_ee->mag()) then cur_mag = cur_ee->mag()
              if obj_valid((cur_ee->wfs_status())->pupils()) then begin
                cur_nsubap = ((cur_ee->wfs_status())->pupils())->nsub()
                if obj_valid(cur_ee->frames()) then cur_nph =  (cur_ee->frames())->nphsub_per_int_av()
              endif
            endif
          endif
        endif
        cur_mode = cur_ee->operation_mode()
        if obj_valid(cur_ee->olmodes()) then begin
          cur_seeingol = (cur_ee->olmodes())->seeing()
        endif
        if obj_valid(cur_ee->adsec_status()) then begin
          if (cur_ee->adsec_status())->disturb_status() ne 0 then begin
            if obj_valid(cur_ee->disturb()) then if (cur_ee->disturb())->type() eq 'atm' then begin
              cur_seeing = (cur_ee->disturb())->seeing()
              if cur_mode eq 'ONSKY' then cur_mode = 'ARGOScalib'
            endif
          endif
        endif
        if obj_valid(cur_ee->control()) then begin
          cur_gains = [(cur_ee->control())->ttgain(),(cur_ee->control())->mogain(),(cur_ee->control())->hogain()]
        endif
        if obj_valid(cur_ee->tel()) then begin
          cur_rot = (cur_ee->tel())->rot_angle()
          cur_ra = (cur_ee->tel())->ra()
          cur_dec = (cur_ee->tel())->dec()
          if (cur_ee->tel())->istracking() ne 1 then begin
            if cur_mode eq 'ONSKY' then cur_mode = 'ARGOScalib'
          endif
          if finite((cur_ee->tel())->dimm_seeing()) then begin
            cur_seeing = (cur_ee->tel())->dimm_seeing()
          endif
        endif
        if obj_valid(cur_ee->modal_rec()) then begin
          cur_recmat = strmid((cur_ee->modal_rec())->fname(),10,6,/reverse)
          cur_nmodes = (cur_ee->modal_rec())->nmodes()
        endif
      endif
    endif

    printf, unit, tns[i]+', '+tab+string(cur_mag,format='(F0.1)')+tab+string(cur_freq,format='(I0)')+tab+ $
      string(cur_bin,format='(I0)')+tab+string(cur_nmodes,format='(I0)')+tab+string(cur_seeing,format='(F0.2)')+tab+ $
      string(cur_seeingol,format='(F0.2)')+tab+string(cur_nsubap,format='(I0)')+tab+string(cur_gains[0],format='(F0.2)')+ $
      tab+string(cur_gains[1],format='(F0.2)')+tab+string(cur_gains[2],format='(F0.2)')+tab+cur_recmat+tab+ $
      string(cur_emgain,format='(I0)')+tab+ string(cur_nph,format='(F0.1)')+tab+string(cur_haspsf,format='(I0)')+ $
      tab+string(cur_sr,format='(F0.3)')+tab+string(cur_sr_fromslopes,format='(F0.3)')+tab+string(cur_fwhm,format='(I0)')+ $
      tab+string(cur_trans,format='(F0.3)')+tab+cur_filter+tab+ string(cur_darktime,format='(I0)')+ $
      tab+string(cur_exptime,format='(F0.2)')+tab+string(cur_rot,format='(F0.1)')+tab+string(cur_ra,format='(F0.15)')+ $
      tab+string(cur_dec,format='(F0.15)')+tab+cur_mode

  endfor

  close, unit

end