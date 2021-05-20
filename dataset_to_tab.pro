;+
;
; This code converts an aodataset to a simplified structure, that is directly saved in a .sav file.
;
; INPUTS:
;   set                     aodataset object or array of TNs.
;
; OUTPUT:
;   tab                     Simplified structure containing the main information on the TNs in input.
;
; KEYWORDS:
;   from                    Starting TN.
;   to                      Ending TN.
;   rec                     Recompute data from TNs.
;   filename                Name of the file in which the structure is saved (without the .sav extension).
;   left                    Flag: Activate if the data is on SX (even if the LBT keyword is set!).
;   right                   Flag: Activate if the data is on DX (even if the LBT keyword is set!).
;   overwrite               Flag: Overwrite the file if it already exists.
;   LBT                     Flag: Activate if running the code on the LBT servers.
;
; HISTORY:
;   Written by:     C. Plantet (cedric.plantet@inaf.it) Mar, 2019

pro dataset_to_tab, set, from=from, to=to, rec = rec, filename = filename, left = left, right = right, overwrite = overwrite, $
lbt = lbt
  
  if (keyword_set(left) and keyword_set(right)) or $
    (not keyword_set(left) and not keyword_set(right)) then begin
    print, 'Choose a damn side!'
    return
  endif
  if keyword_set(left) then begin
    if not keyword_set(lbt) then begin
      ao_init,/left
      dir = '/home/elab/LBT_data_left/'
    endif
    side = 'left'
  endif
  if keyword_set(right) then begin
    if not keyword_set(lbt) then begin
      ao_init,/right
      dir = '/home/elab/LBT_data_right/'
    endif
    side = 'right'
  endif
  
  if keyword_set(lbt) then begin
    ao_init
    dir = '/local/aomeas/'
  endif  

  if file_test(filename+'.sav') and not keyword_set(overwrite) then begin
    print, 'File already exists'
    return
  endif
  
  if keyword_set(rec) then rec2 = rec else rec2 = 0
  if keyword_set(from) and keyword_set(to) then begin
    set = obj_new('aodataset',from=from,to=to,rec=rec)
    rec2 = 0
    set = set->where('wfs_status.isSoul()','eq',1)
    tns = set->tracknums()
  endif else tns = set

  nfiles = n_elements(tns)

  if tns[0] ne '' then begin

    tab_sr = fltarr(nfiles)
    tab_filter = strarr(nfiles)
    tab_seeing = fltarr(nfiles)
    tab_seeingol = fltarr(nfiles)
    tab_mag = fltarr(nfiles)
    tab_gains = fltarr(nfiles,3)
    tab_recmat = strarr(nfiles)
    tab_freq = fltarr(nfiles)
    tab_nmodes = fltarr(nfiles)
    tab_bin = fltarr(nfiles)
    tab_nsubap = fltarr(nfiles)
    tab_trans = fltarr(nfiles)
    tab_counts = fltarr(nfiles)
    tab_mode = strarr(nfiles)
    tab_emgain = fltarr(nfiles)
    tab_hasPSF = fltarr(nfiles)
    tab_dimm = fltarr(nfiles)
    tab_dimm_elevation = fltarr(nfiles)
    tab_darktime = fltarr(nfiles)
    tab_exptime = fltarr(nfiles)
    tab_rot = fltarr(nfiles)
    tab_ra = fltarr(nfiles)
    tab_dec = fltarr(nfiles)
    tab_valid = fltarr(nfiles)

    for i = 0, nfiles-1 do begin

      print,i+1,'/',strtrim(nfiles,2)+'  '+tns[i]

      cur_sr = -1
      cur_gains = fltarr(3)-1
      cur_filter = '******'
      cur_mag = 1e99
      cur_freq = -1
      cur_nmodes = -1
      cur_seeing = -1
      cur_seeingol = 0
      cur_nsubap = -1
      cur_trans = -1
      cur_nph = -1
      cur_bin = -1
      cur_recmat = '******'
      cur_mode = '******'
      cur_emgain = -1
      cur_hasPSF = 0
      cur_dimm = 0
      cur_dimm_elevation = 0
      cur_darktime = !values.f_infinity
      cur_exptime = -1
      cur_rot = 0
      cur_ra = 0
      cur_dec = 0
      cur_valid = 0

      if file_test(dir+'adsec_data/'+strmid(tns[i],0,8)+'/Data_'+tns[i]+'/adsec.sav') or $
        file_test(dir+'adsec_data/'+strmid(tns[i],0,8)+'/Data_'+tns[i]+'/wfs.fits') then begin
        cur_ee = getaoelab(tns[i],rec=rec2)
        if obj_valid(cur_ee) then begin

          sci_camera = cur_ee->luci()

          if NOT obj_valid(sci_camera) then sci_camera = cur_ee->lmircam() 
          
          if obj_valid(sci_camera) then begin
            cur_haspsf = 1
            cur_exptime = sci_camera->nframes() * sci_camera->exptime() 

            darkname = sci_camera->dark_fname()
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

            if keyword_set(sci_camera->filter_name()) then cur_filter = sci_camera->filter_name()
            psf_dl_fname = filepath( root=ao_elabdir(), 'psf_dl_'+strtrim(round(sci_camera->lambda()*1e9),2)+'_scale'+ $
              strtrim(round(sci_camera->pixelscale()*1e3),2)+'_oc'+strtrim(round(sci_camera->oc()*1e3),2)+'.sav')
            if file_test(psf_dl_fname) then sr_tmp = sci_camera->sr_se() else sr_tmp = 0
            if not (sr_tmp gt 1 or sr_tmp lt 0) then cur_sr = sr_tmp
          endif
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
            cur_seeing = (cur_ee->olmodes())->seeing()
            cur_seeingol = 1
          endif
          if obj_valid(cur_ee->adsec_status()) then begin
            if (cur_ee->adsec_status())->disturb_status() ne 0 then begin
              if obj_valid(cur_ee->disturb()) then if (cur_ee->disturb())->type() eq 'atm' then begin
                cur_seeing = (cur_ee->disturb())->seeing()
                cur_seeingol = 0
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
            cur_seeingol = 0
            cur_dimm = 1
            endif
            if finite((cur_ee->tel())->dimm_seeing_elevation()) then begin
            cur_seeing = (cur_ee->tel())->dimm_seeing_elevation()
            cur_seeingol = 0
            cur_dimm_elevation = 1
            endif
          endif
          if obj_valid(cur_ee->modal_rec()) then begin
            cur_recmat = strmid((cur_ee->modal_rec())->fname(),10,6,/reverse)
            cur_nmodes = (cur_ee->modal_rec())->nmodes()
          endif
          cur_valid = 1
        endif
      endif

      tab_sr[i] = cur_sr
      tab_filter[i] = cur_filter
      tab_seeing[i] = cur_seeing
      tab_seeingol[i] = cur_seeingol
      tab_mag[i] = cur_mag
      tab_gains[i,*] = cur_gains
      tab_recmat[i] = cur_recmat
      tab_freq[i] = cur_freq
      tab_nmodes[i] = cur_nmodes
      tab_bin[i] = cur_bin
      tab_nsubap[i] = cur_nsubap
      tab_trans[i] = cur_trans
      tab_counts[i] = cur_nph
      tab_mode[i] = cur_mode
      tab_emgain[i] = cur_emgain
      tab_haspsf[i] = cur_haspsf
      tab_dimm[i] = cur_dimm
      tab_dimm_elevation[i] = cur_dimm_elevation
      tab_darktime[i] = cur_darktime
      tab_exptime[i] = cur_exptime
      tab_rot[i] = cur_rot
      tab_ra[i] = cur_ra
      tab_dec[i] = cur_dec
      tab_valid[i] = cur_valid

    endfor

    tab = create_struct('TN', tns, 'SR', tab_sr, 'filter', tab_filter, $
      'seeing', tab_seeing, 'seeing_ol', tab_seeingol, 'mag', tab_mag, 'gains', tab_gains, 'recmat', tab_recmat, $
      'freq', tab_freq, 'nmodes', tab_nmodes, 'bin', tab_bin, 'nsub', tab_nsubap, 'trans', tab_trans, 'counts', tab_counts, $
      'mode', tab_mode, 'emGain', tab_emgain, 'hasPSF', tab_haspsf, 'DIMM', tab_dimm, 'DIMMELEVATION', tab_dimm_elevation, 'dark_time', tab_darktime, $
      'exp_time', tab_exptime, 'rot', tab_rot, 'ra', tab_ra, 'dec', tab_dec, 'side', strarr(nfiles)+side, $
      'valid', tab_valid)

    save,tab,filename = filename+'.sav'

    print,string(total(tab_valid)/nfiles*100,format='(F0.1)')+'% of the files have been correctly read'

  endif else print, 'No TN in this time period.'

end
