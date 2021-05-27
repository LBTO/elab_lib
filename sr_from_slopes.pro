function sr_from_slopes, data, lambda_, fitting=fitting, seeing = seeing, noise = noise

  ;SR computation from the residual slopes (using Marechal's approximation).
  ;
  ;data: aoelab object, aodataset object or vector of TNs
  ;lambda: Wvelength in nm. Default = 1650 nm.
  ;fitting: (keyword) if set use DIMM SEEING to add fitting error
  ;seeing: User-defined value for the seeing (in arcsec).
  ;        Overrides the DIMM seeing for the computation of the fitting error.
  ;noise: (flag) Estimation and removal of the mode-per-mode noise in the CL variance.

  if size(data,/type) eq 11 then begin
    if obj_class(data) eq 'AOELAB' then tns = data->tracknum()
    if obj_class(data) eq 'AODATASET' then tns = data->tracknums()
  endif else tns = data

  if tns[0] eq '' then return, -1

  if not keyword_set(lambda_) then lambda = 1650. else lambda = lambda_

  nm2torad2 = (2.*!pi/lambda)^2.

  ndata = n_elements(tns)
  tab_sr = fltarr(ndata)

  for i = 0, ndata-1 do begin
    cur_data = getaoelab(tns[i])
    if not obj_valid(cur_data) then begin
      tab_sr[i] = -1
      continue
    endif
    if not obj_valid(cur_data->residual_modes()) then begin
      tab_sr[i] = -1
      continue
    endif
    if not obj_valid(cur_data->wfs_status()) then begin
      tab_sr[i] = -1
      continue
    endif
    if (cur_data->wfs_status())->optg() eq 1 then begin ;if Gopt is not active then the SR is meaningless
      tab_sr[i] = -1
      continue
    endif
    norm_fact_wfs = (cur_data->residual_modes())->norm_factor()/2 ;division by 2 to take into account the active Gopt
    clvar0  = (cur_data->residual_modes())->time_variance() * norm_fact_wfs^2.
    nmodes = (cur_data->residual_modes())->nmodes()
    
    if keyword_set(noise) then begin
      clvar = clvar0*0
      for j = 0, nmodes-1 do begin
        ;Compute temporal PSD of the residuals
        psd = (cur_data->residual_modes())->psd(j) * norm_fact_wfs^2.
        ;Noise variance is a constant offset in the temporal PSD (temporally uncorrelated), mostly visible at high frequencies
        noise_level = median(psd[round(n_elements(psd)/2.):*])/(cur_data->frames_counter())->deltat()/2 ;/2 from psd normalization
        clvar[j] = (clvar0[j]-noise_level) > 0
      endfor
    endif else clvar = clvar0
    
    ;Modification for later if we want to compute the fitting from modalpositions in daytime
;    if cur_data->operation_mode() ne 'ONSKY' and obj_valid(cur_data->modalpositions()) then begin
;      norm_fact_dm = (cur_data->modalpositions())->norm_factor()
;      clvar1  = (cur_data->modalpositions())->time_variance() * norm_fact_dm^2.
;      
;      tab_sr[i] = exp(-(total([clvar[0:nmodes-1],clvar1[nmodes:*]])*nm2torad2))
;    endif else begin

      if keyword_set(fitting) then begin
        rad2asec = 3600.d*180.d/!dpi
        asec2rad = 1.d/rad2asec
        if keyword_set(seeing) then begin
          if n_elements(seeing) eq 1 then seeing_rad = seeing*asec2rad $
          else seeing_rad = seeing[i]*asec2rad
        endif else begin
          if obj_valid(cur_data->disturb()) then begin
            if (cur_data->disturb())->type() eq 'atm' or (cur_data->disturb())->type() eq 'atm+sinus' then $
              seeing_rad = (cur_data->disturb())->seeing()*asec2rad
              if not keyword_set((cur_data->tel())->isTracking()) or $
              cur_data->operation_mode() eq 'ARGOScal' then seeing_rad /= 2
          endif
          if obj_valid(cur_data->tel()) and obj_valid(cur_data->modal_rec()) and cur_data->operation_mode() eq 'ONSKY' then begin
            if finite((cur_data->tel())->dimm_seeing()) then seeing_rad = (cur_data->tel())->dimm_seeing()*asec2rad
            if finite((cur_data->tel())->dimm_seeing_elevation()) then seeing_rad = (cur_data->tel())->dimm_seeing_elevation()*asec2rad
          endif
        endelse
        if n_elements(seeing_rad) eq 0 then message, 'fitting error can not be computed', /info else begin
          r0500 = 0.976d*0.5d-6/seeing_rad ; Fried's r0 @ 500 nm
          r0LAM = r0500*(lambda/500.d)^(6.d/5.d)
          fitting_error = 0.2778d*(cur_data->modal_rec())->nmodes()^(-0.9d) * (8.222d / r0LAM)^(5.d/3.d)
        endelse
      endif

      if n_elements(fitting_error) eq 0 then fitting_error = 0.

      tab_sr[i] = exp(-(total(clvar)*nm2torad2+fitting_error))
  ;  endelse

  endfor

  return, tab_sr

end
