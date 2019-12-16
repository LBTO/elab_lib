function sr_from_slopes, data, lambda_, fitting=fitting

  ;SR computation from the residual slopes (using Marechal's approximation).
  ;
  ;data: aoelab object, aodataset object or vector of TNs
  ;lambda: Wvelength in nm. Default = 1650 nm.
  ;fitting: (keyword) if set use DIMM SEEING to add fitting error
  
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
    clvar  = total((cur_data->residual_modes())->time_variance() * norm_fact_wfs^2.)

    if keyword_set(fitting) then begin
        if obj_valid(cur_data->tel()) and obj_valid(cur_data->modal_rec()) then begin
            if obj_valid((cur_data->tel())->dimm_seeing()) then begin
                rad2asec = 3600.d*180.d/!dpi
                asec2rad = 1.d/rad2asec
                seeing_rad = (cur_data->tel())->dimm_seeing()*asec2rad
                r0500 = 0.98d*0.5d-6/seeing_rad ; Fried's r0 @ 500 nm
                r0LAM = r0500*(lambda/500.d)^(6.d/5.d)
                fitting_error = 0.2778d*(cur_data->modal_rec())->nmodes()^(-0.9d) * (8.222d / r0LAM)^(5.d/3.d) 
            endif
        endif
        if n_elements(fitting_error) eq 0 then message, 'fitting error can not be computed', /info
    endif 
    
    if n_elements(fitting_error) eq 0 then fitting_error = 0.

    tab_sr[i] = exp(-(clvar*nm2torad2+fitting_error))
  endfor
  
  return, tab_sr

end
