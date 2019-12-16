function sr_from_slopes, data, lambda_

  ;SR computation from the residual slopes (using Marechal's approximation).
  ;
  ;data: aoelab object, aodataset object or vector of TNs
  ;lambda: Wvelength in nm. Default = 1650 nm.
  
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
    tab_sr[i] = exp(-clvar*nm2torad2)
  endfor
  
  return, tab_sr

end