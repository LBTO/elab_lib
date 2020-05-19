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
    
    if keyword_set(noise) then begin
      clvar = clvar0*0
      nmodes = (cur_data->residual_modes())->nmodes()
      for j = 0, nmodes-1 do begin
        ;Compute temporal PSD of the residuals
        psd = (cur_data->residual_modes())->psd(j) * norm_fact_wfs^2.
        ;Noise variance is a constant offset in the temporal PSD (temporally uncorrelated), mostly visible at high frequencies
        noise_level = median(psd[round(n_elements(psd)/2.):*])/(cur_data->frames_counter())->deltat()/2 ;/2 from psd normalization
        clvar[j] = (clvar0[j]-noise_level) > 0
      endfor
    endif else clvar = clvar0

    if keyword_set(fitting) then begin
      rad2asec = 3600.d*180.d/!dpi
      asec2rad = 1.d/rad2asec
      if keyword_set(seeing) then seeing_rad = seeing*asec2rad $
      else begin
        if obj_valid(cur_data->tel()) and obj_valid(cur_data->modal_rec()) then begin
          if finite((cur_data->tel())->dimm_seeing()) then seeing_rad = (cur_data->tel())->dimm_seeing()*asec2rad
        endif
      endelse
      if n_elements(seeing_rad) eq 0 then message, 'fitting error can not be computed', /info else begin
        r0500 = 0.98d*0.5d-6/seeing_rad ; Fried's r0 @ 500 nm
        r0LAM = r0500*(lambda/500.d)^(6.d/5.d)
        fitting_error = 0.2778d*(cur_data->modal_rec())->nmodes()^(-0.9d) * (8.222d / r0LAM)^(5.d/3.d)
      endelse
    endif

    if n_elements(fitting_error) eq 0 then fitting_error = 0.

    tab_sr[i] = exp(-(total(clvar)*nm2torad2+fitting_error))

  endfor

  return, tab_sr

end

;main test

ao_init,/left

;TNs with ARGOScal no seeing
;tns = '20190708_'+['230237','230303','230324']

;TNs with ARGOScal 1" seeing
;tns = '20200110_'+['133502','133728','133931']
;tns = [tns,'20200112_'+['100011','100440','113623','113846','114058']]
;tns = [tns,'20190705_'+['214605','214659','214810']]
;tns = [tns,'20191107_'+['053243','053400','053524','053711','053830','053945', $
;  '054057','062410','062705','062834']]
;tns = [tns,'20191110_'+['123944','125256']]

;TNs on sky
set = obj_new('aodataset',from = '20190406_044801', to = '20190406_045345')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) then tns = set->tracknums()

set = obj_new('aodataset',from = '20190406_061225', to = '20190406_080502')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = set->tracknums()
;
set = obj_new('aodataset',from = '20190408_070434', to = '20190408_071044')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191108_035608', to = '20191108_035755')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191109_035011', to = '20191109_042158')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191109_061607', to = '20191109_062703')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191109_070419', to = '20191109_071135')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191212_013608', to = '20191212_013650')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191212_022355', to = '20191212_023650')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191212_030621', to = '20191212_030735')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191212_032132', to = '20191212_032339')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
;set = obj_new('aodataset',from = '20191212_035544', to = '20191212_035758')
;set = set->where('luci.sr_se()','between',[0,1])
;if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191212_062958', to = '20191212_063916')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191213_024249', to = '20191213_025625')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191213_041638', to = '20191213_041747')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]
;
set = obj_new('aodataset',from = '20191213_064311', to = '20191213_065053')
set = set->where('luci.sr_se()','between',[0,1])
if obj_valid(set) and n_elements(tns) gt 0 then tns = [tns,set->tracknums()] else if obj_valid(set) then tns = [tns,set->tracknums()]

fitting = 1
noise = 1
seeing = 0.

tab_sr = fltarr(n_elements(tns))
tab_mag = fltarr(n_elements(tns))
tab_ncpa = fltarr(n_elements(tns))

for i = 0, n_elements(tns)-1 do begin
  ee = getaoelab(tns[i])
  tab_sr[i] = (ee->luci())->sr_se()
  tab_mag[i] = ee->mag()
  tab_ncpa[i] = sqrt(total((ee->adsec_status())->ncpa()^2.))*2e9
endfor

tab_sr2 = sr_from_slopes(tns,fitting=fitting,seeing = seeing,noise=noise)

idx = where(tab_sr2 ne -1)

print, tab_sr[idx], tab_sr2[idx]

set_white_plot
plot,tab_sr[idx],tab_sr[idx],xr = ((minmax([tab_sr[idx],tab_sr2[idx]])+[-.1,.1]) > 0) < 1, $
  yr=((minmax([tab_sr[idx],tab_sr2[idx]])+[-.1,.1]) > 0) < 1,  ytit = 'SR from slopes', xtit = 'SR from PSF'
oplot,tab_sr[idx],tab_sr2[idx],psym=1

end
