pro plot_jitter_dataset, data, rec = rec, from=from_freq, to=to_freq, xlog = xlog, ylog = ylog, z = z_, $
                               xstyle=xstyle, ystyle=ystyle, yrange=yrange, thick=thick, openloop=openloop, $
                               overRes=overRes

;data: aodataset or TN vector
;from: minimum frequency
;to: maximum frequency
;z: zoom factor

  if size(data,/type) eq 11 then begin
    if obj_class(data) eq 'AOELAB' then tns = data->tracknum()
    if obj_class(data) eq 'AODATASET' then tns = data->tracknums()
  endif else tns = data

  if tns[0] eq '' then begin
    print, 'No valid TN'
    return
  endif
  ndata = n_elements(tns)

  ;check valid tns
  for i = 0, ndata-1 do begin
    cur_data = getaoelab(tns[i], rec = rec)
    if not obj_valid(cur_data) then continue
    if not obj_valid(cur_data->modalcommands()) then continue
    if n_elements(idx_valid) eq 0 then idx_valid = i else idx_valid = [idx_valid,i]
  endfor
  if n_elements(idx_valid) ne 0 then tns = tns[idx_valid] else begin
    print, 'No valid TN'
    return
  endelse
  ndata = n_elements(tns)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if not keyword_set(z_) then z = 1 else z = z_

  npix = 400*z

  ny = fix(sqrt(ndata))
  nx = ndata/ny+ceil(ndata/float(ny)-ndata/ny)

  xs = nx*npix
  ys = ny*npix

  window, xs = xs, ys = ys, /free
  
  !p.multi = [0,nx,ny]

  for i = 0, ndata-1 do begin
    cur_data = getaoelab(tns[i])
    
    norm = 4 ; surf from wfs to real wf rms
    if (cur_data->wfs_status())->optg() lt 1 then norm /= 2 
    
    coeff2mas = norm * 4 / ao_pupil_diameter() / 4.848d-6 * 1e3

    freq_ol = (cur_data->olmodes())->freq(from=from_freq, to=to_freq)
    tip_ol  = (cur_data->olmodes())->power(0, from=from_freq, to=to_freq, /cum) * coeff2mas^2
    tilt_ol = (cur_data->olmodes())->power(1, from=from_freq, to=to_freq, /cum) * coeff2mas^2

    freq_res = (cur_data->residual_modes())->freq(from=from_freq, to=to_freq)
    tip_res  = (cur_data->residual_modes())->power(0, from=from_freq, to=to_freq, /cum) * coeff2mas^2
    tilt_res = (cur_data->residual_modes())->power(1, from=from_freq, to=to_freq, /cum) * coeff2mas^2

    if keyword_set(openloop) then begin
        freq = freq_ol
        tip  = tip_ol
        tilt = tilt_ol
    endif else begin
        freq = freq_res
        tip  = tip_res
        tilt = tilt_res
    endelse
    ;freq = (cur_data->modalcommands())->freq(from=from_freq, to=to_freq)
    ;tip  = (cur_data->modalcommands())->power(0, from=from_freq, to=to_freq, /cum) * coeff2mas^2
    ;tilt = (cur_data->modalcommands())->power(1, from=from_freq, to=to_freq, /cum) * coeff2mas^2
    
    plot, freq, sqrt(tip + tilt), xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, $
      title=tns[i], xtitle='Freq [Hz]', ytitle='Cumulated PSD [mas rms]', charsize = 2, $
      col='FFFFFF'x, xlog = xlog, ylog = ylog, xstyle=xstyle, ystyle=ystyle, yrange=yrange, $
      thick=thick
    oplot, freq, sqrt(tip), col='0000FF'x, thick=thick
    oplot, freq, sqrt(tilt), col='00FF00'x, thick=thick
    if keyword_set(overRes) then begin
        oplot, freq, sqrt(tip_res+tilt_res), thick=thick, line=2
        oplot, freq, sqrt(tip_res), col='0000FF'x, thick=thick, line=2
        oplot, freq, sqrt(tilt_res), col='00FF00'x, thick=thick, line=2
    endif
    
;    x = .01+(i mod nx)/float(nx)
;    y = (ny - i/nx)/float(ny)-.05
;    xyouts, x, y, tns[i], color = 'FFFFFF'x,/normal, charthick = 2, charsize = z
  endfor
  
  !p.multi = 0

end
