pro show_psf_dataset, data, fullframe = fullframe, rec = rec, z = z_

  loadct,3, rgb_table = rgb_table
  
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
    if not obj_valid(cur_data->luci()) then continue
    if n_elements(idx_valid) eq 0 then idx_valid = i else idx_valid = [idx_valid,i]
  endfor
  if n_elements(idx_valid) ne 0 then tns = tns[idx_valid] else begin
    print, 'No valid TN'
    return
  endelse
  ndata = n_elements(tns)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if not keyword_set(z_) then z = 1 else z = z_
  
  npix = 200*z
  
  ny = fix(sqrt(ndata))
  nx = ndata/ny+ceil(ndata/float(ny)-ndata/ny) 
  
  xs = nx*npix
  ys = ny*npix
  
  window, xs = xs, ys = ys, /free
  
  for i = 0, ndata-1 do begin
    cur_data = getaoelab(tns[i])
    cur_psf = (cur_data->luci())->longexposure(fullframe=fullframe)
    cur_psf = bytscl(congrid(alog(cur_psf > 1),npix,npix))
    psf_rgb = bytarr(3,npix,npix)
    for j = 0,2 do psf_rgb[j,*,*] = rgb_table[cur_psf[*],j]
    tv, psf_rgb, /true, i
    x = .01+(i mod nx)/float(nx)
    y = (ny - i/nx)/float(ny)-.05
    xyouts, x, y, tns[i], color = 'FFFFFF'x,/normal, charthick = z, charsize = z
  endfor

end