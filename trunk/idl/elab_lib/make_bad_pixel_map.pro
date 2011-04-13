;+
; INPUTS:
;	wunit	(string) either 'W1' or 'W2'
;-

pro make_bad_pixel_map, wunit, fr_w=fr_w, fr_h=fr_h
	if n_params() ne 1 then message, "Usage: make_bad_pixel_map, wunit"

    path = filepath(root=ao_datadir(), sub=['wfs_calib_'+wunit, 'irtc', 'backgrounds', 'bin1'], '')
    files = file_search(path, '*.fits_cube.fits')
    nfile = n_elements(files)

	;Take by default only full-frame data
	if not keyword_set(fr_w) then fr_w = 320L
	if not keyword_set(fr_h) then fr_h = 256L

    cubo_out_rms = fltarr(fr_w,fr_h,nfile)
    cubo_out_med = fltarr(fr_w,fr_h,nfile)
    tot = fltarr(nfile)

    for i=0, nfile-1 do begin
		hdr = headfits(files[i],/SILENT)
		naxis = long(aoget_fits_keyword(hdr,'NAXIS1'))
		if naxis ne fr_w then continue
		naxis = long(aoget_fits_keyword(hdr,'NAXIS2'))
		if naxis ne fr_h then continue

        cubo= float(readfits(files[i],/SILENT))
        s = size(cubo,/DIM)
        tot[i] = total(cubo)
;        median_im = fltarr(s[0],s[1])
;        rms_im = fltarr(s[0],s[1])
        rms = median(cubo)
        cubo = cubo/float(rms)
        median_im = median(cubo, dim=3)
        mean_im   = total(cubo,3)/float(s[2])
        for k=0, s[2]-1 do cubo[*,*,k] -= mean_im
        rms_im    = sqrt(total(cubo^2.,3)/(float(s[2]-1.)))
;        for j = 0,s[0]-1 do begin
;            for k = 0,s[1]-1 do begin
;                median_im[j,k] = median(cubo[j,k,*])
;                rms_im[j,k] = stddev(cubo[j,k,*])
;            endfor
;        endfor
        cubo_out_med[*,*,i] = median_im
        cubo_out_rms[*,*,i] = rms_im
    endfor

	stop

	;Remove empty frames:
	idx = where(tot ne 0.,nfile)
	cubo_out_med = cubo_out_med[*,*,idx]
	cubo_out_rms = cubo_out_rms[*,*,idx]

    for j=0, s[0]-1 do begin
        for k=0, s[1]-1 do begin
            Rms_im[j,k] = median(cubo_out_rms[j,k,*])
        endfor
    endfor
;here:
;restore, 'C:\Documents and Settings\carmelo\Documenti\LBT\AGW\sky\darks\all.sav'
    badpixels = fltarr(s[0],s[1])
    index = where(Rms_im gt median(rms_im)+stddev(rms_im)*3.,count)
    if count ge 1 then  badpixels[index] = 1
    index = where(Rms_im eq 0,count)
    if count ge 1 then badpixels[index] = 1

    for j = 0,s[0]-1 do begin
        for k = 0,s[1]-1 do begin
            median_im[j,k] = median(cubo_out_med[j,k,*])
        endfor
    endfor

    index = where(median_im gt median(median_im)+stddev(median_im)*6.,count)
    if count ge 1 then  badpixels[index] = 1
    index = where(median_im eq 0,count)
    if count ge 1 then badpixels[index] = 1

	fname = 'badpixelmap_w'+strtrim(fr_w,2)+'_h'+strtrim(fr_h,2)+'.fits'
    writefits,filepath(root=path, fname),badpixels

end
