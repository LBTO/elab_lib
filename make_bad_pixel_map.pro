;+
; INPUTS:
;	wunit	(string) either 'W1' or 'W2'
;   camera  (string) either 'irtc' or 'pisces'
;-

pro make_bad_pixel_map, wunit, camera, fr_w=fr_w, fr_h=fr_h
	if n_params() ne 2 then message, "Usage: make_bad_pixel_map, wunit, camera, fr_w=fr_w, fr_h=fr_h"

    path = filepath(root=ao_datadir(), sub=['wfs_calib_'+wunit, camera, 'backgrounds', 'bin1'], '')
    files = file_search(path, '*.fits_cube.fits')
    nfiletot = n_elements(files)
    print, 'Found '+ string(nfiletot) + ' dark files'

	;Take by default only full-frame data
	if not keyword_set(fr_w) then fr_w = camera eq 'irtc' ? 320L : 1024L
	if not keyword_set(fr_h) then fr_h = camera eq 'irtc' ? 256L : 1024L


	;Compute dark cubes statistics (median & rms):
	;---------------------------------------------------------
    
    ; use chunks of max 100 file to prevent memory allocation error
    nfile = nfiletot<100
    files = files[0:nfile-1]
    cubo_out_rms = fltarr(fr_w,fr_h,nfile)
    cubo_out_med = fltarr(fr_w,fr_h,nfile)
    tot = fltarr(nfile)

    for i=0, nfile-1 do begin
        if (i+1) mod 10 eq 0 then print, 'Analyzing  dark file '+string(i+1)+' of ' +string(nfile)

		hdr = headfits(files[i],/SILENT)
		naxis = long(aoget_fits_keyword(hdr,'NAXIS1'))
		if naxis ne fr_w then continue
		naxis = long(aoget_fits_keyword(hdr,'NAXIS2'))
		if naxis ne fr_h then continue

        cubo= float(readfits(files[i],/SILENT))
        s = size(cubo,/DIM)
        nframes = n_elements(s) eq 3 ? s[2] : 1  
        tot[i] = total(cubo)
;        median_im = fltarr(s[0],s[1])
;        rms_im = fltarr(s[0],s[1])
        thismedian = median(cubo)
        cubo = cubo/float(thismedian)
        median_im = nframes gt 1 ? median(cubo, dim=3) : cubo
        mean_im   = nframes gt 1 ? total(cubo,3)/float(nframes) : cubo
        if nframes gt 1 then begin
            for k=0, nframes-1 do cubo[*,*,k] -= mean_im
            rms_im = sqrt(total(cubo^2.,3)/(float(nframes-1.)))
        endif else begin
            rms_im = 0.
        endelse
;        for j = 0,s[0]-1 do begin
;            for k = 0,s[1]-1 do begin
;                median_im[j,k] = median(cubo[j,k,*])
;                rms_im[j,k] = stddev(cubo[j,k,*])
;            endfor
;        endfor
        cubo_out_med[*,*,i] = median_im
        cubo_out_rms[*,*,i] = rms_im
    endfor

	;Remove empty frames:
	idx = where(tot ne 0.,nfile)
	cubo_out_med = cubo_out_med[*,*,idx]
	cubo_out_rms = cubo_out_rms[*,*,idx]
    ;Remove single frames from rms computation
    idxsingle = where(total(total(cubo_out_rms,1),1) ne 0.) 
stop
	cubo_out_rms = cubo_out_rms[*,*,idxsingle]

	;Build bad pixel map:
	;---------------------------------------------------------
    badpixels = fltarr(fr_w,fr_h)

    rms_im = median(cubo_out_rms, dim=3)
;    for j=0, s[0]-1 do begin
;        for k=0, s[1]-1 do begin
;            Rms_im[j,k] = median(cubo_out_rms[j,k,*])
;        endfor
;    endfor

	;Identify pixels that either vary too much or too little...
    index = where(Rms_im ge median(rms_im)+stddev(rms_im)*6.,count)
    if count ge 1 then  badpixels[index] = 1
    index = where(Rms_im le (median(rms_im)-stddev(rms_im)*6.)>0.,count)
    if count ge 1 then badpixels[index] = 1

	median_im = median(cubo_out_med, dim=3)
;    for j = 0,s[0]-1 do begin
;        for k = 0,s[1]-1 do begin
;            median_im[j,k] = median(cubo_out_med[j,k,*])
;        endfor
;    endfor

	;Identify pixels whose median value is too high (hot pixels) or close to zero (pixels off).
    index = where(median_im ge median(median_im)+stddev(median_im)*6.,count)
    if count ge 1 then  badpixels[index] = 1
    index = where(median_im le (median(median_im)-stddev(median_im)*6.)>0.,count)
    if count ge 1 then badpixels[index] = 1

    


    ; create triangulation for trigrid for interpolating bad pixels:
	idxvalid = long(where( badpixels eq 0., nvalid))
	x_valid = idxvalid mod long(fr_w)
	y_valid = idxvalid  /  long(fr_w)
	TRIANGULATE, float(x_valid), float(y_valid), tr
	bpstr = create_struct('x', x_valid, 'y', y_valid, 'idx', idxvalid, 'np', long(nvalid), 'tr', tr)
    
    ; save badpixelmap and triangulate structure
	if camera eq 'irtc' then begin
        if (fr_w ne 320L) and (fr_h ne 256L) then fname = 'badpixelmap_w'+strtrim(fr_w,2)+'_h'+strtrim(fr_h,2)+'.sav' $
        else fname='badpixelmap.sav'
    endif
	if camera eq 'pisces' then begin
        if (fr_w ne 1024L) and (fr_h ne 1024L) then fname = 'badpixelmap_w'+strtrim(fr_w,2)+'_h'+strtrim(fr_h,2)+'.sav' $
        else fname='badpixelmap.sav'
    endif

    save, badpixels, bpstr, files, file=filepath(root=path, fname)
    print, 'Written '+filepath(root=path, fname)

end
