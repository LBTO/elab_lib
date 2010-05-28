
pro make_bad_pixel_map
    path = filepath(root=ao_datadir(), sub=['wfs_calib_W2', 'irtc', 'backgrounds', 'bin1'], '')
    files = file_search(path, '*.fits_cube.fits')
    nfile = n_elements(files)
    for i = 0,nfile-1 do begin

        cubo= float(readfits(files[i]))
        s = size(cubo,/DIM)
        median_im = fltarr(s[0],s[1])
        rms_im = median_im
        if i eq 0 then cubo_out_rms = fltarr(s[0],s[1],nfile)
        if i eq 0 then cubo_out_med = fltarr(s[0],s[1],nfile)
        rms = median(cubo)
        cubo = cubo/float(rms)
        for j = 0,s[0]-1 do begin
            for k = 0,s[1]-1 do begin
                median_im[j,k] = median(cubo[j,k,*])
                rms_im[j,k] = stddev(cubo[j,k,*])
            endfor
        endfor
        cubo_out_med[*,*,i] = median_im
        cubo_out_rms[*,*,i] = rms_im
    endfor

    for j = 0,s[0]-1 do begin
        for k = 0,s[1]-1 do begin
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
    stop
    writefits,filepath(root=path, 'badpixelmap.fits'),badpixels
    return
end
