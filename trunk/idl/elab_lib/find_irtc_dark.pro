
function find_irtc_dark, root_obj, irtc_fname

	if not file_test(irtc_fname) then return,""
    fitsheader = headfits(irtc_fname, /SILENT)
    dark_subdir = ['wfs_calib_'+(root_obj->wfs_status())->wunit(),'irtc','backgrounds','bin1'] ;always bin1???
	exptime = float(aoget_fits_keyword(fitsheader, 'EXPTIME'))*1e-6
    filter_number = long(aoget_fits_keyword(fitsheader, 'FILTRNR'))

	; Verify that the dark and the psf image were taken with the same exposure time!
	if exptime ne 0. then begin
		trackn = root_obj->tracknum()
		all_darks_search = filepath(root=ao_datadir(), sub=dark_subdir,  '*_cube.fits')
		all_darks_fname  = file_search(all_darks_search, count=ndarks)

		if ndarks gt 0 then begin
			thisJulday = (root_obj->obj_tracknum())->JulDay()
			all_darks_julday = dblarr(ndarks)
			for ii=0, ndarks-1 do begin
				dark_tracknum = strmid(file_basename(all_darks_fname[ii]), 0, 15)
    			y  = fix(strmid(dark_tracknum,  0, 4))
    			m  = fix(strmid(dark_tracknum,  4, 2))
    			d  = fix(strmid(dark_tracknum,  6, 2))
    			hh = fix(strmid(dark_tracknum,  9, 2))
    			mm = fix(strmid(dark_tracknum, 11, 2))
    			ss = fix(strmid(dark_tracknum, 13, 2))
    			all_darks_julday[ii] = julday(m, d, y, hh, mm, ss)
			endfor
			idx_closest = sort(abs(all_darks_julday - thisjulday))

			dark_found = 0B & dd=0
			while (dark_found eq 0B) and (dd lt ndarks) do begin
				;Retrieve header info of averaged dark frame because the file with the cube didn't have any info saved in the header...
				closest_av_dark_fname = filepath(root=ao_datadir(), sub=dark_subdir, file_basename(all_darks_fname[idx_closest[dd]], '_cube.fits'))
				dark_header = headfits(closest_av_dark_fname)
				dark_exptime = float(aoget_fits_keyword(dark_header, 'EXPTIME'))*1e-6	;in seconds
				dark_filter_number = long(aoget_fits_keyword(dark_header, 'FILTRNR'))
				if (dark_exptime eq exptime) and (filter_number eq dark_filter_number) then dark_found=1B else dd+=1
			endwhile
			if dark_found then dark_fname = all_darks_fname[idx_closest[dd]] else begin
				message, 'No compatible (i.e. same exposure time or filter) IRTC dark found', /info
			endelse
		endif else begin
			message, 'No darks found in the specified directory', /info
			return, ""
		endelse
	endif else message, 'No IRTC exposure time available. Cannot find the closest dark...', /info

	;If the closest dark was not found, take the one specified in the header (better than nothing)
	if not keyword_set(dark_found) then begin
    	dark_fname = aoget_fits_keyword(fitsheader, 'IRTC.DARK_FILENAME')
 		dark_fname = filepath(root=ao_datadir(), sub=dark_subdir,  dark_fname)
	endif

	return, dark_fname
end
