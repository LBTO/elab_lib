FUNCTION AOhistogram, var, _EXTRA = ex, PLOT=PLOT

	;solving histogram and cghistoplot keyword controversies
	if n_elements(ex) ne 0 then begin
		ex2 = struct_selecttags(ex, except=['min','max'])
		if tag_exist(ex, 'min') then ex2 = create_struct(ex2, 'mininput', ex.min)
		if tag_exist(ex, 'max') then ex2 = create_struct(ex2, 'maxinput', ex.max)
	endif

	histo  = histogram(var, locations=loc, reverse_indices=r, _EXTRA = ex)
	nbins  = n_elements(histo)
	idxarr = ptrarr(nbins)
	mybinsize = loc[1]-loc[0]
	for i=0, nbins-1 do $
		IF r[i] NE r[i+1] THEN idxarr[i] = ptr_new([r[r[i]:r[i+1]-1]]) ELSE idxarr[i] =ptr_new([-1])

	if nbins gt 1 then cols = comp_colors(nbins) else cols = [0]
	leg  = strtrim(string(loc+mybinsize/2.,format='(f8.2)'),2)+replicate(textoidl('\pm')+strtrim(string(mybinsize/2.,format='(f8.2)'),2),nbins)

	struct = {  		      $
		nelem      : n_elements(var), $
		histo      : histo  , $
		loc		   : loc    , $
		bz		   : mybinsize, $
		nbins 	   : nbins  , $
		idxarr     : idxarr , $
		cols	   : cols   , $
		leg		   : leg      $
	}

	if keyword_set(PLOT) then begin
		cghistoplot, var, binsize=mybinsize, _EXTRA=ex2, histdata=histo2, locations=loc2, REVERSE_INDICES=r2
		if n_elements(histo2) ne n_elements(histo) then message, 'Warning, nbins not the same',/info
		if total(histo2-histo) ne 0 then message, 'Warning: histo not the same...',/info
		if total(loc2-loc) ne 0 then message, 'Warning: loc not the same...',/info
		if total(r2-r) ne 0 then message, 'Warning: reverse_indices not the same...',/info
	endif

	return, struct
END
