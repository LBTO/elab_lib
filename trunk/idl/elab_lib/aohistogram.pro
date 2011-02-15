FUNCTION AOhistogram, var, _EXTRA = ex

	histo  = histogram(var, locations=loc, reverse_indices=r, _EXTRA = ex)
	nbins  = n_elements(histo)
	idxarr = ptrarr(nbins)
	binsize = loc[1]-loc[0]
	for i=0, nbins-1 do $
		IF r[i] NE r[i+1] THEN idxarr[i] = ptr_new([r[r[i]:r[i+1]-1]]) ELSE idxarr[i] =ptr_new([-1])

	if nbins gt 1 then cols = comp_colors(nbins) else cols = [0]
	leg  = strtrim(string(loc+binsize/2.,format='(f8.2)'),2)+replicate(textoidl('\pm')+strtrim(string(binsize/2.,format='(f8.2)'),2),nbins)

	struct = {  		      $
		histo      : histo  , $
		loc		   : loc    , $
		bz		   : binsize, $
		nbins 	   : nbins  , $
		idxarr     : idxarr , $
		cols	   : cols   , $
		leg		   : leg      $
	}

	return, struct
END
