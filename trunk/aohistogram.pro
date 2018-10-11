FUNCTION AOhistogram, var, _EXTRA = ex, NOPLOT=NOPLOT

	aohistoplot, var, histdata=histo, locations=loc, reverse_indices=r, NOPLOT=NOPLOT, _EXTRA = ex
	nbins  = n_elements(histo)
	idxarr = ptrarr(nbins)
	mybinsize = loc[1]-loc[0]
	for i=0, nbins-1 do $
		IF r[i] NE r[i+1] THEN idxarr[i] = ptr_new([r[r[i]:r[i+1]-1]]); ELSE idxarr[i] =ptr_new([-1])

	validbins = ptr_valid(idxarr)
	validbins = where(validbins, nvalidbins)
	if nvalidbins gt 1 then cols = comp_colors(nvalidbins) else cols = [0]
	leg  = strtrim(string(loc+mybinsize/2.,format='(f8.2)'),2)+replicate(textoidl('\pm')+strtrim(string(mybinsize/2.,format='(f8.2)'),2),nbins)
	leg  = leg[validbins]

	struct = {  		        $
		var        : var	  , $
		histo      : histo    , $
		loc		   : loc      , $
		bz		   : mybinsize, $
		nbins 	   : nbins    , $
		idxarr     : idxarr   , $
		cols	   : cols     , $
		leg		   : leg      , $
		valididx   : validbins  $
	}


	return, struct
END
