
pro ao_global_data_init, wunit
	COMMON ao_global_data, DpupM, oc

	if strmid(wunit,0,2) eq 'W1' then begin	;LBT
		DpupM = 8.222
		oc = 0.111
	endif else $
	if strmid(wunit,0,2) eq 'W2' then begin	;LBT
		DpupM = 8.222
		oc = 0.314
	endif else $
	if strmid(wunit,0,4) eq 'LBTI' then begin	;LBT
		DpupM = 8.222
		oc = 0.111
	endif else $
	if wunit eq 'MAG' then begin			;MAGELLAN
	 	DpupM = 6.5
	 	oc = 0.29
    endif else $
	if wunit eq 'LUCI' then begin			;MAGELLAN
	 	DpupM = 8.222
	 	oc = 0.314
	endif else message, 'Which telescope are you using?????'
end

