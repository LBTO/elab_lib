
pro ao_global_data_init, wunit
	COMMON ao_global_data, DpupM, oc

	if strmid(wunit,0,1) eq 'W' then begin	;LBT
		DpupM = 8.222
		oc = 0.111
	endif else $
	if wunit eq 'MAG' then begin			;MAGELLAN
	 	DpupM = 6.5
	 	oc = 0.29
	endif else message, 'Which telescope are you using?????'
end

