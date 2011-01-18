;+
; This function is invoked by the estimate_r0 method of AOolmodes
;-

FUNCTION fit_r0_errfunc, r0i

	COMMON olmodes_data

	r0    = abs(r0i)   ;only positive values make sense...
	DpupM = ao_pupil_diameter()
	theoVar = (4.*!pi^2) * (DpupM/r0)^(5./3.) * theovar1

	;Minimizing criterion:
	 ResError = total( abs(olvarMean - theoVar) / olvarMean)
	;ResError = total( ((olvarMean - theoVar) / olvarMean)^2.)
	;ResError = total( (olvarMean - theoVar)^2. / olvarMean)

	return, resError
END