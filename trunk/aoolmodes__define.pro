;+
;
; Open Loop Modes (reconstructed from closed-loop data).
;
;-

function AOolmodes::Init, root_obj

	if root_obj->operation_mode() ne 'ONSKY' then return,0

	self._totdelay   = 2L	;Assuming a total of 2 frames delay
	self._decimation = (root_obj->frames_counter())->decimation()
	self._nnMin  =  3L
	self._r0	 =  -1.

	if not total(self._decimation eq [0,1,2]) then begin
		message, 'OLmodes cannot be reconstructed: decimation unknown.',/info
		return, 0
	endif

	if not obj_valid(root_obj->residual_modes()) then begin
	 	message, 'OLmodes cannot be reconstructed: residual modes not available.',/info
		return, 0
	endif

	if not obj_valid(root_obj->modes()) then begin
	 	message, 'OLmodes cannot be reconstructed: integrated modes not available.',/info
		return, 0
	end

	if (self._decimation eq 2) and not obj_valid(root_obj->modalpositions()) then begin
	 	message, 'OLmodes cannot be reconstructed: modal positions not available.',/info
		return, 0
	end

    self._store_fname     = filepath(root=root_obj->elabdir(), 'olmodes.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'olmodes_psd.sav')
    self._r0_store_fname  = filepath(root=root_obj->elabdir(), 'olmodes_r0.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
        file_delete, self._r0_store_fname, /allow_nonexistent
    endif

    if not self->AOtime_series::Init( (root_obj->frames_counter())->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
	self._spectra_units = textoidl('[nm-wf Hz^{-1/2}]')
	self._plots_title   = root_obj->tracknum()

	;Keep root_obj to easily retrieve residual_modes(), modes() and modalpositions()
    self._root_obj = root_obj

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOolmodes', 'Represent reconstructed open loop modes') then return, 0
    self->addMethodHelp, "modes()", "reconstructed open loop modes matrix [niter,nmodes]"
    self->addMethodHelp, "nmodes()", "number of modes"
    self->addMethodHelp, "r0([lambda=lambda] [,/PLOT])", "Estimates r0 @ lambda (default 500nm)"
    self->addMethodHelp, "seeing()", "Seeing value (@ 500nm)"
    self->AOtime_series::addHelp, self

    return, 1
end

pro AOolmodes::datiProducer
    if file_test(self._store_fname) then begin
        restore, self._store_fname, /v
    endif else begin
		rmodes = (self._root_obj->residual_modes())->modes()
		imodes = (self._root_obj->modes())->modes()
		sz = size(rmodes,/dim)
		nframes = sz[0]
		nmodes  = sz[1]

		olmodes = fltarr(nframes,nmodes)
		if self._decimation eq 2 then begin
			mpos = (self._root_obj->modalpositions())->modalpositions()
			mpos = mpos[*,0:nmodes-1]
			for ii=0, nmodes-1 do mpos[*,ii] = mpos[*,ii] - mean(mpos[*,ii]) + mean(imodes[*,ii])
			olmodes[1:*,*] = rmodes[1:*,*] + 0.5*( mpos[1:*,*] + imodes[0:nframes-2,0:nmodes-1] )
		endif else begin
			dd = self._totdelay - self._decimation
		    olmodes[dd:*,*] = rmodes[dd:*,*] + imodes[0:nframes-1-dd,0:nmodes-1]
		endelse

        save, olmodes, file=self._store_fname
    endelse
    self._modes = ptr_new(olmodes, /no_copy)
end


function AOolmodes::modes, _extra=ex
    return, self->dati(_extra=ex)
end


function AOolmodes::nmodes
    return, self->AOtime_series::nseries()
end


; to be implemented in AOtime_series subclasses
function AOolmodes::GetDati
    if not ptr_valid(self._modes) then self->datiProducer
    return, self._modes
end

; Estimate r0 from reconstructed open loop data
;-----------------------------------------------------
function AOolmodes::r0, lambda=lambda, PLOT=PLOT
	if self._r0 eq -1. then self->retrieve_r0

	if keyword_set(PLOT) then begin
		olrms = sqrt(self->time_variance()) * 1e9 * self._root_obj->reflcoef()	;nm wf rms
		nmodes = self->nmodes()
		DpupM = ao_pupil_diameter()
		theovarfit = (4.*!PI^2) * (DpupM/self._r0)^(5./3.) * diag_matrix(kolm_mcovar(nmodes+1)) ;rad^2 @ 500nm
		theorms = sqrt(theovarfit) * 500./(2.*!PI)	; nm wf rms
		yrange = minmax([olrms,theorms])
		window,/free
		plot_oo, lindgen(nmodes)+1, olrms, psym=-1, symsize=0.8, charsize=1.5, $
            ytitle='nm wf rms', xtitle='mode number', title=self._root_obj->tracknum(), yrange=yrange
		oplot, lindgen(nmodes)+1, theorms, color=255L
		legend, ['r0 = '+string(self._r0*1e2, format='(f4.1)')+'cm @ 500nm'], /right, charsize=1.2
	endif

	if n_elements(lambda) ne 0 then return, self._r0*(lambda/500e-9)^(6./5.) else return, self._r0
end

pro AOolmodes::retrieve_r0
	if file_test(self._r0_store_fname) then begin
        restore, self._r0_store_fname
        self._r0 = r0fit
        self._nnMin = nnMin
    endif else begin
    	self->estimate_r0
	endelse
end

;Estimate r0 @ 500nm
pro AOolmodes::estimate_r0

	COMMON olmodes_data, olvarMean, theovar1

	olvar = self->time_variance() * (self._root_obj->reflcoef()*2.*!PI/500e-9)^2.	;in rad^2 @ 500nm
	nmodes  = self->nmodes()
	nnMax   = long(sqrt(8L*nmodes-7L)-1L)/2L
	nnRange = [self._nnMin,nnMax]
	Zern_number = lindgen(nmodes)+2
	nn = long(sqrt(8L*Zern_number-7L)-1L)/2L
	nnValues = findgen(nnRange[1]-nnRange[0]+1)+nnRange[0]
	norders = n_elements(nnValues)

	;Experimental data: the average per radial order of the coeff variances is computed:
	olvarMean = fltarr(norders)
	FOR j=0, norders-1 DO BEGIN
    	FOR i = 0, nmodes-1 DO IF nn(i) EQ nnValues[j] THEN olvarMean[j] = olvarMean[j] + olvar[i]
 	   	olvarMean[j] = olvarMean[j] / (nnValues(j)+1.)
	ENDFOR

	;Theoretical data: a single variance per radial order:
	FirstZerns = (nnValues*(nnValues+1)/2)+1   ;Fist zernike of each radial order.
	theovar1 = (diag_matrix(kolm_mcovar(nmodes+1)))[FirstZerns-2]	;D/r0=1

	;Find best fit
	r0a=0.01 & r0b=1.   ;range of r0s in m
	minf_bracket, r0a,r0b,r0c, erra,errb, errc, FUNC_NAME='AOolmodes::fit_r0_errfunc'
	minf_parabolic, r0a,r0b,r0c, r0fit, errmin, FUNC_NAME='fit_r0_errfunc'

	;Save results
	self._r0 = r0fit
	nnMin = self._nnMin
	save, r0fit, nnMin, filename=self._r0_store_fname
end

function AOolmodes::seeing
	return, 0.5 / (self->r0()*4.85)
end

pro AOolmodes::free
    ptr_free, self._modes
    self->AOtime_series::free
end


pro AOolmodes::Cleanup
    ptr_free, self._modes
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end


pro AOolmodes__define
    struct = { AOolmodes, $
        _root_obj         : obj_new()	, $
    	_totdelay		  : 0L			, $		;total delay of the system in frames.
    	_decimation		  : 0L		  	, $
        _modes            : ptr_new()	, $
        _store_fname      : ""			, $
    	_nnMin			  : 0L			, $		;minimum radial order used in r0 estimation.
        _r0_store_fname   : ""			, $
        _r0				  : 0.			, $
        INHERITS    AOtime_series		, $
        INHERITS    AOhelp 			$
    }

end

