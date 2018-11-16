
;+
;
; modalpositions
; mirror positions projected on modal basis
;-

function AOmodalpositions::Init, root_obj

    self._pos_obj = root_obj->positions()
    self._control_obj = root_obj->control()
    self._fc_obj  = root_obj->frames_counter()
    self._root_obj = root_obj
    if not obj_valid(self._pos_obj) then return, 0
	self._ts_surf_rms = -1.

    self._store_fname = filepath(root=root_obj->elabdir(), 'modalpositions.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'modalpositions_psd.sav')
    self._store_peaks_fname = filepath(root=root_obj->elabdir(), 'modalpositions_peaks.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
        file_delete, self._store_peaks_fname, /allow_nonexistent
    endif

    if not obj_valid(self._fc_obj) then return,0
    if not self->AOtime_series::Init(self._fc_obj->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
	self._spectra_units = textoidl('nm-wf')
	self._plots_title = root_obj->tracknum()

	;Initialize WF
    if not self->AOwf::Init(self._root_obj, self._root_obj->modeShapes()) then message, 'WF object not available', /info

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOmodalpositions', 'Represent mirror positions projected on modal basis') then return, 0
    self->addMethodHelp, "modalpositions()",  "mirror position modes matrix [nmodes,niter]"
    self->addMethodHelp, "plotjitter, from_freq=from_freq, to_freq=to_freq",  "Plots TT cum PSDs"
    self->addMethodHelp, "ts_surf_rms()",  "Thin Shell Surface RMS [m]"
    self->addMethodHelp, "residual_wf_asm()",  "Residual wavefront RMS from modal positions, Tip-Tilt subtracted, only with RR [m]"
    self->AOwf::addHelp, self
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOmodalpositions::datiProducer

    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        pos = self._pos_obj->positions()
        nframes = n_elements(pos[*,0])
        nact = n_elements(pos[0,*])
        ;piston = total(pos,2) / nact
        act_w_cl = (self._root_obj->adsec_status())->act_w_cl()
        pos_w_cl = pos[*,act_w_cl]
        piston = total(pos_w_cl,2) / n_elements(act_w_cl)        
        modalpositions = self._control_obj->c2m() ## (pos - rebin(piston, nframes, nact))
        save, modalpositions, file=self._store_fname
    endelse
    self._modalpositions = ptr_new(modalpositions, /no_copy)

end

function AOmodalpositions::modalpositions, _extra=ex
    return, self->dati(_extra=ex)
end

function AOmodalpositions::nmodes
    return, self->AOtime_series::nseries()
end

function AOmodalpositions::residual_wf_asm
    if self._root_obj->operation_mode() eq 'RR' then begin
        return, 4.0*sqrt(total( self->time_variance(findgen(670)+2) ) ) 
    endif else begin
        return, -1
    endelse
end


; to be implemented in AOtime_series subclasses
function AOmodalpositions::GetDati
    if not ptr_valid(self._modalpositions) then self->datiProducer
    return, self._modalpositions
end

pro AOmodalpositions::plotJitter, from_freq=from_freq, to_freq=to_freq, _extra=ex, overplot=overplot
    coeff2arcsec = self._root_obj->reflcoef() * 4 / ao_pupil_diameter() / 4.848d-6
    freq = self->freq(from=from_freq, to=to_freq)
    tip  = self->power(0, from=from_freq, to=to_freq, /cum) * coeff2arcsec^2
    tilt = self->power(1, from=from_freq, to=to_freq, /cum) * coeff2arcsec^2
    if not keyword_set(overplot) then begin
    	plot, freq, sqrt(tip + tilt), xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, $
        	title=self._plots_title, xtitle='Freq [Hz]', ytitle='Cumulated PSD [arcsec rms]', _extra=ex
    	oplot, freq, sqrt(tip), col='0000ff'x
    	oplot, freq, sqrt(tilt), col='00ff00'x
    	legend, ['Tilt+Tip', 'Tip', 'Tilt'],linestyle=[0,0,0],colors=[!P.COLOR, '0000ff'x, '00ff00'x], charsize=1.2
	endif else begin
    	oplot, freq, sqrt(tip + tilt)
    	oplot, freq, sqrt(tip), col='0000ff'x
    	oplot, freq, sqrt(tilt), col='00ff00'x
	endelse
end

function AOmodalpositions::ts_surf_rms
	if self._ts_surf_rms eq -1 then begin
		meanpos = total(self->modalpositions(),1)/self->niter()
		self._ts_surf_rms  = sqrt(total(meanpos[2:*]^2.))
;		posShape = (self._root_obj->modeShapes())->modemat(mode_idx=indgen(n_elements(meanpos)-2)+2) ## meanpos[2:*]
;		self._ts_surf_rms  = sqrt(total(posShape^2.)/float(n_elements(posShape)))
	endif
	return, self._ts_surf_rms
end


pro AOmodalpositions::free
    if ptr_valid(self._modalpositions) then ptr_free, self._modalpositions
    self->AOwf::free
    self->AOtime_series::free
end


pro AOmodalpositions::Cleanup
    if ptr_valid(self._modalpositions) then ptr_free, self._modalpositions
    self->AOwf::Cleanup
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOmodalpositions__define
    struct = { AOmodalpositions, $
        _modalpositions    :  ptr_new(), $
        _root_obj          :  obj_new(), $
        _control_obj       :  obj_new(), $
        _pos_obj           :  obj_new(), $
        _fc_obj            :  obj_new(), $
        _store_fname       : ""		   , $
        _ts_surf_rms	   : 0.		   , $
        INHERITS    AOwf			   , $
        INHERITS    AOtime_series	   , $
        INHERITS    AOhelp $
    }

end

