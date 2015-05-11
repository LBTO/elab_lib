
;+
;
; modalcommand
; mirror commands projected on modal basis
;-

function AOmodalcommands::Init, root_obj

    self._comm_obj = root_obj->commands()
    self._m2c_obj = root_obj->control()
    self._fc_obj  = root_obj->frames_counter()
    self._modalrec= root_obj->modal_rec()
    self._root_obj = root_obj
    if not obj_valid(self._comm_obj) then return, 0
	self._ts_surf_rms = -1.

    self._store_fname = filepath(root=root_obj->elabdir(), 'modalcommands.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'modalcommands_psd.sav')
    self._store_peaks_fname = filepath(root=root_obj->elabdir(), 'modalcommands_peaks.sav')
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
    if not self->AOhelp::Init('AOmodalcommands', 'Represent mirror commands projected on modal basis') then return, 0
    self->addMethodHelp, "modalcommands()",  "mirror command modes matrix [nmodes,niter]"
    self->addMethodHelp, "plotjitter, from_freq=from_freq, to_freq=to_freq",  "Plots TT cum PSDs"
    self->addMethodHelp, "ts_surf_rms()",  "Thin Shell Surface RMS [m]"
    self->AOwf::addHelp, self
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOmodalcommands::datiProducer

    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        n_modes = self._modalrec->num_svd_filt_modes()
        modalcommands = self._m2c_obj->c2m() ## self._comm_obj->commands()
        save, modalcommands, file=self._store_fname
    endelse
    self._modalcommands = ptr_new(modalcommands, /no_copy)

end

function AOmodalcommands::modalcommands, _extra=ex
    return, self->dati(_extra=ex)
end

function AOmodalcommands::nmodes
    return, self->AOtime_series::nseries()
end

; to be implemented in AOtime_series subclasses
function AOmodalcommands::GetDati
    if not ptr_valid(self._modalcommands) then self->datiProducer
    return, self._modalcommands
end

pro AOmodalcommands::plotJitter, from_freq=from_freq, to_freq=to_freq, _extra=ex, overplot=overplot
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

function AOmodalcommands::ts_surf_rms
	if self._ts_surf_rms eq -1 then begin
		meanpos = total(self->modalcommands(),1)/self->niter()
		self._ts_surf_rms  = sqrt(total(meanpos[2:*]^2.))
	endif
	return, self._ts_surf_rms
end


pro AOmodalcommands::free
    if ptr_valid(self._modalcommands) then ptr_free, self._modalcommands
    self->AOwf::free
    self->AOtime_series::free
end


pro AOmodalcommands::Cleanup
    if ptr_valid(self._modalcommands) then ptr_free, self._modalcommands
    self->AOwf::Cleanup
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOmodalcommands__define
    struct = { AOmodalcommands, $
        _modalcommands     :  ptr_new(), $
        _root_obj          :  obj_new(), $
        _m2c_obj           :  obj_new(), $
        _comm_obj          :  obj_new(), $
        _fc_obj            :  obj_new(), $
        _modalrec          :  obj_new(), $
        _store_fname       : ""		   , $
        _ts_surf_rms	   : 0.		   , $
        INHERITS    AOwf			   , $
        INHERITS    AOtime_series	   , $
        INHERITS    AOhelp $
    }

end

