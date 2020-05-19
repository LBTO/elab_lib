
;+
;
; modaldisturb
; disturb projected on modal basis
;-

function AOmodaldisturb::Init, root_obj
    if not obj_valid(root_obj->disturb()) then return, 0
    if not obj_valid(root_obj->control()) then return, 0

    self._disturb_obj = root_obj->disturb()
    self._m2c_obj     = root_obj->control()

    self._store_fname       = filepath(root=root_obj->elabdir(), 'modaldisturb.sav')
    self._store_psd_fname   = filepath(root=root_obj->elabdir(), 'modaldisturb_psd.sav')
    self._store_peaks_fname   = filepath(root=root_obj->elabdir(), 'modaldisturb_peaks.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
        file_delete, self._store_peaks_fname, /allow_nonexistent
    endif

	dist_freq = self._disturb_obj->dist_freq()
	if dist_freq ne -1. then dt=1./dist_freq else dt=1.
    if not self->AOtime_series::Init(dt, fftwindow="hamming") then return,0
   	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
	self._spectra_units = textoidl('[nm-wf Hz^{-1/2}]')
	self._plots_title = root_obj->tracknum()

	;Initialize WF
    if not self->AOwf::Init(root_obj, root_obj->modeShapes()) then message, 'WF object not available', /info

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOmodaldisturb', 'Represent disturb projected on modal basis') then return, 0
    self->addMethodHelp, "modaldisturb()",  "disturb modes matrix [nmodes,niter]"
    self->AOwf::addHelp, self
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOmodaldisturb::datiProducer

    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        modaldisturb = self._m2c_obj->c2m() ## self._disturb_obj->command()
        save, modaldisturb, file=self._store_fname
    endelse
    self._modaldisturb = ptr_new(modaldisturb, /no_copy)

end

function AOmodaldisturb::modaldisturb, _extra=ex
    return, self->dati(_extra=ex)
end

function AOmodaldisturb::nmodes
    return, self->AOtime_series::nseries()
end

; to be implemented in AOtime_series subclasses
function AOmodaldisturb::GetDati
    if not ptr_valid(self._modaldisturb) then self->datiProducer
    return, self._modaldisturb
end

pro AOmodaldisturb::free
    if ptr_valid(self._modaldisturb) then ptr_free, self._modaldisturb
    self->AOwf::free
    self->AOtime_series::free
end


pro AOmodaldisturb::Cleanup
    if ptr_valid(self._modaldisturb) then ptr_free, self._modaldisturb
    self->AOwf::Cleanup
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOmodaldisturb__define
    struct = { AOmodaldisturb, $
        _modaldisturb      :  ptr_new(), $
        _m2c_obj           :  obj_new(), $
        _disturb_obj       :  obj_new(), $
        _store_fname       : "", $
        INHERITS    AOwf, $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }

end

