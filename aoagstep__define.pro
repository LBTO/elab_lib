
;+
; autogain data.
;
; Wrapper object that holds all data in a single gain optimization object,
; used to avoid namespace clashes with normal aodata in an aoelab dataset.
;-

function AOagstep::Init, parent, root_obj, first_step, last_step, target, middle_ho

    self._parent = parent
    self._root_obj = root_obj
    self._target = target
    self._middle_ho = middle_ho
    self._first_step = first_step
    self._last_step = last_step

    print,'Creating aoagstep',self._first_step, self._last_step
    if not self->AOhelp::Init('AOagstep', 'Autogain step') then return, 0

    self->addMethodHelp, "recalc()", "Recalculate step"
    return, 1
end

pro AOagstep::recalc, PRONAME=PRONAME, GAINTT=GAINTT, GAINHO1=GAINHO1, GAINHO2=GAINHO2, ERRMSG=ERRMSG, $
                      OUTFILE_PLOT=OUTFILE_PLOT, MODESI_PATTERN=MODESI_PATTERN

    print,'Recalc aoagstep',self._first_step, self._last_step
    if n_elements(PRONAME) eq 0 then PRONAME='autogain_analyze_offline'
    if n_elements(OUTFILE_PLOT) eq 0 then OUTFILE_PLOT='/tmp/plot_step'+strtrim(self._last_step,2)+'.bmp'
    if n_elements(MODESI_PATTERN) eq 0 then MODESI_PATTERN='/tmp/modesi_step%d.fits'

    title = self._root_obj->tracknum() + ' ' + self._target
    steps = indgen(self._last_step - self._first_step + 1) + self._first_step
    join_steps = strjoin(strtrim(steps,2), ' ')
    print,'JOIN STEPS',join_steps
    gainsfile_pattern = filepath(root=self._root_obj->datadir(), 'gains_step%d.fits')
    slopesfile_pattern = filepath(root=self._root_obj->datadir(), 'slopes_step%d.fits')
    recpath = self._parent->recpath()
    binning = ((self._parent->wfs_status())->camera())->binning()
    if binning eq 1 then begin
        threshold_tt = 0.7
    endif else begin
        threshold_tt = 0  ; unset keyword
    endelse

    ;; Delete files that need to be recalculated

    FILE_DELETE, OUTFILE_PLOT, /ALLOW_NONEXISTENT

    for i=0, n_elements(steps)-1 do begin
        step = strtrim(steps[i],2)
        modesi_filename = oaa_str_replace(MODESI_PATTERN, '%d', step)
        FILE_DELETE, modesi_filename
    endfor

    CALL_PROCEDURE, PRONAME, GAINTT=GAINTT, GAINHO1=GAINHO1, GAINHO2=GAINHO2, ERRMSG=ERRMSG, $
                    TARGET=self._target, MIDDLE_HO=self._middle_ho , OUTFILE=OUTFILE_PLOT,                $
                    TITLE=title, JOIN_STEPS=join_steps, GAINSFILE_PATTERN=gainsfile_pattern,              $
                    SLOPESFILE_PATTERN=slopesfile_pattern, MODESIFILE_PATTERN=MODESI_PATTERN,             $
                    RECPATH=recpath, THRESHOLD_TT=threshold_tt
 
end

pro AOagstep__define
struct = { AOagstep, $
    _parent      : obj_new(), $
    _root_obj    : obj_new(), $
    _first_step  : 0L, $
    _last_step   : 0L, $
    _target      : '', $
    _middle_ho   : 0L, $
    INHERITS    AOhelp 		        $
}
end


