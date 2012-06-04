

function AOsinus_acq::Init, root_obj, recompute=recompute

    if not obj_valid(root_obj->disturb()) then return, 0
    if not obj_valid(root_obj->slopes()) then return, 0
    if not obj_valid(root_obj->modalpositions()) then return, 0
    if not obj_valid((root_obj->wfs_status())->pupils()) then return, 0

    self._rootobj = root_obj

    modes = (self._rootobj->disturb())->sin_mode()
    freqs = (self._rootobj->disturb())->sin_freq()

    self._demodulation_fname = filepath(root=self._elabdir, 'demodulation.sav')
    if recompute eq 1B then begin
        file_delete, self._demodulation_fname, /allow_nonexistent
    endif

    self._modes = ptr_new(modes)
    self._freqs = ptr_new(freqs)

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOsinus_acq', 'Multiplexed acquisition with a sinusoidal modulation') then return, 0
    self->addMethodHelp, "demodulate, slowly=slowly, visu=visu, win_id=win_id",     "perform demodulation"
    self->addMethodHelp, "modes()",  "list of multiplexed modes"
    self->addMethodHelp, "freqs()",  "list of temporal frequencies (Hz)"
    self->addMethodHelp, "AA()",   ""    
    self->addMethodHelp, "BB()",   ""    
    self->addMethodHelp, "delta()",   ""    
    self->addMethodHelp, "show, num",   "show delta plot of multiplexed mode <num>"
    return, 1
end

function AOsinus_acq::modes
    return, *(self._modes)
end

function AOsinus_acq::freqs
    return, *(self._freqs)
end

function AOsinus_acq::AA
    return, *(self._AA)
end

function AOsinus_acq::BB
    return, *(self._BB)
end

function AOsinus_acq::delta
    return, *(self._delta)
end

pro AOsinus_acq::show, meas, WIN_ID = WIN_ID

   if n_elements(win_id) eq 0 then window, 10 else wset, WIN_ID
   if not ptr_valid(self._delta) then begin
       message,'AOsinus_acq: no demodulation done yet', /info
       return
   endif
   nel = n_elements(self._delta[0,*])
   if (meas lt 0) or (meas ge nel) then begin
       message,'AOsinus_acq: cannot show measurement #'+strtrim(meas)+': range is 0-'+strtrim(nel-1,2), /info
       return
   endif

   aohistoplot, self._delta[*,meas]*180d/!dPI, /fill, bin=2, mininput=-200, maxinput=200, $
                xtitle='delay between c(t) and s(t) [degrees]', $
                title='mode'+strtrim(self._modes[meas],2)+', '+ strtrim(string(self._freqs[meas],format='(f7.2)'),2)+'Hz'

end

pro AOsinus_acq::demodulate, VISU = VISU, WIN_ID = WIN_ID, slowly = slowly

    if n_elements(WIN_ID) eq 0 then WIN_ID = 10
    if file_test(self._demodulation_fname) then begin
        restore, self._demodulation_fname
        if keyword_set(VISU) then begin
            if n_elements(WIN_ID) eq 0 then WIN_ID = 10
            for ii=0,n_elements(AA)-1 do begin
               self->show, ii, WIN_ID = WIN_ID
               if n_elements(slowly) eq 1 then wait, slowly
            endfor
        endif
    endif else begin
        coeffs = ((self._rootobj->modalpositions())->modalpositions())[*,self._modes]
        nslopes = ((self._rootobj->wfs_status())->pupils())->nsub()*2L
        slopes = ((self._rootobj->slopes())->slopes())[*,0:nslopes-1]
        fs    = 1. / (self._rootobj->modalpositions())->deltat() ;data sampling frequency considering decimation!

        nmodes = n_elements(self._modes)
        AA = fltarr(nmodes)
        BB = fltarr(nslopes, nmodes)
        delta = fltarr(nslopes, nmodes)
        for m=0,nmodes-1 do begin
            coeff = reform(coeffs[*,m])
            demodulate_signals, coeff, slopes, self._freqs[m], fs, A, B, d
            AA[m] = A
            BB[*,m] = B
            delta[*,m] = d
            self->show, m, WIN_ID = WIN_ID
            if n_elements(slowly) eq 1 then wait, slowly
        endfor
        save, AA, BB, delta, filename=self._demodulation_fname
    endelse

    if ptr_valid(self._AA) then ptr_free, self._AA
    if ptr_valid(self._BB) then ptr_free, self._BB
    if ptr_valid(self._delta) then ptr_free, self._delta
    self._AA = ptr_new(AA)
    self._BB = ptr_new(BB)
    self._delta = ptr_new(delta)

end


pro AOsinus_acq::free
    ptr_free, self._modes
    ptr_free, self._freqs
    ptr_free, self._AA
    ptr_free, self._BB
    ptr_free, self._delta
end

pro AOsinus_acq::Cleanup
    self->free
    self->AOhelp::Cleanup
end


pro AOsinus_acq__define
    struct = { AOframes_counter, $
        _modes               : ptr_new() , $
        _freqs               : ptr_new() , $
        _AA                  : ptr_new() , $
        _BB                  : ptr_new() , $
        _delta               : ptr_new() , $
        _root_obj            : obj_new(), $
        _demodulation_fname : ""       , $

        INHERITS AOhelp $
    }
end
