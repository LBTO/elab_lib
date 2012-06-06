

function AOsinus_acq::Init, root_obj

    if not obj_valid(root_obj->disturb()) then return, 0
    if not obj_valid(root_obj->slopes()) then return, 0
    if not obj_valid(root_obj->modalpositions()) then return, 0
    if not obj_valid((root_obj->wfs_status())->pupils()) then return, 0

    self._rootobj = root_obj

    modes = (self._rootobj->disturb())->sin_mode()
    freqs = (self._rootobj->disturb())->sin_freq()

    self._demodulation_fname = filepath(root=self._rootobj->elabdir(), 'demodulation.sav')
    if self._rootobj->recompute() eq 1B then begin
        file_delete, self._demodulation_fname, /allow_nonexistent
    endif

    self._saturation_fname = filepath(root=self._rootobj->elabdir(), 'saturation.sav')
    if self._rootobj->recompute() eq 1B then begin
        file_delete, self._saturation_fname, /allow_nonexistent
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
    self->addMethodHelp, "saturation()",   "saturation data"
    return, 1
end

function AOsinus_acq::nmodes
    return, n_elements(*(self._modes))
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

function AOsinus_acq::saturation
    if not ptr_valid(self._saturation) then self->calc_saturation
    return, *(self._saturation)
end

pro AOsinus_acq::calc_saturation

    if file_test(self._saturation_fname) then begin
        restore, self._saturation_fname
    endif else begin
        if not ptr_valid(self._delta) then self->demodulate
        nslopes = ((self._rootobj->wfs_status())->pupils())->nsub()*2L
        slopes = ((self._rootobj->slopes())->slopes())[*,0:nslopes-1]

        ; Soglie di rumore/saturazione
        th_min = 0.03
        th_max = 0.7

        saturation = fltarr( self->nmodes(), 3)
        for m=0,self->nmodes()-1 do begin
            dummy = where((*self._BB)[*,m] lt th_min, cnt)
            under_min = float(cnt)/nslopes

            dummy = where(slopes gt th_max, cnt)
            over_max = float(cnt)/n_elements(slopes)

            saturation[m,*] = [under_min, over_max, 1-(under_min+over_max)]
        endfor

        save, saturation, filename = self._saturation_fname
    endelse

    self._saturation = ptr_new(saturation)

end

pro AOsinus_acq::show, meas, WIN_ID = WIN_ID

   if not ptr_valid(self._delta) then begin
       message,'AOsinus_acq: no demodulation done yet', /info
       return
   endif
   nel = n_elements((self->delta())[0,*])
   if (meas lt 0) or (meas ge nel) then begin
       message,'AOsinus_acq: cannot show measurement #'+strtrim(meas)+': range is 0-'+strtrim(nel-1,2), /info
       return
   endif
   if n_elements(win_id) eq 0 then window, 10 else wset, WIN_ID

   aohistoplot, (self->delta())[*,meas]*180d/!dPI, /fill, bin=2, mininput=-200, maxinput=200, $
                xtitle='delay between c(t) and s(t) [degrees]', $
                title='mode'+strtrim((self->modes())[meas],2)+', '+ strtrim(string((self->freqs())[meas],format='(f7.2)'),2)+'Hz'

end

pro AOsinus_acq::demodulate, VISU = VISU, WIN_ID = WIN_ID, slowly = slowly

    self->free
    if file_test(self._demodulation_fname) then begin
        restore, self._demodulation_fname
    endif else begin
        coeffs = ((self._rootobj->modalpositions())->modalpositions())[*,self->modes()]
        nslopes = ((self._rootobj->wfs_status())->pupils())->nsub()*2L
        slopes = ((self._rootobj->slopes())->slopes())[*,0:nslopes-1]
        fs    = 1. / (self._rootobj->modalpositions())->deltat() ;data sampling frequency considering decimation!

        nmodes = n_elements(self->modes())
        AA = fltarr(nmodes)
        BB = fltarr(nslopes, nmodes)
        delta = fltarr(nslopes, nmodes)
        for m=0,nmodes-1 do begin
            coeff = reform(coeffs[*,m])
            demodulate_signals, coeff, slopes, (self->freqs())[m], fs, A, B, d
            AA[m] = A
            BB[*,m] = B
            delta[*,m] = d
        endfor
        save, AA, BB, delta, filename=self._demodulation_fname
    endelse

    self._AA = ptr_new(AA)
    self._BB = ptr_new(BB)
    self._delta = ptr_new(delta)

    if keyword_set(VISU) then begin
        if n_elements(win_id) eq 0 then win_id=10
        for ii=0,self->nmodes()-1 do begin
           self->show, ii, WIN_ID = WIN_ID
           if n_elements(slowly) eq 1 then wait, slowly
        endfor
    endif

end


pro AOsinus_acq::free
    if ptr_valid(self._AA) then ptr_free, self._AA
    if ptr_valid(self._BB) then ptr_free, self._BB
    if ptr_valid(self._delta) then ptr_free, self._delta
    if ptr_valid(self._saturation) then ptr_free, self._saturation
end

pro AOsinus_acq::Cleanup
    self->free
    if ptr_valid(self._modes) then ptr_free, self._modes
    if ptr_valid(self._freqs) then ptr_free, self._freqs
    self->AOhelp::Cleanup
end


pro AOsinus_acq__define
    struct = { AOsinus_acq, $
        _modes               : ptr_new() , $
        _freqs               : ptr_new() , $
        _AA                  : ptr_new() , $
        _BB                  : ptr_new() , $
        _delta               : ptr_new() , $
        _rootobj            : obj_new(), $
        _demodulation_fname : ""       , $
        _saturation_fname   : ""       , $
        _saturation         : ptr_new(), $

        INHERITS AOhelp $
    }
end
