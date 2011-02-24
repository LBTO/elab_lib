;+
;
; Accelerometers measurements.
;
;-

; accelerometers

function AOaccel::Init, root_obj, proj, file

  self._file_name = file

  fr = 4000.
  self._proj_fname = proj
  self._store_fname     = filepath(root=root_obj->elabdir(), 'accel.sav')
  self._store_psd_fname = filepath(root=root_obj->elabdir(), 'accel_psd.sav')
  self._store_peaks_fname = filepath(root=root_obj->elabdir(), 'accel_peaks.sav')
  if root_obj->recompute() eq 1B then begin
    file_delete, self._store_fname, /allow_nonexistent
    file_delete, self._store_psd_fname, /allow_nonexistent
    file_delete, self._store_peaks_fname, /allow_nonexistent
  endif

  if not self->AOtime_series::Init( 1./fr, fftwindow="hamming", nwindows=root_obj->n_periods() ) then return,0
  self._norm_factor   = 1e0
  ;  self._spectra_units = textoidl('[arcsec Hz^{-1/2}]')
  self._plots_title   = root_obj->tracknum()

  ;Keep root_obj to easily retrieve residual_modes(), modes() and modalpositions()
  self._root_obj = root_obj

  ; initialize help object and add methods and leafs
  if not self->AOhelp::Init('AOaccel', 'data from the accelerometers') then return, 0
  self->addMethodHelp, "X()", "x-displacement determined from the accelerometers"
  self->addMethodHelp, "Y()", "y-displacement determined from the accelerometers"
  self->addMethodHelp, "Z()", "z-displacement determined from the accelerometers"
  self->addMethodHelp, "RX()", "x-rotation determined from the accelerometers"
  self->addMethodHelp, "RY()", "y-rotation determined from the accelerometers"
  self->addMethodHelp, "RZ()", "z-rotation determined from the accelerometers"
  self->addMethodHelp, "centroid()", "centroid determined from the accelerometers"
  self->addMethodHelp, "plotJitter(from_freq=from_freq, to_freq=to_freq, _extra=ex)", ""
  self->AOtime_series::addHelp, self

  return, 1
end

pro AOaccel::datiProducer
  a = 0.22d
  b = 0.11d
  coef = 3.65d-6
  arcsec2rad = 4.848d-6
  ;offset = 32768L
  if file_test(self._store_fname) then begin
    restore, self._store_fname
  endif else begin
    ;restore file
    restore, self._file_name
    ; compute mechanical xyz displacements and rotations and centroid from accelerometers data
    P = readfits(self._proj_fname,/SILENT)
    out00 = total((data.acc_0-mean(data.acc_0))*self._dt,/cum)
    out0  = total((out00-mean(out00))*self._dt,/cum)
    out20 = total((data.acc_2-mean(data.acc_2))*self._dt,/cum)
    out2  = total((out20-mean(out20))*self._dt,/cum)
    out30 = total((data.acc_3-mean(data.acc_3))*self._dt,/cum)
    out3  = total((out30-mean(out30))*self._dt,/cum)
    out40 = total((data.acc_4-mean(data.acc_4))*self._dt,/cum)
    out4  = total((out40-mean(out40))*self._dt,/cum)
    out50 = total((data.acc_5-mean(data.acc_5))*self._dt,/cum)
    out5  = total((out50-mean(out50))*self._dt,/cum)
    out60 = total((data.acc_6-mean(data.acc_6))*self._dt,/cum)
    out6  = total((out60-mean(out60))*self._dt,/cum)
    xyzR = P##[[out0],[out2],[out3],[out4],[out5],[out6]]*coef ;out0=x, out2=y, out3=z, out4=Rx, out5=Ry, out6=Rz
    cx = (b*xyzR[*,0]-a*xyzR[*,4])/arcsec2rad  ;x-centroid=b*x-a*Ry
    cy = (b*xyzR[*,1]+a*xyzR[*,3])/arcsec2rad  ;y-centroid=b*y+a*Rx
    accel = [[cx],[cy],[xyzR]]
    save, accel, file=self._store_fname, /compress
  endelse
  self._accel = ptr_new(accel, /no_copy)
end

pro AOaccel::plotJitter, from_freq=from_freq, to_freq=to_freq, _extra=ex
  freq = self->freq(from=from_freq, to=to_freq)
  tip  = self->power(0, from=from_freq, to=to_freq, /cum)
  tilt = self->power(1, from=from_freq, to=to_freq, /cum)
  plot, freq, sqrt(tip + tilt), $
    title=self._plots_title, xtitle='Freq [Hz]', ytitle='Jitter [arcsec]', _extra=ex
  oplot, freq, sqrt(tip), col='0000ff'x
  oplot, freq, sqrt(tilt), col='00ff00'x
  legend, ['X+Y', 'X', 'Y'],/fill,psym=[6,6,6],colors=['ffffff'x, '0000ff'x, '00ff00'x]

  sigmatot2 = max ( tip + tilt)  / 2
  ldmas = 1.6d-6 / ao_pupil_diameter() / 4.848d-6 ; l/D in arcsec
  print, 'SR attenuation in H band due to XY jitter ', 1. / (1. + (!pi^2 /2 )*( sqrt(sigmatot2)/ ldmas)^2)
end

function AOaccel::GetDati
  if not ptr_valid(self._accel) then self->datiProducer
  return, self._accel
end

function AOaccel::centroid
  if not ptr_valid(self._accel) then self->datiProducer
  return, (*self._accel)[*,0:1]
end

function AOaccel::X
  if not ptr_valid(self._accel) then self->datiProducer
  return, (*self._accel)[*,2]
end

function AOaccel::Y
  if not ptr_valid(self._accel) then self->datiProducer
  return, (*self._accel)[*,3]
end

function AOaccel::Z
  if not ptr_valid(self._accel) then self->datiProducer
  return, (*self._accel)[*,4]
end

function AOaccel::RX
  if not ptr_valid(self._accel) then self->datiProducer
  return, (*self._accel)[*,5]
end

function AOaccel::RY
  if not ptr_valid(self._accel) then self->datiProducer
  return, (*self._accel)[*,6]
end

function AOaccel::RZ
  if not ptr_valid(self._accel) then self->datiProducer
  return, (*self._accel)[*,7]
end

function AOolmodes::finddirections, from_freq=from_freq, to_freq=to_freq, plot=plot, nfr=nfr, fstep=fstep
  IF not keyword_set(plot) THEN plot=0
  IF not keyword_set(fstep) THEN fstep=0.25
  IF not keyword_set(nfr) THEN nfr=5

  if n_elements(from_freq) eq 0 then from_freq = min(self->freq())
  if n_elements(to_freq)   eq 0 then to_freq = max(self->freq())
  if from_freq ge to_freq then message, "from_freq must be less than to_freq"
  if from_freq lt min(self->freq()) then from_freq = min(self->freq())
  if from_freq gt max(self->freq()) then from_freq = max(self->freq())
  if to_freq lt min(self->freq()) then to_freq = min(self->freq())
  if to_freq gt max(self->freq()) then to_freq = max(self->freq())

  idx_from = closest(from_freq, self->freq())
  idx_to   = closest(to_freq, self->freq())

  peaks=self->findpeaks([0,1], from_freq=from_freq, to_freq=to_freq)

  frtemp = [peaks.(0).fr,peaks.(1).fr]
  pwtemp = [peaks.(0).pw,peaks.(1).pw]
  flag = 0
  j = 0

  cc = [-1,255.,255.*256,255.*256*256,255.*256*100,255.*100]

  while flag eq 0 do begin
    idx = where(abs(frtemp - frtemp[j]) lt 0.6)
    if total(idx) ne -1 then begin
      for k=0,n_elements(idx)-1 do begin
        if idx[k] ne j then begin
          if idx[k] lt n_elements(frtemp)-1 then begin
            frtemp = [frtemp[0:idx[k]-1],frtemp[idx[k]+1:*]]
            pwtemp = [pwtemp[0:idx[k]-1],pwtemp[idx[k]+1:*]]
          endif else begin
            frtemp = frtemp[0:idx[k]-1]
            pwtemp = pwtemp[0:idx[k]-1]
          endelse
          idx = idx-1
        endif
      endfor
    endif
    j+=1
    if j gt n_elements(frtemp)-1 then flag=1
  endwhile

  if n_elements(pwtemp) lt nfr then nnn=n_elements(pwtemp) else nnn=nfr
  maxr = dblarr(nnn)
  idxmax = dblarr(nnn)
  fvibmax = dblarr(nnn)
  pow = dblarr(nnn)
  rm1 = dblarr(self->niter(),nnn)
  rm2 = dblarr(self->niter(),nnn)
  ab = dblarr(2,nnn)
  cor = dblarr(nnn)
  var = dblarr(nnn)
  xy = dblarr(nnn)
  plt = 0
  if total(fvibmax gt 0) then begin
    if plot eq 1 then $
      window, /free
    for ijk = 0, nnn-1 do begin
      if ijk eq 0 then pw=pwtemp else pw[idxmax(ijk-1)]=0
      maxr(ijk) = max(pw,idxmaxtemp)
      idxmax(ijk) = idxmaxtemp
      fvibmax(ijk) = frtemp[idxmax(ijk)]
      pow(ijk) = pwtemp[idxmax(ijk)]
      a1t = fft((self->modes())[*,0])
      a2t = fft((self->modes())[*,1])
      p = self->niter()*(self._root_obj->frames_counter())->deltat()

      if p*fstep lt 1 then fstep=1./p
      a1t[0:p*(fvibmax(ijk)-fstep)-1] = 0
      a1t[p*(fvibmax(ijk)+fstep):p*(1./(self._root_obj->frames_counter())->deltat()-fvibmax(ijk)-fstep)-1] = 0
      a1t[p*(1./(self._root_obj->frames_counter())->deltat()-fvibmax(ijk)+fstep):*] = 0
      a2t[0:p*(fvibmax(ijk)-fstep)-1] = 0
      a2t[p*(fvibmax(ijk)+fstep):p*(1./(self._root_obj->frames_counter())->deltat()-fvibmax(ijk)-fstep)-1] = 0
      a2t[p*(1./(self._root_obj->frames_counter())->deltat()-fvibmax(ijk)+fstep):*] = 0
      rm1[*,ijk] = fft(a1t,1)
      rm2[*,ijk] = fft(a2t,1)
      ab1 = linfit(rm1[*,ijk],rm2[*,ijk])
      ab2 = linfit(rm2[*,ijk],rm1[*,ijk])
      cor1 = variance( rm2[*,ijk]-ab1[1]*rm1[*,ijk]-ab1[0] )
      cor2 = variance( rm1[*,ijk]-ab2[1]*rm2[*,ijk]-ab2[0] )
      if cor1 lt cor2 then begin
        cor[ijk] = cor1
        ab[*,ijk] = ab1
        var[ijk] = variance( rm2[*,ijk] )
      endif else begin
        cor[ijk] = cor2
        ab[*,ijk] = ab2
        xy[ijk] = 1
        var[ijk] = variance( rm1[*,ijk] )
      endelse
      if ijk eq 0 then begin
        if plot eq 1 then $
          plot, 1.1*minmax([rm1[*,ijk],rm2[*,ijk]]), 1.1*minmax([rm1[*,ijk],rm2[*,ijk]]), $
          xtitle='direction 0', ytitle='direction 1', title='vibrations from '+strtrim(from_freq,2)+'Hz to '+strtrim(to_freq,2)+'Hz', charsize=1.2, /nodata
        if plot eq 1 then $
          oplot, rm1[*,ijk], rm2[*,ijk], psym=3
        frvib = fvibmax(ijk)
        colo = -1
      endif else begin
        if plot eq 1 then $
          oplot, rm1[*,ijk], rm2[*,ijk], psym=3, col=CC[ijk]
        frvib = [frvib, fvibmax(ijk)]
        colo = [colo,CC[ijk]]
      endelse
      if plot eq 1 then begin
        if fvibmax(ijk) lt to_freq and fvibmax(ijk) gt from_freq then begin
          if xy[ijk] eq 0 then oplot, 1.2*minmax(rm1[*,0]), ab[1,ijk]*1.2*minmax(rm1[*,0])+ab[0,ijk], col=CC[ijk] $
          else oplot, ab[1,ijk]*1.2*minmax(rm2[*,0])+ab[0,ijk], 1.2*minmax(rm2[*,0]), col=CC[ijk]
        endif
      endif
    endfor
    if plot eq 1 then $
      legend, strtrim(frvib,2)+'Hz', psym=fltarr(nnn)-1, col=colo
    angle =( (-1)^xy*atan(ab[1,*])+xy*!pi/2 )*180/!pi
  endif else begin
    frvib = -1
    pow = -1
    cor = -1
    var = -1
    angle = -1
  endelse

  directions={$
    freq: frvib, $
    power: pow, $
    error_var: cor, $
    signal_var: var, $
    angle: angle $
    }

  return, directions
end

pro AOaccel::free
  if ptr_valid(self._accel) then ptr_free, self._accel
  self->AOtime_series::free
end

pro AOaccel::Cleanup
  if ptr_valid(self._accel) then ptr_free, self._accel
  self->AOtime_series::Cleanup
  self->AOhelp::Cleanup
end

pro AOaccel__define
  struct = { AOaccel, $
    _root_obj   : obj_new(), $
    _file_name  : "" , $
    _store_fname: "" , $
    _proj_fname : "" , $
    _accel      : ptr_new(), $
    INHERITS    AOtime_series		, $
    INHERITS    AOhelp 			$
    }
end