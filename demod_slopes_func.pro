;+
; NAME:
;   demod_slopes_func
;
; PURPOSE:
;   Demodulates each slope signal from an adaptive optics system at the disturbance 
;   frequency and returns the amplitude of demodulation.
;
; CALLING SEQUENCE:
;   result = demod_slopes_func(trackno, pphi=pphi, recompute=recompute)
;
; INPUTS:
;   trackno    - Track number identifying the AO dataset to process
;
; KEYWORDS:
;   pphi       - Output variable that will contain the phase for each slope
;   recompute  - Set to force recomputation of data structures
;
; OUTPUTS:
;   Returns an array containing the demodulated amplitude for each slope
;
; DESCRIPTION:
;   This function loads an adaptive optics dataset, identifies the modulation
;   frequency from the residual modes, and applies demodulation to each slope signal
;   at this frequency. It's used to analyze the response of wavefront sensor slopes
;   to a sinusoidal disturbance.
;-

function demodulate_vect, vect, fdist, fsamp, pphi_mean=pphi_mean
  ; Calculate time step between samples
  dt = 1.0/fsamp
  ; Number of samples in input vector
  nt = n_elements(vect)
  ; Number of frequencies to demodulate at
  nf = n_elements(fdist)
  ; Create time vector
  t = findgen(nt)*dt
  ; Initialize arrays for demodulated amplitude at each time point and frequency
  dem_temp = fltarr(nt,nf)
  ; Array for mean demodulated amplitude at each frequency
  dem_mean = fltarr(nf)
  
  ; Initialize arrays for phase at each time point and frequency
  pphi_temp = fltarr(nt,nf)
  ; Array for mean phase at each frequency
  pphi_mean = fltarr(nf)
  
  ; Repeat the demodulation for each frequency in fdist vector
  for i=0,nf-1 do begin
    ; Angular frequency (radians/sec)
    w = 2*!DPI*fdist[i]
    
    ; Calculate number of samples for 4 periods of modulated signal
    ; Used later for averaging the final result
    N4mean = round(4./fdist[i]*fsamp)
    
    ; Generate reference sinusoidal signals for demodulation
    dem_sin = sin(w*t)
    dem_cos = cos(w*t)
    
    ; Initialize arrays for accumulated demodulated signals
    ds = fltarr(nt)
    dc = fltarr(nt)
    
    ; Process signal progressively (cumulative approach)
    for j = 1, nt-1 do begin
        ; Extract current portion of signal and remove mean
        cur_vect = vect[0:j]-mean(vect[0:j])
        
        ; Calculate and remove linear trend (tilt)
        tilt = (cur_vect[j]-cur_vect[0])/j
        cur_vect = cur_vect-tilt*findgen(j+1)-cur_vect[0]
        
        ; Multiply with reference signals and average
        ds[j] = total(cur_vect*dem_sin[0:j])/(j+1)
        dc[j] = total(cur_vect*dem_cos[0:j])/(j+1)
    endfor
    
    ; Calculate magnitude (amplitude) from sin and cos components
    ; Factor of 2 accounts for RMS to amplitude conversion
    dem_temp[*,i] = 2.*sqrt(ds^2.+dc^2.)
    
    ; Calculate phase angle using arctangent
    pphi_temp[*,i] = atan(ds,dc)
    
    ; Calculate mean amplitude over the last 4 periods
    dem_mean[i] = mean(dem_temp[max([0,nt-N4mean]):nt-1,i])
    
    ; Calculate mean phase over the last 4 periods
    pphi_mean[i] = mean(pphi_temp[max([0,nt-N4mean]):nt-1,i])
  endfor
  
  ; Return the mean demodulated amplitude for each frequency
  return, dem_mean
end

function demod_slopes_func, trackno, pphi=pphi, recompute=recompute 
  ; Initialize bins variable if not provided
  if n_elements(bins) eq 0 then bins = 1
  
  ; Load the AO dataset for the specified track number
  data_obj = getaoelab(TrackNo, recompute=recompute)
  
  ; Get camera framerate from the dataset
  fs = ((data_obj->wfs_status())->camera())->framerate()
  
  ; Get decimation factor from frames counter
  decimation = (data_obj->frames_counter())->decimation()
  
  ; Calculate actual sampling frequency accounting for decimation
  fsamp = fs/(decimation+1.0)
  
  ; Get the nominal modulation frequency from the disturb object
  freq_modulation = (data_obj->disturb())->sin_freq()
  
  ; Get the residual modal commands
  data_deltacomm = (data_obj->residual_modes())->modes()
  
  ; Get the index of the mode that was modulated
  mode_idx = (data_obj->disturb())->sin_mode()
  
  ; Extract the residual command for the modulated mode
  deltacomm = data_deltacomm[*,mode_idx]
  
  ; Define frequency range for refinement (±10 Hz around nominal frequency)
  fr_range = [freq_modulation-10., freq_modulation+10.]
  
  ; Compute FFT of the residual mode command
  fft1, deltacomm, 1./(fs/(decimation+1.0)), fspec=fspec, psd=psd, /no
  
  ; Find indices within the frequency range of interest
  idx_red = where((fspec gt fr_range[0]) and (fspec lt fr_range[1]))
  
  ; Extract frequency and PSD values in the range
  fspec_red = fspec[idx_red]
  psd_red = psd[idx_red]
  
  ; Find the actual modulation frequency (peak in the PSD)
  dummy = max(psd_red, idx_max)
  freq_modulation = fspec_red[idx_max]
  
  ; Report the identified modulation frequency
  print, 'modulation frequency is: ', freq_modulation
  
  ; Get the slopes data
  slopes = (data_obj->slopes())->slopes()
  
  ; Get the size of the slopes array
  size_slopes = size(slopes, /dim)
  
  ; Number of slope measurements (typically 2 times the number of subapertures)
  nnn = size_slopes[1]
  
  ; Initialize arrays for output amplitude and phase
  out = fltarr(nnn)
  pphi = fltarr(nnn)
  
  ; Process each slope signal
  for i=0, nnn-1 do begin
    ; Check if the slope has any signal (not all zeros)
    if total(abs(slopes[*,i])) gt 0 then begin
      ; Demodulate the slope signal at the identified modulation frequency
      out[i] = demodulate_vect(slopes[*,i], freq_modulation, fsamp, pphi_mean=pphi_mean)
      ; Store the phase information
      pphi[i] = pphi_mean
    endif
  endfor
  
  ; Return the array of demodulated amplitudes
  return, out
end
