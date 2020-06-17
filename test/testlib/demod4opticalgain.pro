;
; INPUTS:
;   trackno                     tracking number
;   mode_idx                    modulated mode number
;   freq_modulation             modulation frequency
; KEYWORDS:
;   autodetect                  is used to retrieve mode_idx and freq_modulation from elab_lib data.
;   extendfr                    is used to make the demodulation of three bin of frequency, the correct, the previous and the following one.
;   find_peak                   is used to compute the freq_modulation as frequency corrensponding to the maximum of the residual mode PSD.
;                               NOTE: a frequency range is used (deafault 20-40Hz)
;   fr_range                    frequency range where the peak is search for when using find_peak
;   check_stability             if this keyword is set two outputs keywords: idx_unstable_modes and coeff_unstable_modes
;                               are computed.
;   idx_unstable_modes          index of unstable modes
;   coeff_unstable_modes        coefficient of unstability for the modes defined by idx_unstable_modes
;                               an unstable mode has a coeff. > 1.1
;                               this coeff. is the ratio between energy on the Closed Loop resonance peak and on the lower frequencies
;
; written by G. Agapito agapito@arcetri.astro.it
;
function demod4opticalgain, trackno, mode_idx, freq_modulation, autodetect=autodetect, extendfr=extendfr, $
                            find_peak=find_peak, fr_range=fr_range, check_stability=check_stability, $
                            idx_unstable_modes=idx_unstable_modes, coeff_unstable_modes=coeff_unstable_modes

if n_elements(fr_range) eq 0 then fr_range=[20.,40.]

data_obj = getaoelab(TrackNo,/rec)

; frequency and decimation
fs = ((data_obj->wfs_status())->camera())->framerate()
decimation = (data_obj->frames_counter())->decimation()
frames_counter = (data_obj->frames_counter())->frames_counter()
;t = (frames_counter-frames_counter(0)) * (1/fs)

if keyword_set(autodetect) then mode_idx = (data_obj->disturb())->sin_mode()

; modal delta commands
data_deltacomm = (data_obj->residual_modes())->modes()
deltacomm = data_deltacomm[*,mode_idx]
nnn = n_elements(deltacomm)
; removes the mean value
deltacomm = deltacomm - mean(deltacomm)
; trend fitting
k_trend = (deltacomm[0] - deltacomm[nnn-1])/(nnn-1)
deltacomm = deltacomm + k_trend*findgen(nnn)
;deltacomm -= mean(deltacomm)

; mirror modal positions object
data_modalposition = (data_obj->modalpositions())->modalpositions()
modalposition = data_modalposition[*,mode_idx]
; removes the mean value
modalposition = modalposition - mean(modalposition)
; trend fitting
k_trend = (modalposition[0] - modalposition[nnn-1])/(nnn-1)
modalposition = modalposition + k_trend*findgen(nnn)
;modalposition -= mean(modalposition)

if keyword_set(autodetect) then begin
  freq_modulation = (data_obj->disturb())->sin_freq()
  dist_freq = (data_obj->disturb())->dist_freq()
  ; corrects freq_modulation if loop freq. and disturb freq. are not matching
  if fs ne dist_freq then freq_modulation = freq_modulation/dist_freq*fs
endif

if keyword_set(check_stability) then begin
    freq = (data_obj->residual_modes())->freq()
    psd  = (data_obj->residual_modes())->psd()

    ; search for resonance freqeuncy (fs_peak)
    frames_delay = (data_obj->delay())->frames_delay()
    resonance_peak = [0.112,0.107,0.103,0.099,0.096,0.092,0.0089]*fs
    ref_delay = [2.7,2.8,2.9,3.0,3.1,3.2,3.3]
    fs_peak = interpol(resonance_peak,ref_delay,frames_delay)

    ; define frequencies interval where oscillation can be found
    f1 = fs/1000.*(fs_peak-fs_peak/3.)
    f2 = fs/1000.*(fs_peak+fs_peak/3.)
    idx1 = closest(f1,freq)
    idx2 = closest(f2,freq)

    ; compute ratio between first part of psd and oscillation interval
    stability_coeff = total(psd[idx1:idx2,*],1)/total(psd[0:idx1,*],1)/((f2-f1)/f1)

    ; oscillation modes are the ones which hava a ratio greater than 1.1
    ; NOTE: optimal gain gives a flat residual mode PSD,
    ;       if the oscillation interval show more energy than the lower frequency the gain is too high and there is an oscillation,
    ;       in fact:
    ;           - if the PSD has more power at lower frequency the gain must be increased
    ;           - if the PSD has less power at lower frequency the gain must be decreased
    idx_unstable_modes = where(stability_coeff gt 1.1)
    coeff_unstable_modes = stability_coeff[idx_unstable_modes]

endif

if keyword_set(find_peak) then begin
  fft1, deltacomm, 1./(fs/(decimation+1.0)), fspec=fspec, psd=psd, /no
  idx_red = where((fspec gt fr_range[0]) and (fspec lt fr_range[1]))
  fspec_red = fspec[idx_red]
  psd_red = psd[idx_red]
  dummy = max(psd_red,idx_max)
  freq_modulation = fspec_red[idx_max]
endif

if keyword_set(extendfr) then begin
    ; frequebcy bin
    freq_bin = fs / ( (decimation+1.0) * n_elements(frames_counter))
    freq_modulation_temp = freq_modulation + [-1*freq_bin, 0, +1*freq_bin]
endif else freq_modulation_temp = freq_modulation 

demodulate_signals_optg, deltacomm, modalposition, freq_modulation_temp, fs/(decimation+1.0), deltacomm_amp, modalposition_amp

data_obj =0 
if keyword_set(autodetect) then begin
   return, [deltacomm_amp,modalposition_amp, mode_idx, freq_modulation]
endif else begin
   return, [deltacomm_amp,modalposition_amp]
endelse

end 
