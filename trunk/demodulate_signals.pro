;+
;
; demodulate_signals, coeff_vec, sens_mat, freq_disturb, freq_data, coeff_amp, sens_amp_vec, delta_phase_vec
;
;  INPUT
;
;    coeff_vec:    float or double vector. Time history of the position commands to the mirror
;                  projected on the mode (i.e. modal amplitude time history)
;
;    sens_mat:     float or double matrix. Time history of the WFS signals (sx and sy) for the
;                  subapertures inside the pupil. Size(sens_mat)=[n_time_samples,2*n_subapertures]
;
;    freq_disturb: float or double scalar. Frequency of the modal modulation in Hz.
;
;    freq_data:    float or double scalar. Frequency of the time sampling of the data
;
;  OUTPUT
;
;    coeff_amp:       double scalar. Amplitude of the demodulated command history from coeff_vec
;
;    sens_amp_vec:    double vector. Vector of amplitudes of the demodulated signal history from sens_mat
;                     Size(sens_amp_vec)=2*n_subapertures
;
;    delta_phase_vec: double vector, same size of sens_amp_vec. vector of phase differences between the
;                     WFS demodulated signal for each subaperture and the command demodulated signal
;
;-

pro demodulate_signals, a_hist, s_hist, fdist, fsamp, AA, BB, delta, VISU=visu, WIN_ID=win_id, _extra=ex

  dt = 1.0/fsamp
  nt = n_elements(a_hist)
  t=findgen(nt)*dt

  sz = size(s_hist, /DIM)
  if sz[0] ne nt then message, "Mismatch between size of a_hist and s_hist"
  if n_elements(sz) eq 1 then p_count = 1L else p_count = sz[1]

  w = 2*!DPI*fdist

  ; 1) Demodulate modal coeff with a "reference" carrier.
  ;   a) Find the closed-loop amplitude (AA) of the applied mode.
  ;   b) Find the phase (pphi0) between the reference carrier and the sinusoidal modal coefficient applied.
  Qa = mean(a_hist*sin(w*t))
  Pa = mean(a_hist*cos(w*t))
  AA = 2d0*sqrt(Qa^2+Pa^2)
  pphi0 = atan(Qa,Pa)

  ; 2) Demodulate modal coeff with a phased carrier.
  ;   a) The amplitude AA should be the same as above.
  ;   b) The remaining phase (pphi) should be in this case very small.
  Qa = mean(a_hist*sin(w*t-pphi0))
  Pa = mean(a_hist*cos(w*t-pphi0))
  AA = 2d0*sqrt(Qa^2+Pa^2)
  pphi = atan(Qa,Pa)

  ; 3) Demodulate the signals with the phased carrier.
  ;   a) Find the amplitude (BB) of each demodulated signal element.
  ;	  b) Find the phase (sphi) between the carrier and the signals.
  Qs = rebin(s_hist*rebin(sin(w*t-pphi0),nt,p_count,/SAMP),1,p_count)
  Ps = rebin(s_hist*rebin(cos(w*t-pphi0),nt,p_count,/SAMP),1,p_count)
  BB = 2d0*sqrt(Qs^2+Ps^2)
  sphi = atan(Qs,Ps)

  ; 4) Phase (delta) between the applied modal coeff and the signal elements.
  ;    NOTE: Because of the sign uncertainty (i.e. BB[i] could be positive or negative),
  ;          the phase (delta) could be either X or X+!PI.
  ;          Furthermore, the phase (delta) estimated from subapertures with
  ;			 signal values close to zero are really noisy (almost random).
  delta = (sphi-pphi)
  idx = where(delta gt !DPI, count)
  if count ne 0 then delta[idx] -= 2*!DPI

  ; 5) Histogram of phase (delta) values.
  ;    NOTE: The histogram should show in most cases (not tip/tilt) two peaks separated
  ;          by !PI (180 degrees).
  if keyword_set(VISU) then begin
    if n_elements(win_id) eq 0 then window, id_win, /FREE else wset, win_id
;    plothist, delta*!CONST.RtoD, XR=[-180.0,180.0], _extra=ex
	aohistoplot, delta*!CONST.RtoD, /fill, bin=2, mininput=-200, maxinput=200, _extra=ex, xtitle='phase [degrees]'
  endif
end

