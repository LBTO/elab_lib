;+
;
; demodulate_signals_optg, deltacomm_hist, comm_hist, fdist, fsamp, AA, BB
;
;  INPUT
;
;    comm_hist:       float or double vector. Time history of the position commands to the mirror
;                     projected on the mode (i.e. modal amplitude time history)
;
;    deltacomm_hist:  float or double matrix. Time history of the WFS signals (sx and sy) for the
;                     subapertures inside the pupil. Size(sens_mat)=[n_time_samples,2*n_subapertures]
;
;    fdist:           float or double scalar. Frequency of the modal modulation in Hz.
;
;    fsamp:           float or double scalar. Frequency of the time sampling of the data
;
;  OUTPUT
;
;    comm_out:        double scalar. Amplitude of the demodulated command history from deltacomm_hist
;
;    deltacomm_out:   double scalar. Amplitude of the demodulated signal history from comm_hist
;
;-

pro demodulate_signals_optg, deltacomm_hist, comm_hist, fdist, fsamp, deltacomm_out, comm_out

  dt = 1.0/fsamp
  nt = n_elements(deltacomm_hist)
  t=findgen(nt)*dt

  sz = size(comm_hist, /DIM)
  if sz[0] ne nt then message, "Mismatch between size of a_hist and s_hist"

  w = 2*!DPI*fdist
  
  ; 10 periods of modulated signal
  N4mean = round(4./fdist*fsamp)
  
  ; sinusoidals
  dem_sin = sin(w*t)
  dem_cos = cos(w*t)
  
  ; cumulated demodulated signals
  ds = total(deltacomm_hist*dem_sin,/cum)/(findgen(nt)+1)
  dc = total(deltacomm_hist*dem_cos,/cum)/(findgen(nt)+1)
  cs = total(comm_hist*dem_sin,/cum)/(findgen(nt)+1)
  cc = total(comm_hist*dem_cos,/cum)/(findgen(nt)+1)
  
  ; square root
  d_dem = 2.*sqrt(ds^2.+dc^2.)
  c_dem = 2.*sqrt(cs^2.+cc^2.)
  
  ; mean on the last N points
  deltacomm_out = mean(d_dem[nt-N4mean:nt-1])
  comm_out = mean(c_dem[nt-N4mean:nt-1])

end