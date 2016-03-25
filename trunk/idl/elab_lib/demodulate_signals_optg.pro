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
;    fdist:           float or double vector. Frequency of the modal modulation in Hz.
;                     if more than one element the demodulation is repeated for each frequency and
;                     the outputs are the square root of the sum of the square values. 
;
;    fsamp:           float or double scalar. Frequency of the time sampling of the data
;
;  OUTPUT
;
;    comm_out:        float scalar. Amplitude of the demodulated command history from deltacomm_hist
;
;    deltacomm_out:   float scalar. Amplitude of the demodulated signal history from comm_hist
;
;  KEYWORDS
;
;    c_dem:           (output) Amplitude history of the demodulated command history from deltacomm_hist, for debug purpose.
;
;    d_dem:           (output) Amplitude history of the demodulated signal history from deltacomm_hist, for debug purpose.
;
;-

pro demodulate_signals_optg, deltacomm_hist, comm_hist, fdist, fsamp, deltacomm_out, comm_out, c_dem=c_dem, d_dem=d_dem

  dt = 1.0/fsamp
  nt = n_elements(deltacomm_hist)
  nf = n_elements(fdist)
  t = findgen(nt)*dt

  sz = size(comm_hist, /DIM)
  if sz[0] ne nt then message, "Mismatch between size of a_hist and s_hist"

  d_dem_temp = fltarr(nt,nf)
  c_dem_temp = fltarr(nt,nf)
  deltacomm_temp = fltarr(nf)
  comm_temp = fltarr(nf)

  ; repeat s the demodulation for each frequency in fdist vector
  for i=0,nf-1 do begin
    w = 2*!DPI*fdist[i]
    
    ; 10 periods of modulated signal
    N4mean = round(4./fdist[i]*fsamp)
    
    ; sinusoidals
    dem_sin = sin(w*t)
    dem_cos = cos(w*t)
    
    ; cumulated demodulated signals
    ds = total(deltacomm_hist*dem_sin,/cum)/(findgen(nt)+1)
    dc = total(deltacomm_hist*dem_cos,/cum)/(findgen(nt)+1)
    cs = total(comm_hist*dem_sin,/cum)/(findgen(nt)+1)
    cc = total(comm_hist*dem_cos,/cum)/(findgen(nt)+1)
    
    ; square root
    d_dem_temp[*,i] = 2.*sqrt(ds^2.+dc^2.)
    c_dem_temp[*,i] = 2.*sqrt(cs^2.+cc^2.)

    ; mean on the last N points
    deltacomm_temp[i] = mean(d_dem_temp[max([0,nt-N4mean]):nt-1,i])
    comm_temp[i] = mean(c_dem_temp[max([0,nt-N4mean]):nt-1,i])

  endfor  

  if nf gt 1 then begin
    ; sum the values
    deltacomm_out = sqrt(total(deltacomm_temp^2.))
    comm_out =  sqrt(total(comm_temp^2.))
    d_dem =  sqrt(total(d_dem_temp^2.,2))
    c_dem =  sqrt(total(c_dem_temp^2.,2))
  endif else begin
    deltacomm_out = deltacomm_temp
    comm_out = comm_temp
    d_dem = d_dem_temp
    c_dem = c_dem_temp
  endelse

end
