;+
;
;  idx = sgn_sin_modes(delta_phase_mat [, /VISU])
;
;  INPUT
;    delta_phase_mat: float or double matrix. Each row is a vector of phase differences between the
;                     WFS demodulated signal for each subaperture and the command demodulated signal.
;                     See extract_sin_modes routine for more info. Each row is an output of the
;                     delta_phase_vec produced by the extract_sin_modes routine.
;
;  OUTPUT
;    idx:  index of the delta_phase_mat. Let sens_amp_mat the matrix collecting per row
;          the sens_amp_vec vectors as output of extract_sin_modes (in the same order as for
;          delta_phase_mat). The direct output of extract_sin_modes routine for sens_amp_vec
;          does not give the correct sign of subaperture amplitude. To set the correct sign
;          do that:
;          sens_amp_mat[idx] *= -1
;		   sens_amp_mat *= delay/abs(delay)
;
;    delay: Estimate of the phase delay (rad)
;+

function sgn_sin_modes, delta_mat, VISU=VISU, WIN_ID=win_id, N_TERMS_FIT=n_terms_fit, delay=delay, _extra=ex, idx_comp=idx_comp

  if n_elements(n_terms_fit) eq 0 then n_terms_fit=3

  ; Compute histogram of estimated delay (delta_mat)
  ;  NOTE: In general, two lobes separated by !PI
  bin 	= 2.0/180.0*!PI
  n_bin = round(2*!DPI/bin)
  hist 	= histogram(delta_mat, LOC=binx, MIN=-!DPI, MAX=!DPI, NBINS=n_bin, OMIN=omin, OMAX=omax)
  bin 	= binx[1]-binx[0]
  cbinx = binx+bin/2

  ; Select just one of the lobes of the histogram (located within +/-!PI/2)
  idx = where(abs(cbinx) le !DPI/2, count)

  ; Estimate the lobe's peak position
  maxv = max(hist[idx], max_idx)
  max_idx_hist = idx[max_idx]
  max_cbinx = cbinx[max_idx_hist]

  ; Shift histogram so that the peak is located at zero
  shift_hist = shift(hist, -max_idx_hist+n_bin/2)
  shift_cbinx = bin*(findgen(n_bin)-n_bin/2)+max_cbinx
  idx = where(abs(shift_cbinx-max_cbinx) le !DPI/2, count)

  ; Fit a Gaussian on the selected lobe of the histogram.
  ; The center of the Gaussian is an estimate of the delay!
  estimates = fltarr(n_terms_fit)
  estimates[0:1] = [maxv, max_cbinx]
  gfit = gaussfit(shift_cbinx[idx], shift_hist[idx], fit_coeff, ESTIMATES=estimates, NTERMS=n_terms_fit)
  delay = fit_coeff[1]

  ; Show original histogram and fitted Gaussian
  if keyword_set(VISU) then begin
    if n_elements(win_id) eq 0 then window, win_id, /FREE else wset, win_id
;    plot, cbinx, hist, PSYM=10
	aohistoplot, delta_mat, bin=bin, xtitle='phase [rad]', mininput=-!PI, maxinput=!PI, _extra=ex
    ff = fltarr(6)
    ff[0] = fit_coeff
    oplot, [!PI/2,!PI/2], [0,max(hist)*2], linestyle=2
    oplot, [-!PI/2,-!PI/2], [0,max(hist)*2], linestyle=2
    oplot, cbinx, ff[0]*exp(-((cbinx-ff[1])/ff[2])^2/2)+ff[3]+ff[4]*cbinx+ff[5]*cbinx^2, COLOR=255L, thick=1.5
    wait,2.0
  endif

  idx = where(abs(delta_mat-delay) le !DPI/2, count, comp=idx_comp)
  if count eq 0 then message,"UNEXPECTED DATA VALUES. No data having abs(delta-phase)<90deg", /info

  return, idx

end
