function recalc_mag, countsPerSA, trans = trans, nsubap = nsubap, freq = freq, EMgain = EMgain, $
binning = binning, zeroflux = zeroflux, flux = flux

;zeroflux in ph/s
;flux: optional output (ph/s)

if not keyword_set(zeroflux) then zeroflux = 1. ;in case we just want the flux, so that we don't have to define zeroflux

flux = countsPerSA*nsubap*freq/trans

for i = 0, n_elements(countsperSA)-1 do begin
  case binning[i] of
    1: ADU2nph = 30.3/EMgain[i]
    2: ADU2nph = 23.0/EMgain[i]
    3: ADU2nph = 21.2/EMgain[i]
    4: ADU2nph = 20.7/EMgain[i]
    else: ADU2nph = 30.0/EMgain[i]
  endcase

  flux[i] *= ADU2nph
endfor

mag = -2.5*alog10(flux/zeroflux)

return, mag

end 