
; Find equivalent OCAM magnitude
;
; countsPerFrame:  average ADU counts per frame (whole pupil)
; binning       :  CCD binning
; framerate     :  Ocam framerate in Hz
; emGain        :  EM gain
; zeromag_flux  :  calibrated flux for a zero-magnite star

function tell_me_the_mag_ocam, countsPerFrame, binning, framerate, emGain, zeromag_flux

    case binning of
      1: ADU2nph = 30.3/emGain
      2: ADU2nph = 23.0/emGain
      3: ADU2nph = 21.2/emGain
      4: ADU2nph = 20.7/emGain
      else: ADU2nph = 30.0/emGain
    endcase

    flux = countsPerFrame * ADU2nph * framerate

    ;Equivalent magnitude:
    mag =  2.5 * alog10( zeromag_flux / flux )

    return,mag

end
