
; Find equivalent OCAM magnitude
;
; countsPerFrame:  average ADU counts per frame (whole pupil)
; framerate     :  Ocam framerate in Hz
; emGain        :  EM gain
; zeromag_flux  :  calibrated flux for a zero-magnite star

function tell_me_the_mag_ocam, countsPerFrame, framerate, emGain, zeromag_flux

    flux = countsPerFrame * 30.0/emGain * framerate

    ;Equivalent magnitude:
    mag =  2.5 * alog10( zeromag_flux / flux )

    return,mag

end
