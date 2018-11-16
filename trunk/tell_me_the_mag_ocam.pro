
; Find equivalent OCAM magnitude
;
; countsPerSubap:  average ADU counts per subaperture
; framerate     :  Ocam framerate in Hz
; emGain        :  EM gain
; binning       :  Ocam binning

function tell_me_the_mag_ocam, countsPerSubap, framerate, binning, emGain

;; Zero point
ref_mag  = 5.5
ref_flux = 1.67E6

flux = countsPerSubap * 30.0/emGain * framerate / (binning*binning)

;Equivalent magnitude:
mag =  2.5 * alog10( ref_flux / flux ) + ref_mag

return,mag

end
