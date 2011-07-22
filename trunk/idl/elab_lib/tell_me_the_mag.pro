;Find equivalent magnitude from nph (number of photons in the telescope pupil per frame)
;other inputs:
; fs : sampling frequency (Hz)
function tell_me_the_mag, nph, fs


;Telescope Area:
DpupM = ao_pupil_diameter()
oc = ao_pupil_oc()
PupilArea = !pi/4. * (DpupM)^2. *(1.-oc^2.)

;Telescope+FLAO transmission
trans = 0.44

;CCD39 central wavelength:
lwfs = 750e-9


;Flux
e0 =  1.760e-008						  ; A0 star 0-mag. brightness [J/s/m^2/um]
h = 6.626d-34                             ; Planck constant [Js]
c = 3d8                                   ; light velocity [m/s]


; QE curve in the band of interest (600 to 950 nm)
qe_width_ccd39_b = 0.90 * 100e-9	+ $	;qe=0.90 [600,700] nm
				   0.81 * 100e-9	+ $ ;qe=0.81 [700,800] nm
				   0.73 * 100e-9	+ $ ;qe=0.73 [800,900] nm
				   0.45 *  50e-9		;qe=0.45 [900,950] nm


;number of photons:
;nph = trans * (qe_width_ccd39_b*1e6) * lwfs/fs*PupilArea*e0/(h*c) * 10^(-mag/2.5)

;Equivalent magnitude:
mag = -2.5 * alog10(nph * fs * (h*c) / (trans*(qe_width_ccd39_b*1e6)*lwfs*PupilArea*e0))

return,mag

end