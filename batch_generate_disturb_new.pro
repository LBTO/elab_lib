;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Atmospheric disturbance generations script

;******************************************************************

verbose = 1

; Disturbance type
; 'atm': phasescreen
; 'vib': vibration
; 'atm+vib':  phasescreen+vibration

disturb_type = 'atm'

;; Seeing and frequency lists
;; A separate disturbance file for each combination will be generated

seeing = [ 0.8, 1.0, 1.2, 2.0]
hz = [1500., 750.]

; Parameters common toall generated disturbances

L0 = 40.
v_wind = 15.
seed = 3892L
n_steps = 4000

; Input mirror modes file
; It is usually associated to a KL matrix

mirmodes_file = '/raid/lbtdata/adsecdx/phase_maps/MMmatrix_TS4_20130831_forKLv16.sav'

; Output path and filename prefix

output_dir = '/tmp'
filename_prefix = 'dist_flao2_KL_v29_'

; optional vibration parameters
; Only used if disturb_type='vib' or 'atm+vib'

m2c_file = 'adsec_calib/M2C/KL_20110311/m2c.fits'
modes   = [3]  ; modes with vibrations (line # in M2C)
dampvib = [0]  ; 1=damped, 0=sinusoidal, -1=no vibration
d = [0.]
f = [30.]     ; [Hz]
s = [0]	      ;m wf rms (the sin amplitude will be s/4*sqrt(2.)

; Optional pre-correction parameters
; Comment out this section to avoid the pre-correction
;
;nmodes_cor = 150
;first_mode_cor = 3
;m2c_cor_fname = 'adsec_calib/M2C/KL/m2c.fits'
; Boost TT
;boost_tt = 4.	;boosting factor


;; End of configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ao_init

nseeing = n_elements(seeing)
nhz = n_elements(hz)

; Read the mirmodes_file to get the DPIX value
restore, mirmodes_file, /V

;vibration structure
;******************************************************************
vib={$
      m2c_file	: m2c_file,  $
      modes	: modes, 				$
      dampvib	: dampvib, 				$
      d		: d, 					$
      f		: f, 					$
      s		: s					$
}


;generate disturbance
;******************************************************************
for ii=0, nseeing-1 do $
  for jj=0, nhz-1 do $
    generate_disturb, disturb_type, $
    seeing        = seeing[ii]    , $
    L0            = L0            , $
    v_wind        = v_wind        , $
    seed          = seed          , $
    disturb_dir   = output_dir 	  , $
    n_steps       = n_steps       , $
    hz            = hz[jj]        , $
    vib           = vib	          , $
    datavib       = datavib       , $
    nmodes_cor	  = nmodes_cor    , $
    first_mode_cor= first_mode_cor, $
    m2c_cor_fname = m2c_cor_fname , $
    boost_tt      = boost_tt      , $
    Dpix          = Dpix          , $
    mirmodes_file = mirmodes_file , $
    verbose       = verbose

end
