;>>>>>>>>>> on AOSIMUL -> cd, /home/FLAO_data <<<<<<<<<<<<<<<
;>>>>>>>>>> on CORSICA -> cd, /savedata
;disturbance parameters
;******************************************************************
disturb_type = 'atm' ;'vib', 'atm', 'atm+vib'
;disturb_dir = 'phase_screens/'

;FLAO#1
disturb_dir = '/savedata/phase_screens_flao1_ts4/'

;FLAO#2
;disturb_dir = getenv('HOME')+'/FLAO_data/phase_screens_flao2/'
;disturb_dir = '/savedata/phase_screens_flao2/'
;prefix = 'dist_FLAO2_20111210_'

;MAG585
;disturb_dir = '/savedata/phase_screens_mag585/'
;prefix = 'dist_mag585_'

ao_init;, root='~'

;atmospheric parameters
;******************************************************************
;seeing = [0.4, 0.6, 1.0, 1.2, 1.5]
seeing = [1.0, 1.2, 1.5, 0.8]
nseeing = n_elements(seeing)
L0 = 40.
v_wind = 15.
seed = 3892L
n_steps = 4000
hz = [625., 800.]
;hz = [1000.]
nhz = n_elements(hz)

;mirror parameters:
;******************************************************************
;Dpix		  = 233			; pupil diameter [pix]			;FLAO1
;Dpix		  = 232			; pupil diameter [pix]			;FLAO2
;Dpix		  = 219			; pupil diameter [pix]			;FLAO2 20111210
;Dpix		  = 217			; pupil diameter [pix]			;MAG585
Dpix      = 221     ; pupil diameter [ix]       ;FLAO1 with TS4

;mirror modes file (required to compute zonal IFmatrix)
;mirmodes_file = getenv('HOME')+'/FLAO_data/phase_maps/MMmatrix_20090811_setg1.sav'	;FLAO1
;mirmodes_file = getenv('HOME')+'/FLAO_data/phase_maps/MMmatrix_FLAO2_20101207.sav'	;FLAO2 @ aosimul
;mirmodes_file = '/savedata/phase_maps/MMmatrix_FLAO2_20110311.sav'	;FLAO2 @ corsica
;mirmodes_file = '/savedata/phase_maps/MMmatrix_MAG585_20110706.sav'	;MAG585 @ corsica
;mirmodes_file = '/savedata/phase_maps/MMmatrix_FLAO2_20111210.sav'
mirmodes_file = '/savedata/phase_maps/MMmatrix_TS4_20130831.sav'

; Pre-correction parameters:
;*************************************************************
;nmodes_cor = 150
;first_mode_cor = 3
;m2c_cor_fname = 'adsec_calib/M2C/KL/m2c.fits'

; Boost TT
;boost_tt = 4.	;boosting factor

;compile general disturb procedure
;******************************************************************
;.com generate_disturb

;vibration parameters
;******************************************************************
;modes = [1,2]			;modes with vibrations (line # in M2C)
modes = [3]
;dampvibTIP =  [0]		;1 damped, 0 sinusoidal, -1 no vibration
;dampvibTILT =  [0]		;1 damped, 0 sinusoidal, -1 no vibration
;dTIP =  [0.]
;dTILT =  [0.]
;fTIP =  [20.8]		;[Hz]
;fTILT =  [20.8]		;[Hz]
;stddevTIP = [0.1]/sqrt(2) 	;[arcsec]
;stddevTILT = [0.1]/sqrt(2)	;[arcsec]

;dampvib = [[dampvibTIP],[dampvibTILT]]
;d = [[dTIP],[dTILT]]
;f = [[fTIP],[fTILT]]
;ttsec = [[stddevTIP],[stddevTILT]]

dampvib = [0]
d = [0.]
f = [30.]
s = [40e-9]		;m wf rms (the sin amplitude will be s/4*sqrt(2.)

;TTm2sec, 8.222, ttsec=ttsec, ttm=s	;determine standard deviation of vibrations on TIP and TILT in m

;vibration structure
;******************************************************************
vib={$
      m2c_file	: 'adsec_calib/M2C/KL_20110311/m2c.fits', 	$
      modes	: modes, 				$
      dampvib	: dampvib, 				$
      d		: d, 					$
      f		: f, 					$
      s		: s					$
      }

;undefine, s 				;ttm must not be definite at the next TTm2sec call

;;analyse data from IRTC
;;******************************************************************
;dir = '/home/guido/IDL/DATAIRTC/'
;date = ['20090427','20090509','20090511']
;num = [	['00020','00021','00032','00033','00034','00035'], $
;	['00203','00204','00205','00206','-1'   ,'-1'   ], $
;	['00057','00058','00059','00060','00061','-1'   ] ]
;fs = [	[64.,64,64,65,64,64], $
;	[617,617,617,617,-1,-1], $
;	[1032,1032,1032,1032,65,-1] ]
;pixelsc = 0.1
;i = 1
;j = 0
;datavib = date[i]+'_'+num[j,i]+'_m'+strtrim(modes[0],2)+'.fits'
;stringfile = file_search(disturb_dir+datavib, count=count, /FULLY)
;TTm2sec, 8.222, ttsec=1., ttm=ttm
;if count eq 0 then result = analyseTT(dir + date[i]+'/', num[j,i], pixelsc, fs[j,i])
;if count eq 0 then mkhdr, hdr, result.TTcentroid, /EXTEND
;if count eq 0 then sxaddpar, hdr, 'fr', fs[j,i], 'sampling frequency'
;if count eq 0 then sizett = size(result.TTcentroid,/dim)
;if count eq 0 then meanTT1 = replicate((total(result.TTcentroid,2)/n_elements(result.TTcentroid/2))[0],sizett[1])
;if count eq 0 then meanTT2 = replicate((total(result.TTcentroid,2)/n_elements(result.TTcentroid/2))[1],sizett[1])
;if count eq 0 then meanTT = [[meanTT1],[meanTT2]]
;if count eq 0 then temp = fltarr(sizett[1],3)
;if count eq 0 then temp[*,1:2] = transpose((result.TTcentroid - meanTT)*ttm)
;print, size(result.TTcentroid,/dim)
;print, size(temp,/dim)
;if count eq 0 then writefits, disturb_dir+datavib, temp, hdr
;undefine, ttm

;generate disturbance
;******************************************************************
for ii=0, nseeing-1 do $
  for jj=0, nhz-1 do $
    generate_disturb, disturb_type      , $
;    generate_disturb_mag585, disturb_type      , $
    seeing        =     seeing[ii]     	, $
    L0            =     L0          	, $
    v_wind        =     v_wind      	, $
    seed          =     seed        	, $
    disturb_dir   =     disturb_dir 	, $
    n_steps       =     n_steps     	, $
    hz            =     hz[jj]         	, $
    vib           =     vib	    		, $
    datavib	  	  =		datavib			, $
	nmodes_cor	  =		nmodes_cor		, $
	first_mode_cor=		first_mode_cor	, $
	m2c_cor_fname =		m2c_cor_fname	, $
	boost_tt	  = 	boost_tt		, $
	Dpix		  =		Dpix			, $
	mirmodes_file =		mirmodes_file	;, $
;	prefix		  =		prefix

end