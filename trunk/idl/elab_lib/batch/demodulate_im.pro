;====================================================
;
;demodulation of sinusoidal modulated signals
;
;====================================================
; WARNING ATOMATIC SIGN RECOVERY DOES NOT WORK!!!
; swap the sign changing sign_swap (first code row) in between +1 and -1

function demodulate_im, ao, sinmodeno, VISU=VISU, VERBOSE=VERBOSE

; INPUT
; ao -> an acquisition with sinusoidal modulation of a single mode
; example: ao = getaoelab('20110615_110948')
;
; OUTPUT
; a vector containing the interaction matrix of the modulated mode
;
; OPTION
; /VISU   	do the plots
; /VERBOSE 	do the prints


sign_swap	= +1.
;sinmodeno = 0

fs 		= ((ao->wfs_status())->ccd39())->framerate()
mode 	= (ao->disturb())->sin_mode(sinmodeno)

; ----------- THRESHOLD FOR THE PEAK DETECTION ----------
(ao->modaldisturb())->set_threshold, 1e-5

;freq 	= ao->ex('modaldisturb.findpeaks('+strtrim(mode,2)+').spec'+string(mode,format='(i04)')+'.fr')
;pw   	= ao->ex('modaldisturb.findpeaks('+strtrim(mode,2)+').spec'+string(mode,format='(i04)')+'.pw')

;dim_freq 	= (size(freq))[0]
;if  dim_freq GT 0 then begin
;	f_idx 	= closest(100., freq)
;	freq	= freq(f_idx)
;	pw 	= pw[f_idx]
;endif

freq 	= (ao->disturb())->sin_freq(sinmodeno)
amp 	= (ao->disturb())->mode_amp(sinmodeno)
;amp  	= sqrt(pw)*sqrt(2.)*1e9	;nm
md_in 	= ((ao->modaldisturb())->modaldisturb())[*,mode]
md_dt 	= (ao->modaldisturb())->deltat()


if keyword_set(VISU) then begin
	window,0
	(ao->modaldisturb())->specplot,mode, title='input disturbance, mode'+strtrim(mode,2)
	legend, ['f='+strtrim(string(freq, format='(f6.2)'),2)+'Hz, Amp='+strtrim(string(amp, format='(f6.2)'),2)+' nm surf'], charsize=1.2
	(ao->intmat())->visu_im2d, [mode], zoom=5
endif

;Classical IM signal
;--------------------------------------------------

slim = ((ao->intmat())->im())[mode,*]	; DL source calibrated IM


;Amplitude of Sinusoidal signal in closed-loop
;--------------------------------------------------
mpfreq = (ao->modalpositions())->freq()
df    = mpfreq[1]-mpfreq[0]
pw_in = (ao->modalpositions())->power(mode, from_freq=freq-df/2., to_freq=freq+df/2.)
amp_in = sqrt(pw_in)*sqrt(2.);*1e9

if keyword_set(VISU) then begin
	window,1
	(ao->modalpositions())->specplot,mode,title='closed-loop spec, mode '+strtrim(mode,2)
	legend, ['f='+strtrim(string(freq, format='(f6.2)'),2)+'Hz, Amp='+strtrim(string(amp_in*1e9, format='(f6.2)'),2)+' nm surf'], charsize=1.2, /bottom
endif

freq_correction 	= 1.
freq				*= freq_correction

;Create a sinusoidal signal with same sampling as real-time data
decim 	= (ao->frames_counter())->decimation()
dt 		= (ao->slopes())->deltat()
niter 	= (ao->slopes())->niter()
time_vec = findgen(niter)*dt
coeff_in = amp_in * sin(2.*!PI*freq*time_vec)

if keyword_set(VISU) then begin
window,2
	plot, time_vec, coeff_in*1e9, psym=-1, xrange=[0,2./freq], xtitle='time [s]', ytitle='coeff [nm surf]' $
		,title='mode '+strtrim(mode,2)+', decimation factor = '+strtrim(decim,2)
	oplot, findgen(niter)*md_dt, md_in*1e9, psym=-2, linestyle=1
	legend, ['input disturb', 'closed-loop'], linestyle=[1,0], psym=-[2,1], /right
endif

; WFS Signals of the corresponding mode:
;------------------------------------------------------------
sl = (ao->slopes())->slopes()
sl_idx = where(total(sl,1) ne 0., nsl)
nsub = ((ao->wfs_status())->pupils())->nsub()
if nsub*2 ne nsl then message, 'wrong number of slope signals!'
sl = sl[*,sl_idx]




;Sinusoidal IM demodulation with Summation and shift
;--------------------------------------------------

sin_ref		= sin(2.*!PI*freq*time_vec)	; reference sin at the modulation freq
amp			= fltarr(nsl)
dstep		= fltarr(nsl)
dphase		= fltarr(nsl)
sign		= fltarr(nsl)
dfine		= fltarr(nsl)
IM			= fltarr(nsl)
phase_vec	= (2.*!pi*time_vec*freq) mod (2*!PI)


; --------------------------- PRLIMINARY PHASE SCAN O ALL THE SIGNALS:
; ----------------------------GOOD SIGNAL IDENTIFICATION

for j=0, nsl-1 do begin
	mysl 		=  sl[*,j] - mean(sl[*,j])					; remove mean
	n_dfase		= 10
	temp 		= fltarr(n_dfase,2)
	for dfase = 0, n_dfase-1 do begin				; 9 e' esagerato!!!! Si puo' ottimizzare
		temp[dfase,0] 	= total( shift(mysl, dfase) * sin_ref) / total(sin_ref^2)
		temp[dfase,1] 	= dfase
	endfor
	amp_max 		= where(temp[*,0] GT 0.95*max(temp[*,0]))
	amp[j]			= temp[amp_max[0], 0] 	; archivio il massimo dell'ampiezza
 	dstep[j]		= temp[amp_max[0], 1]	; archivio il corrispondente shift (non e' fase ma dt)
	dphase[j]		= phase_vec[amp_max[0]]
endfor


good_sl_pos		= where(amp GT 1.5*mean(amp))   ; slopes w/ 'good' signals
n_gsl 			= n_elements(good_sl_pos)
optimal_step	= median(dstep(good_sl_pos))
if keyword_set(VERBOSE) then print, 'optimal step =', optimal_step, 'rms =', stddev(dstep(good_sl_pos))

; --------------------------- PHASE SHIFT IDENTIFICATION ON 2!PI RANGE
; --------------------------- USIGN GOOD SLOPES ONLY
amp			= fltarr(n_gsl)
dfine		= fltarr(n_gsl)

for j = 0, n_gsl-1 do begin
	slope_indx 	= good_sl_pos[j]
	mysl 		= sl[*,slope_indx] - mean(sl[*,slope_indx])					; remove mean
	n_dffase	= 20
	temp 		= fltarr(n_dffase,2)
	for dffase 	= 0, n_dffase-1 do begin
		shift_step		= 2.* (dffase *!pi/(n_dffase-1.))
		fine_shift 		= (-1.*!pi) + shift_step   ; shift fine di una frazione del bin dello scan precedente
		;print, fine_shift
		sin_ref			= sin(2.*!pi*freq*time_vec + fine_shift)
		temp[dffase,0] 	= total( mysl * sin_ref) / total(sin_ref^2)
		temp[dffase,1] 	= fine_shift
	endfor
	amp_max 	= where(abs(temp[*,0]) GE max(abs(temp[*,0])))
	amp[j]		= abs(temp[amp_max[0], 0]) 	; archivio il massimo dell'ampiezza
 	dfine[j]	= temp[amp_max[0], 1]	; archivio il corrispondente shift (non e' fase ma dt)
endfor

; Looking for the most frequent good shift
histbin 			= 0.3
hist_shift 			= histogram(dfine, binsize= histbin)
max_hist			= where(hist_shift EQ max(hist_shift))
median_shift 		= median(dfine)
optimal_shift 		= median_shift
; PERCHE? CA**O NON FUNZIONA QUESTO?!?!?
;optimal_shift		= min(dfine) + (histbin * max_hist)	; shift found as the peak of the distribution histogram

if keyword_set(VISU) then begin
	window,3
	plot, dfine, psym=4
	window,4
	plot, hist_shift, psym=10
endif

if keyword_set(VERBOSE) then begin
	print, 'optimal shift (rad) =', optimal_shift, 'rms =', stddev(dfine)
	print, 'fraction of points in the optimal bin = ', max(hist_shift)/total(hist_shift)
endif

;print, 'median shift (rad) =',  median_shift,  'rms =', stddev(dfine)


; --------------------------- SMALL SHIFT CHECK
; --------------------------- EVALUATING FINAL SIGNAL RMS

shift_range		= !pi	; RAD
n_shift			= 18	; step of the fine phase shift
optimal_shift 	-= shift_range / 2.
shift_step		=  shift_range / n_shift

IMM				= fltarr(nsl, n_shift)
IM_rms			= fltarr(n_shift)
shifts			= fltarr(n_shift)
for k = 0, n_shift-1 do begin
	optimal_shift 	+= shift_step
	amp				=  fltarr(nsl)
	sin_ref			=  sin(2.*!pi*freq*time_vec + optimal_shift)
	for j=0, nsl-1 do begin
		mysl 		= sl[*,j] - mean(sl[*,j])					; remove mean
		amp[j] 		= total(mysl * sin_ref) / total(sin_ref^2)
		IMM[j,k]	= amp[j] /amp_in
	endfor
	IM_rms[k]		= stddev(IMM[*,k])
	shifts[k]		= optimal_shift
endfor

stop

; -------------------- FINAL SELECTION WITH SIGNAL SPATIAL RMS
selected	= where(IM_rms EQ max(IM_rms))
sign 		= -1.* sign_swap
if shifts[selected] GT (!pi/2.) 	then sign *= -1.
if shifts[selected] LT (-1*!pi/2.) 	then sign *= -1.

IM			= sign * IMM[*,selected]

if keyword_set(VERBOSE) then begin
	print, '------FINAL SELECTION ------'
	print,  'IM rms = ', IM_rms[selected],'[sign/m]'
	print,  'Phase shift =',  shifts[selected],'[RAD]'
endif

if keyword_set(VISU) then begin
	window, 6
	plot, shifts, IM_rms, xtitle='Shifts [rad]', ytitle='IM rms [sign/m]'
	oplot, shifts[selected], IM_rms[selected], col=255, psym=4
endif

if selected EQ 0 			then print, '---------- WARNING: SELECTED PHASE AT THE FINE RANGE LIMIT! ----------'
if selected EQ n_shift-1	then print, '---------- WARNING: SELECTED PHASE AT THE FINE RANGE LIMIT! ----------'



; DISPLAY SLOPES
;---------------

if keyword_set(VISU) then begin
	window,7, xsize=640,ysize= 260,title = 'Ref source IM'
	sl2d_ref 		= (ao->slopes())->slopes2d(slopevec=slim)				; 2D IM ref
	image_show, sl2d_ref, /as,/sh
endif

if keyword_set(VISU) then begin
	window,8, xsize=640,ysize= 260,title = 'On-sky IM best guess'								; 2D IM onsky
	sl2d_onsky_v2 	= (ao->slopes())->slopes2d(slopevec=IM)
	image_show, sl2d_onsky_v2, /as,/sh
endif

if keyword_set(VISU) then begin
	window,9, xsize=640,ysize= 260,title = 'Ref - On-sky IM'
	sl2d_onsky_v2 	= (ao->slopes())->slopes2d(slopevec=slim-2*IM)
	image_show, sl2d_onsky_v2, /as,/sh
endif

P2V_IM_OS 	= max(2*IM)-min(2*IM)
rms_IM_OS	= stddev(2*IM)
P2V_IM_RF 	= max(slim)-min(slim)
rms_IM_RF	= stddev(slim)
renorm_fact	= P2V_IM_RF/P2V_IM_OS
renorm_fact_rms	=	rms_IM_RF/rms_IM_OS
;Renorm_IM	= renorm_fact * (2*IM )
Renorm_IM	= renorm_fact_rms * (2*IM )

if keyword_set(VERBOSE) then print, 'Required renormallization =', renorm_fact


if keyword_set(VISU) then begin
	window,10, xsize=640,ysize= 260,title = 'Ref - On-sky IM renomralized'
	sl2d_onsky_v2 	= (ao->slopes())->slopes2d(slopevec=slim-Renorm_IM)
	image_show, sl2d_onsky_v2, /as,/sh
endif

stop

return, IM
end




; ____________ TEST ON-SKY ACQUISITIONS
;mode 0
;ao = getaoelab('20110615_103158')

;mode 30
;ao = getaoelab('20110615_110948')
;set_30 = obj_new('aodataset', from='20110615_110931',to='20110615_111037')
;['20110615_110931','20110615_110948','20110615_111004','20110615_111022','20110615_111037']

;mode 105
;ao = getaoelab('20110615_111100')  -> no buona
;ao = getaoelab('20110615_111117')  -> meglio
;ao = getaoelab('20110615_111146')  -> ancora meglio
;['20110615_111100','20110615_111117','20110615_111131','20110615_111146','20110615_111200']

;mode 300
;ao = getaoelab('20110615_111226')
;['20110615_111226','20110615_111242','20110615_111257','20110615_111312','20110615_111330']


