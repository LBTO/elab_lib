;+
;Generate periodic damped vibration
;
; INPUTS:
;	damp	damping coefficent
;	fvib	vibration frequency
;	fc	sampling frequency
;	nstep	no. of iterations
; KEYWORDS:
;	seed	white noise seed
;-
function generate_dampvib, damp, fvib, fc, nstep, seed=seed
  if n_elements(seed) eq 0 then seed = 1
  omega = 2d * !pi * fvib
  damp = damp * omega
  ndamp = omega
  viba1 = -real(2 * exp(- damp / fc) * cos( sqrt(complex( omega^2 - damp^2 )) / fc ))
  viba2 = exp(- 2 * damp / fc)
  vibb1 = -real(2 * exp(- ndamp / fc) * cos( sqrt(complex( omega^2 - ndamp^2 )) / fc ))*0
  vibb2 = exp(- 2* ndamp / fc)*0
  out = dblarr(nstep)
  z1 = dblarr(nstep+1)
  z2 = dblarr(nstep+1)
  v = dblarr(nstep)
  for i = 0, nstep-1 do begin
    v[i] = randomn(seed)
    out[i] = v[i] + z1[i]
    z1[i+1] = vibb1*v[i] +z2[i] -viba1*out[i]
    z2[i+1] = vibb2*v[i] -viba2*out[i]
  endfor
  fft1, out, 1/fc, SPEC=spec, /NOPLOT
  out = real_part(fft(abs(spec),1))
  return, out
end


;+
;Generate periodic sinusoidal vibration
;
; INPUTS:
;	fc	sampling frequency
;	amp	vibration amplitude
;	nstep	no. of iterations
; INPUTS/OUPUTS;
;	fdist	vibration frequency
;-
function generate_sindisturb, fc, fdist=fdist, amp, nstep
  a = fltarr(nstep)
  c = round(fdist/fc*nstep/2.)
  fdist = c*fc/nstep*2.
  for j = 0L, nstep-1 do begin
    a[j] += amp*sin(2.*!pi*fdist*j/fc)
  endfor
  return, a
end

;+
; Convert TT from arcsec to m (or viceversa) for a given diameter
;
; INPUTS:
;	diam	telescope diameter
; INPUTS/OUTPUTS:
;	ttsec	TT in arcsec
;	ttm	TT in m
;-
pro TTm2sec, diam, ttsec=ttsec, ttm=ttm
  sec2rad = 4.848d-6
  g = 1 / diam / sec2rad
  if n_elements(ttsec) eq 0 then ttsec = g * ttm
  if n_elements(ttm) eq 0 then ttm = ttsec / g
  if n_elements(ttsec) ne 0 and n_elements(ttm) eq 0 then message, 'both ttm and ttsec have been already defined!'
end

;+
; Restores modal influence functions and computes associated zonal influence functions
;
function modalif_to_zonalif, mirmodes_file, idx_mask=idx_mask
  ; Restore MMmatrix and mm2c
  restore, mirmodes_file
  IFmatrix = transpose(mm2c ## transpose(MMmatrix))
  undefine, MMmatrix, mm2c
  return, IFmatrix
end

function restore_modalif, mirmodes_file, idx_mask=idx_mask, mm2c=mm2c
  ; Restore MMmatrix and mm2c
  restore, mirmodes_file
  return, MMmatrix
end


;+
; Generate disturbance vector

; INPUTS:
;	disturb_type	String variable defining type of disturbance: 'atm', 'atm+vib', 'vib'
;-
pro generate_disturb, disturb_type, $
		seeing			=			seeing			, $
		L0				=			L0				, $
		v_wind			=			v_wind			, $
		seed			=			seed			, $
		disturb_dir		=			disturb_dir 	, $
		nmodes_cor		=			nmodes_cor		, $
		first_mode_cor	=			first_mode_cor	, $
		m2c_cor_fname	=			m2c_cor_fname	, $
		boost_tt		=			boost_tt		, $
		n_steps			=			n_steps			, $
		hz				=			hz  			, $
		vib      		=    		vib     		, $
		datavib  		=    		datavib			, $
		Dpix			=			Dpix			, $
		mirmodes_file	=			mirmodes_file

; General Parameters
;*************************************************************
lambda 		  = 0.75d-6 	; WFS wavelength	[m]
Refl_coeff	  = 4.			; 2 times the number if reflections on the screen (4 when working with retroreflector)
Diam		  = 8.222		; pupil diameter on sky	[m]

; Phase screen parameters
;*************************************************************
if n_elements(seeing) 		eq 0 then seeing 	= 0.8				; arcsec
if n_elements(L0) 			eq 0 then L0		= 40.				; outer scale			[m]
if n_elements(v_wind)		eq 0 then v_wind	= 15.0				; wind velocity			[m/s]
if n_elements(seed)			eq 0 then seed		= 1538L				; seed for the screen
if n_elements(disturb_dir) 	eq 0 then disturb_dir = './'

n_layers	= 1					; number of layers
angle_wind  = 45.				; angle of phase screen translation [degrees]

; Pre-correction parameters:
;*************************************************************
if n_elements(nmodes_cor)	  eq 0 then nmodes_cor=0				; number of modes to pre-correct the turbulence
if n_elements(first_mode_cor) eq 0 then first_mode_cor=0			; index to first pre-corrected mode
if keyword_set(nmodes_cor) then begin
	if not keyword_set('towerdata/'+m2c_cor_fname) then begin
		message, 'ERROR: Modal pre-correction not possible. M2C not specified.', /INFO
		return
	endif else begin
		m2c_cor = readfits('towerdata/'+m2c_cor_fname, /SILENT)
		if m2c_cor[0] eq -1 then begin
			message, 'ERROR: Modal pre-correction not possible. M2C not found.', /INFO
			return
		endif
	endelse
endif


; AdSec parameters used for disturbance emulation
;*************************************************************
if n_elements(n_steps)		eq 0 then n_steps 	= 4000				; buffer size on the Adsec unit  		[steps]
if n_elements(hz)			eq 0 then hz		= 1000.				; frequency of the oversampling loop 	[Hz]


; Derived parameters
;*************************************************************
r_0 		 = 0.9759*0.5/(seeing*4.85)				; r0 @500nm 			[m]
angle_coef   = [cos(angle_wind*!PI/180.), sin(angle_wind*!PI/180.)]
t_int		 = 1. / hz								; sampling time			[s/step]
sample_size  = Diam / Dpix							; sample size			[m/pix]
scr_size_m_x = (v_wind*t_int*n_steps)*angle_coef[0]	; screen dimension 		[m]
scr_size_m_y = (v_wind*t_int*n_steps)*angle_coef[1]	; screen dimension 		[m]
scr_size_m   = max([scr_size_m_x,scr_size_m_y], ang_idx)		;
scr_size_pix = round(scr_size_m / sample_size)		; side of the screen	[pix]

;rname = 'dist_'+disturb_type		;FLAO1
;rname = 'dist_flao2a'+disturb_type	;FLAO2
;rname = 'dist_flao2_20111210_'+disturb_type	;FLAO2
rname = 'dist_flao1_ts4_'+disturb_type  ;FLAO1 with TS4


; Vibration Disturbance Handling
;**************************************************************
if disturb_type eq 'vib' or disturb_type eq 'atm+vib' then begin

	;vib={$
	;      m2c_file, 	$ 	;M2C path + file name
	;      modes, 		$	;modes with vibrations (line # in M2C)
	;      dampvib, 	$	;matrix, with elements: 1 damp vibration, 0 sinusoidal vibration, -1 no vibration
	;      d, 		$ 	;vibration damping coefficent
	;      f, 		$ 	;vibration frequency
	;      s 		$ 	; vibration stddev [m]
	;      }

	;initialize M2c, # of modes with vibrations, modes history, commands history
  	M2C = readfits('towerdata/'+vib.m2c_file, /SILENT)
  	nmodes = n_elements(vib.modes)
  	tvectt = dblarr(672,n_steps)
  	command_hist_vib = fltarr(n_steps,672)

	;make header
  	mkhdr, hdr, command_hist_vib, /EXTEND
  	sxaddpar, hdr, 'm2c', vib.m2c_file, 'M2C'

	;vibration tracking number
	time = systime(/julian, /utc)
	caldat, time, M,D,Y,hour,min,sec
	if M lt 10 then strm = '0'+strtrim(string(m),2) else strm = strtrim(string(m),2)
	if D lt 10 then strd = '0'+strtrim(string(d),2) else strd = strtrim(string(d),2)
	if hour lt 10 then strhour = '0'+strtrim(string(hour),2) else strhour = strtrim(string(hour),2)
	if min lt 10 then strmin = '0'+strtrim(string(min),2) else strmin = strtrim(string(min),2)
	if sec lt 10 then strsec = '0'+strtrim(string(round(sec)),2) else strsec = strtrim(string(round(sec)),2)
  	command_hist_vib_fname = '_vibtr_'+strtrim(string(Y),2)+strm+strd+'_'+strhour+strmin+strsec
	casev = 0
	if n_elements(datavib) ne 0 then casev += 1
	if casev eq 1 then begin
		dv = readfits(disturb_dir+datavib,hdrdata, /SILENT)
		size_dv = size(dv, /dim)
		frdata = float(aoget_fits_keyword(hdrdata, 'fr'))
		if frdata ne hz then begin
			coeff = hz / frdata
			if coeff lt 1 then size_dv[0] = fix(size_dv[0]*coeff)
			grid = findgen(size_dv[0]) / coeff
			for jjj = 0L, size_dv[1]-1 do $
			tvectt[jjj,0:size_dv[0]-1] = interpolate(dv[*,jjj], grid)
		endif else begin
			tvectt[0:size_dv[1]-1,0:size_dv[0]-1] = transpose(dv)
		endelse
		if size_dv[0] lt 4000 then begin
			kkk = 0
			while kkk ge 0 do begin
				kkk += 1
				if not is_even(kkk) then temp = reverse(tvectt[*,max([(kkk+1)*size_dv[0]-3999,0]):size_dv[0]],2) $
				else temp = tvectt[*,0:min([3999-kkk*size_dv[0],size_dv[0]-1])]
			        tvectt[*,kkk*size_dv[0]:min([3999,(kkk+1)*size_dv[0]-1])] =  temp
				if kkk+1 ge 4000./size_dv[0] then break
			endwhile
		endif
		tvectt /= Refl_coeff
		sxaddpar, hdr, 'totnvib', 0, 'total number of vibrations'
		sxaddpar, hdr, 'casevib', 1, '0 vibration coefficents, 1 temporal data, 2 psds'
		sxaddpar, hdr, 'datavib', disturb_dir+datavib, 'path of fits file of temporal data'
	endif
	if casev eq 0 then begin
		iii = 1
		;generation of vibration mode history
		;------------------------------------------------------
  		for k = 0L, nmodes-1 do begin
    			wht = where(vib.dampvib[*,k] ne -1, cnt)
    			tvect = dblarr(cnt,n_steps)
				; header -> modes with vibrations
       			for p = 0L, cnt - 1 do begin
				sxaddpar, hdrla, 'mode'+strtrim(iii,2), vib.modes[k], 'no. of mode with vibration no.'+strtrim(iii,2)
      				if vib.dampvib[p,k] eq 1 then $
      				tvect[p,*] = generate_dampvib(vib.d[p,k],vib.f[p,k],hz,n_steps) $
      				else tvect[p,*] = generate_sindisturb(hz, fdist=vib.f[p,k], 1., n_steps)
      				temp = stddev(tvect[p,*])
      				tvect[p,*] *= vib.s[p,k]/temp/Refl_coeff
      				tvectt[vib.modes[k],*] += tvect[p,*]
					; header -> vibration parameters
      				sxaddpar, hdr, 'fr'+strtrim(iii,2), vib.f[p,k], 'frequency of the vibration no.'+strtrim(iii,2)
      				sxaddpar, hdr, 'stddev'+strtrim(iii,2), vib.s[p,k], 'stddev of the vibration no.'+strtrim(iii,2)
				if vib.dampvib[p,k] eq 1 then $
      				sxaddpar, hdr, 'damp'+strtrim(iii,2), vib.d[p,k], 'damping of the damped vibration no.'+strtrim(iii,2) $
				else sxaddpar, hdr, 'damp'+strtrim(iii,2), 0., 'damping of the damped vibration no.'+strtrim(iii,2)
				iii += 1
    			endfor
  		endfor
		sxaddpar, hdr, 'totnvib', iii-1, 'total number of vibrations'
		sxaddpar, hdr, 'casevib', 0, '0 vibration coefficents, 1 temporal data'
	endif
	command_hist_vib = M2C ## transpose(tvectt)
endif


; Atmospheric Disturbance Handling
;**************************************************************
if disturb_type eq 'atm' or disturb_type eq 'atm+vib' then begin

	;update parameters due to rounding:
	scr_size_m	= scr_size_pix * sample_size

	print, 'Wind speed requested: ', v_wind
	v_wind = scr_size_m / (t_int * n_steps) / angle_coef[ang_idx]
	print, 'Wind speed obtained: ', v_wind

	shft = v_wind * t_int / sample_size			; Shift to be applied to phase screen at each step	[pix]

	; Compute matrices for modal pre-correction
	if keyword_set(nmodes_cor) then begin
		IFmatrix = modalif_to_zonalif(mirmodes_file, idx_mask=idx_mask)
		print, 'computing matrices for pre-correction of modes...'
		modes_cor_matrix = IFmatrix ## m2c_cor[first_mode_cor:first_mode_cor+nmodes_cor-1,*]
		inv_modes_cor_matrix = pseudo_invert(modes_cor_matrix, EPS=1e-4, COUNT_ZEROS=count1, /VERBOSE)
		undefine, m2c_cor
	endif

	if keyword_set(boost_tt) then begin
		if n_elements(IFmatrix) eq 0 then IFmatrix = modalif_to_zonalif(mirmodes_file, idx_mask=idx_mask)
		xx = idx_mask mod Dpix
		yy = idx_mask / Dpix
		cx = mean(xx)
		cy = mean(yy)
		rr = sqrt(n_elements(idx_mask)/ !PI)
		tip  = zern(2, (xx-cx)/rr, (yy-cy)/rr)
		tilt = zern(3, (xx-cx)/rr, (yy-cy)/rr)
		tt_mat = [transpose(tip),transpose(tilt)]
		inv_tt_mat = pseudo_invert(tt_mat, EPS=1e-4, COUNT_ZEROS=count_tt, /VERBOSE)
	endif

	; Inverse of zonal IF matrix, for projection of disturb realization onto DM space
	;inv_IFmat = disturb_dir+'inv_IFmatrix_flao2.sav'
	;inv_IFmat = disturb_dir+'inv_MMmatrix_mag585.sav'
	inv_IFmat = filepath(root=disturb_dir,'inv_'+file_basename(mirmodes_file))
	if file_test(inv_IFmat) then begin
		undefine, IFmatrix
		restore, inv_IFmat
	endif else begin
		if n_elements(IFmatrix) eq 0 then IFmatrix = restore_modalif(mirmodes_file, idx_mask=idx_mask, mm2c=mm2c)
		inv_IFmatrix = pseudo_invert(IFmatrix, EPS=1e-4, W_VEC=ww, U_MAT=uu, V_MAT=vv, INV_W=inv_ww,  IDX_ZEROS=idx, COUNT_ZEROS=count, /VERBOSE, N_MODES_TO_DROP=1)
		save, inv_IFmatrix, Dpix, idx_mask, mm2c, filename=inv_IFmat, /compress
		undefine, IFmatrix
	endelse

	; Screen generation/retrieval
	phase = get_layers(n_layers, scr_size_pix, scr_size_m, lambda, r_0, L0=L0, par=par, seed=seed, dir=disturb_dir, FILE=ph_file, /no_sha, /VERBOSE)

	; and convert to surface [m]
	phase = phase * lambda/(2*!pi*Refl_coeff)

	; Pupil mask
	maskPup = fltarr(Dpix,Dpix)
	maskPup[idx_mask] = 1.
	np = total(maskPup)

	; Generate atmospheric disturbance history
	command_hist = fltarr(672,n_steps)
	position = 0.
	for ii=0L, n_steps-1 do begin
		ph = fshift(phase, position*angle_coef[0], position*angle_coef[1])
		ph = ph[0:Dpix-1,0:Dpix-1] * maskPup
		ph = (ph - total(ph)/np) * maskPup					;remove piston

		; Project out selected modes:
		if keyword_set(nmodes_cor) then begin
			coeff_mode_cor = inv_modes_cor_matrix ## ph[idx_mask]
			ph[idx_mask] = ph[idx_mask] - modes_cor_matrix ## coeff_mode_cor
			ph = (ph - total(ph)/np) * maskPup
		endif

		if keyword_set(boost_tt) then begin
			ttcoeff = inv_tt_mat ## ph[idx_mask]
			ph[idx_mask] = ph[idx_mask] + tt_mat ## ttcoeff * (boost_tt-1.)
			ph = (ph - total(ph)/np) * maskPup
		endif

		command_hist[*,ii] = mm2c ## reform(inv_IFmatrix ## ph[idx_mask])	;project phase onto IFs
		;command_hist[*,ii] = inv_IFmatrix ## ph[idx_mask]	;project phase onto IFs
		position -= shft									;update position
		print, 'step number '+strtrim(ii,2)
	;	tvscl, ph
	;	wait,0.5
	endfor

	command_hist = float(transpose(command_hist))


	; Save disturbance command history fits file:
	;*****************************************************************

	; Create header:
	if disturb_type eq 'atm' then mkhdr, hdr, command_hist, /EXTEND
	if disturb_type ne 'vib' then begin
		sxaddpar, hdr, 'type', disturb_type, 'Type of disturbance (atm, vib, atm+vib, etc. etc.)'
		sxaddpar, hdr, 'seeing', seeing, '[arcsec]'
		sxaddpar, hdr, 'r0', r_0, '@ 500nm [m]'
		sxaddpar, hdr, 'L0', L0, '[m]'
		sxaddpar, hdr, 'vwind', v_wind, '[m/s]'
		sxaddpar, hdr, 'angle_w', angle_wind, 'Direction of phase screen translation [degrees]'
		sxaddpar, hdr, 'ph_fname', ph_file, 'Name of phase screen used'
		sxaddpar, hdr, 'Reflcoef', Refl_coeff, 'Reflection Coefficient'
		if keyword_set(nmodes_cor) then begin
			sxaddpar, hdr, 'nmodes_cor', nmodes_cor, 'Number of pre-corrected modes'
			sxaddpar, hdr, 'first_mode_cor',first_mode_cor , 'Index to first pre-corrected mode'
			sxaddpar, hdr, 'm2c_cor', m2c_cor_fname, 'M2C used to remove the pre-corrected modes'
		endif
		if keyword_set(boost_tt) then $
			sxaddpar, hdr, 'boost_tt', boost_tt, 'TT boosting factor'
	endif
	sxaddpar, hdr, 'overfreq', hz, 'AdSec oversampling frequency'

	; Name of fits file
	command_hist_fname = $
				'_s'+strtrim(string(seeing,format='(f3.1)'),2)
	if finite(L0) then command_hist_fname +=						  $
				'_L0'+strtrim(string(L0,format='(f5.1)'),2)
	command_hist_fname += 											  $
				'_v'+strtrim(string(v_wind,format='(f4.1)'),2)		+ $
				'_ovfreq'+strtrim(string(hz,format='(f7.2)'),2)		+ $
;				'_np'+strtrim(n_steps,2)
				'_sd'+strtrim(seed,2)
	if keyword_set(nmodes_cor) then command_hist_fname += $
 				'_cor'+strtrim(fix(nmodes_cor),2)
 	if keyword_set(boost_tt) then command_hist_fname += $
 				'_btt'+strtrim(fix(boost_tt),2)
endif

; Write the file!
if disturb_type eq 'atm' then $
	writefits, filepath(rname+command_hist_fname+'.fits', root=disturb_dir), command_hist, hdr
if disturb_type eq 'atm+vib' then $
	writefits, filepath(rname+command_hist_fname+command_hist_vib_fname+'.fits', root=disturb_dir), $
           command_hist+command_hist_vib, hdr
if disturb_type eq 'vib' then $
	writefits, filepath(rname+command_hist_vib_fname+'.fits', root=disturb_dir), command_hist_vib, hdr


end
