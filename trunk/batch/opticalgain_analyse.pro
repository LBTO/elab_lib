

pro opticalgain_analyse, set
;set = obj_new('aodataset', from='20110614_064100', to='20110614_065417')

sin_mode = set->value('disturb.sin_mode')
if n_elements(rem_dup(sin_mode)) gt 1 then message, 'different modes applied!'

sin_freq = set->value('disturb.sin_freq')
if n_elements(rem_dup(sin_freq)) gt 1 then message, 'different frequencies applied!'

amp_in_r = set->value('disturb.mode_amp')
df = (set->value('modaldisturb.freq'))[0,*]
if n_elements(rem_dup(df)) gt 1 then message, 'different sampling frequencies applied!'

freq_range = [sin_freq[0]-df[0]/2., sin_freq[0]+df[0]/2.]

pow_in = set->value('modaldisturb.power('+strtrim(sin_mode[0],2)+',from_freq='+string(freq_range[0],format='(f7.3)') + $
																 ',to_freq  ='+string(freq_range[1],format='(f7.3)') +')')
amp_in = sqrt(pow_in)*sqrt(2.)

pow_out = set->value('residual_modes.power('+strtrim(sin_mode[0],2)+',from_freq='+string(freq_range[0],format='(f7.3)') + $
																 ',to_freq  ='+string(freq_range[1],format='(f7.3)') +')')
amp_out = sqrt(pow_out)*sqrt(2.)

pow_spe = set->value('modalpositions.power('+strtrim(sin_mode[0],2)+',from_freq='+string(freq_range[0],format='(f7.3)') + $
																 ',to_freq  ='+string(freq_range[1],format='(f7.3)') +')')
amp_spe = sqrt(pow_spe)*sqrt(2.)


olseeing = set->value('olmodes.seeing')
;dimmseeing = set->value('tel.dimm_seeing')

!X.MARGIN=[7,3]
window,0
plot, olseeing, amp_out*1e9, psym=1, xtitle='seeing from AO data', ytitle='amp out [nm surf]',charsize=1.5
;window,1
;plot, dimmseeing, amp_out*1e9, psym=1, xtitle='seeing from DIMM', ytitle='amp out [nm surf]',charsize=1.5
window,2
plot, olseeing, amp_spe*1e9, psym=1, xtitle='seeing from AO data', ytitle='amp mir [nm surf]',charsize=1.5
window,3
plot, olseeing, amp_out/amp_spe, psym=1, xtitle='seeing from AO data', ytitle='amp_res / amp_amp',charsize=1.5


end


;
;
;;============================
;; generate additional disturbance files:
;0,30,100,300
;
;mode = 0
;amp = 2e-9
;freq = 100.
;loopfreq = 990.
;modalbasis='KL_v7'
;track = sinusmode_generate(mode, amp, freq, loopfreq, modalbasis, /EVEN_PERIODS, EFF=efffreq)
