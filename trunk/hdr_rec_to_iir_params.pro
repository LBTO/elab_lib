;+
;   This procedure takes as input:
;       hdr_rec                 header of a_delay (temporal filtering) or b0_a (reconstruction) matrix
;   and returns:
;       orig_rec_fname          original reconstruction matrix filename
;       num_mat                 IIR filter numerator coefficients matrix
;       den_mat                 IIR filter denominator coefficients matrix
;       modes_idx               index of modes with IIR filter
;       ff_no_excluded_modes    integrator modes without leaky (forgetting factor = 1)
;       ff_min_value            minimum value of forgetting factor
;       ff_exp                  exponential used to compute forgetting factor vector
;       nmodes_filter           total number of mode with filtering
;
;   first version: 2019/10/10
;   author: Guido Agapito guido.agapito@inaf.it
;
;-

pro hdr_rec_to_iir_params, hdr_rec, orig_rec_fname, num_mat, den_mat, modes_idx, ff_no_excluded_modes, ff_min_value, ff_exp, nmodes_filter

if aoget_fits_keyword(hdr_rec, 'ORIG_REC') ne '' then orig_rec_fname = aoget_fits_keyword(hdr_rec, 'ORIG_REC')
snum0 = long(aoget_fits_keyword(hdr_rec, 'IIRNELE0'))
snum1 = long(aoget_fits_keyword(hdr_rec, 'IIRNELE1'))
MODEIDXN = long(aoget_fits_keyword(hdr_rec, 'MODEIDXN'))
ff_no_excluded_modes = long(aoget_fits_keyword(hdr_rec, 'FF_NOEXMO'))
ff_min_value = float(aoget_fits_keyword(hdr_rec, 'FF_MINVA'))
ff_exp = float(aoget_fits_keyword(hdr_rec, 'FF_EXP'))
num_mat = fltarr(snum0,snum1)
den_mat = fltarr(snum0,snum1)
modes_idx = lonarr(MODEIDXN)
for i=0,snum0-1L do for j=0,snum1-1L do num_mat[i,j] = float(aoget_fits_keyword(hdr_rec, 'IIRNUM'+strtrim(i,2)+strtrim(j,2)))
for i=0,snum0-1L do for j=0,snum1-1L do den_mat[i,j] = float(aoget_fits_keyword(hdr_rec, 'IIRDEN'+strtrim(i,2)+strtrim(j,2)))
for i=0,MODEIDXN-1L do modes_idx[i] = long(aoget_fits_keyword(hdr_rec, 'MODEIDX'+strtrim(i,2)))

nstates_to_be_removed = 2*MODEIDXN*(snum0-1L)
nmodes_int = 672 - nstates_to_be_removed
nmodes_filter = nmodes_int + MODEIDXN

end
