;; gen_reconstructor
;
; This procedure compute a reconstruction matrix using
; a int. matrix measurement.
;
; parameters:        
;
;                       INPUT_FILE      FITS file with the interaction matrix
;
;                       OUTPUT_FILE     if set, saves the reconstructor matrix
;
;                       CUT             cut threshold for rec. matrix eigenvalues.
;
;                       FINAL_DIM      intarr(2). If specified, inscribe the resulting modal rec
;                                      into a bigger matrix with those dimensions
;
;                       PATH_PREFIX   prefix for all filenames
;
;-


pro gen_reconstructor, PATH_PREFIX = PATH_PREFIX, CUT=CUT, INPUT_FILE= INPUT_FILE, OUTPUT_FILE = OUTPUT_FILE, FINAL_DIM = FINAL_DIM

if not keyword_set(INPUT_FILE) then begin
	print,"Error: please specify input file"
	exit
endif


; Read the interaction matrix and check the type

if keyword_set(PATH_PREFIX) then INPUT_FILE = PATH_PREFIX + path_sep() + INPUT_FILE
int_mat = readfits( INPUT_FILE, intmat_hdr)

print,'***************************'
print,'Reading interaction matrix'
help, int_mat
print,'***************************'

; Now start the calculations

print,"Starting SVDC..."
svdc, int_mat, w, u, v, /DOUBLE
print,"Done"

; Ask how many modes to reject

window,1, RETAIN=2
plot_io,w[where(w gt 1e-6)],psym=-4, charsize=3
zeros = where(w lt 1e-6, count)
if count gt 0 then begin
	print, '---------------------'
	print, 'WARNING: ',count,' eigenvalues near zero:'
	print, w[zeros]
	print, '---------------------'
endif

count=-1
print, w
if n_elements(CUT) eq 0 then begin
    CUT = 0.0
	read,CUT, prompt="Enter cut threshold:"
endif

nmodes = n_elements(w)
winv = fltarr(nmodes,nmodes)

;if count gt 0 then begin
	w_bad = where(w LT CUT, bad)
	for i=0, nmodes-1 do if w(i) GT CUT then winv(i,i) = 1./w(i)
	print, 'rejected', bad,' modes (w < ',CUT,')'

rec = v ## winv ## transpose(u)


; Expand rec to final dimension
if keyword_set(FINAL_DIM) then begin
    temp_rec = fltarr( FINAL_DIM[0], FINAL_DIM[1])
    temp_rec[0,0] = rec
    rec = temp_rec
endif

; Save result

if keyword_set(OUTPUT_FILE) then begin

;            12345678 
    hdr = [ 'CT_MODES', strtrim(bad, 2)]

    data = float(REC)

    if keyword_set(PATH_PREFIX) then OUTPUT_FILE = PATH_PREFIX + path_sep() + OUTPUT_FILE
    writefits, OUTPUT_FILE, data, make_fits_header( data, HDR=hdr, FILETYPE='MODALREC', APPEND_HDR = intmat_hdr)
endif

end






