
pro autogain_analyze_offline, GAINV=GAINV, GAINTT = GAINTT, GAINHO1 = GAINHO1, GAINHO2 = GAINHO2, $
                       TARGET = TARGET, MIDDLE_HO = MIDDLE_HO, QUIET = QUIET, CUBIC=CUBIC, $
                       OUTFILE=OUTFILE, ERRMSG=ERRMSG, TITLE=TITLE, $
                       JOIN_STEPS=JOIN_STEPS, $
                       GAINSFILE_PATTERN=GAINSFILE_PATTERN, SLOPESFILE_PATTERN=SLOPESFILE_PATTERN, $
                       MODESIFILE_PATTERN=MODESIFILE_PATTERN, RECOMPUTE=RECOMPUTE, $
                       CLEANWINDOWS=CLEANWINDOWS, $
                       THRESHOLD_TT=THRESHOLD_TT, THRESHOLD_HO = THRESHOLD_HO, $
                       RECPATH=RECPATH, SINGLEWINDOW=SINGLEWINDOW

MAKE_PLOTS = 0b    ; AP 20200705 remove on-screen plots in IDL windows, now shown in the FLAOGUI.

if not keyword_set(CUBIC) then CUBIC = 0
if not keyword_set(TITLE) then TITLE=TARGET

if not keyword_set(THRESHOLD_TT) then THRESHOLD_TT=0.85
if not keyword_set(THRESHOLD_HO) then THRESHOLD_HO=0.85

LASTIMAGE_FILENAME='/tmp/lastimage.bmp'  ;; Must match the one in PyramidFlaoI.cpp
ERRMSG=''

;1) caricare il ricostruttore in uso
;2) misurare le slopes ai vari gain
;3) calcolare la decomposizione modale residua dalle slopes
;4) calcolarsi il yilt rms dai coefficiento di tip e tilt  sqrt(t12+T22)
;5) calcolarsi o rm dei modi corretti residui sqrt(sum_1_n mode_i2) 

rec = readfits(RECPATH, hhh)
n_modes = long(aoget_fits_keyword(hhh, 'IM_MODES'))

joinsteps = strsplit(JOIN_STEPS, /extract)

; Slopes for each step are read and used to reconstruct the modes.
; Reconstructed modes are cached, so that they do not need to be recalculated
; every time the autogain is exploring a wider gain range.
;
; Modes are then inserted into a single modes array that contains
; all reconstructed modes for all explored gain points.

for idx=0,n_elements(joinsteps)-1 do begin

   step = joinsteps[idx]
   gains_filename  = oaa_str_replace(GAINSFILE_PATTERN, '%d', step)
   slopes_filename = oaa_str_replace(SLOPESFILE_PATTERN, '%d', step)
   modesi_filename = oaa_str_replace(MODESIFILE_PATTERN, '%d', step)

   this_gains = readfits( gains_filename, hdr)
   this_steps = fix(aoget_fits_keyword(hdr, 'STEPS_RAMP'))

   ; Read FITS cached file, or: read slopes, reconstruct modes, and save FITS file.

   if FILE_TEST(modesi_filename) and (not keyword_set(RECOMPUTE)) then begin

       this_modesi = readfits(modesi_filename)

   endif else begin

       this_gains = readfits( gains_filename, hdr)
       this_slopes= readfits( slopes_filename)

       n_frames = n_elements(this_slopes[0,*,0])
       n_steps  = n_elements(this_slopes[0,0,*])
       this_modesi = fltarr(n_elements(rec[0,*]), n_frames, n_steps)

       for i=0,n_steps-1 do this_modesi[*,*,i] = transpose(rec ## transpose(reform(this_slopes[*,*,i])))

       writefits, modesi_filename, this_modesi

   endelse

   ; Build the single modes array, inserting each partial acquisition where it belongs

   if idx eq 0 then begin
       gains = this_gains
       modesi = this_modesi
       steps = this_steps
       cycles     = aoget_fits_keyword(hdr, 'CYCLES')
       modalbasis = aoget_fits_keyword(hdr, 'M2C')
   endif else begin
       old_steps = steps
       steps += this_steps

       ngains = n_elements(gains[*,0])

       old_gains = gains
       old_modesi = modesi
       gains = fltarr( ngains, cycles*steps)
       modesi= fltarr( n_elements(modesi[*,0,0]), n_elements(modesi[0,*,0]), cycles*steps)
 
       for i=0,cycles-1 do begin

           gains[*, i*steps : i*steps + old_steps-1]   = old_gains[*, i*old_steps : (i+1)*old_steps-1]
           gains[*, i*steps+old_steps : (i+1)*steps-1] = this_gains[*, i*this_steps : (i+1)*this_steps-1]

           modesi[*,*, i*steps : i*steps + old_steps-1]   = old_modesi[*,*, i*old_steps : (i+1)*old_steps-1]
           modesi[*,* ,i*steps+old_steps : (i+1)*steps-1] = this_modesi[*,*, i*this_steps : (i+1)*this_steps-1]

       endfor      

   endelse

   help, this_steps, steps

endfor

help,gains,modesi

steps_ramp = steps

idx = where(modesi[*,0,0] ne 0)
n_frames = n_elements(modesi[0,*,0])
n_steps  = n_elements(modesi[0,0,*])
modes = modesi[0:n_modes-1, *,*]

ratio = gains[1,0] / gains[0,0]
; Detect if tt or ho were used
mmtt = minmax(gains[0,*])
mmho1 = minmax(gains[1,*])
mmho2 = minmax(gains[2,*])

nn = max(idx)
ttrms = fltarr(n_frames, n_steps)
horms1 = fltarr(n_frames, n_steps)
horms2 = fltarr(n_frames, n_steps)

help,n_frames, n_steps

; Calcola rms dei modi residui (tip-tilt, modi medi, modi alti)

if MIDDLE_HO gt n_modes then MIDDLE_HO = n_modes

for i=0,n_steps-1 do for j=0,n_frames-1 do ttrms[j,i] = sqrt(total(modes[0:1,j,i]^2))
for i=0,n_steps-1 do for j=0,n_frames-1 do horms1[j,i] = sqrt( total(modes[2:MIDDLE_HO-1,j,i]^2))
if MIDDLE_HO lt n_modes then begin
    for i=0,n_steps-1 do for j=0,n_frames-1 do horms2[j,i] = sqrt( total(modes[MIDDLE_HO:*,j,i]^2))
endif
    

; Seleziona quale usare come funzione di merito
if (TARGET eq 'tt') then use_rms = ttrms	
if (TARGET eq 'ho1') then use_rms = horms1
if (TARGET eq 'ho2') then use_rms = horms2

if (TARGET eq 'tt') then usegain = gains[0,*]	
if (TARGET eq 'ho1') then usegain = gains[1,*]
if (TARGET eq 'ho2') then usegain = gains[2,*]

help,use_rms

rmst = total(use_rms,1)/n_frames
x = usegain

help,rmst

;LB 120916 la mediana e' piu' robusta della media
rms_avg_v = fltarr(cycles, steps_ramp)
help,rms_avg_v

for cycle=0,cycles-1 do begin
    for step=0,steps_ramp-1 do begin
        rms_avg_v[cycle,step] = rmst[ cycle*steps_ramp + step]
    endfor
endfor
rms_avg = median(rms_avg_v, dim=1)
;LB end

xmin = min(x)
xmax = max(x)
npoints=100
xc = findgen(npoints)/npoints * (xmax-xmin)+xmin
rms_avg_extended=interpol(rms_avg, npoints)
curve = smooth(rms_avg_extended, 5, /edge_truncate)

mm = where(curve eq min(curve), count)
nn = n_elements(curve)
mmtt=mm
mmho=mm
; Force iteration of gain sweep if the minimum is not deep enough
if (target eq 'tt') and (curve[mmtt] gt curve[nn-1]*THRESHOLD_TT) then begin

   ERRMSG = 'TT minimum was not deep enough, forcing iteration (threshold is '+strtrim(THRESHOLD_TT,2)+')'
   print
   print, ERRMSG
   print
   mmtt = nn-1
endif
if ((target eq 'ho1') or (target eq 'ho2')) and (curve[mmho] gt curve[nn-1]*THRESHOLD_HO) then begin
   ERRMSG = 'HO minimum was not deep enough, forcing iteration (threshold is '+strtrim(THRESHOLD_HO,2)+')'
   print
   print, ERRMSG
   print
   mmho = nn-1
endif

; Recalc gain array
gg = fltarr(n_elements(gains[*,0]), npoints)
for i=0, n_elements(gains[*,0])-1 do begin
    gmin = min(gains[i,*])
    gmax = max(gains[i,*])
    gg[i,*] = findgen(npoints)/npoints * (gmax-gmin)+gmin
endfor

plot_cubic=0
if keyword_set(CUBIC) then begin

   print,'Trying cubic interpolation...'

   res = poly_fit( xc, curve, 3, yfit=yfit)
   test = where(yfit eq min(yfit), count)
   help,count
   if count eq 1 then begin
      mmtt=test
      plot_cubic = 1 ; Plotting flag for later
      help,mmtt
      print,'curve[mmtt]=',curve[mmtt], '   curve[nn-1]=',curve[nn-1]
      if curve[mmtt] gt curve[nn-1]*THRESHOLD_TT then begin
          ERRMSG = 'TT minimum (CUBIC interpolation) was not deep enough, forcing iteration (threshold is '+strtrim(THRESHOLD_TT,2)+')'
          print
          print, ERRMSG
          print
          mmtt = nn-1
      endif
   endif else begin
      print, 'Cubic fit failed'
   endelse

endif

gaintt = gg[0,mmtt]
gainho1 = gg[1,mmho]
gainho2 = gg[2,mmho]

help,gaintt, gainho1, gainho2

if not keyword_set(QUIET) then print, 'Minimum (TT) found at step: ', mmtt, ' rms: ',rmst
if not keyword_set(QUIET) then print, 'Minimum (HO) found at step: ', mmho, ' rms: ',rmst
if not keyword_set(QUIET) then print, 'gains: ', gaintt, gainho1, gainho2

; On-screen plot

if keyword_set(MAKE_PLOTS) then begin

    ;; Catch plot errors in case X display is not set
    catch, error_plot

    if n_elements(SINGLEWINDOW) gt 0 then begin
       WNUM=SINGLEWINDOW
    endif else begin
       WNUM=0
       if TARGET eq 'tt' then WNUM=1
       if TARGET eq 'ho1' then WNUM=2
       if TARGET eq 'ho2' then WNUM=3
    endelse

    if not keyword_set(QUIET) then begin
        if error_plot eq 0 then begin
           if keyword_set(CLEANWINDOWS) then begin
               while (!D.WINDOW ne -1) do wdelete, !D.WINDOW
           endif
           set_plot, 'X'
           window,WNUM,retain=2, XSIZE=800, YSIZE=600, TITLE=TITLE
        endif else begin
           catch, /cancel
        endelse
    endif

endif

range = max(rms_avg)
if range lt 5e-8 then range=5e-8

rangefactor = 1.0/range
commands  = 'plot,x, rmst,psym=4,/ystyle,yrange=[0,range], title=title, xtitle="Gain", charsize=3, charthick=1.5, xmargin=[2,3], ycharsize=0.1 &'
commands += 'oplot, x, rms_avg, color=255L, thick=2 &'
commands += 'oplot, xc, curve, color=255L*256, thick=2'
vars = ['x', 'rmst', 'xc', 'rms_avg', 'curve', 'range', 'title']
if plot_cubic ne 0 then begin
    commands += ' & oplot, xc, yfit, color=255L*256*256, thick=2'
    vars += ['yfit']
endif

; This routine writes the plots into a hidden frame buffer
; and saves them on disk. The /NOGRAPHICS keyword prevents
; it from plotting on the screen.

display_and_save, USEWINDOW=WNUM, XSIZE=800, YSIZE=800, $
                  commands = commands, $
                  filename = LASTIMAGE_FILENAME, /BMP, $
                  vars = vars, NOGRAPHICS=~MAKE_PLOTS

if keyword_set(OUTFILE) then begin
   FILE_COPY, LASTIMAGE_FILENAME, OUTFILE
endif

end

