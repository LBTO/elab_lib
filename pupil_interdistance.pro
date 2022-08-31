
; ---------------------------
; ---------------------------
; ---------------------------

;*****************************************
; ANALIZZA
; immagine  = immagine in ingresso da dare, array 2d
; results   = array con i centri e i raggi delle 4 pup
; SPLIT     = se è settata divide l'immagine nelle 4 pupille
; SOGL      = soglia in percentuale al picco-valle dell'intera
;             immagine, serve a togliere il background
; S2_P2V    = seconda soglia in frazione di max-min. Se non
;             settato e' una mean dei valori sopra la prima soglia
; OFFX,OFFY = offset the center point used for image split
;
;***************************************
pro analizza_temp, immagine, results, SOGL = SOGL, SPLIT = SPLIT, S2_P2V=S2_P2V, QUIET=QUIET, DOPLOT=DOPLOT, DRAWTO = DRAWTO, $
                                 DISPFACTOR = DISPFACTOR, NOSAVE = NOSAVE, PLOT_TO = PLOT_TO, OFFX=OFFX, OFFY=OFFY, $
                                 MINIM=MINIM, MAXIM=MAXIM,ron = ron

if not keyword_set(DISPFACTOR) then dispfactor = 1

start = systime(1)

dim_imm=size(immagine)

;immagine[*,0] = 0
;immagine[*,dim_imm[2]-1] =0
;immagine[0,*] = 0
;immagine[dim_imm[1]-1,*] =0

;;;zero the middle two lines in x and y directtions (XZ, LB 20171206)
;immagine[*,dim_imm[2]/2-1:dim_imm[2]/2] =0
;immagine[dim_imm[1]/2-1:dim_imm[1]/2,*] =0



if keyword_set(SPLIT) then begin
    if not keyword_set(QUIET) then print,'SPLIT attivo'

    cx=fix(dim_imm[1]/2)
    cy=fix(dim_imm[2]/2)
    offset=0
    dimx = cx
    dimy = cy

    ;creo array a tre dimensioni, dove la prima mi indica l'imm. delle quattro selezionata

    cx += OFFX
    cy += OFFY
    dimx -= abs(OFFX)
    dimy -= abs(OFFY)

    k = fltarr(4,dimx,dimy)

    k[0,*,*] = immagine[cx-dimx:cx-1, cy:cy+dimy-1]
    k[1,*,*] = immagine[cx:cx+dimx-1,   cy:cy+dimy-1]
    k[2,*,*] = immagine[cx-dimx:cx-1, cy-dimy:cy-1]
    k[3,*,*] = immagine[cx:cx+dimx-1,   cy-dimy:cy-1]

    if file_test('/tmp/',/dir) then writefits,'/tmp/pup1.fits',k[0,*,*]
    if file_test('/tmp/',/dir) then writefits,'/tmp/pup2.fits',k[1,*,*]
    if file_test('/tmp/',/dir) then writefits,'/tmp/pup3.fits',k[2,*,*]
    if file_test('/tmp/',/dir) then writefits,'/tmp/pup4.fits',k[3,*,*]

    ;creo array a due dimensioni [la prima mi indica l'imm fra le quattro, la seconda le variabili (r,centro[0],centro[1])]
    results = fltarr(4,3)

endif else begin

    k = fltarr(1,dim_imm[1],dim_imm[2])
    k[0,*,*] = immagine
    results = fltarr(1,3)

endelse

if keyword_set(SPLIT) then n_pup = 4 else n_pup=1

;introduco un ciclo for perchè per ciascuna delle 4 imm devo attuare la procedura per ricavare centro e raggio


soglie = fltarr(n_pup, 2)

if not keyword_set(QUIET) then window,0, xsize=dim_imm[1]*DISPFACTOR,ysize=dim_imm[2]*DISPFACTOR
if not keyword_set(QUIET) then window,1, xsize=dim_imm[1]*DISPFACTOR,ysize=dim_imm[2]*DISPFACTOR

for pup=0,n_pup-1 do begin

    ;per estrarre una imm a due dim da un array a tre dim (si chiama 'cubo')
    imm=reform(k[pup,*,*])

    ;così mi divide la procedura per ciascuna immagine
    if not keyword_set(QUIET) then print,'Analizzo pupilla ',pup

    imm_orig = imm

    ;calcolo la soglia per ciascuna delle quattro pupille

    ; PRIMA SOGLIA
    if not keyword_set(SOGL) then begin
       ;s1 = MIN(imm) + (MAX(imm)-MIN(imm))*.05
       if n_elements(MINIM) gt 0 and n_elements(MAXIM) gt 0 then $
           s1 = MINIM + (MAXIM-MINIM)*.05 else $
               s1 = MIN(k) + (MAX(k)-MIN(k))*.05

    endif else begin
       ;s1 = MIN(imm) + (MAX(imm)-MIN(imm))* SOGL
       if n_elements(MINIM) gt 0 and n_elements(MAXIM) gt 0 then $
           s1 = MINIM + (MAXIM-MINIM)*SOGL else $
               s1 = MIN(k) + (MAX(k)-MIN(k))* SOGL
    endelse

    imm_soglia = imm
    imm_soglia[ where( imm_soglia lt s1) ]=0

    ; SECONDA SOGLIA
    ;if keyword_set(S2_P2V) then s2=(MAX(imm_soglia)-MIN(imm_soglia))*S2_P2V else s2= mean(imm(where(imm gt s1)))
    if keyword_set(S2_P2V) then s2=mean(imm_soglia)*S2_P2V else s2= mean(imm(where(imm gt s1)))
    if not keyword_set(QUIET) then print,'Soglia: ',s2

    soglie[pup, 0] = s1
    soglie[pup, 1] = s2

    imm_soglia[ where( imm_soglia lt s2) ]=0

    ;inizia il ciclo per ciascuna imm
    repeat begin

       centro = calcola_baricentro(imm_soglia)
       r  = calcola_raggio(imm_soglia,s2)
;res = disc_estimation(imm,ron = ron)
;centro = res[0:1]
;r = res[2]
    sottosoglia, s2, imm_soglia, r, centro, pixel, cerchio, cerchio_count

       if keyword_set(DRAWTO) then wset, DRAWTO
       if (not keyword_set(QUIET)) or (keyword_set(DRAWTO)) then begin


            m = max(imm_orig)
            i = imm_orig
            ;if cerchio_count gt 0 then i[cerchio] = m
            wset,0
            tvscl, rebin(i+imm_soglia*max(i)/(max(i)/10.0), n_elements(i[*,0])*dispfactor, n_elements(i[0,*])*dispfactor, /SAMPLE),pup
            wset,1
            tvscl, rebin(i, n_elements(i[*,0])*dispfactor, n_elements(i[0,*])*dispfactor, /SAMPLE),pup
        endif

       ;

       ; Correggo l'origine per le varie pupille solo se ho splittato in 4

       if keyword_set(SPLIT) then begin
         if pup eq 0 then centro = centro + [cx-dimx,cy]
         if pup eq 1 then centro = centro + [cx,cy]
         if pup eq 2 then centro = centro + [cx-dimx,cy-dimy]
         if pup eq 3 then centro = centro + [cx,cy-dimy]

       endif


       ;per salvare i dati per ciascun ciclo iterativo


    ;; AGGIUSTINO2

       ;results[pup,*]=[r,centro[0], centro[1]]
       results[pup,*]=[r,centro[0] +0.5 ,centro[1] +0.5]

       if not keyword_set(QUIET) then begin
         ;print, '______________________'
            print, 'Pupilla', pup, '        Diametro =', 2.*r ,'       Coord centro =',centro
        endif

    endrep until (pixel eq 0)


    if not keyword_set(NOSAVE) then save, results ,filename='risultati'+ string(long(systime(1))) +'.sav'
    if not keyword_set(QUIET) then print ,'ho finito'

endfor


if keyword_set(DOPLOT) then begin
    if keyword_set(PLOT_TO) then wset, PLOT_TO else window,3, xs=1200, ys=800
    if keyword_set(SPLIT) then begin
        !p.multi = [0,2,2]
    endif
    for pup=0,n_pup-1 do begin

       ; Correggo l'origine per le varie pupille solo se ho splittato in 4
       sezione_plot = results[pup,2]
       centro_plot  = results[pup,1]
       if keyword_set(SPLIT) then begin
         if pup eq 0 then begin
          centro_plot = centro_plot - cx + dimx
          sezione_plot = sezione_plot - cy
         endif
         if pup eq 1 then begin
           centro_plot = centro_plot - cx 
           sezione_plot = sezione_plot - cy
         endif
         if pup eq 2 then begin
           centro_plot = centro_plot - cx + dimx
           sezione_plot = sezione_plot - cy + dimy
         endif
         if pup eq 3 then begin
           centro_plot = centro_plot - cx
           sezione_plot = sezione_plot - cy + dimy
         endif      
       endif

        plot, k[pup, *,round(sezione_plot)]
        oplot, [centro_plot-r,centro_plot-r],[0,MAX(immagine)], color=255
        oplot, [centro_plot+r,centro_plot+r],[0,MAX(immagine)], color=255

       oplot, [-1,1e6], [soglie[pup,0], soglie[pup,0]], color = 255l
       oplot, [-1,1e6], [soglie[pup,1], soglie[pup,1]], color = 255l*256l
       
       if n_elements(MINIM) then minPlot = MINIM else minPlot = MIN(k)
       if n_elements(MAXIM) then maxPlot = MAXIM else maxPlot = MAX(k)

       oplot, [-1,1e6], [MINIM, MINIM], linest=2
       oplot, [-1,1e6], [MAXIM, MAXIM], linest=2
    endfor
    if keyword_set(SPLIT) then begin
        !p.multi = 0
    endif
 endif

fine = systime(1)

end

; ---------------------------
; ---------------------------
; ---------------------------

pro distanza_centri_temp, nome, IM_TYPE=IM_TYPE, m, w, diagonale_lato, media_d, QUIET = QUIET, OUTFILE = OUTFILE, SPLIT = SPLIT, $
                           TH1 = TH1, TH2 = TH2, DISPFACTOR = DISPFACTOR, OFFX=OFFX, OFFY=OFFY, DOPLOT = DOPLOT, MINIM=MINIM, $
                           MAXIM=MAXIM,ron = ron

;******************************************
; nome      = stringa con il nome del file
;          o array con l'immagine
; IM_TYPE   = TIFF, FITS   tipo di file
;   se non selezionata e' atteso un array
; m        = array con i centri e i raggi
;          delle 4 pup
; w         = misura dei lati del quadrato
;******************************************


;Caricamento immagine
if keyword_set(IM_TYPE) then begin
    if IM_TYPE EQ 'TIFF' then a=read_tiff(nome)
    if IM_TYPE EQ 'FITS' then a=readfits(nome)
endif else a=nome

; SE IL FRAME HA DIMENSIONI DISPARI LE AGGIUSTA IN PARI
new_size=intarr(2)
dimensioni=size(a)
for i=0,1 do begin
	if dimensioni[i+1]/2. GT dimensioni[i+1]/2 then begin
		new_size[i]=dimensioni[i+1]+1
		if not keyword_set(quiet) then print, 'dimensione',i+1,' variata:', dimensioni[i+1], '->',new_size[i]
	endif else new_size[i]=dimensioni[i+1]
endfor
dummy_a=fltarr(new_size[0],new_size[1])
dummy_a[0:dimensioni[1]-1,0:dimensioni[2]-1]=a
a=dummy_a


analizza_temp, a , m, SOGL = TH1, S2_P2V = TH2, SPLIT = SPLIT, QUIET = QUIET, /NOSAVE, DISPFACTOR = DISPFACTOR, $
  OFFX=OFFX, OFFY=OFFY, DOPLOT = DOPLOT, MINIM=MINIM, MAXIM=MAXIM,ron = ron

w=replicate(1.,4)
media_d=fltarr(1.,4)

if keyword_set(SPLIT) then begin

for i=0,3 do begin
    ;il mod serve per bloccarmi il ciclo, lavora con il resto della divisione
    ;ad es. 3+1=4 -->4/4 ha resto zero , quindi mi porta l'indice j=0

    j=(i+1) mod 4

    d_a=sqrt((m(j,1)-m(i,1))^2  + (m(j,2)-m(i,2))^2)


    if i EQ 1 OR i EQ 3 then begin

       if not keyword_set(QUIET) then print,'diag',i,'->',j, '(pix) =', d_a

    endif else begin

       if not keyword_set(QUIET) then print, 'lato ',i,'->',j ,'(pix) =', d_a

       w[i]=d_a
    endelse

endfor

for h=0,1 do begin

    k=(h+2) mod 4

    d_b=sqrt((m(k,1)-m(h,1))^2  + (m(k,2)-m(h,2))^2)

    if not keyword_set(QUIET) then print, 'lato ',h,'->',k ,'(pix) =', d_b

    w[h*2+1]=d_b

endfor
for pup=0,3 do begin

if not keyword_set(QUIET) then print, 'Pupilla n.',pup+1,'   diam=', m(pup,0)*2,'pix'

media_d[pup]=m(pup,0)*2

endfor

media_diam=mean(media_d)

if not keyword_set(QUIET) then print, 'diametro medio', media_diam

media=mean(w)

if not keyword_set(QUIET) then print, 'media_lato =' , media

diagonale_lato=sqrt(2)*media

if not keyword_set(QUIET) then print, 'diagonale =' , diagonale_lato

rapporto=media_diam/media

if not keyword_set(QUIET) then print, 'rapporto =', rapporto

endif else begin  ; keyword SPLIT

media_d[0]=m(0,0)*2

endelse

; Text output in this format:
;
; d1, x1, y1, l1
; d2, x2, y2, l2
; d3, x3, y3, l3
; d4, x4, y4, l4

if not keyword_set(quiet) then print, media_d
if not keyword_set(quiet) then print, m

n_pup = 3
if not keyword_set(SPLIT) then n_pup =0
if keyword_set(OUTFILE) then begin
    openw, unit, OUTFILE, /GET_LUN
    for pup=0,n_pup do printf, unit, media_d[pup], m[pup,1], m[pup,2], w[pup]
    close, unit
    free_lun, unit
endif

end

; ---------------------------
; ---------------------------
; ---------------------------

function pupil_interdistance, trck, verbose=verbose, display=display, OFFX=OFFX, OFFY=OFFY, $
  OFF_AUTO = OFF_AUTO, TH1 = TH1, TH2 = TH2

if n_elements(OFFX) eq 0 then OFFX = 0
if n_elements(OFFY) eq 0 then OFFY = 0
if n_elements(TH1) eq 0 then TH1 = 0.15
if n_elements(TH2) eq 0 then TH2 = 0.15

data_obj = getaoelab(trck)
modeNo = ((data_obj->wfs_status())->camera())->mode()
if keyword_set(verbose) then print, 'Tracking Number',trck

fr = (data_obj->frames())->frames(/dark_subtracted)
fr_ave = total(fr,3)/(size(fr))[2]
if modeNo eq 1 then fr_ave = fr_ave[60:179,60:179]
if modeNo eq 2 then fr_ave = fr_ave[60:179,*]
if modeNo eq 3 then fr_ave = fr_ave[30:89,30:89]
if modeNo eq 4 then fr_ave = fr_ave[15:44,15:44]

sFr_ave = size(fr_ave,/dim)

if keyword_set(OFF_AUTO) then begin
  x = lindgen(sFr_ave[0],sFr_ave[1]) mod sFr_ave[0] - sFr_ave[0]/2
  y = lindgen(sFr_ave[0],sFr_ave[1])/sFr_ave[0] - sFr_ave[1]/2
  OFFX = round(total(x*fr_ave)/total(fr_ave))
  OFFY = round(total(y*fr_ave)/total(fr_ave))
endif

dispfactor = 8

m = [[0,0],[0,0],[0,0],[0,0]]
side = sFr_ave[0]/2.
w = replicate(side,4)

max_fr_ave = max(fr_ave)

bin = round(max_fr_ave/1e3)
plothist, fr_ave, xhist, yhist, /noplot, bin=bin
yhistSmooth = smooth(yhist,10)

; ------- remove pixels dominated by ron ------- ;

fr_ave_noron = fr_ave
fr_ave_cut = fr_ave
; --- search peak in the histogram --- ;
; first guess of the maximum is at 0.8 of the histogram
; that is ~(120x120 - 40x40x!pix2) / 120x120
fr_ave_sorted = fr_ave[sort(fr_ave)]
maxHistGuess = fr_ave_sorted(round(n_elements(fr_ave_sorted) * 0.82))

rangeSearchMax = maxHistGuess*[0.8,1.2]
idxRangeSearchMax = where(xhist gt rangeSearchMax[0] and xhist lt rangeSearchMax[1], count)
if count eq 0 then message, 'idxRangeSearchMax is empty.'
maxHistLocal = max(yhistSmooth[idxRangeSearchMax],idxMaxHistLocal)
idxMaxHistLocal += idxRangeSearchMax[0]
thrMax = max([2,maxHistLocal/10.])
idxMax = where(yhistSmooth[idxMaxHistLocal:*] lt thrMax, count)
if count gt 0 then maxRange = xhist[idxMax[0]-1+idxMaxHistLocal] else maxRange = fr_ave_sorted(round(n_elements(fr_ave_sorted) * 0.99))

; first guess of the minimum count is frame median value
; because more than half of the frame is dark (we get
; ~40x40x!pix4 illuminated pixels over 120x120 total pixels)
; and minimum is betrween RON + dark and poisson distribution 
minHistGuess = median(fr_ave)
idxRangeSearchMin = where(xhist gt minHistGuess, count)
if count eq 0 then message, 'idxRangeSearchMin is empty.'
minHistLocal = min(yhistSmooth[idxRangeSearchMin],idxMinHistLocal)
idxMinHistLocal += idxRangeSearchMin[0]
if idxMinHistLocal gt idxMaxHistLocal then minRange = minHistGuess else minRange = xhist[idxMinHistLocal]
if minRange le 0 then minRange = max([0,mean(fr_ave[0:9,0:9])])
; ---

fr_ave_cut[where(fr_ave_cut lt minRange or fr_ave_cut gt maxRange)] = 0

idxRonThr = where(fr_ave_noron lt minHistGuess, count)
if count gt 0 then fr_ave_noron[idxRonThr] = 0

plothist, fr_ave_noron, xhist_noron, yhist_noron, /noplot, bin=bin
plothist, fr_ave_cut, xhist_cut, yhist_cut, /noplot, bin=bin

; ------- ron level ------- ;
plothist, fr_ave[*,0:10], xhist_ron, yhist_ron, /noplot, bin=bin
g = gaussfit(xhist_ron,yhist_ron,nterms=4,a)
ron = a[2]

;TH1 = 20*ron/maxRange

; ---------------------------------------------- ;

if keyword_set(display) then begin
    window, 2
    plot, xhist, yhist, psym=10, tit='!17full', xtit='!17ADU level', ytit='!17counts', /xst, charsize=2, yra=[0,max(yhist)/5]
    oplot, xhist_noron, yhist_noron, psym=10, col=255l
    oplot, xhist_cut, yhist_cut, psym=10, col=255l*256l
endif

if ~keyword_set(verbose) and ~keyword_set(display) then QUIET = 1B

distanza_centri_temp, fr_ave, IM_TYPE=IM_TYPE, m, w, diagonale_lato, media_d, QUIET = QUIET, OUTFILE = OUTFILE, $
  /SPLIT, TH1 = TH1, TH2 = TH2, DISPFACTOR = dispfactor, OFFX=OFFX, OFFY=OFFY, DOPLOT = display, $
  MINIM=minRange, MAXIM=maxRange,ron = ron

; ------ all quarters --------- ;

make_xy, side, side/2., x, y
x = x-min(x)+0.5
y = y-min(y)+0.5

xcMean = fltarr(4)
ycMean = fltarr(4)
xcCog = fltarr(4)
ycCog = fltarr(4)
radiusMeanMax = fltarr(4)
radiusCogMax = fltarr(4)

for i=0,3 do begin
    j = i mod 2
    k = i / 2
    fr_ith = fr_ave_cut[side*j:side*(j+1)-1,side*k:side*(k+1)-1]
    idxGt0 = where(fr_ith gt 0,countGt0)

    if countGt0 gt 0 then begin
        xcMean[i] = mean(x[idxGt0]) 
        ycMean[i] = mean(y[idxGt0])
        xcCog[i] = total(fr_ith[idxGt0]*x[idxGt0])/total(fr_ith[idxGt0])
        ycCog[i] = total(fr_ith[idxGt0]*y[idxGt0])/total(fr_ith[idxGt0])
    
        distanceMean = sqrt((x-mean(x[idxGt0]))^2.+(y-mean(y[idxGt0]))^2.)
        distanceCog = sqrt((x-mean(x[idxGt0]))^2.+(y-mean(y[idxGt0]))^2.)

        radiusMeanMax[i] = max(distanceMean[idxGt0])
        radiusCogMax[i] = max(distanceCog[idxGt0])
        
        xcMean[i] += side*j 
        ycMean[i] += side*k
        xcCog[i] += side*j
        ycCog[i] += side*k
    endif
   
endfor

if keyword_set(display) then begin
    window, 4, xs=dispfactor*sFr_ave[0], ys=dispfactor*sFr_ave[1]
    tvscl, rebin(fr_ave, sFr_ave[0]*dispfactor, sFr_ave[1]*dispfactor, /SAMPLE)
    for i=0,3 do tvcircle, m[i,0]*dispfactor, m[i,1]*dispfactor, m[i,2]*dispfactor, /device, thick=1, col=255l
    for i=0,3 do tvcircle, 1, m[i,1]*dispfactor, m[i,2]*dispfactor, /device, color=255l, thick=2
    for i=0,3 do tvcircle, radiusMeanMax[i]*dispfactor, xcMean[i]*dispfactor, ycMean[i]*dispfactor, /device, thick=1
    for i=0,3 do tvcircle, 1, xcMean[i]*dispfactor, ycMean[i]*dispfactor, /device, thick=2
endif

dx = [(m[1,1] - m[0,1]), (m[3,1] - m[2,1])]
dy = [ (m[0,2] - m[2,2]), (m[1,2] - m[3,2])]
dxCog = [(xcCog[1] - xcCog[0]), (xcCog[3] - xcCog[2])]
dyCog = [(ycCog[2] - ycCog[0]), (ycCog[3] - ycCog[1])]
dxMean = [(xcMean[1] - xcMean[0]), (xcMean[3] - xcMean[2])]
dyMean = [(ycMean[2] - ycMean[0]), (ycMean[3] - ycMean[1])]

if keyword_set(verbose) then begin
    print, 'X distance', dx
    print, 'Y distance', dy 
    print, 'average', mean([dx,dy])
    print, 'CoG'
    print, 'X distance', dxCog
    print, 'Y distance', dyCog
    print, 'average', mean([dxCog,dyCog])
    print, 'average position'
    print, 'X distance', dxMean
    print, 'Y distance', dyMean
    print, 'average', mean([dxMean,dyMean])
endif

return, [[dx], [dy]]

end





