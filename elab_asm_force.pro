; +
;
; This procedure is used to display estimated Adaptive Seondary Mirror forces from a TN
; 
; inputs:
;   tn                  tracking number
;   left                (keyword) left side
;   right               (keyword) right side
;   wsize               (optinal) 2 elements widnow size
;   charsize            (optional) character size in figure
;   doCumulateOnModes   (keyword) cumulate force on increasing number of modes
;                       It means that average and maximum force per mode are
;                       computed summing the first i modes, with i going
;                       from 1 to N (N is maximum number of corrected modes).
;
;   written by Guido Agapito guido.agapito@inaf.it
;
; -

pro elab_asm_force, tn, left=left, right=right, wsize=wsize, charsize=charsize, $
                    doCumulateOnModes=doCumulateOnModes

if n_elements(wsize) le 1 then wsize=[800,600]
if n_elements(charsize) eq 0 then charsize=2

ao_init, left=left, right=right, /white

ee=getaoelab(tn)

nm = (ee->modal_rec())->nmodes()
modes = (ee->modes())->modes()

idxF = where(finite(modes) eq 0, countF)
if countF gt 0 then modes[idxF] = 0

FF_MATRIX = ((ee->adsec_status())->struct_adsec()).FF_MATRIX
m2c = (ee->control())->m2c()
Modes2Force = matrix_multiply(m2c,FF_MATRIX)

force = matrix_multiply(modes[*,0:nm-1],Modes2Force[0:nm-1,*])

maxForcePerAct = max(abs(force),dim=1)
maxForcePerIter = max(abs(force),dim=2)

bigForceMat = fltarr((size(force,/dim))[0],(size(force,/dim))[1],nm)
for j=0,nm-1 do bigForceMat[*,*,j] = matrix_multiply(modes[*,j],Modes2Force[j,*])
maxForcePerMode = fltarr((size(force,/dim))[0],nm)
if keyword_set(doCumulateOnModes) then begin
    for i=0,(size(force,/dim))[0]-1 do begin
        temp = bigForceMat[i,*,0]*0.
        for j=0,nm-1 do begin
            temp += bigForceMat[i,*,j]
            maxForcePerMode[i,j] = max(abs(temp))
        endfor
    endfor
endif else for i=0,(size(force,/dim))[0]-1 do maxForcePerMode[i,*] = max(abs(reform(bigForceMat[i,*,*])),dim=1)

window, 0, xs=wsize[0], ys=wsize[1]
plothist, force, xhist, yhist, bin=0.025, thick=2, $
    charsize=charsize, tit='!17', xtit='!17force !4[!17N!4]!17', $
    ytit='!17occurence', xra=[0,1.0]

window, 1, xs=wsize[0], ys=wsize[1]
plot, maxForcePerAct, thick=2, charsize=charsize, $
    xgridstyle=1, ygridstyle=1, xticklen=1, yticklen=1, /xst, $
    tit='!17', $
    xtit='!17actuator number', $
    ytit='!17maximum force !4[!17N!4]!17', yran=[0., 1.0]

window, 2, xs=wsize[0], ys=wsize[1]
plot, maxForcePerIter, thick=2, charsize=charsize, $
    xgridstyle=1, ygridstyle=1, xticklen=1, yticklen=1, tit='!17', $
    xtit='!17iteration number', $
    ytit='!17maximum force !4[!17N!4]!17', yran=[0., 1.0]

window, 3, xs=wsize[0], ys=wsize[1]
image_show, /sh, maxForcePerMode, charsize=charsize, $
    min=0.01, max=1., $
    tit='!17max Force per mode', $
    xtit='!17iteration number', $
    ytit='!17mode number', /log

window, 4, xs=wsize[0], ys=wsize[1]
plot, max(maxForcePerMode,dim=1), charsize=charsize, $
    thick=2, /xst, xgridstyle=1, ygridstyle=1, xticklen=1, yticklen=1, $
    tit='!17ave. and max. force per mode', $
    xtit='!17mode number', $
    ytit='!17Force !4[!17N!4]!17', yran=[0., 1.0]
oplot, mean(maxForcePerMode,dim=1),  col=255l, thick=2
elab_legend, ['max','ave'], charsize=charsize, col=[1l,255l], $
             psym=[4,4], symsize=[2,2], thick=2, $
             /top, /left, /clear


end
