
;+
;
;-


function ao_datadir
    return, !ao_env.datadir
end

function ao_elabdir
    return, !ao_env.elabdir
end

function ao_phasemapdir
    return, !ao_env.phasemapdir
end


pro AO_init, rootdir=rootdir, elabdir=elabdir, phasemapdir=phasemapdir, $
             LEFT=LEFT, RIGHT=RIGHT, WHITE_PLOTS=WHITE_PLOTS, NOARCETRI=NOARCETRI

; environment string definitions
;
; rootdir : directory where towerdata is located (can be taken from env variable: ADOPT_MEAS)
; elabdir : directory where elab is located. (DEFAULT: same as rootdir)
;											 (can be taken from env variable: ADOPT_ELAB)
; phasemapdir : directory where phasemapdir is located (DEFAULT: same as elabdir)
;													   (can be taken from env variable: ADOPT_PHASEMAPS)
;
; LEFT and RIGHT, if specified, will force the rootdir and elabdir directories to end in "left" or "right" respectively

    if keyword_set(WHITE_PLOTS) then begin
        loadct, 3
        DEVICE, RETAIN=2
        !p.background=-1
        !p.color=0
        !P.CHARTHICK = 1.25
        !P.THICK = 1.25
        !P.CHARSIZE = 2.0
    endif

    ; Special case for Arcetri local directories
    if FILE_TEST('/home/elab') and not keyword_set(NOARCETRI) then begin
        print, 'Found /home/elab, setting Arcetri elab paths'

        rootdir = '/home/elab/LBT_data_left'
        elabdir = '/home/elab/elab_left'
        phasemapsdir = '/home/elab/phase_maps'
    endif

    if not keyword_set(rootdir) then begin
        if getenv('ADOPT_MEAS') ne '' then rootdir = getenv('ADOPT_MEAS') else $
			message, 'Root dir not specified. Is environment variable ADOPT_MEAS set?'
    endif

	if not keyword_set(elabdir) then begin
		if getenv('ADOPT_ELAB') ne '' then elabdir = getenv('ADOPT_ELAB') else $
			elabdir = rootdir
	endif

	if not keyword_set(phasemapdir) then begin
		if getenv('ADOPT_PHASEMAPS') ne '' then phasemapdir = getenv('ADOPT_PHASEMAPS') else begin
			message, 'WARNING: PHASEMAPDIR not initialized.', /info
			phasemapdir = rootdir
		endelse
	endif

    if keyword_set(LEFT) or keyword_set(RIGHT) then begin
        if strpos(rootdir, 'left') ge 0 then rootdir = strmid(rootdir, 0, strlen(rootdir)-4)
        if strpos(elabdir, 'left') ge 0 then elabdir = strmid(elabdir, 0, strlen(elabdir)-4)
        if strpos(rootdir, 'right') ge 0 then rootdir = strmid(rootdir, 0, strlen(rootdir)-5)
        if strpos(elabdir, 'right') ge 0 then elabdir = strmid(elabdir, 0, strlen(elabdir)-5)
        if keyword_set(LEFT) then begin
            rootdir += 'left'
            elabdir += 'left'
        endif
        if keyword_set(RIGHT) then begin
            rootdir += 'right'
            elabdir += 'right'
        endif
    endif

    ao_env = {       $
        root           : rootdir, $
        datadir        : rootdir, $
        elabdir        : elabdir, $
        phasemapdir    : phasemapdir $
    }

;defsysv, "!ao_env", EXISTS=exists
;if not exists then defsysv, "!ao_env", ao_env

defsysv, "!ao_env", ao_env

; IDL8 defines !CONST, but IDL7 does not
defsysv, "!CONST", EXISTS=exists
if not exists then begin
    consts = { RtoD: 180.0d/!PI, DtoR: !PI/180.0d }
    defsysv, "!CONST", consts
endif

name = ['DATASET', 'AOELAB', 'BIPBIP']
fmt  = ['Dataset error: %s.', 'AOelab error: %s.', 'Road Runner not captured.']
DEFINE_MSGBLK, PREFIX = 'AO_OAA_', 'ELAB', name, fmt , /IGNORE_DUPLICATE

;pathsep = PATH_SEP(/SEARCH_PATH)
;!PATH = EXPAND_PATH('+'+rootdir+path_sep()+'elab_lib') + pathsep + !PATH

!PROMPT = "AO> "
cd, !ao_env.root

end
