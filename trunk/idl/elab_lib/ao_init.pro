
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


pro AO_init, rootdir=rootdir, elabdir=elabdir, phasemapdir=phasemapdir
; environment string definitions
;
; rootdir : directory where towerdata is located (can be taken from env variable: ADOPT_DATA)
; elabdir : directory where elab is located. (DEFAULT: same as rootdir)
;											 (can be taken from env variable: ADOPT_ELAB)
; phasemapdir : directory where phasemapdir is located (DEFAULT: same as elabdir)
;													   (can be taken from env variable: ADOPT_PHASEMAPS)

    if not keyword_set(rootdir) then begin
        if getenv('ADOPT_DATA') ne '' then rootdir = getenv('ADOPT_DATA') else $
			message, 'Root dir not specified. Is environment variable ADOPT_DATA set?'
    endif

	if not keyword_set(elabdir) then begin
		if getenv('ADOPT_ELAB') ne '' then elabdir = getenv('ADOPT_ELAB') else $
			elabdir = rootdir
	endif

	if not keyword_set(phasemapdir) then begin
		if getenv('ADOPT_PHASEMAPS') ne '' then phasemapdir = getenv('ADOPT_PHASEMAPS') else $
			phasemapdir = rootdir
	endif

    ao_env = {       $
        root           : rootdir, $
        datadir        : rootdir+path_sep()+'towerdata', $
        elabdir        : elabdir+path_sep()+'elab', $
        phasemapdir    : phasemapdir+path_sep()+'phase_maps' $
    }

defsysv, "!ao_env", EXISTS=exists
if not exists then defsysv, "!ao_env", ao_env

name = ['DATASET', 'AOELAB', 'BIPBIP']
fmt  = ['Dataset error: %s.', 'AOelab error: %s.', 'Road Runner not captured.']
DEFINE_MSGBLK, PREFIX = 'AO_OAA_', 'ELAB', name, fmt , /IGNORE_DUPLICATE

;pathsep = PATH_SEP(/SEARCH_PATH)
;!PATH = EXPAND_PATH('+'+rootdir+path_sep()+'elab_lib') + pathsep + !PATH

!PROMPT = "AO> "
cd, !ao_env.root

end
