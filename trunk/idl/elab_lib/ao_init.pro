
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

pro AO_init, rootdir=rootdir
; environment string definitions
;

    if not keyword_set(rootdir) then  rootdir = getenv('ADOPT_DATA')
    if rootdir eq "" then message, 'Root dir not specified. Is environment variable ADOPT_DATA set?'

    ao_env = {       $
        root           : rootdir, $
        datadir        : rootdir+path_sep()+'towerdata', $
        elabdir        : rootdir+path_sep()+'elab', $
        phasemapdir    : rootdir+path_sep()+'phase_map' $
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
