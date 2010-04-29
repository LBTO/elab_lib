

function getaoelab, tracknum, $
            recompute = recompute, $
            dark_fname=dark_fname, $
            modal_reconstructor_file=modal_reconstructor_file       ; this is used in case of kalman filter
    ;data = strmid(tracknum, 0, 8)
    ;datadir = data+path_sep()+'Data_'+tracknum
    ;sss = filepath(root=ao_elabdir(), sub=[datadir],  'aoelab_'+tracknum+'.sav')
    ;if file_test(sss) then begin
    ;    restore, sss
    ;    stop
    ;    return, ee_obj
    ;endif else

    ;on_error, 2
    defsysv, "!ao_env", EXISTS=exists
    if not exists then message, 'Call ao_init first!'
    message, 'Getting tracknum '+tracknum, /info
    return, obj_new('AOelab', tracknum, modal_rec=modal_reconstructor_file, recompute=recompute, dark_fname=dark_fname)

end
