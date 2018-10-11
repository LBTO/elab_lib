
pro saveaoelab, ee_obj
    sss = filepath(root=ao_elabdir(), sub=[ee_obj->datadir()],  'aoelab_'+ee_obj->tracknum()+'.sav')
    file_mkdir, file_dirname(sss)
    save, ee_obj, file=sss
end
