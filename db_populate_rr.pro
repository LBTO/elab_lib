;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; F L A O  # 1  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;D A T A B A S E    C R E A T I O N ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;( ADD RETRO-REFLECTOR DATA TO THE DATABASE );;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro db_populate_rr, recompute=recompute

    if keyword_set(recompute) then rec=1 else rec=0   ; set this if you want to recompute

    db = getdb(1)

	rrMay1 = obj_new('aodataset', from='20100525_184042', to='20100525_184603', rec=rec)
	db->insert, rrMay1->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrMay1

	rrMay2 = obj_new('aodataset', from='20100525_190552', to='20100525_191535', rec=rec)
	db->insert, rrMay2->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrMay2

	rrMay3 = obj_new('aodataset', from='20100528_045828', to='20100528_083854', rec=rec)
	db->insert, rrMay3->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrMay3

	rrJune1 = obj_new('aodataset', from='20100606_190504', to='20100606_225208', rec=rec)
	rrJune1->removeTracknum, (obj_new('aodataset', from='20100606_200930', to='20100606_214113'))->tracknums() ;with IRTC subframe not supported.
	db->insert, rrJune1->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrJune1

	rrJune2 = obj_new('aodataset', from='20100607_181353', to='20100607_191858', rec=rec)
	db->insert, rrJune2->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrJune2

	rrJune3 = obj_new('aodataset', from='20100608_195744', to='20100608_235720', rec=rec)
	rrJune3->removeTracknum, (obj_new('aodataset', from='20100608_235101', to='20100608_235720'))->tracknums() ;with IRTC subframe not supported.
	db->insert, rrJune3->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrJune3

	rrJune4 = obj_new('aodataset', from='20100610_231802', to='20100610_234129', rec=rec)
	rrJune4->removeTracknum, ['20100610_233252', '20100610_234002','20100610_234129']  ;with IRTC subframe not supported.
	db->insert, rrJune4->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrJune4

	rrJune5 = obj_new('aodataset', from='20100611_000146', to='20100611_011930', rec=rec)
	rrJune5->removeTracknum, (obj_new('aodataset', from='20100611_000146', to='20100611_002525'))->tracknums() ;with IRTC subframe not supported.
	db->insert, rrJune5->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrJune5

	rrJune5a = obj_new('aodataset', from='20100623_053609', to='20100623_065042', rec=rec)
	db->insert, rrJune5a->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrJune5a

	rrJune5b = obj_new('aodataset', from='20100623_084141', to='20100623_085521', rec=rec)
	db->insert, rrJune5b->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrJune5b

	rrOct1 = obj_new('aodataset', from='20101013_223603', to='20101013_224101', rec=rec)
	db->insert, rrOct1->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrOct1

	rrOct2 = obj_new('aodataset', from='20101014_181004', to='20101014_222609', rec=rec)
	db->insert, rrOct2->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrOct2

	rrOct3 = obj_new('aodataset', from='20101015_165405', to='20101015_232106', rec=rec)
	db->insert, rrOct3->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrOct3

	rrOct4 = obj_new('aodataset', from='20101019_230448', to='20101020_021614', rec=rec)
	db->insert, rrOct4->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrOct4

	rrOct5 = obj_new('aodataset', from='20101021_084944', to='20101021_122329', rec=rec)
	rrOct5->removeTracknum, (obj_new('aodataset', from='20101021_085702', to='20101021_100406'))->tracknums()	;doppia riflessione nella IRTC
	db->insert, rrOct5->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrOct5

	rrNov1 = obj_new('aodataset', from='20101117_225913', to='20101117_230428', rec=rec)
	db->insert, rrNov1->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrNov1

	rrNov2 = obj_new('aodataset', from='20101119_165736', to='20101119_222105', rec=rec)
	db->insert, rrNov2->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrNov2

	rrNov3 = obj_new('aodataset', from='20101120_184651', to='20101121_191234', rec=rec)
	db->insert, rrNov3->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrNov3

	rrNov4 = obj_new('aodataset', from='20101122_012115', to='20101122_013439', rec=rec)
	db->insert, rrNov4->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, rrNov4

	print, 'COMPLETED :)'
	heap_gc

end
