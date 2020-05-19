;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; F L A O  # 2  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;D A T A B A S E    C R E A T I O N ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;+ ===============================================================================
;
; ADD REFERENCE SOURCE INFORMATION TO THE DATABASE
;
;- ===============================================================================
pro db_add_wref_flao2
    db = getdb(2)

	;RUN #2
    db->alter, (obj_new('aodataset', from='20120207_041001', to='20120207_051821'))->tracknums(), 'refStar', 'AO1071'
    db->alter, (obj_new('aodataset', from='20120207_062910', to='20120207_063424'))->tracknums(), 'refStar', 'AO305'
	db->alter, ['20120207_123246','20120207_123342','20120207_123438'], 'refStar', 'AO470'

	db->alter, (obj_new('aodataset', from='20120209_041644', to='20120209_055149'))->tracknums(), 'refStar', 'AO1071'
	db->alter, (obj_new('aodataset', from='20120209_063310', to='20120209_070326'))->tracknums(), 'refStar', 'A0305'
	db->alter, (obj_new('aodataset', from='20120209_073849', to='20120209_075526'))->tracknums(), 'refStar', 'AO329'
	db->alter, (obj_new('aodataset', from='20120209_082606', to='20120209_090223'))->tracknums(), 'refStar', 'AO860'
	db->alter, (obj_new('aodataset', from='20120209_092945', to='20120209_094140'))->tracknums(), 'refStar', 'AO472'
	db->alter, (obj_new('aodataset', from='20120210_032119', to='20120210_033257'))->tracknums(), 'refStar', 'AO842'
	db->alter, (obj_new('aodataset', from='20120210_104430', to='20120210_110235'))->tracknums(), 'refStar', 'AO860'
	db->alter, (obj_new('aodataset', from='20120210_111429', to='20120210_113554'))->tracknums(), 'refStar', 'AO472'
	db->alter, (obj_new('aodataset', from='20120210_120709', to='20120210_121949'))->tracknums(), 'refStar', 'AO450'

	;RUN #3
    db->alter, (obj_new('aodataset', from='20120404_112626', to='20120404_123133'))->tracknums(), 'refStar', 'AO581'

    db->alter, (obj_new('aodataset', from='20120405_023330', to='20120405_070000'))->tracknums(), 'refStar', 'AO366'
    db->alter, (obj_new('aodataset', from='20120405_084436', to='20120405_091259'))->tracknums(), 'refStar', 'AO535'
    db->alter, (obj_new('aodataset', from='20120405_092907', to='20120405_093237'))->tracknums(), 'refStar', 'AO875'
    db->alter, (obj_new('aodataset', from='20120405_095836', to='20120405_100318'))->tracknums(), 'refStar', 'AO873'

    db->alter, (obj_new('aodataset', from='20120406_071611', to='20120406_072554'))->tracknums(), 'refStar', 'AO366'
    db->alter, (obj_new('aodataset', from='20120406_084246', to='20120406_100318'))->tracknums(), 'refStar', 'AO510'
    db->alter, (obj_new('aodataset', from='20120406_105524', to='20120406_111324'))->tracknums(), 'refStar', 'AO590'
    db->alter, (obj_new('aodataset', from='20120406_113713', to='20120406_114601'))->tracknums(), 'refStar', 'AO875'
    db->alter, (obj_new('aodataset', from='20120406_115749', to='20120406_121931'))->tracknums(), 'refStar', 'AO1016'

    db->alter, (obj_new('aodataset', from='20120407_030300', to='20120407_033122'))->tracknums(), 'refStar', 'AO264'
    db->alter, (obj_new('aodataset', from='20120407_045407', to='20120407_045638'))->tracknums(), 'refStar', 'AO956'
    db->alter, (obj_new('aodataset', from='20120407_054926', to='20120407_070615'))->tracknums(), 'refStar', 'AO991'
    db->alter, (obj_new('aodataset', from='20120407_081711', to='20120407_100206'))->tracknums(), 'refStar', 'AO1004'
    db->alter, (obj_new('aodataset', from='20120407_101659', to='20120407_102756'))->tracknums(), 'refStar', 'AO535'
    db->alter, (obj_new('aodataset', from='20120407_105615', to='20120407_110527'))->tracknums(), 'refStar', 'AO511'
    db->alter, (obj_new('aodataset', from='20120407_111915', to='20120407_114846'))->tracknums(), 'refStar', 'AO602'
    db->alter, (obj_new('aodataset', from='20120407_115611', to='20120407_121718'))->tracknums(), 'refStar', 'AO603'
    db->alter, (obj_new('aodataset', from='20120407_122622', to='20120407_124306'))->tracknums(), 'refStar', 'AO600'

	db->alter, (obj_new('aodataset', from='20120408_071103', to='20120408_071722'))->tracknums(), 'refStar', 'HD93717'

	;RUN #4
	db->alter, (obj_new('aodataset', from='20120503_053532', to='20120503_063527'))->tracknums(), 'refStar', 'M5_0441'
	db->alter, (obj_new('aodataset', from='20120503_074119', to='20120503_081729'))->tracknums(), 'refStar', 'M5_1416'
	db->alter, (obj_new('aodataset', from='20120503_090130', to='20120503_091818'))->tracknums(), 'refStar', 'BD372790'
	db->alter, (obj_new('aodataset', from='20120503_093346', to='20120503_094014'))->tracknums(), 'refStar', 'LTT14949'
	db->alter, (obj_new('aodataset', from='20120503_100441', to='20120503_100534'))->tracknums(), 'refStar', 'GSC02630'
	db->alter, (obj_new('aodataset', from='20120503_101753', to='20120503_103410'))->tracknums(), 'refStar', 'G20425'
	db->alter, (obj_new('aodataset', from='20120503_104310', to='20120503_105247'))->tracknums(), 'refStar', 'G20333'
	db->alter, (obj_new('aodataset', from='20120503_111329', to='20120503_112056'))->tracknums(), 'refStar', 'GSC03076'
	db->alter, (obj_new('aodataset', from='20120503_113429', to='20120503_114047'))->tracknums(), 'refStar', 'Wref31'
	db->alter, (obj_new('aodataset', from='20120503_115956', to='20120503_121908'))->tracknums(), 'refStar', '38Aql'

	db->alter, (obj_new('aodataset', from='20120504_030232', to='20120504_032409'))->tracknums(), 'refStar', 'S10420'
	db->alter, (obj_new('aodataset', from='20120504_034027', to='20120504_040428'))->tracknums(), 'refStar', 'M5_0441'
	db->alter, (obj_new('aodataset', from='20120504_043853', to='20120504_050834'))->tracknums(), 'refStar', 'TYC3864'
	db->alter, (obj_new('aodataset', from='20120504_054229', to='20120504_061400'))->tracknums(), 'refStar', 'SAO63408'
	db->alter, (obj_new('aodataset', from='20120504_062551', to='20120504_070700'))->tracknums(), 'refStar', 'SAO63177'
	db->alter, (obj_new('aodataset', from='20120504_071759', to='20120504_073201'))->tracknums(), 'refStar', 'SAO44634'
	db->alter, (obj_new('aodataset', from='20120504_075037', to='20120504_081425'))->tracknums(), 'refStar', 'SAO44789'
	db->alter, (obj_new('aodataset', from='20120504_100744', to='20120504_101729'))->tracknums(), 'refStar', 'G20333'
	db->alter, (obj_new('aodataset', from='20120504_104301', to='20120504_110815'))->tracknums(), 'refStar', 'G18122'
	db->alter, (obj_new('aodataset', from='20120504_111827', to='20120504_114711'))->tracknums(), 'refStar', 'G18234'

	db->alter, (obj_new('aodataset', from='20120505_030605', to='20120505_031935'))->tracknums(), 'refStar', 'SAO43309'
	db->alter, (obj_new('aodataset', from='20120505_034048', to='20120505_041110'))->tracknums(), 'refStar', 'SAO43357'
	db->alter, (obj_new('aodataset', from='20120505_055139', to='20120505_060417'))->tracknums(), 'refStar', 'SAO63400'
	db->alter, (obj_new('aodataset', from='20120505_070117', to='20120505_071828'))->tracknums(), 'refStar', 'SAO64098'
	db->alter, (obj_new('aodataset', from='20120505_080758', to='20120505_093350'))->tracknums(), 'refStar', 'HIP76566'
	db->alter, (obj_new('aodataset', from='20120505_095824', to='20120505_104259'))->tracknums(), 'refStar', 'HIP91128'
	db->alter, (obj_new('aodataset', from='20120505_115256', to='20120505_115946'))->tracknums(), 'refStar', 'SAO66660'

	db->save
end


;+ ===============================================================================
;
; DB IGNORE LIST
; When adding tracknum to this list, please specify the reason.
;
;- ===============================================================================
PRO db_add_to_ignore_list_flao2

	db = getdb(2)

	;Initialze Ignore Property:
	db->alter, db->tracknums(), 'ignore', 0

	;IRTC image lost at the end of the acquisition
	db->alter, ['20120207_045707'], 'ignore', 1

	;Immagini saturate
	db->alter, ['20120209_050620','20120209_050750', '20120209_050919', '20120209_051535', '20120209_051705', '20120209_051835'], 'ignore', 1

	;Immagini saturate
	db->alter, ['20120209_064639', '20120209_064808', '20120209_064938'], 'ignore', 1

	;Immagini saturate
	db->alter, ['20120404_113234','20120404_113402', '20120404_113529', '20120404_113839','20120404_114007'], 'ignore', 1

	;Immagini saturate
	db->alter, ['20120404_115203','20120404_115331', '20120404_115457'], 'ignore', 1

	;Immagini saturate
	db->alter, ['20120405_023409','20120405_023501', '20120405_023555'], 'ignore', 1

	; Fuoco IRTC sbagliato
	db->alter, (obj_new('aodataset', from='20120406_105524', to='20120406_110516'))->tracknums(), 'ignore', 1

	; Fuoco IRTC sbagliato
	db->alter, (obj_new('aodataset', from='20120406_113713', to='20120406_113903'))->tracknums(), 'ignore', 1

	; Void
	db->alter, ['20120407_105553','20120407_121510','20120407_121523','20120407_121535' $
				   ,'20120407_123215','20120407_123815'], 'ignore', 1

	; Stella doppia!
	db->alter, (obj_new('aodataset', from='20120503_104310', to='20120503_105247'))->tracknums(), 'ignore', 1

	; Skip frame
	db->alter, ['20120505_070241'], 'ignore', 1

	; HIP76566 (Stella doppia)
	db->alter, (obj_new('aodataset', from='20120505_080758', to='20120505_093350'))->tracknums(), 'ignore', 1

	db->save
end


;+ ===============================================================================
;
; RETRIEVE ALL DATA FROM THE WHOLE COMMISSIONING PERIOD OF FLAO#2 AND FILL IN THE DATABASE
;
;- ===============================================================================
pro db_populate_flao2, recompute=recompute

    if keyword_set(recompute) then rec=1 else rec=0   ; set this if you want to recompute
    db = getdb(2)


	;-----------------------
	; RUN #2 Jan-Feb 2012
	;-----------------------

	run_sky = obj_new('aodataset', from='20120207_041001', to='20120207_063424', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', ['20120207_123246','20120207_123342','20120207_123438'], rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120209_041644', to='20120209_055149', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120209_063310', to='20120209_070326', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120209_073849', to='20120209_074554', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', ['20120209_075503', '20120209_075526'], rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120209_082606', to='20120209_090223', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120209_092945', to='20120209_094140', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120210_032119', to='20120210_033257', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120210_104430', to='20120210_110235', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120210_111429', to='20120210_113554', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120210_120709', to='20120210_121949', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky


	;-----------------------
	; RUN #3 Mar-Apr 2012
	;-----------------------

	;bin1 (AO581)
    run_sky = obj_new('aodataset', from='20120404_112626', to='20120404_114007', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

 	;bin1 (AO581) Jband
    run_sky = obj_new('aodataset', from='20120404_115046', to='20120404_115457', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

 	;bin1 (AO581) Jband, synthetic REC
    run_sky = obj_new('aodataset', from='20120404_121259', to='20120404_121532', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

 	;bin1 (AO581) Hband, synthetic REC
    run_sky = obj_new('aodataset', from='20120404_122901', to='20120404_123133', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky


	;bin1 (AO366)
    run_sky = obj_new('aodataset', from='20120405_023330', to='20120405_023555', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

    run_sky = obj_new('aodataset', from='20120405_041739', to='20120405_042515', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

    run_sky = obj_new('aodataset', from='20120405_051623', to='20120405_051740', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin2 (AO366) cambiamo lo zero-point della magnitudine shift 3.3 mags
    run_sky = obj_new('aodataset', from='20120405_062940', to='20120405_070000', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin3 (AO535)
	run_sky = obj_new('aodataset', from='20120405_084436', to='20120405_091259', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin3 (AO875)
	run_sky = obj_new('aodataset', from='20120405_092907', to='20120405_093237', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin4 (AO873)
	run_sky = obj_new('aodataset', from='20120405_095836', to='20120405_100318', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky


	;bin1 (AO366) 250modi sintetici
	run_sky = obj_new('aodataset', from='20120406_071611', to='20120406_072554', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin1 (AO510) 250modi sintetici
	run_sky = obj_new('aodataset', from='20120406_084246', to='20120406_084834', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin1 (AO510) (sinusoidal REC 105modi)
	run_sky = obj_new('aodataset', from='20120406_094345', to='20120406_094920', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin1 (AO510) (Synthetic REC 100modi)
	run_sky = obj_new('aodataset', from='20120406_095825', to='20120406_100318', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin4 (AO590)
	run_sky = obj_new('aodataset', from='20120406_105524', to='20120406_111324', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin4 (AO875)
	run_sky = obj_new('aodataset', from='20120406_113713', to='20120406_114601', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin4 (AO1016)
	run_sky = obj_new('aodataset', from='20120406_115749', to='20120406_121931', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky


	;bin3 (AO264)
	run_sky = obj_new('aodataset', from='20120407_030300', to='20120407_033122', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin2 (AO956)
	run_sky = obj_new('aodataset', from='20120407_045407', to='20120407_045638', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin4 (AO991)
	run_sky = obj_new('aodataset', ['20120407_054926','20120407_055111','20120407_055257' $
				  ,'20120407_064818','20120407_065003','20120407_065149','20120407_070243' $
				  ,'20120407_070429','20120407_070615'], rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin4 (AO1004)
	run_sky = obj_new('aodataset', ['20120407_081711','20120407_081753','20120407_081835','20120407_083228', $
		           '20120407_083310','20120407_083402','20120407_083444','20120407_083526','20120407_084110', $
				   '20120407_084152','20120407_084234','20120407_085410','20120407_085517','20120407_085623', $
				   '20120407_093748','20120407_093855','20120407_094002','20120407_094328','20120407_094434', $
				   '20120407_095138','20120407_095625','20120407_095929','20120407_100053','20120407_100206'], rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;bin4
	run_sky = obj_new('aodataset', from='20120407_101659', to='20120407_124306', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky


	;bin1 (250 synthetic)
	run_sky = obj_new('aodataset', from='20120408_071103', to='20120408_071722', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	;-----------------------
	; RUN #4 Apr-May 2012
	;-----------------------

	;bin1 (compressi rec sintetici a modulazione 5.0)
	run_sky = obj_new('aodataset', from='20120503_053532', to='20120503_063527', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120503_074119', to='20120503_081729', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120503_090130', to='20120503_091818', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120503_093346', to='20120503_094014', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120503_100441', to='20120503_100534', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120503_101753', to='20120503_103410', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120503_104310', to='20120503_105247', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120503_111329', to='20120503_112056', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120503_113429', to='20120503_114047', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120503_115956', to='20120503_121908', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky


	run_sky = obj_new('aodataset', from='20120504_030232', to='20120504_032409', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120504_034027', to='20120504_040428', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120504_043853', to='20120504_050834', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120504_054229', to='20120504_061400', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120504_062551', to='20120504_070700', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120504_071759', to='20120504_073201', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120504_075037', to='20120504_081425', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120504_100744', to='20120504_101729', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120504_104301', to='20120504_110815', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120504_111827', to='20120504_114711', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120505_030605', to='20120505_041110', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120505_055139', to='20120505_060417', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120505_070117', to='20120505_071828', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120505_080758', to='20120505_080952', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120505_091025', to='20120505_093350', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120505_095824', to='20120505_102749', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120505_103939', to='20120505_104259', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky

	run_sky = obj_new('aodataset', from='20120505_115256', to='20120505_115946', rec=rec)
    db->insert, run_sky->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, run_sky


	;Add Reference Source name:
	db_add_wref_flao2

	;Add Ignore property
	db_add_to_ignore_list_flao2

	heap_gc

end