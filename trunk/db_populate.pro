;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; F L A O  # 1  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;D A T A B A S E    C R E A T I O N ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;+ ===============================================================================
;
; ADD REFERENCE SOURCE INFORMATION TO THE DATABASE
;
;- ===============================================================================
pro db_add_wref
    db = getdb(1)

    db->alter, (obj_new('aodataset', from='20100526_043000', to='20100526_055000'))->tracknums(), 'refStar', 'Wref3'
    db->alter, (obj_new('aodataset', from='20100526_063700', to='20100526_071000'))->tracknums(), 'refStar', 'Wref5'
    db->alter, (obj_new('aodataset', from='20100526_080000', to='20100526_091000'))->tracknums(), 'refStar', 'Wref8'
    db->alter, (obj_new('aodataset', from='20100526_093000', to='20100526_101000'))->tracknums(), 'refStar', 'Wref9'
    db->alter, (obj_new('aodataset', from='20100526_110000', to='20100526_120000'))->tracknums(), 'refStar', 'Wref11'

    db->alter, (obj_new('aodataset', from='20100527_040000', to='20100527_044000'))->tracknums(), 'refStar', 'Wref1'
    db->alter, (obj_new('aodataset', from='20100527_054000', to='20100527_061000'))->tracknums(), 'refStar', 'Wref20'
    db->alter, (obj_new('aodataset', from='20100527_090000', to='20100527_114000'))->tracknums(), 'refStar', 'Wref31'
    db->alter, (obj_new('aodataset', from='20100527_115000', to='20100527_130000'))->tracknums(), 'refStar', 'Wref32'

    db->alter, (obj_new('aodataset', from='20100529_035000', to='20100529_043000'))->tracknums(), 'refStar', 'Wref1'
    db->alter, (obj_new('aodataset', from='20100529_044000', to='20100529_052000'))->tracknums(), 'refStar', 'Wref3'
    db->alter, (obj_new('aodataset', from='20100529_062000', to='20100529_065000'))->tracknums(), 'refStar', 'Wref39'
    db->alter, (obj_new('aodataset', from='20100529_073000', to='20100529_074000'))->tracknums(), 'refStar', 'Wref40'
    db->alter, (obj_new('aodataset', from='20100529_092000', to='20100529_112000'))->tracknums(), 'refStar', 'Wref46'

    db->alter, (obj_new('aodataset', from='20100530_043000', to='20100530_060000'))->tracknums(), 'refStar', 'Wref53'
    db->alter, (obj_new('aodataset', from='20100530_110000', to='20100530_112000'))->tracknums(), 'refStar', 'Wref41'
    db->alter, (obj_new('aodataset', from='20100530_113000', to='20100530_122000'))->tracknums(), 'refStar', 'Wref7'

    db->alter, (obj_new('aodataset', from='20100531_033000', to='20100531_050000'))->tracknums(), 'refStar', 'Wref1'
    db->alter, (obj_new('aodataset', from='20100531_054000', to='20100531_074000'))->tracknums(), 'refStar', 'Wref93'
    db->alter, (obj_new('aodataset', from='20100531_075000', to='20100531_082000'))->tracknums(), 'refStar', 'Wref63'
    db->alter, (obj_new('aodataset', from='20100531_085000', to='20100531_100000'))->tracknums(), 'refStar', 'Wref111'   ; this has error in the modal_rec
    db->alter, (obj_new('aodataset', from='20100531_102000', to='20100531_130000'))->tracknums(), 'refStar', 'Wref114'

    db->alter, (obj_new('aodataset', from='20100601_034910', to='20100601_053257'))->tracknums(), 'refStar', 'Wref3'
    db->alter, (obj_new('aodataset', from='20100601_064330', to='20100601_072414'))->tracknums(), 'refStar', 'Wref122'
    db->alter, (obj_new('aodataset', from='20100601_081846', to='20100601_090403'))->tracknums(), 'refStar', 'Wref84'
    db->alter, (obj_new('aodataset', from='20100601_094519', to='20100601_103659'))->tracknums(), 'refStar', 'Wref74'
    db->alter, (obj_new('aodataset', from='20100601_112900', to='20100601_113920'))->tracknums(), 'refStar', 'Wref119'

    db->alter, (obj_new('aodataset', from='20100603_035522', to='20100603_045038'))->tracknums(), 'refStar', 'Wref3'
    db->alter, (obj_new('aodataset', from='20100603_051742', to='20100603_052810'))->tracknums(), 'refStar', 'Wref93'
    db->alter, (obj_new('aodataset', from='20100603_055620', to='20100603_061655'))->tracknums(), 'refStar', 'Wref122'
    db->alter, (obj_new('aodataset', from='20100603_075636', to='20100603_085456'))->tracknums(), 'refStar', 'Wref151'
    db->alter, (obj_new('aodataset', from='20100603_093104', to='20100603_103500'))->tracknums(), 'refStar', 'Wref119'
    db->alter, (obj_new('aodataset', from='20100603_113016', to='20100603_113718'))->tracknums(), 'refStar', 'Wref74'

    db->alter, (obj_new('aodataset', from='20100604_033641', to='20100604_044918'))->tracknums(), 'refStar', 'Wref162'
    db->alter, (obj_new('aodataset', from='20100604_054114', to='20100604_054348'))->tracknums(), 'refStar', 'Wref72'
    db->alter, (obj_new('aodataset', from='20100604_064115', to='20100604_075023'))->tracknums(), 'refStar', 'Wref5'
    db->alter, (obj_new('aodataset', from='20100604_084119', to='20100604_091401'))->tracknums(), 'refStar', 'Wref203'
    db->alter, (obj_new('aodataset', from='20100604_110255', to='20100604_114533'))->tracknums(), 'refStar', 'Wref11'

    db->alter, (obj_new('aodataset', from='20100619_035359', to='20100619_041236'))->tracknums(), 'refStar', 'Wref162'
    db->alter, (obj_new('aodataset', from='20100619_045346', to='20100619_052032'))->tracknums(), 'refStar', 'Wref303'
    db->alter, (obj_new('aodataset', from='20100619_055759', to='20100619_061117'))->tracknums(), 'refStar', 'Wref122'
    db->alter, (obj_new('aodataset', from='20100619_063555', to='20100619_073121'))->tracknums(), 'refStar', 'Wref300'
    db->alter, (obj_new('aodataset', from='20100619_080051', to='20100619_091652'))->tracknums(), 'refStar', 'Wref301'
    db->alter, (obj_new('aodataset', from='20100619_100320', to='20100619_103253'))->tracknums(), 'refStar', 'Wref84'
    db->alter, (obj_new('aodataset', from='20100619_111847', to='20100619_112651'))->tracknums(), 'refStar', 'Ignoto1'

    db->alter, (obj_new('aodataset', from='20100620_034520', to='20100620_034813'))->tracknums(), 'refStar', 'Wref92'
    db->alter, (obj_new('aodataset', from='20100620_042045', to='20100620_051210'))->tracknums(), 'refStar', 'Wref93'
    db->alter, (obj_new('aodataset', from='20100620_054853', to='20100620_062004'))->tracknums(), 'refStar', 'Wref20'
    db->alter, (obj_new('aodataset', from='20100620_064710', to='20100620_065959'))->tracknums(), 'refStar', 'Wref320'
    db->alter, (obj_new('aodataset', from='20100620_072834', to='20100620_074815'))->tracknums(), 'refStar', 'Wref310'
    db->alter, (obj_new('aodataset', from='20100620_081427', to='20100620_082614'))->tracknums(), 'refStar', 'Wref323'
    db->alter, (obj_new('aodataset', from='20100620_085250', to='20100620_092135'))->tracknums(), 'refStar', 'Wref330'
    db->alter, (obj_new('aodataset', from='20100620_093709', to='20100620_095843'))->tracknums(), 'refStar', 'Wref311'
    db->alter, (obj_new('aodataset', from='20100620_104256', to='20100620_112224'))->tracknums(), 'refStar', 'Wref32'

    db->alter, (obj_new('aodataset', from='20100621_033728', to='20100621_040904'))->tracknums(), 'refStar', 'Wref93'
    db->alter, (obj_new('aodataset', from='20100621_044412', to='20100621_052740'))->tracknums(), 'refStar', 'Wref122'
    db->alter, (obj_new('aodataset', from='20100621_060917', to='20100621_062856'))->tracknums(), 'refStar', 'Wref338'
    db->alter, (obj_new('aodataset', from='20100621_070659', to='20100621_072836'))->tracknums(), 'refStar', 'Wref49'
    db->alter, (obj_new('aodataset', from='20100621_093017', to='20100621_102946'))->tracknums(), 'refStar', 'M71'
    db->alter, (obj_new('aodataset', from='20100621_105221', to='20100621_112409'))->tracknums(), 'refStar', 'Wref340'

    db->alter, (obj_new('aodataset', from='20100622_032730', to='20100622_061250'))->tracknums(), 'refStar', 'Wref93'
    db->alter, (obj_new('aodataset', from='20100622_072035', to='20100622_075158'))->tracknums(), 'refStar', 'Wref39'
    db->alter, (obj_new('aodataset', from='20100622_082046', to='20100622_083754'))->tracknums(), 'refStar', 'Wref347'
    db->alter, (obj_new('aodataset', from='20100622_090824', to='20100622_092403'))->tracknums(), 'refStar', 'Wref50'
    db->alter, (obj_new('aodataset', from='20100622_102613', to='20100622_104015'))->tracknums(), 'refStar', 'Wref349'
    db->alter, (obj_new('aodataset', from='20100622_113459', to='20100622_114451'))->tracknums(), 'refStar', 'Wref7'

    db->alter, (obj_new('aodataset', from='20100626_082929', to='20100626_094212'))->tracknums(), 'refStar', 'Wref95'

	db->alter, (obj_new('aodataset', from='20101026_103141', to='20101026_112925'))->tracknums(), 'refStar', 'AO_04P000M08_2'

    db->alter, (obj_new('aodataset', from='20101028_021212', to='20101028_025046'))->tracknums(), 'refStar', 'AO738'
    db->alter, (obj_new('aodataset', from='20101028_030910', to='20101028_035157'))->tracknums(), 'refStar', 'AO730'
    db->alter, (obj_new('aodataset', from='20101028_041856', to='20101028_044630'))->tracknums(), 'refStar', 'AO56'
    db->alter, (obj_new('aodataset', from='20101028_051334', to='20101028_053701'))->tracknums(), 'refStar', 'AO960'
    db->alter, (obj_new('aodataset', from='20101028_070217', to='20101028_070822'))->tracknums(), 'refStar', 'Brilla1'
    db->alter, (obj_new('aodataset', from='20101028_071856', to='20101028_072656'))->tracknums(), 'refStar', 'AO74'
    db->alter, (obj_new('aodataset', from='20101028_073841', to='20101028_075133'))->tracknums(), 'refStar', 'AO97'
    db->alter, (obj_new('aodataset', from='20101028_081136', to='20101028_084008'))->tracknums(), 'refStar', 'AO195'
    db->alter, (obj_new('aodataset', from='20101028_091801', to='20101028_093448'))->tracknums(), 'refStar', 'AO80'
    db->alter, (obj_new('aodataset', from='20101028_095056', to='20101028_100603'))->tracknums(), 'refStar', 'AO838'
    db->alter, (obj_new('aodataset', from='20101028_103120', to='20101028_104258'))->tracknums(), 'refStar', 'AO940'
    db->alter, (obj_new('aodataset', from='20101028_105353', to='20101028_105625'))->tracknums(), 'refStar', 'AO182'

    db->alter, (obj_new('aodataset', from='20101029_015810', to='20101029_021316'))->tracknums(), 'refStar', 'AO741'
    db->alter, (obj_new('aodataset', from='20101029_023420', to='20101029_024530'))->tracknums(), 'refStar', 'AO793'
    db->alter, (obj_new('aodataset', from='20101029_030515', to='20101029_031629'))->tracknums(), 'refStar', 'AO743'
    db->alter, (obj_new('aodataset', from='20101029_045021', to='20101029_051028'))->tracknums(), 'refStar', 'AO1066'
    db->alter, (obj_new('aodataset', from='20101029_055504', to='20101029_060058'))->tracknums(), 'refStar', 'AO1067'
    db->alter, (obj_new('aodataset', from='20101029_062242', to='20101029_062839'))->tracknums(), 'refStar', 'AO128'
    db->alter, (obj_new('aodataset', from='20101029_071118', to='20101029_075003'))->tracknums(), 'refStar', 'HD15745'
    db->alter, (obj_new('aodataset', from='20101029_081803', to='20101029_090445'))->tracknums(), 'refStar', 'LKHA330'
    db->alter, (obj_new('aodataset', from='20101029_102932', to='20101029_105145'))->tracknums(), 'refStar', 'AO1072'
    db->alter, (obj_new('aodataset', from='20101029_113812', to='20101029_115417'))->tracknums(), 'refStar', 'AO1073'
    db->alter, (obj_new('aodataset', from='20101029_121820', to='20101029_130010'))->tracknums(), 'refStar', 'AO849'

    db->alter, (obj_new('aodataset', from='20101123_025032', to='20101123_034038'))->tracknums(), 'refStar', 'AO738'
    db->alter, (obj_new('aodataset', from='20101123_035913', to='20101123_043232'))->tracknums(), 'refStar', 'AO49'
    db->alter, (obj_new('aodataset', from='20101123_065003', to='20101123_065039'))->tracknums(), 'refStar', 'OC_M36'
    db->alter, (obj_new('aodataset', from='20101123_081342', to='20101123_083526'))->tracknums(), 'refStar', 'AO1071'
    db->alter, (obj_new('aodataset', from='20101123_103938', to='20101123_105016'))->tracknums(), 'refStar', 'AO263'

    db->alter, (obj_new('aodataset', from='20101126_012742', to='20101126_020716'))->tracknums(), 'refStar', 'AO794'
    db->alter, (obj_new('aodataset', from='20101126_051543', to='20101126_065450'))->tracknums(), 'refStar', 'AO74'
    db->alter, (obj_new('aodataset', from='20101126_073042', to='20101126_075157'))->tracknums(), 'refStar', 'AO78'
	db->alter, (obj_new('aodataset', from='20101126_082021', to='20101126_094841'))->tracknums(), 'refStar', 'LKHA330'
    db->alter, (obj_new('aodataset', from='20101126_104557', to='20101126_122350'))->tracknums(), 'refStar', 'AO247'
    db->alter, (obj_new('aodataset', from='20101126_132424', to='20101126_132747'))->tracknums(), 'refStar', 'AO327'

    db->alter, (obj_new('aodataset', from='20101127_013243', to='20101127_021101'))->tracknums(), 'refStar', 'AO816'
    db->alter, (obj_new('aodataset', from='20101127_022517', to='20101127_025816'))->tracknums(), 'refStar', 'AO54'
    db->alter, (obj_new('aodataset', from='20101127_031410', to='20101127_031511'))->tracknums(), 'refStar', 'AO37'
    db->alter, (obj_new('aodataset', from='20101127_034418', to='20101127_040017'))->tracknums(), 'refStar', 'AO892'
    db->alter, (obj_new('aodataset', from='20101127_042406', to='20101127_043437'))->tracknums(), 'refStar', 'AO899'
    db->alter, (obj_new('aodataset', from='20101127_051358', to='20101127_052030'))->tracknums(), 'refStar', 'AO107'
    db->alter, (obj_new('aodataset', from='20101127_053420', to='20101127_055148'))->tracknums(), 'refStar', 'AO1068'
    db->alter, (obj_new('aodataset', from='20101127_065324', to='20101127_073600'))->tracknums(), 'refStar', 'AO245'
    db->alter, (obj_new('aodataset', from='20101127_084821', to='20101127_090259'))->tracknums(), 'refStar', 'AO836'
    db->alter, (obj_new('aodataset', from='20101127_115526', to='20101127_122220'))->tracknums(), 'refStar', 'AO301'

    db->alter, (obj_new('aodataset', from='20101128_030120', to='20101128_030242'))->tracknums(), 'refStar', 'AO1066'

    ;db->alter, (obj_new('aodataset', from='', to=''))->tracknums(), 'refStar', 'AO'

	db->save
end


;+ ===============================================================================
;
; DB IGNORE LIST
; When adding tracknum to this list, please specify the reason.
;
;- ===============================================================================
PRO db_add_to_ignore_list

	db = getdb(1)

	;Initialze Ignore Property:
	db->alter, db->tracknums(), 'ignore', 0

	;The following sets are not cited in the twiki and/or they contain partial saved data:
	db->alter, ['20100530_095514', '20100530_095546', '20100530_095639'], 'ignore', 1
	db->alter, (obj_new('aodataset', from='20100603_065422', to='20100603_080459'))->tracknums(), 'ignore', 1
	db->alter, ['20100620_030841', '20100620_033244', '20100620_033345', '20100620_034431', '20100620_040445'], 'ignore', 1
	db->alter, '20100621_051133', 'ignore', 1

	;Cube inside with lamp on...
	db->alter, ['20100530_095707', '20100530_095745', '20100530_095823', '20100530_103510', '20100530_103532', '20100530_103715', '20100530_103944', '20100530_104012'], 'ignore', 1

	;unsuccessful open-loop PSF: at the border of IRTC...
	db->alter, ['20100530_103606'], 'ignore', 1

	;unsuccessful dark acquisitions:
	db->alter, ['20100601_090252', '20100601_090403'], 'ignore', 1

	;Problems with telescope:
	db->alter, ['20100530_055504'], 'ignore', 1	;rotator stopped
	db->alter, ['20100604_103353', '20100604_103418', '20100604_103608', '20100604_103646'], 'ignore', 1
	db->alter, ['20101127_042641'], 'ignore', 1
	db->save

END


;+ ===============================================================================
;
; RETRIEVE ALL DATA FROM THE WHOLE FLAO#1 COMMISSIONING PERIOD AND FILL IN THE DATABASE
;
;- ===============================================================================
pro db_populate, recompute=recompute

    if keyword_set(recompute) then rec=1 else rec=0   ; set this if you want to recompute

    db = getdb(1)

    setMay1 = obj_new('aodataset', from='20100526_043000', to='20100527_114000', rec=rec)
    db->insert, setMay1->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setMay1

    setMay2 = obj_new('aodataset', from='20100529_030000', to='20100601_120000', rec=rec)
    db->insert, setMay2->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setMay2

	setJune0 = obj_new('aodataset', from='20100603_035522', to='20100604_114533', rec=rec)
	db->insert, setJune0->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setJune0

    setJune1 = obj_new('aodataset', from='20100619_000000', to='20100622_120000', rec=rec)
    db->insert, setJune1->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setJune1

    setJune2 = obj_new('aodataset', from='20100626_080000', to='20100626_100000', rec=rec)
    db->insert, setJune2->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setJune2

	setOcta = obj_new('aodataset', from='20101026_103141', to='20101026_112925', rec=rec)
    db->insert, setOcta->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setOcta

    setOct  = obj_new('aodataset', from='20101028_000000', to='20101029_235959', rec=rec)
    db->insert, setOct->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setOct

    setNov = obj_new('aodataset', from='20101123_025000', to='20101123_130000', rec=rec)
    db->insert, setNov->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setNov

	setNov2 = obj_new('aodataset', from='20101126_012742', to='20101126_132747', rec=rec)
    db->insert, setNov2->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setNov2

	setNov3 = obj_new('aodataset', from='20101127_013243', to='20101127_122220', rec=rec)
    db->insert, setNov3->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setNov3

	setNov4 = obj_new('aodataset', from='20101128_030120', to='20101128_030242', rec=rec)
    db->insert, setNov4->tracknums()
    db->save
    !aomultiton_elab->release
    obj_destroy, setNov4

	;Add Reference Source name:
	db_add_wref

	;Add Ignore property
	db_add_to_ignore_list

    heap_gc

end



