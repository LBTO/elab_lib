; FLAO#1 TRACKNUMS SELECTION!!!!!!!!!!!!!!
;THIS PROGAM LOADS THE ONSKY TRACKNUMS FOR WHICH A GOOD SR ESTIMATE IS AVAILABLE.

function db_good_for_onsky_sr

	db=getdb(1)
	setstar = db->query('refStar', 'eq', 'Wref1')
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref3'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref5'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref7'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref9'))
;	setstar = setstar->union(db->query('refStar', 'eq', 'Wref11'))  ;aka Wref32: pianetino
;	setstar = setstar->union(db->query('refStar', 'eq', 'Wref20'))	;stella multipla
;	setstar = setstar->union(db->query('refStar', 'eq', 'Wref32'))  ;pianetino
;	setstar = setstar->union(db->query('refStar', 'eq', 'Wref39'))	;stella doppia
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref41'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref46'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref49'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref50'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref53'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref63'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref72'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref74'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref84'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref93'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref95'))

	setstar = setstar->union(db->query('refStar', 'eq', 'Wref111'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref114'))
;	setstar = setstar->union(db->query('refStar', 'eq', 'Wref119'))	;M92 con tante stelline
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref122'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref151'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref162'))

;	setstar = setstar->union(db->query('refStar', 'eq', 'Wref203')) ;pianetino

	setstar = setstar->union(db->query('refStar', 'eq', 'Wref300'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref301'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref303'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref310'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref311'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref323'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref330'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref338'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref340'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Wref347'))

	setstar = setstar->union(db->query('refStar', 'eq', 'AO37'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO49'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO54'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO56'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO74'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO78'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO80'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO97'))

	setstar = setstar->union(db->query('refStar', 'eq', 'AO107'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO128'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO182'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO195'))

	setstar = setstar->union(db->query('refStar', 'eq', 'AO245'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO247'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO263'))

	setstar = setstar->union(db->query('refStar', 'eq', 'AO301'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO327'))

	setstar = setstar->union(db->query('refStar', 'eq', 'AO730'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO738'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO741'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO743'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO793'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO794'))

	setstar = setstar->union(db->query('refStar', 'eq', 'AO816'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO836'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO838'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO849'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO892'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO899'))

	setstar = setstar->union(db->query('refStar', 'eq', 'AO940'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO960'))

	setstar = setstar->union(db->query('refStar', 'eq', 'AO1066'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO1067'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO1068'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO1071'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO1072'))
	setstar = setstar->union(db->query('refStar', 'eq', 'AO1073'))

	setstar = setstar->union(db->query('refStar', 'eq', 'Brilla1'))
;	setstar = setstar->union(db->query('refStar', 'eq', 'LKHA330')) ;scientific target
	setstar = setstar->union(db->query('refStar', 'eq', 'AO_04P000M08_2'))
	setstar = setstar->union(db->query('refStar', 'eq', 'Ignoto1'))

	; is a dark
	setstar->removeTracknum, ['20100526_043707', '20100526_045607', '20100526_064255', '20100526_082344', '20100526_083258', '20100526_083435', '20100526_084526']
	setstar->removeTracknum, ['20100526_085542', '20100526_085718', '20100526_094735', '20100526_094735', '20100526_094840']
	setstar->removeTracknum, ['20100526_100224', '20100526_100438', '20100526_100558', '20100526_100703', '20100526_112053', '20100526_114210', '20100526_115208', '20100526_115841']
	setstar->removeTracknum, ['20100529_063702']
	setstar->removeTracknum, ['20100530_050814']
	setstar->removeTracknum, ['20100601_040553', '20100601_113920']
	setstar->removeTracknum, ['20100619_035843', '20100619_040938', '20100619_045722','20100619_052032', '20100619_055759','20100619_073121', '20100619_080427', '20100619_091652']
	setstar->removeTracknum, ['20100619_101254', '20100619_103253', '20100619_112053', '20100619_112651']
	setstar->removeTracknum, ['20101028_021824', '20101028_022547']

	; modal rec sbagliato
	setstar->removeTracknum, ['20100529_051107', '20100529_051205', '20100529_110014', '20100529_110126', '20100529_110213', '20100529_110801']
	setstar->removeTracknum, ['20100531_093514', '20100531_093542', '20100531_093739', '20100531_093859']
	setstar->removeTracknum, ['20100601_065431','20100601_065609','20100601_071105','20100601_071640','20100601_071754']
	setstar->removeTracknum, (obj_new('aodataset', from='20100603_082927', to='20100603_083506'))->tracknums()

	; modulazione sbagliata
	setstar->removeTracknum, ['20100529_102815', '20100529_102917', '20100529_103302']

	; sfuocata
	setstar->removeTracknum, ['20100529_035928', '20100529_040831']
	setstar->removeTracknum, (obj_new('aodataset', from='20101028_071856', to='20101028_072656'))->tracknums()
	setstar->removeTracknum, (obj_new('aodataset', from='20101028_073841', to='20101028_075133'))->tracknums()
	setstar->removeTracknum, (obj_new('aodataset', from='20101028_081136', to='20101028_084008'))->tracknums()
	setstar->removeTracknum, (obj_new('aodataset', from='20101126_100000', to='20101126_110200'))->tracknums()

	; dark is bad
	setstar->removeTracknum, (obj_new('aodataset', from='20100527_042822', to='20100527_043942'))->tracknums()

	; bias
	setstar->removeTracknum, ['20100603_045748','20100603_045845', '20100603_050100', '20100603_050303']

	; saturata
	setstar->removeTracknum, ['20100604_035732', '20100604_040051', '20100604_040130', '20100604_044853', '20100604_044918']
	setstar->removeTracknum, (obj_new('aodataset', from='20100622_054733', to='20100622_061250'))->tracknums()
	setstar->removeTracknum, (obj_new('aodataset', from='20100626_084620', to='20100626_085437'))->tracknums()

	; dark too old
	setstar->removeTracknum, ['20100531_075529']
	setstar->removeTracknum, (obj_new('aodataset', from='20100531_102705', to='20100531_105144'))->tracknums()
	setstar->removeTracknum, (obj_new('aodataset', from='20100619_072029', to='20100619_072732'))->tracknums()
	setstar->removeTracknum, (obj_new('aodataset', from='20101126_110353', to='20101126_110844'))->tracknums()
	setstar->removeTracknum, (obj_new('aodataset', from='20101126_132424', to='20101126_132747'))->tracknums()

	; very few number of controlled modes
	setstar->removeTracknum, ['20100621_112006']

	; Problems with telescope:
	setstar->removeTracknum, '20101127_043437'		;close to zenith (fast rotation of telescope)

	; boh?
	setstar->removeTracknum, '20101123_030855'
	setstar->removeTracknum, '20101127_034913'

	; test particolare (test ADC, dice five, e altre minchiate)
	setstar->removeTracknum, (obj_new('aodataset', from='20100622_113459', to='20100622_114451'))->tracknums()
	setstar->removeTracknum, '20101127_025816'	;verification of TT coefficient estimation.

	; is open-loop, but for some reason was identified as closed-loop....
	setstar->removeTracknum, ['20101028_051938', '20101028_052025']

	heap_gc
	return, setstar
end

;
;
;db = getdb()
;;setgain = db->query('control.mingain', 'gt', 0.3, sub=setcl )
;setstar = db_good_for_onsky_sr()
;
;setcl 			= db->query('control.zerogain', 'eq', 0, sub = setstar )
;setga 			= db->query('control.mingain', 'gt', 0.3, sub = setcl )
;setnokalman 	= db->query('control.iskalman', 'eq', 0, sub=setga)
;setsr 			= db->query('irtc.sr_se', 'in', [0.001, 10], sub=setnokalman)
;setsky 			= db->query('operation_mode', 'like', 'sky', sub=setsr)
;setseeing  		= db->query('olmodes.seeing', 'in', [0.25, 4], sub=setsky)
;setH  			= db->query('irtc.lambda', 'eq', 1.6e-6, sub=setseeing)
;setsmallfield 	= db->query('irtc.pixelscale', 'in', [0.009, 0.011],  sub=setH)
;setfwhm 		= db->query('irtc.gaussfit.fwhm', 'in', [0.001, 0.5], sub=setsmallfield)
;
;
;
;setnew = db->query('tracknum', 'gt', '20100901_000000', sub=setfwhm)
;setold = db->query('tracknum', 'lt', '20100901_000000', sub=setfwhm)
;
;
;setgood = setold
;
;set_15  = db->query('mag', 'in', [14.5,15.5], sub=setgood)
;set_14  = db->query('mag', 'in', [13.5,14.5], sub=setgood)
;set_13  = db->query('mag', 'in', [12.5,13.5], sub=setgood)
;set_12  = db->query('mag', 'in', [11.5,12.5], sub=setgood)
;set_11  = db->query('mag', 'in', [10.5,11.5], sub=setgood)
;set_10  = db->query('mag', 'in', [9.5,10.5], sub=setgood)
;set_9   = db->query('mag', 'in', [8.5,9.5], sub=setgood)
;set_8   = db->query('mag', 'in', [2,8.5], sub=setgood)
;
;set_8->RemoveTracknum, ['20100626_090314', '20100626_090333', '20100626_090350', '20100626_090523'] ;
;set_8 = db->query('modal_rec.nmodes', 'gt', 390, sub=set_8)
;
;plot, db->value('olmodes.seeing', sub=set_8), db->value('irtc.sr_se', sub=set_8), psym=4
;plot, db->value('olmodes.seeing', sub=set_9), db->value('irtc.sr_se', sub=set_9), psym=4
;plot, db->value('olmodes.seeing', sub=set_10), db->value('irtc.sr_se', sub=set_10), psym=4
;plot, db->value('olmodes.seeing', sub=set_11), db->value('irtc.sr_se', sub=set_11), psym=4
;plot, db->value('olmodes.seeing', sub=set_12), db->value('irtc.sr_se', sub=set_12), psym=4
;plot, db->value('olmodes.seeing', sub=set_13), db->value('irtc.sr_se', sub=set_13), psym=4
;plot, db->value('olmodes.seeing', sub=set_14), db->value('irtc.sr_se', sub=set_14), psym=4
;plot, db->value('olmodes.seeing', sub=set_15), db->value('irtc.sr_se', sub=set_15), psym=4


;end