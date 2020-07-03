
ao_test_init

dum = merit_ncpa('20190708_234139')

ee = getaoelab('20190708_234139')
print, ((ee->adsec_status())->ncpa())[0:20]

end
