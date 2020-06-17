
ao_test_init

; Pure integrator
a = getaoelab('20180129_220007')

; Fogetting factor
b = getaoelab('20190302_190223')

; TODO add a TN with IIR control

print,'FLAO pure integrator modes:'
help,where((a->control())->ff() eq 1)

print,'SOUL pure integrator modes:'
help,where((b->control())->ff() eq 1)

print,'FLAO IIR:', (a->control())->isIIR()
print,'SOUL IIR:', (b->control())->isIIR()

print,'FLAO IIR modes:'
help,(a->control())->IIR_mode_idx()

print,'SOUL IIR modes:'
help,(b->control())->IIR_mode_idx()

end
