
ao_test_init

a = getaoelab('20180129_220007',/rec)
b = getaoelab('20190302_190223',/rec)

print,'FLAO delay (ms) = ', (a->delay())->delay()
print,'SOUL delay (ms) = ', (b->delay())->delay()

print,'FLAO delay (frames) = ', (a->delay())->frames_delay()
print,'SOUL delay (frames) = ', (b->delay())->frames_delay()

end
