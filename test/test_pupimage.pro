
ao_test_init

a = getaoelab('20180129_220007',/rec)
b = getaoelab('20190302_190223',/rec)

print,'FLAO pupil:'
help,(a->frames())->pup_image()
print,'SOUL pupil:'
help,(b->frames())->pup_image()

end
