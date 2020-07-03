
ao_test_init

a = getaoelab('20190302_190223',/rec)

s2d = (a->slopes())->slopes2d()
help, s2d

s2d = (a->slopes())->slopes2d(iter_idx = [1,2,3])
help, s2d

end
