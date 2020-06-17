
; Test that telescope elevation, azimuth and rotator angle
; are all in degrees

ao_test_init

; Fogetting factor
a = getaoelab('20190302_190223')

print,'Tel elevation:', (a->tel())->el()
print,'Tel azimuth:', (a->tel())->az()
print,'Tel rotator angle:', (a->tel())->rot_angle()

end
