
; Test that telescope elevation, azimuth and rotator angle
; are all in degrees

ao_test_init

; Forgetting factor
a = getaoelab('20190302_190223',/rec)

print,'Tel elevation:', (a->tel())->el()
print,'Tel azimuth:', (a->tel())->az()
print,'Tel rotator angle:', (a->tel())->rot_angle()

end
