
ao_test_init

a = getaoelab('20190302_190223',/rec)

set_plot,'z'
a->modalplot
a->modalplot, /over, /wfresiduals
a->modalplot, color=255L, olcolor=128L, clvar=clvar, olvar=olcvar
a->modalplot, /argos

help,clvar
help,olvar
print,'std(clvar)=',stddev(clvar)

end
