function measure_images_shift, a,b

sza = size(a, /dim)
szb = size(b, /dim)

if total(sza-szb) ne 0 then message, 'The images have different sizes'
; 

corr = shift(real_part(fft(fft(a)*conj(fft(b)),-1)), sza[0]/2,sza[1]/2)
dum = max(corr,idx) 
offx= idx mod sza[0] - sza[0] / 2. 
offy= idx / sza[0] - sza[0] / 2. 

return, [offx,offy] 
end


function test_measure_images_shift
; set = obj_new('aodataset', from='20110611_072023', to='20110611_073822') ;M92 J
; set = obj_new('aodataset', from='20110611_080949', to='20110611_082911') ;M92 J dark
; set = obj_new('aodataset', from='20110611_094415', to='20110611_100106') ;QSO Falomo
; set = obj_new('aodataset', from='20110612_100823', to='20110612_103010') ; JH  0deg
; set = obj_new('aodataset', from='20110612_105300', to='20110612_113000') ; JH 90deg
; setFe = obj_new('aodataset', from='20110611_040149', to='20110611_041702') ;HD111525 
; setH2 = obj_new('aodataset', from='20110611_042647', to='20110611_043541') ;HD111525 
set = obj_new('aodataset', from='20110613_075027', to='20110613_081933') ;M92 K lungo
set = obj_new('aodataset', from='20110613_083350', to='20110613_090516') ;M92 K dark lungo
set = obj_new('aodataset', from='20110613_095236', to='20110613_100615') ;M92 J lungo
set = obj_new('aodataset', from='20110613_090750', to='20110613_092129') ;M92 J dark lungo
set = obj_new('aodataset', from='20110613_094121', to='20110613_094121')  ;M92 K corto
set = obj_new('aodataset', from='20110613_092448', to='20110613_092448') ;M92 K dark corto
set = obj_new('aodataset', from='20110613_095151', to='20110613_095151') ;M92 J corto
set = obj_new('aodataset', from='20110613_092319', to='20110613_092344') ;M92 J dark corto
set = obj_new('aodataset', from='20110613_100834', to='20110613_100834') ;M92 J corto Open Loop
set = obj_new('aodataset', from='20110613_100929', to='20110613_100929') ;M92 J lungo Open Loop



imas=set->value('pisces.longexposure(/full)') & nimas = n_elements(imas[0,0,*])
off = fltarr(2,nimas) & for i=0,nimas-1 do off[*,i]=measure_images_shift(imas[*,*,0], imas[*,*,i])
sum=fltarr(1024,1024) & for i =0,nimas-1 do begin sum+=shift(imas[*,*,i], -off[0,i], -off[1,i]) & endfor & sum/=nimas
return, sum
end
