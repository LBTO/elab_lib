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
set = obj_new('aodataset', from='20110611_072023', to='20110611_073822') ;M92
; set = obj_new('aodataset', from='20110611_094415', to='20110611_100106') ;QSO Falomo
; set = obj_new('aodataset', from='20110612_100823', to='20110612_103010') ; JH  0deg
; set = obj_new('aodataset', from='20110612_105300', to='20110612_113000') ; JH 90deg
; setFe = obj_new('aodataset', from='20110611_040149', to='20110611_041702') ;HD111525 
; setH2 = obj_new('aodataset', from='20110611_042647', to='20110611_043541') ;HD111525 
imas=set->value('pisces.longexposure(/full)') & nimas = n_elements(imas[0,0,*])
off = fltarr(2,nimas) & for i=0,nimas-1 do off[*,i]=measure_images_shift(imas[*,*,0], imas[*,*,i])
sum=fltarr(1024,1024) & for i =0,nimas-1 do begin sum+=shift(imas[*,*,i], -off[0,i], -off[1,i]) & endfor & sum/=nimas
return, sum
end
