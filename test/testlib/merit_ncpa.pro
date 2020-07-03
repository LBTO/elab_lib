

function merit_ncpa, tracknum, right=right, left=left

   ao_init, right=right, left=left
   ee = getaoelab(tracknum)

   ; Initialize a "result" variable, that will be read
   ; by the calling program if the longexposure() call fails
   result = 0

   ss = (ee->luci())->longexposure(/full)

   print, format='("SR of long exposure: ", (F0.2))', (ee->luci())->sr_se(ima=ss)
;   luci = ee->luci()
;   return, luci->sr_se()

   ee = getaoelab(tracknum, /rec)
   cube = (ee->luci())->imagecube()
   nframes = (ee->luci())->nframes()

   sr = fltarr(nframes)
   for i=0,nframes-1 do begin
      sr[i] = (ee->luci())->sr_se( ima = cube[*,*,i], psf_dl_ima=psf_dl_ima)
      print, format='("SR '+strtrim(i,2)+': ", (F0.2))',sr[i]
   endfor

   ss = (sr[reverse(sort(sr))])[0:1]

   print, format='("MEAN       :  ", (F0.2))',mean(sr)
   print, format='("MEAN (top2):  ", (F0.2))',mean(ss)
 
   return, mean(ss)

end
