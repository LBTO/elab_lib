
; Wrapper around ImageMagick's convert to produce an animated GIF
;
; DELAY: delay in ms between frames, defaults to 100
; LOG: save log-rescaled animation
; ZOOM: magnify the image by this factor
; XSIZE and/or YSIZE: if at least one has a value, image will be rescaled
;                     to that size in pixels
; CUBIC and INTERP: passed as-is to CONGRID for image rescaling
;
; Written by: A. Puglisi, Jul 2019

pro write_anim_gif, cube_in, filename, DELAY=DELAY, LOG=LOG, ZOOM=ZOOM, XSIZE=XSIZE_, YSIZE=YSIZE_, CUBIC=CUBIC, INTERP=INTERP

   cube = cube_in
   if not keyword_set(DELAY) then DELAY=100

   if min(cube) LT 0 then cube[where(cube lt 0)] = 0
   if keyword_set(LOG) then cube = alog(cube)
   mm = float(minmax(cube))
   if mm[1] ne mm[0] then cube = (cube-min(cube)) / (mm[1]-mm[0]) * 255

   doresize=0b
   if keyword_set(ZOOM) then begin
       XSIZE_ = n_elements(cube[*,0,0]) * ZOOM
       YSIZE_ = n_elements(cube[0,*,0]) * ZOOM
   endif
   if not keyword_set(XSIZE_) then XSIZE_=n_elements(cube[*,0,0]) else doresize=1b
   if not keyword_set(YSIZE_) then YSIZE_=n_elements(cube[0,*,0]) else doresize=1b

   dir = '/tmp'
   nimages = n_elements(cube[0,0,*])
   filenames = strarr(nimages)
   for i=0,nimages-1 do begin
       filenames[i] = dir+path_sep()+'img'+strtrim(i,2)+'.png'
       if doresize then begin
           img = congrid(cube[*,*,i], XSIZE_, YSIZE_, CUBIC=CUBIC, INTERP=INTERP)
       endif else begin
           img = cube[*,*,i]
       endelse
       write_png, filenames[i], img
   endfor

   cmd = 'convert -delay '+strtrim(fix(DELAY/10))
   for i=0,nimages-1 do cmd += ' '+filenames[i]
   cmd += ' '+filename
   spawn, cmd

   for i=0,nimages-1 do file_delete, filenames[i]

end
