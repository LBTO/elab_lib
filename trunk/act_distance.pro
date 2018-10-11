
 
pro act_distance, tracknum, ACT_DISTANCE = ACT_DISTANCE, SOGLIA=SOGLIA

  a = getaoelab(tracknum)
  s = (a->slopes())->slopes2d()
  m = total(s,3) / n_elements(s[0,0,*])
  loadct,3
  window,5, retain=2, xsize=700,ysize=300
  image_show, m, /as,/sh


  xmezzi = n_elements(m[*,0])/2
  absm = abs(m[0:xmezzi-1,*]) + abs(m[xmezzi:*,*])

  if not keyword_set(soglia) then begin
     soglia = max(absm) * 0.4
  endif else begin
     soglia *= max(absm)
  endelse

  absm[ where(absm lt soglia)] =0
  window,0,retain=2, xsize=400, ysize=300
  image_show, absm, /as, /sh
 
  x2 = n_elements(absm[*,0])/2
  y2 = n_elements(absm[0,*])/2 

  window,1,retain=2, xsize=200, ysize=50
  cent = fltarr(4,2)
  for i=0,3 do begin
   
     if i eq 0 then s = [0,0]
     if i eq 1 then s = [0,y2]
     if i eq 2 then s = [x2,0]
     if i eq 3 then s = [x2,y2]

   frame = absm[ s[0]:s[0]+x2-1, s[1]:s[1]+y2-1]
   ff = absm*0
   ff[0,0] = frame
   ff[0,0] = -1
   tvscl,ff,i
   cent[i,*] = centroid(frame) + s + [0.5, 0.5]  ; Mezzo pixel perche' l'energia e' nel mezzo del pixel e non sul bordo
  endfor

  ok = where(finite(cent[*,0]))

  x1 = cent[ok[0],0]
  y1 = cent[ok[0],1]
  x2 = cent[ok[1],0]
  y2 = cent[ok[1],1]

  print, cent
  distance = sqrt((x2-x1)*(x2-x1)+ (y2-y1)*(y2-y1))
  print,'Distance: ', distance
  if keyword_set(act_distance) then $
      print,'Percentage of theoretical distance: ', (distance/30. * 911.) / act_distance

end
