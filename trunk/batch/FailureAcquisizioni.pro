post = [ [-109, 26, 0],   $
         [-110, 18.5, 1], $ 
         [-100,10,1], $
         [-109, 26, 0], $
         [-84, 0, 0], $
         [-110, 34, 0], $
         [-100, 10, 1], $
         [-105, 17, 1], $ 
         [-94, 5, 1], $
         [-85, 1, 0], $
         [-49, 61, 1], $
         [-40,44,1], $
         [-75,71,1], $
         [-39,36,1], $
         [-66,70,1], $
         [-43,53,1], $
         [-9.5-75, -23.6+35, 0], $ 
         [-1-75, 30.5+35, 0], $
         [-0-75, 30.5+35, 0], $
         [25-75, -4+35, 0] , $
         [-11-75, -16+35,0] , $
         [22-75,42+35,0] , $
         [2-75,-12+35,1] , $
         [21-75,43+35,0] $ 
]


postOk = post[*,where(post[2,*] eq 1)]
postNo = post[*,where(post[2,*] eq 0)]


plot, postOk[0,*], postOk[1,*], psym=4, symsi=2, xra=[-120, 0], yra=[0, 80], /iso, xtit='stage x [mm]', ytit='stage y [mm]'
oplot, postNo[0,*], postNo[1,*], psym=4, col='0000ff'x, symsi=2, thick=2
oplot, postOk[0,*], postOk[1,*], psym=4, col='00ff00'x, symsi=2, thick=2
oplot,  [-75.16,-75.16], [35.40,35.40], psym=2, col=0, symsi=1, thick=1
write_jpeg, 'FailureAcquisizioni.jpg', tvrd(true=3), true=3

end
