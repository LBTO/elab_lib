;+
;
; turbulence parameters estimation for ARMA model
;
; Created by Guido Agapito agapito@arcetri.astro.it
;
;-

function turb_est2, vec, a, b, res=res

  m=n_elements(a)
  p=n_elements(b)
  n=n_elements(vec)
  CLT=dblarr(n)
  d=dblarr(n)

  COST=dblarr(m,m,p)
  ivect = findgen(n)+1
 
  for l = 0, p-1 do begin
    for j = 0, m-1 do begin
      for k = 0, m-1 do begin
        z = exp(complex(0, !pi/n*ivect))
        CP = (1-a(j)*z^(-1))*(1-a(k)*z^(-1))
        CLT = abs(b(l)/CP)
        idx = where(vec ne 0, count)
; TODO must CLT be raised to the power of 2?
        if count gt 0 then d=(vec[idx]-CLT[idx])^2.
        COST(j,k,l)=total(d)
      endfor
    endfor
  endfor
  
  res=min(COST,idx)
  a0=b(fix(idx/m^2))
  a1=a(fix((idx mod m^2)/m))
  a2=a( (idx mod m^2) mod m)
  a=[a0,a1,a2]
  return, a

end
