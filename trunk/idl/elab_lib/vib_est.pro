function vib_est, vec, fvib, fc, nn=nn, res=res

  if not keyword_set(nn) then nn=3
  
  omega = 2d * !pi * fvib
  damp = log_array(1d-10, 1d, nn) * omega
  b = log_array(1d-30,1d-10,nn)
  ndamp = omega
  
  n=n_elements(vec)
  m=n_elements(damp)
  p=n_elements(b)
  CLT=dblarr(n)
  d=dblarr(n)

  COST=dblarr(p,m)
  
  for l = 0, m-1 do begin
    for k = 0, p-1 do begin
      va1 = -real(2 * exp(- damp[l] / fc) * cos( sqrt(complex( omega^2 - damp[l]^2 )) / fc ))
      va2 = exp(- 2 * damp[l] / fc)
      vb1 = -real(2 * exp(- ndamp / fc) * cos( sqrt(complex( omega^2 - ndamp^2 )) / fc ))
      vb2 = exp(- 2* ndamp / fc)
      for i = 1, n do begin
        z = exp(complex(0, !pi/n*i))
        CP1 = POLY(z^(-1), [1d, vb1, vb2])
        CP2 = POLY(z^(-1), [1d, va1, va2])
        CLT(i-1) = abs(CP1/CP2)*b[k]
        d(i-1) = (vec(i-1)-CLT(i-1))^2.
      endfor
      COST(k,l)=total(d)
    endfor
  endfor
  res=min(COST,idx)
  a0=b(idx mod p)
  a1=damp(fix(idx/p))/omega
  a=[a0,a1]
  
  return, a
end