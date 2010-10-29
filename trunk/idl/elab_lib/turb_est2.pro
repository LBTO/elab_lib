function turb_est2, vec, a, b, res=res

  m=n_elements(a)
  p=n_elements(b)
  n=n_elements(vec)
  CLT=dblarr(n)
  d=dblarr(n)

  COST=dblarr(m,m,p)
  
  for l = 0, p-1 do begin
    for j = 0, m-1 do begin
      for k = 0, m-1 do begin
        for i = 1, n do begin
          z = exp(complex(0, !pi/n*i))
          CP = (1-a(j)*z^(-1))*(1-a(k)*z^(-1))
          CLT(i-1) = abs(b(l)/CP)
          if vec(i-1) ne 0 then d(i-1) = (vec(i-1)-CLT(i-1))^2.
        endfor
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