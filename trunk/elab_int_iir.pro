function elab_int_iir, gain, ff=ff
	; integrator determination
	
	if n_elements(ff) eq 0 then ff = dblarr(n_elements(gain)) + 1.
	if n_elements(ff) ne n_elements(gain) then ff = dblarr(n_elements(gain)) + ff
	
	n = n_elements(gain)
	; filter initialization:
	; numerator & numerator order
	num = dblarr(n,2)
	ord_num = dblarr(n)
	; denominator & denominator order
	den = dblarr(n,2)
	ord_den = dblarr(n)
	; output & input hystory
	ost = dblarr(n,2)
	ist = dblarr(n,2)
	
	for i=0,n-1 do begin
		num[i,0:1] = [ 0, 	gain[i]  ]
		ord_num[i] = 2
		den[i,0:1] = [-ff[i], 	1 ]
		ord_den[i] = 2
	endfor
	
	iir = {n:n, ord_num:ord_num,ord_den:ord_den,num:num,den:den,ost:ost,ist:ist}
	return, iir
end
