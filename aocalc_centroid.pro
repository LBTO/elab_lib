function aocalc_centroid, x, debug=debug, double = double

;+
;NAME :
;	CALC_CENTROID
;
;SYNTAXIS :
;	calc_centroid(x [,double=double])
;
;DESCRIPTION :
;	Computes the center of gravity (first order moment) of an array.
;	The array can be of any dimension. The result is a vector
;	with a number of elements equal to the number of dimensions of the array.
;	Warning: no degenerate dimensions should be present in the array.
;	Hence, the array is reformed previous to the computation, and so it loses
;	the degenerate dimensions.

;   /double : double-precision computation
;
;EXAMPLE :
; 1)  print,calc_centroid([1,2,5,2,1])	; prints the centroid of the array.
;
;-


IF NOT keyword_set(double) THEN double = 0

x = reform(x)
denom = total(x, double = double)
IF denom EQ 0 THEN message, "calc_centroid: the array has a total equal to zero."

taille = size(x) & nb_dim=taille(0)
IF keyword_set(debug) THEN print, 'dim:', taille, '  n_dim', nb_dim
result = fltarr(nb_dim)                     ; init du resultat

FOR i=1, nb_dim DO BEGIN                    ; goes through the dimensions
    proj = x                                ; projection useful coords
    FOR j=nb_dim, 1, -1 DO IF j NE i THEN proj = total(proj, j, double=double)

    ; calcul cdg (coeffs entiers => float suffit meme si calcul en double)
    result(i-1) = total( proj*findgen(taille[i])-0.5*taille[i] ) / denom
    IF keyword_set(debug) THEN BEGIN
        print, "i=", i
        print, 'proj:', proj
        wait,0.5
    ENDIF
ENDFOR

return, result
end
