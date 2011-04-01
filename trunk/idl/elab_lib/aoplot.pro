PRO AOplot, X, Y, HISTO_VAR=H, GROUP_VAR=G, _EXTRA = ex, CURSOR=CURSOR, tr=tr $
		  , g_leg_title=g_leg_title, h_leg_title=h_leg_title

	nparams = n_params()
	if nparams ne 2 then begin
		message, 'Usage: aoplot, X, Y, [,...]',/info
		return
	endif

	if n_elements(X) ne n_elements(Y) then begin
		message, 'X and Y not compatible dimensions',/info
		return
	endif

	PLOT_TYPE = ''

	if n_elements(H) ne 0 then begin

		if n_elements(H) ne n_elements(X) then begin
			message, '[X,Y] and HISTO_VAR not compatible dimensions',/info
			return
		endif

		histo = aohistogram(H, _EXTRA=ex, /NOPLOT)
		PLOT_TYPE += 'H'
		nvalidbins = n_elements(histo.leg)

		;legend should appear below the plot. The size of the legend should be computed beforehand.
		usersym, [-2,-2,2, 2,-2], [-1, 1,1,-1,-1], /fill	;rectangle
		if n_elements(H_leg_title) eq 0 then begin
			legend, histo.leg, corners=corners, psym=replicate(8,nvalidbins), linestyle=intarr(nvalidbins), box=0, pspacing=1, CHARSIZE=1.2
		endif else begin
			legend, [[H_leg_title], histo.leg], corners=corners, psym=[[0],replicate(8,nvalidbins)], linestyle=[[-1],intarr(nvalidbins)], box=0, pspacing=1, CHARSIZE=1.2
		endelse
		h_leg_xydims = [corners[2]-corners[0],corners[3]-corners[1]]
	endif else h_leg_xydims = [0,0]

	;Preferred symbols (Note: requires symcat() ).
	plotsym, 0	;Open circle for HISTO_VAR and normal plot.
	sym_type = setdifference(indgen(47),[0,3,9,10])	;for GROUP_VAR

	if n_elements(G) ne 0 then begin

		if n_elements(G) ne n_elements(X) then begin
			message, '[X,Y] and GROUP_VAR not compatible dimensions',/info
			return
		endif

		g_ele = G[rem_dup(G)]
		ng = n_elements(g_ele)
		if ng gt n_elements(sym_type) then begin
			message, 'GROUP_VAR: not enough psym available',/info
			return
		endif
		PLOT_TYPE += 'G'

		;legend should appear below the plot. The size of the legend should be computed beforehand.
		g_type = size(G,/type)
		CASE 1 OF
		  g_type eq 1 or g_type eq 2 or g_type eq 3:	g_leg = strtrim(g_ele,2)
		  g_type eq 4 or g_type eq 5:					g_leg = string( g_ele, format='(f8.2)')
		  g_type eq 7:									g_leg = file_basename(strtrim(g_ele,2))
		ENDCASE
		if n_elements(g_leg_title) eq 0 then begin
			legend, [g_leg], corners=corners, psym=sym_type[indgen(ng)], linestyle=intarr(ng), box=0, pspacing=1, charsize=1.2
		endif else begin
			legend, [[g_leg_title],g_leg], corners=corners, psym=[[0],sym_type[indgen(ng)]], linestyle=[[-1],intarr(ng)], box=0, pspacing=1, charsize=1.2
		endelse
		g_leg_xydims = [corners[2]-corners[0],corners[3]-corners[1]]
	endif else g_leg_xydims = [0,0]


	winsize = get_screen_size()/2
	window, !D.WINDOW > 0, xsize=winsize[0], ysize=winsize[1]
	!X.MARGIN=[8,3]
	!P.REGION = [0,max([g_leg_xydims[1],h_leg_xydims[1]]),1,1]
	plot, X, Y, xrange=minmax(X), yrange=minmax(Y), xgridstyle=1, ygridstyle=1, xticklen=1, yticklen=1, charsize=1.5, /nodata, _EXTRA=ex

	CASE PLOT_TYPE OF

	''	:	oplot, X, Y, psym=8, symsize=1.0

	'H' :	for i=0, nvalidbins-1 do  begin
				oplot, X[*histo.idxarr[histo.valididx[i]]], Y[*histo.idxarr[histo.valididx[i]]], psym=8, color=histo.cols[i], symsize=1.0
			endfor

	'G' :	for i=0, ng-1 do begin
				grpidx = where(G eq g_ele[i])
				oplot, X[grpidx], Y[grpidx], psym=symcat(sym_type[i]), symsize=1.0
			endfor
	'HG':	for i=0, nvalidbins-1 do $
	 			for j=0, ng-1 do begin
;	 				if not ptr_valid(histo.idxarr[i]) then continue
					grpidx = where(G eq g_ele[j])
	 				idx = setintersection(*histo.idxarr[histo.valididx[i]],grpidx)
	 				if idx[0] eq -1 then continue
	 				if sym_type[j] eq 8 then plotsym,0
	 				oplot, [X[idx]], [Y[idx]], color=histo.cols[i], psym=symcat(sym_type[j]), symsize=1.0
	 			endfor
	ENDCASE

	; Add GROUP legend
	if n_elements(G) ne 0 then begin
		plotsym,0
		if n_elements(g_leg_title) eq 0 then begin
			legend, g_leg, corners=corners, psym=sym_type[indgen(ng)], linestyle=intarr(ng) $
				 , pos=[0.5*!X.WINDOW[0], !P.REGION[1]], /norm, box=0, pspacing=1, charsize=1.2
		endif else begin
			legend, [[g_leg_title],g_leg], psym=[[0],sym_type[indgen(ng)]], linestyle=[[-1],intarr(ng)] $
				 , pos=[0.5*!X.WINDOW[0], !P.REGION[1]], /norm, box=0, pspacing=1, charsize=1.2
		endelse
	endif

	; Add HISTO legend
	if n_elements(H) ne 0 then begin
		usersym, [-2,-2,2, 2,-2], [-1, 1,1,-1,-1], /fill	;rectangle
		if PLOT_TYPE eq 'HG' then h_leg_pos = [!X.WINDOW[1]-h_leg_xydims[0],!P.REGION[1]] else h_leg_pos = [0.5*!X.WINDOW[0], !P.REGION[1]]
		if n_elements(H_leg_title) eq 0 then begin
		legend, histo.leg, psym=replicate(8,nvalidbins), linestyle=intarr(nvalidbins) $
			  , color=histo.cols, pos=h_leg_pos, /norm, box=0, pspacing=1, charsize=1.2
		endif else begin
			legend, [[H_leg_title], histo.leg], psym=[[0],replicate(8,nvalidbins)], linestyle=[[-1],intarr(nvalidbins)] $
			  , color=[[0],histo.cols], pos=h_leg_pos, /norm, box=0, pspacing=1, charsize=1.2
		endelse
	endif

	; Interactive show of tracknums of points of interest! che figata!
	if keyword_set(CURSOR) then $
		if n_elements(tr) eq n_elements(X) then begin
			print, 'Place the cursor over point of interest, and left-click! (Right-click to exit)'
			CURSOR, X1, Y1, /DOWN
			WHILE (!MOUSE.button NE 4) DO BEGIN
				meritfunc = abs(X-X1) + abs(Y-Y1)
				tridx = where(meritfunc eq min(meritfunc))
				print, 'Selected tracknum: '+tr[tridx]
				CURSOR, X1, Y1, /DOWN
			ENDWHILE
		endif else message, '[X,Y] and tr not compatible dimensions',/info


	;Free pointers in histo:
	if n_elements(HISTO_VAR) gt 0 then ptr_free, histo.idxarr
	!P.REGION = 0

END
