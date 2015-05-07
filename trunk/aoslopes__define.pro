
;+
;
;-

function AOslopes::Init, root_obj, slopes_file, fc_obj, store_label=store_label
	if not file_test(slopes_file) then begin
        message, slopes_file + ' not found', /info
        return,0
    endif
    self._file = slopes_file
    self._fc_obj = fc_obj
    self._wfs_status = root_obj->wfs_status()
    self._fitsheader = ptr_new(headfits(self._file ,/SILENT), /no_copy)

    if not keyword_set(store_label) then store_label=''

    self._store_fname = filepath(root=root_obj->elabdir(), store_label+'.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), store_label+'_psd.sav')
    self._store_peaks_fname = filepath(root=root_obj->elabdir(), store_label+'_peaks.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
        file_delete, self._store_peaks_fname, /allow_nonexistent
    endif

    if not obj_valid(fc_obj) then return,0
    if not self->AOtime_series::Init(fc_obj->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
	self._spectra_units = textoidl('a.u.')
	self._plots_title = root_obj->tracknum()

    ;self->datiProducer
    ;self->AOtime_series::Compute

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOslopes', 'Represent measured slopes') then return, 0
    self->addMethodHelp, "fname()", "fitsfile name (string)"
    self->addMethodHelp, "header()", "header of fitsfile (strarr)"
    self->addMethodHelp, "slopes()", "return slopes matrix [nslopes x niter]"
    self->addMethodHelp, "nslopes()", "return number of slopes"
    self->addMethodHelp, "niter()", "return number of iteration (eventually after reforming using lost frames infos)"
    self->addMethodHelp, "sx( [subap_idx=subap_idx, iter_idx=iter_idx] )", "return x-slopes [nsubaps x niter]."
    self->addMethodHelp, "sy( [subap_idx=subap_idx, iter_idx=iter_idx] )", "return y-slopes [nsubaps x niter]."
    self->addMethodHelp, "slopes2d( [iter_idx=iter_idx] )", "return cube with remapped slopes in 2D."
    self->addMethodHelp, "replay[,wait=wait ,zoom=zoom]", "Replays the slopes history in 2D."
    self->addMethodHelp, "hist[,xhist=xhist, yhist=yhist, cumulated=cumulated]", "plot slopes histogram or cumulated histogram (cumulated keyword)."
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOslopes::datiProducer
    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        slopes = readfits(self._file, /SILENT)
        slopes = transpose(temporary(slopes))
        slopes  = interpolate_with_frames_counter(slopes, self._fc_obj)

        save, slopes, file=self._store_fname
    endelse
    self._slopes = ptr_new(slopes, /no_copy)

end


function AOslopes::fname
    return, self._file
end

function AOslopes::Header
    if (PTR_VALID(self._fitsheader)) THEN return, *(self._fitsheader) else return, 0d
end

function AOslopes::Slopes
    slopesPtr = self->GetDati()
    return, *(slopesPtr)
end

function AOslopes::NSlopes
    return, self->AOtime_series::nseries()
end

function AOslopes::NIter
    return, self->AOtime_series::niter()
end

; to be implemented in AOtime_series subclasses
function AOslopes::GetDati
    if not ptr_valid(self._slopes) then self->datiProducer
    return, self._slopes
end

function AOslopes::wfs_status
	return, self._wfs_status
end

; returns Sx
function AOslopes::sx, subap_idx=subap_idx, iter_idx=iter_idx, slopevec=slopevec

	nsub = ((self->wfs_status())->pupils())->nsub()
	niter = self->niter()

	if n_elements(subap_idx) ne 0 then begin
		if max(subap_idx) ge nsub then message, "Maximum subapertures index: "+strtrim(nsub-1,2)
	endif else subap_idx = lindgen(nsub)

	if n_elements(iter_idx) ne 0 then begin
		if max(iter_idx) ge niter then message, "Maximum number of iterations: "+strtrim(niter,2)
	endif else iter_idx  = lindgen(niter)

	if n_elements(slopevec) eq 0 then begin
		sl = self->slopes()
		sl = sl[*,0:nsub*2-1]
		sx = sl[*,0:*:2]
		sx = sx[*,subap_idx]
		sx = sx[iter_idx,*]
	endif else begin
		if n_elements(slopevec) lt nsub*2L then message, 'SLOPEVEC has wrong dimensions'
		sl = reform(slopevec)
		sl = sl[0:nsub*2-1]
		sx = sl[0:*:2]
		sx = sx[subap_idx]
	endelse
	return, sx
end

; returns Sy
function AOslopes::sy, subap_idx=subap_idx, iter_idx=iter_idx, slopevec=slopevec

	nsub = ((self->wfs_status())->pupils())->nsub()
	niter = self->niter()

	if n_elements(subap_idx) ne 0 then begin
		if max(subap_idx) ge nsub then message, "Maximum subapertures index: "+strtrim(nsub-1,2)
	endif else subap_idx = lindgen(nsub)

	if n_elements(iter_idx) ne 0 then begin
		if max(iter_idx) ge niter then message, "Maximum number of iterations: "+strtrim(niter,2)
	endif else iter_idx  = lindgen(niter)

	if n_elements(slopevec) eq 0 then begin
		sl = self->slopes()
		sl = sl[*,0:nsub*2-1]
		sy = sl[*,1:*:2]
		sy = sy[*,subap_idx]
		sy = sy[iter_idx,*]
	endif else begin
		if n_elements(slopevec) lt nsub*2L then message, 'SLOPEVEC has wrong dimensions'
		sl = reform(slopevec)
		sl = sl[0:nsub*2-1]
		sy = sl[1:*:2]
		sy = sy[subap_idx]
	endelse
	return, sy
end

; return remapped signal vector
function AOslopes::slopes2d, iter_idx=iter_idx, slopevec=slopevec

	if n_elements(iter_idx) eq 0 then niter = self->niter() else niter=n_elements(iter_idx)
	if n_elements(slopevec) ne 0 then niter=1

	mypup = 0	;use this pupil info to remap signals
	nsub = ((self->wfs_status())->pupils())->nsub()
	indpup = ((self->wfs_status())->pupils())->indpup()
	fr_sz =80/((self->wfs_status())->ccd39())->binning()		;pixels

	cx  = (((self->wfs_status())->pupils())->cx())[mypup]
	cy  = (((self->wfs_status())->pupils())->cy())[mypup]
	rad = (((self->wfs_status())->pupils())->radius())[mypup]
	xr = [floor(cx-rad),ceil(cx+rad)]
	yr = [floor(cy-rad),ceil(cy+rad)]
	sl2d_w = xr[1]-xr[0]+1
	sl2d_h = yr[1]-yr[0]+1

	sx = self->sx(iter_idx=iter_idx, slopevec=slopevec)
	sy = self->sy(iter_idx=iter_idx, slopevec=slopevec)

	s2d = fltarr(fr_sz,fr_sz)
	sl_2d = fltarr(sl2d_w*2, sl2d_h, niter)
	for kk=0L, long(niter)-1 do begin
		if n_elements(slopevec) eq 0 then s2d[indpup[*,mypup]] = sx[kk,*] else $
			s2d[indpup[*,mypup]] = sx
		s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
		if n_elements(slopevec) eq 0 then s2d[indpup[*,mypup]] = sy[kk,*] else $
			s2d[indpup[*,mypup]] = sy
		s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
		sl_2d[*,*,kk] = [s2d_tmpA,s2d_tmpB]
	endfor
	return, sl_2d
end

;Replay the slopes in 2D
pro AOslopes::replay, wait=wait, zoom=zoom
	if n_elements(wait) eq 0 then wait=0.01
	if not keyword_set(zoom) then zoom=1
	sl = self->slopes2d()
	dim = (size(sl,/dim))[0:1]
	rr = minmax(sl)

    window,/free, xsize=dim[0]*zoom, ysize=dim[1]*zoom
    print, 'Type "s" to stop!'
	for ii=0, self->niter()-1 do begin
;		tv, rebin(bytscl(sl[*,*,ii], min=rr[0], max=rr[1]), dim[0]*zoom, dim[1]*zoom)
		tv, rebin(bytscl(sl[*,*,ii], min=-1., max=1.), dim[0]*zoom, dim[1]*zoom)
;		cgImage, rebin(bytscl(sl[*,*,ii], min=-1., max=1.), dim[0]*zoom, dim[1]*zoom)
		wait, wait
		key = get_kbrd(0.01)
		if STRLOWCASE(key) eq 's' then break
	endfor
end

;slopes histogram
pro AOslopes::hist, xhist=xhist, yhist=yhist, cumulated=cumulated

  if not keyword_set(cumulated) then cumulated = 0
  
  slopes = self->slopes()
  optg = (self->wfs_status())->optg()
  ; normalizes the slopes if optg has been saved
  if n_elements(optg) gt 0 then if optg gt 0 then slopes *= optg
  ; find where the slopes are always zero
  idx0 = (where(total(abs(slopes),1) eq 0))[0]
  ; histogram plot
  plothist, slopes[*,0:idx0-1], bin=0.1, xhist, yhist, /noplot
  if cumulated then begin 
    idxg0 = where(xhist ge 0)
    idxl0 = where(xhist le 0)
    if n_elements(idxg0) gt n_elements(idxl0) then begin
      xcum = xhist[idxg0]
      ycum = yhist[idxg0]
      ytemp = reverse(yhist[where(xhist lt 0)])
      ycum[1:n_elements(ytemp)] += ytemp
    endif else begin
      xcum = abs(reverse(xhist[idxl0]))
      ycum = reverse(yhist[idxl0])
      ytemp = yhist[where(xhist gt 0)]
      ycum[1:n_elements(ytemp)] += ytemp
    endelse
    ycum = total(ycum,/cum)/total(ycum)
    window, /free, xs=640, ys=480
    plot, xcum, ycum, xst=17, yst=17, xtit='!17slopes value', ytit='!17ratio'
    oplot, [1,1], [0,1], line=2
  endif else begin
    window, /free, xs=640, ys=480
    plot, xhist, yhist/total(yhist), xtit='!17slopes value', ytit='!17occurence (normalized)'
  endelse

end


pro AOslopes::Free
    if ptr_valid(self._slopes) then ptr_free, self._slopes
    self->AOtime_series::free
end

pro AOslopes::Cleanup
    if ptr_valid(self._slopes) then ptr_free, self._slopes
    if ptr_valid(self._fitsheader) then ptr_free, self._fitsheader
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOslopes__define
    struct = { AOslopes, $
        _file             : "", $
        _fitsheader       :  ptr_new(), $
        _slopes           :  ptr_new(), $
        _fc_obj           :  obj_new(), $
        _wfs_status		  :  obj_new(), $
        _store_fname      : "", $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }
end

