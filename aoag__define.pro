
;+
; autogain data.
;
; Wrapper object that holds all data in a single gain optimization object,
; used to avoid namespace clashes with normal aodata in an aoelab dataset.
;-

function AOag::Init, root_obj
    self._root_obj = root_obj

    ; create wfs_status leaf
    wfs_status_file = filepath(root=root_obj.datadir(), 'wfs.fits')
    self._wfs_status = obj_new('AOwfs_status', root_obj, wfs_status_file)
    if not obj_valid(self._wfs_status) then message, 'Warning: wfs object not available!', /info ;return, 0

    self._old_plot_fnames = ptr_new(file_search(filepath(root=root_obj.datadir(), 'plot*step*.bmp')))
    self._new_plot_fnames = ptr_new(file_search(filepath(root=root_obj.datadir(), 'plot*_.bmp')))

    optimal_gains_filename = filepath(root=root_obj.datadir(), 'optimal_gains.txt')
    if file_test(optimal_gains_filename) then begin
        gains = read_ascii(optimal_gains_filename, delimiter=':')
        gains = gains.field1[1,*]
    endif else begin
        message,'Warning: optimal gains file no found', /info
        gains = [0,0,0]
    endelse

    self._gains = ptr_new(gains)

    if not self->AOhelp::Init('AOag', 'Autogain data container') then return, 0
    if obj_valid(self._wfs_status) then self->addleaf, self._wfs_status, 'wfs_status'

    self->addMethodHelp, "old_plot_fnames()", "old-style plot filenames"
    self->addMethodHelp, "new_plot_fnames()", "new-style plot filenamew"
    self->addMethodHelp, "gains()", "optimal gains found by autogain measurement"
    return, 1
end

function AOag::old_plot_fnames
    return, self._old_plot_fnames
end

function AOag::new_plot_fnames
    return, self._new_plot_fnames
end

function AOag::gains
    return, (*self._gains)[0:2]
end

pro AOag::old_plot, i

    images = *(self.old_plot_fnames())
    if n_elements(images) lt 2 then begin
        print,'There are no plots to show'
        return
    endif

    img = read_image(images[i])  
    tvscl, img[0, *, *]

end

pro AOag::plot_new, dim=dim, wnum=wnum

    if n_elements(dim) eq 0 then dim = 400
    if n_elements(wnum) eq 0 then wnum=0

    images = *(self.new_plot_fnames())
    if n_elements(images) lt 2 then begin
        print,'There are no plots to show'
        return
    endif

    window, wnum, xsize=dim*3, ysize=dim*2

    for i=0, n_elements(images)-1 do begin

        pieces = strsplit(images[i], '_', /extract)
        row = fix(pieces[n_elements(pieces)-4])
        col = fix(pieces[n_elements(pieces)-3])
        
        idx = row*3+col

        img = read_image(images[i])
        self.plot_single, img, idx, dimx=dim, dimy=dim

    endfor

end

pro AOag::plot_single, img, idx, dimx=dimx, dimy=dimy

    if not keyword_set(dimx) then dimx = n_elements(r[*,0])
    if not keyword_set(dimy) then dimy = n_elements(r[0,*])
    tv, rebin(img, 3, dimx, dimy), true=1, idx
end

pro AOag::free
    if ptr_valid(self._old_plot_fnames) then ptr_free, self._old_plot_fnames
    if ptr_valid(self._new_plot_fnames) then ptr_free, self._new_plot_fnames
    if ptr_valid(self._gains) then ptr_free, self._gains
end

pro AOag::Cleanup
  self->AOhelp::Cleanup
end

pro AOag__define
struct = { AOag, $
    _root_obj          : obj_new(), $
    _wfs_status        : obj_new(), $
    _old_plot_fnames   : ptr_new(), $
    _new_plot_fnames   : ptr_new(), $
    _gains             : ptr_new(), $
    INHERITS    AOhelp 		        $
}
end


