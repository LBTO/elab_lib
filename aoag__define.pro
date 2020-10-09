
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
    self._wfs_status = obj_new('AOwfs_status', self, wfs_status_file)
    if not obj_valid(self._wfs_status) then message, 'Warning: wfs object not available!', /info ;return, 0

    self._old_plot_fnames = file_search(filepath(root=self._datadir, 'plot*step*.bmp'))
    self._new_plot_fnames = file_search(filepath(root=self._datadir, 'plot*_.bmp'))



    if not self->AOhelp::Init('AOag', 'Autogain data container') then return, 0
    if obj_valid(self._wfs_status) then self->addleaf, self._wfs_status, 'wfs_status'

    self->addMethodHelp, "old_plot_fnames()", "old-style plot filenames"
    self->addMethodHelp, "new_plot_fnames()", "new-style plot filenamew"
    return, 1
end

function AOag::old_plot_fnames
    return, self._old_plot_fnames
end

function AOag::new_plot_fnames
    return, self._new_plot_fnames
end

pro AOag::free
end

pro AOag::Cleanup
  self->AOhelp::Cleanup
end

pro AOag__define
struct = { AOag, $
    _root_obj          : obj_new(), $
    _old_plot_fnames   : obj_new(), $
    _new_plot_fnames   : obj_new(), $
    INHERITS    AOhelp 		        $
}
end


