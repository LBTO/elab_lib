
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

    self.readconf
    self.readsteps

    if not self->AOhelp::Init('AOag', 'Autogain data container') then return, 0
    if obj_valid(self._wfs_status) then self->addleaf, self._wfs_status, 'wfs_status'

    self->addMethodHelp, "nsteps()", "returns the number of measurements steps"
    self->addMethodHelp, "step(n)", "returns a single step object"
    self->addMethodHelp, "recpath()", "returns the reconstructor file path"
    self->addMethodHelp, "conf(key)", "returns a configuration value"
    self->addMethodHelp, "old_plot_fnames()", "old-style plot filenames"
    self->addMethodHelp, "new_plot_fnames()", "new-style plot filenames"
    self->addMethodHelp, "gains()", "optimal gains found by autogain measurement"
    self->addMethodHelp, "plot_new", "attempt to replicate the autogain plots"
    self->addMethodHelp, "wfs_status()", "reference to wfs status object (AOwfs_status)"
    return, 1
end

function AOag::recpath
    gainfile = filepath(root=self._root_obj.datadir(), 'gains_step1.fits')
    dummy = readfits(gainfile, hdr)
    m2c = aoget_fits_keyword(hdr, 'M2C')
    rec = aoget_fits_keyword(hdr, 'REC')
    ;; TODO generalize path
    recpath = '/aodata/lbtdata/adsecsx/adsec_calib/M2C/'+strtrim(m2c)+'/RECs/'+strtrim(rec)
    return, recpath
end


pro AOag::readconf
   conffile = filepath(root=self._root_obj.datadir(), 'conf.txt')
   line=''
   tags = strarr(1)
   values = strarr(1)
   tags[0] = 'dummy'
   values[0] = 'dummy'

   openr, lun, conffile, /get_lun
   while not eof(lun) do begin
     readf, lun, line
     if line ne '' then begin
         tokens = strsplit(line, /extract)
         tag = tokens[0]
         tag = strmid(tag,0,strlen(tag)-1)
         value = strjoin(tokens[1:*], ' ')
         tags = [tags, tag]
         values = [values, value]
     endif
   endwhile

   if ptr_valid(self._conftags) then ptr_free, self._conftags
   if ptr_valid(self._confvalues) then ptr_free, self._confvalues

   self._conftags = ptr_new(tags)
   self._confvalues = ptr_new(values)
end

function AOag::_actuated_group, gains
    return, where(max(gains,dim=2) - min(gains,dim=2))
end

pro AOag::readsteps

    gain_fnames = file_search(filepath(root=self._root_obj.datadir(), 'gains_step*.fits'))
    prev_actuated_group = -1
    sequence = strsplit(self.conf('sequence'), /extract)

    self._steps = ptr_new(objarr(n_elements(gain_fnames)))

    for step=1,n_elements(gain_fnames) do begin
        ; Regenerate gain_fname to get rid of problems with lexical sorting
        gain_fname =filepath(root=self._root_obj.datadir(), 'gains_step'+strtrim(step,2)+'.fits')
        gains = readfits(gain_fname)
        if self._actuated_group(gains) ne prev_actuated_group then begin
            first_step = step
            prev_actuated_group = self._actuated_group(gains)
            target = sequence[0]
            if n_elements(sequence) gt 1 then sequence = sequence[1:*]
            iterations = 0
        endif

        iterations = iterations +1
        stepobj = obj_new('AOagstep', self, self._root_obj, first_step, step, target, fix(self.conf('ho_middle')))
        (*self._steps)[step-1] = stepobj
     endfor


end

function AOag::step, n
   return, (*self._steps)[n-1]
end

function AOag::conf, key

   for i=0, n_elements(*self._conftags) do begin
      if (*self._conftags)[i] eq key then return, (*self._confvalues)[i]
   endfor
   return, ''

end

function AOag::nsteps
    return, n_elements(*self._steps)
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

function AOag::wfs_status
    IF OBJ_VALID(self._wfs_status) THEN return, self._wfs_status else return, obj_new()
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
;    Removed because aodataset::values calls his method.
;    To be investigated...
;
;    if ptr_valid(self._old_plot_fnames) then ptr_free, self._old_plot_fnames
;    if ptr_valid(self._new_plot_fnames) then ptr_free, self._new_plot_fnames
;    if ptr_valid(self._gains) then ptr_free, self._gains
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
    _conftags          : ptr_new(), $
    _confvalues        : ptr_new(), $
    _steps             : ptr_new(), $
    INHERITS    AOhelp 		        $
}
end


