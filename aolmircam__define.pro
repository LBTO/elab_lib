
;+
; aolmircam object initialization
;-

function aolmircam::Init, root_obj, psf_fname, dark_fname
  if psf_fname eq '' then return,0

  if not file_test(psf_fname) then begin
    message, psf_fname + ' not found', /info
    return,0
  endif
  fitsheader = headfits(psf_fname, /SILENT, errmsg=errmsg)
  if errmsg ne ''  then message, psf_fname+ ': '+ errmsg, /info

  ;Camera Temperature (K)
  ; not sure which 'TEMP' should be used...
  self._lmircam_temp = float(aoget_fits_keyword(fitsheader, 'DETTEMP'))

  ;Camera
  ;self._camera_name= strtrim(aoget_fits_keyword(fitsheader, 'DETECTOR'), 2)
  self._camera_name= strtrim(aoget_fits_keyword(fitsheader, 'INSTRUMENT'), 2)

  ; Pixelscale (arcsec):
  pixelscale = 0.0107

  ; Detect filter:
  ;self._filter_name = strtrim(aoget_fits_keyword(fitsheader, 'FILTERS'),2)
  ; how does this changes for LMIRCAM ?

  self._filter_nameA = strtrim(aoget_fits_keyword(fitsheader, 'LM_FW25'),2)
  self._filter_nameB = strtrim(aoget_fits_keyword(fitsheader, 'LMIR_FW2'),2)
  self._filter_nameC = strtrim(aoget_fits_keyword(fitsheader, 'LMIR_FW4'),2)
  
  self._filter_name = self._filter_nameA

  valid_filt_number = 1B
  CASE strtrim(self._filter_nameA,2) OF
    'MK-J':           lambda = 1.25e-6
    'H':              lambda = 1.65e-6
    'Kshort':         lambda = 2.16e-6
    else: begin
      self._filter_name = self._filter_nameB
      CASE strtrim(self._filter_nameB,2) OF
        'Fe-II':          lambda = 1.645e-6
        else: begin
          self._filter_name = self._filter_nameC
          CASE strtrim(self._filter_nameC,2) OF
            'Std-L':          lambda = 3.70e-6
            'Std-M':          lambda = 4.78e-6
            else: begin
              lambda = !VALUES.F_NAN
              msg_temp = 'Unknown lmircam filter <'+self._filter_name+'>'
              message, msg_temp, /info
              self._lmircam_err_msg += ' - ' + msg_temp
              valid_filt_number = 0B
            end
          ENDCASE
        end
      ENDCASE
    end
  ENDCASE

  ; Exposure time:
  exptime = float(aoget_fits_keyword(fitsheader, 'EXPTIME'))	;in seconds
  valid_exptime = 1B
  if exptime eq 0 then begin
    msg_temp = 'Unknown lmircam exposure time'
    message, msg_temp, /info
    self._lmircam_err_msg += ' - ' + msg_temp
    valid_exptime = 0B
  endif

  ;Framerate:
  framerate =  1./exptime  ; TODO

  ; Frame size
  frame_w  = long(aoget_fits_keyword(fitsheader, 'NAXIS1'))
  frame_h  = long(aoget_fits_keyword(fitsheader, 'NAXIS2'))

  ;ra  = (root_obj->tel())->ra()
  ;dec = (root_obj->tel())->dec()
  ;stages_x = ((root_obj->wfs_status())->stages())[0]
  ;stages_y = ((root_obj->wfs_status())->stages())[1]
  ;thisJulday = (root_obj->obj_tracknum())->JulDay()
  ;dark = self->find_dark_from_tn(thisJulday, dark_subdir, exptime, filter_tag, frame_w, frame_h, ra, dec, stages_x, stages_y, err_msg=dark_err_msg)
  ;stop
  ;Dark Frame:
  if obj_valid(root_obj->wfs_status()) then begin
    dark_subdir = ['wfs_calib_'+(root_obj->wfs_status())->wunit(),'lmircam','backgrounds','bin1']
    if (n_elements(dark_fname) eq 0) then begin
      if valid_exptime and valid_filt_number then begin
        thisJulday = (root_obj->obj_tracknum())->JulDay()
        full_dark_fname = self->find_dark(thisJulday, dark_subdir, exptime, self._filter_name, frame_w, frame_h, err_msg=dark_err_msg)
        if strtrim(dark_err_msg,2) ne '' then self._lmircam_err_msg += dark_err_msg
      endif else begin
        msg_temp = 'lmircam dark cannot be searched: ('
        if not valid_exptime then msg_temp += 'exptime '
        if not valid_filt_number then msg_temp += 'filter '
        msg_temp += 'unknown)'
        message, msg_temp, /info
        self._lmircam_err_msg += ' - ' + msg_temp
      endelse
    endif else begin
      full_dark_fname = filepath(root=ao_datadir(), sub=dark_subdir,  dark_fname)
      if not file_test(full_dark_fname) then begin
        msg_temp = 'Overidden lmircam dark file does not exist'
        message, msg_temp, /info
        self._lmircam_err_msg += ' - ' + msg_temp
      endif
    endelse


    ;Badpixelmap filename:
    badpixelmap_fname = filepath(root=ao_datadir(), sub=dark_subdir, 'badpixelmap.sav')
  endif

  store_radix = filepath(root=root_obj->elabdir(), 'lmircam')
  
  self._lmircam = 1B

  ; initialize PSF object
  if not self->AOscientificimage::Init(root_obj, psf_fname, full_dark_fname, pixelscale, lambda, exptime, framerate, $
    self._lmircam, badpixelmap_fname=badpixelmap_fname, store_radix=store_radix, recompute=root_obj->recompute()) then return,0

  ; Override diameter & obstruction + dependent parameters
  self._pupdiam = 8.222
  self._oc = 0.12
  self._pixelscale_lD = self._pixelscale / ((self->lambda()/self->pupdiam())/4.848d-6)
  nsup = 40.  ;maximum radius to ensure all star light is in.
  self._object_size = nsup * self->lambda() / self->pupdiam() / 4.85e-6 / self._pixelscale

  ; initialize help object and add methods and leafs
  if not self->AOhelp::Init('aolmircam', 'lmircam image') then return, 0
  self->addMethodHelp, "temp()", "lmircam temperature (K)"
  self->addMethodHelp, "filter_name()", "filter name ['empty J', ...]"
  self->addMethodHelp, "camera()", "camera name ['N30 Camera', ...]"
  self->AOscientificimage::addHelp, self

  return, 1
end

pro aolmircam::summary
  self->AOscientificimage::summary
  print, string(format='(%"%-30s %s")','Camera name', self->camera() )
  print, string(format='(%"%-30s %s")','Filter name', self->filter_name() )
  print, string(format='(%"%-30s %f")','Temperature (K)', self->temp() )
end


;Searches a lmircam dark closest in time to the lmircam image with the same set of parameters
;------------------------------------------------------------------------------------------
function aolmircam::find_dark, thisJulday, dark_subdir, exptime, filter_tag, frame_w, frame_h, err_msg=err_msg

  err_msg = ""

  ; Verify that the dark and the psf image were taken with the same exposure time!
  all_darks_search = filepath(root=ao_datadir(), sub=dark_subdir,  '*_cube.fits')
  all_darks_fname  = file_search(all_darks_search, count=ndarks)

  if ndarks gt 0 then begin
    all_darks_julday = dblarr(ndarks)
    for ii=0, ndarks-1 do begin
      dark_tracknum = strmid(file_basename(all_darks_fname[ii]), 0, 15)
      y  = fix(strmid(dark_tracknum,  0, 4))
      m  = fix(strmid(dark_tracknum,  4, 2))
      d  = fix(strmid(dark_tracknum,  6, 2))
      hh = fix(strmid(dark_tracknum,  9, 2))
      mm = fix(strmid(dark_tracknum, 11, 2))
      ss = fix(strmid(dark_tracknum, 13, 2))
      all_darks_julday[ii] = julday(m, d, y, hh, mm, ss)
    endfor
    idx_closest = sort(abs(all_darks_julday - thisjulday))

    dark_found = 0B & dd=0
    while (dark_found eq 0B) and (dd lt ndarks) do begin
      ;Retrieve header info of averaged dark frame because the file with the cube didn't have any info saved in the header...
      closest_av_dark_fname = filepath(root=ao_datadir(), sub=dark_subdir, file_basename(all_darks_fname[idx_closest[dd]], '_cube.fits'))
      dark_header = headfits(closest_av_dark_fname)
      dark_exptime = float(aoget_fits_keyword(dark_header, 'EXPTIME'))	;in seconds

      dark_filter_nameA = strtrim(aoget_fits_keyword(dark_header, 'LM_FW25'), 2)
      dark_filter_nameB = strtrim(aoget_fits_keyword(dark_header, 'LMIR_FW2'), 2)
      dark_filter_nameC = strtrim(aoget_fits_keyword(dark_header, 'LMIR_FW4'), 2)

      dark_filter_name = dark_filter_nameA

      CASE strtrim(dark_filter_nameA,2) OF
        'MK-J':           lambdad = 1.25e-6
        'H':              lambdad = 1.65e-6
        'Kshort':         lambdad = 2.16e-6
        else: begin
          dark_filter_name = dark_filter_nameB
          CASE strtrim(dark_filter_nameB,2) OF
            'Fe-II':          lambdad = 1.645e-6
            else: begin
              dark_filter_name = dark_filter_nameC
              CASE strtrim(dark_filter_nameC,2) OF
                'Std-L':          lambdad = 3.70e-6
                'Std-M':          lambdad = 4.78e-6
                else: begin
                  lambdad = !VALUES.F_NAN
                  msg_temp = 'Unknown lmircam DARK filter <'+self._filter_name+'>'
                  message, msg_temp, /info
                  self._lmircam_err_msg += ' - ' + msg_temp
                end
              ENDCASE
            end
          ENDCASE
        end
      ENDCASE

      dark_frame_w = long(aoget_fits_keyword(dark_header, 'NAXIS1'))
      dark_frame_h = long(aoget_fits_keyword(dark_header, 'NAXIS2'))
      if (dark_exptime eq exptime) and (strtrim(filter_tag,2) eq strtrim(dark_filter_name,2)) and  $
        (dark_frame_w eq frame_w) and (dark_frame_h eq frame_h) then dark_found=1B else dd+=1
    endwhile
    if dark_found then begin
      dark_fname = all_darks_fname[idx_closest[dd]]
      time_elapsed = abs(all_darks_julday[idx_closest[dd]] - thisjulday)
      max_time_elapsed = julday(01, 01, 2010, 01, 00, 00) -  julday(01, 01, 2010, 00, 00, 00)
      if time_elapsed gt max_time_elapsed then begin
        msg_temp = 'Warning: Selected lmircam dark +'+strtrim(round(time_elapsed/max_time_elapsed),2)+'h old!'
        message, msg_temp, /info
        err_msg += ' - ' + msg_temp
      endif
    endif else begin
      msg_temp = 'No compatible lmircam dark found (i.e. same exposure time or filter or dimensions)'
      message, msg_temp, /info
      err_msg += ' - ' + msg_temp
      return, ""
    endelse
  endif else begin
    msg_temp = 'No lmircam darks found matching '+all_darks_search
    message, msg_temp, /info
    err_msg += ' - ' + msg_temp
    return, ""
  endelse

  return, dark_fname
end

;function aolmircam::find_dark_from_tn, thisJulday, dark_subdir, exptime, filter_tag, frame_w, frame_h, ra, dec, stages_x, stages_y, err_msg=err_msg
;     set    = obj_new('aodataset', from=thisJulday-1d/48, to=thisJulday+1d/48) ; set of tn close in time to this
;     setl   = set->where('meas_type', 'eq', 'LOOP') ; only LOOP, no AutoGain, SlopesNull etc
;     xxx = 1d/60
;     setra  = setl->where('tel.ra',   'between', [ra-xxx*15, ra+xxx*15])  ; similar telescope pointing
;     setdec = setra->where('tel.dec', 'between', [dec-xxx, dec+xxx])  ; similar telescope pointing
;
;    ; find
;
;    return, setdec
;end

function aolmircam::camera
  return, self._camera_name
end


function aolmircam::filter_name
  return, self._filter_name
end

function aolmircam::temp
  return, self._lmircam_temp
end

function aolmircam::linearize, frame

  return, frame + 4.218*1e-6*(frame^2)
end

;function aolmircam::imagecube
;  stop
;  if ~(PTR_VALID(self._imagecube)) THEN self->process_cube
;  dim = size(*(self._imagecube),/dim)
;  tmp = (*(self._imagecube))[*,*,1:dim[2]-1:2]-(*(self._imagecube))[*,*,0:dim[2]-2:2]
;  return, tmp
;end

;Returns the error messages
;-----------------------------------------------------
function aolmircam::isok, cause=cause
  isok=1B
  isok *= self->AOscientificimage::isok(cause=cause)
  if strtrim(self._lmircam_err_msg,2) ne '' then begin
    isok*=0B
    cause += self._lmircam_err_msg
  endif
  return, isok
end

pro aolmircam__define
  struct = { aolmircam				, $
    _camera_name         : ""       , $
    _lmircam_temp           : 0.0      , $
    _filter_name         : ""       , $
    _filter_nameA        : ""       , $
    _filter_nameB        : ""       , $
    _filter_nameC        : ""       , $
    _lmircam_err_msg        : ""       , $
    INHERITS  AOscientificimage     , $
    INHERITS  AOhelp                  $
  }
end
