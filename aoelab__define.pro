
;+
;
;-


function AOelab::Init, tracknum, $
  recompute = recompute, $
  modal_reconstructor_file = modal_reconstructor_file, $	; this is used in case of kalman filter
  dark_fname = dark_fname, $                                  ; override IRTC/PISCES dark filename
  SILENT=SILENT                                               ; Do not print warnings for non-existant TNs

  self._recompute = keyword_set(recompute)

  date = strmid(tracknum, 0, 8)
  self._datadir = filepath(root=ao_datadir(), sub=['adsec_data', date, 'Data_'+tracknum], '')
  self._elabdir = filepath(root=ao_elabdir(), sub=[date, 'Data_'+tracknum], '')


  ;verify that the datadir exists before anything.
  if not FILE_TEST(self._datadir, /DIR) then begin
    if not keyword_set(SILENT) then print, 'data directory does not exist: Data_'+tracknum
    if not keyword_set(SILENT) then print, 'complete path is:'+self._datadir
    return, 0
  endif

  ;Skip empty directories
  dummy = FILE_SEARCH(self._datadir+path_sep()+'*', count=count)
  if count eq 0 then begin
    if not keyword_set(SILENT) then print, 'data directory is empty: Data_'+tracknum
    return, 0
  endif


  ; Detect measurement type
  typefile = filepath(root=self._datadir,'type.fits')
  if file_test(typefile) then begin
    dummy = readfits(typefile, hdr)
    meas_type = aoget_fits_keyword(hdr, 'MEASTYPE')
    case meas_type of
      'AG': begin
        print, 'gain optimization data: Data_'+tracknum
        self._meas_type = 'AG'
      end
      'NC': begin
        print, 'non-commonpath aberration: Data_'+tracknum
        self._meas_type = 'NCPA'
      end
      'LOOP': begin
        self._meas_type = 'LOOP'
      end
      ELSE: begin
        message,'Unknown measurement type: '+strtrim(meas_type,2), /info
        return, 0
      end
    endcase
  endif else begin
    temp = FILE_SEARCH(FILEPATH(root=self._datadir, 'gains_step*.fits'), COUNT=ngainfiles)
    if ngainfiles ne 0 then begin
      print, 'gain optimization data: Data_'+tracknum
      self._meas_type = 'AG'
    endif else begin
      self._meas_type = ''  ; Detect type later, based on file contents
    endelse
  endelse



  ; create elab dir and set permission / owner
  tmp_dir = filepath(root=ao_elabdir(), sub=[date], '')
  if file_test(tmp_dir, /dir) eq 0 then begin
    file_mkdir, tmp_dir
    file_chmod,  tmp_dir, /a_read, /a_write, /g_read, /g_write
  endif
  if file_test(self._elabdir, /dir) eq 0 then begin
    file_mkdir, self._elabdir
    file_chmod,  self._elabdir, /a_read, /a_write
  endif

  ; Override object. must be first of the list (?)
  override_fname = filepath(root=self._datadir,  'override_'+tracknum+'.sav')
  self._override = obj_new('AOoverride', override_fname)

  ; tracknum object
  self._obj_tracknum = obj_new('AOtracknum', tracknum)

  ;;;;; AUTOGAIN wrapper object
  if self._meas_type eq 'AG' then begin

      self._ag = obj_new('AOag', self)
      if not obj_valid(self._ag) then message, 'Warning: autogain data not available!', /info ;return, 0

      if not self->AOhelp::Init('AOag', 'Represents a gain optimization measure') then return, 0
      if obj_valid(self._ag) then self->addleaf, self._ag, 'ag'
      return, 1

  endif

  ; create adsec_status leaf
  adsec_status_file = filepath(root=self._datadir, 'adsec.sav')
  self._adsec_status = obj_new('AOadsec_status', self, adsec_status_file)
  if not obj_valid(self._adsec_status) then message, 'Warning: adsec_status object not available!', /info ;return, 0

  ; create wfs_status leaf
  wfs_status_file = filepath(root=self._datadir, 'wfs.fits')
  self._wfs_status = obj_new('AOwfs_status', self, wfs_status_file)
  if not obj_valid(self._wfs_status) then message, 'Warning: wfs object not available!', /info ;return, 0

  ; Global variables (e.g. telescope diameter and OC, ...)
  if obj_valid(self->wfs_status()) then begin
    ao_global_data_init, (self._wfs_status)->wunit()
    ; create telescope leaf
    self._tel = obj_new('AOtel', self, wfs_status_file)
  endif

  ; Operation mode: "RR"    : @SolarTower, or @Telescope with RR.
  ;         "ARGOScal"    : @Telescope with ARGOS calibration unit.
  ;				  "ONSKY" : @Telescope on-sky!
  if obj_valid(self._tel) then begin
    ;if (self->tel())->el() lt 89. then self._operation_mode = "ONSKY" $
    ; ADI breaks next check!
    ;if not (finite( (self->tel())->rot_angle())) then self._operation_mode = "RR" else begin

    if keyword_set((self->tel())->isTracking()) then self._operation_mode = 'ONSKY' else begin
      if obj_valid(self->wfs_status()) then if strmid((self->wfs_status())->wunit(),0,4) eq 'LBTI' then begin
        if ((self->wfs_status())->cube_stage() gt -10.) then self._operation_mode = 'ARGOScal' $
        else self._operation_mode = 'RR'
      endif else begin
        if obj_valid(self->wfs_status()) then if strmid((self->wfs_status())->wunit(),0,3) eq 'MAG' then begin

          ; MAG does not know where the cube stage is, so we just use the lamp intensity
          if (self->wfs_status())->lamp_intensity() gt .001 then self._operation_mode = "RR" else self._operation_mode = "ARGOScal"
        endif else begin
          if (self->obj_tracknum())->julday() ge 2458879l then begin
            if ((self->wfs_status())->cube_stage() gt -40.) then self._operation_mode = "ARGOScal" else $
              if (self->wfs_status())->lamp_intensity() gt .001 then self._operation_mode = "RR" else begin
              self._operation_mode = "ARGOScal"
              message, 'Warning: Cube stage inside with lamp off.... Better move it out!',/info
            endelse
          endif else begin
            if ((self->wfs_status())->cube_stage() lt -40.) then self._operation_mode = "ARGOScal" else $
              if (self->wfs_status())->lamp_intensity() gt .001 then self._operation_mode = "RR" else begin
              self._operation_mode = "ARGOScal"
              message, 'Warning: Cube stage inside with lamp off.... Better move it out!',/info
            endelse
          endelse

        endelse
      endelse
    endelse
  endif else self._operation_mode = "RR"	;in Solar Tower
  ; operation mode can be overridden
  catch, err
  ;if err ne 0 then catch, /cancel else self._operation_mode = (self->override())->overriden_value('operation_mode')
  if err eq 0 then self._operation_mode = (self->override())->overriden_value('operation_mode')
  catch, /cancel

  ;Single or double reflection
  if self->operation_mode() eq "RR" then self._reflcoef=4. else self._reflcoef=2.

  ; create sanity check leaf
  crcerrors_fname = filepath(root=self._datadir,  'CrcErrors_'+tracknum+'.fits')
  fltimeout_fname = filepath(root=self._datadir,  'FlTimeout_'+tracknum+'.fits')
  loopclosed_fname = filepath(root=self._datadir,  'LoopClosed_'+tracknum+'.fits')
  pendingcounter_fname = filepath(root=self._datadir,  'PendingCounter_'+tracknum+'.fits')
  skipcounter_fname = filepath(root=self._datadir,  'SkipCounter_'+tracknum+'.fits')
  timestamp_fname = filepath(root=self._datadir,  'Timestamp_'+tracknum+'.fits')
  wfsglobaltimeout_fname = filepath(root=self._datadir,  'WFSGlobalTimeout_'+tracknum+'.fits')
  self._sanitycheck = obj_new('AOSanityCheck', $
    crcerrors_fname, $
    fltimeout_fname, $
    loopclosed_fname, $
    pendingcounter_fname, $
    skipcounter_fname, $
    timestamp_fname, $
    wfsglobaltimeout_fname $
    )


  ; create control filter leaf
  if not obj_valid(self._adsec_status) then begin
    message, 'Warning: control object not available!' , /info
  endif else begin
    self._control = obj_new('AOcontrol', self, $
      self._adsec_status->b0_a_file(), $
      self._adsec_status->a_delay_file(), $
      self._adsec_status->b_delay_a_file(), $
      self._adsec_status->c_file(), $
      self._adsec_status->g_gain_a_file(), $
      self._adsec_status->adsec_struct_file() $
      )
  endelse

  ; create frames counter leaf
  frames_counter_file = filepath(root=self._datadir,  'FramesCounter_'+tracknum+'.fits')
  self._frames_counter = obj_new('AOframes_counter', frames_counter_file, self._wfs_status)
  if not obj_valid(self._frames_counter) then message, 'Warning: FramesCounter object not initialized!', /info ;return,0

  ; create frames counter leaf
  valid_pixels_file = filepath(root=self._datadir,  'ValidPixels_'+tracknum+'.fits')
  self._valid_pixels = obj_new('AOvalid_pixels', valid_pixels_file, self._wfs_status)
  if not obj_valid(self._valid_pixels) then message, 'Warning: ValidPixels object not initialized, it is OK for pre-SOUL data.', /info

  ; loop_delay
  self._delay = obj_new('AOdelay', self)

  ; modal_rec
  if keyword_set(modal_reconstructor_file) then begin
    self._modal_rec = getrecmatrix( modal_reconstructor_file )
  endif else begin
    if obj_valid(self._control) then begin
      if (self->control())->isKalman() then begin
        intmat_fname = (self->control())->intmat_fname()
        pos = strpos(intmat_fname, 'Intmat_', /REVERSE_SEARCH)
        if pos eq -1 then message, 'Missing info on interaction matrix', BLOCK='elab', name='ao_oaa_elab'
        rec_fname = strmid(intmat_fname, 0, pos) +'Rec_'+strmid(intmat_fname, pos+7)
      endif else begin
        rec_fname = (self->control())->b0_a_fname()
      endelse
      self._modal_rec = getrecmatrix( rec_fname )
    endif else message, 'Control info not available: Reconstructor object not initialized!', /info
  endelse

  ; (modal) interaction matrix
  if obj_valid(self._control) then begin
    intmat_fname = (self->control())->intmat_fname()
    self._intmat = getintmat( intmat_fname )
  endif else message, 'Control info not available: Interaction Matrix object not initialized!', /info


  ; Modes Shapes
  if obj_valid(self->intmat()) then begin
    basis = (self->intmat())->basis()
    modeshapes_fname = filepath(root=ao_phasemapdir(), 'KLmatrix_'+basis+'.sav')
    if file_test(modeshapes_fname) then self._modeShapes = get_modes_shapes(modeShapes_fname) else $
      message, 'Mode Shapes file not found',/info
  endif else message, 'Unknown modal basis: mode shapes not initialized...', /info


  ; disturb & modaldisturb
  if obj_valid(self->wfs_status()) then begin
    disturb_sync = long(aoget_fits_keyword((self->wfs_status())->header(), "sc.DISTURBANCE"))
    if disturb_sync gt 0 then begin
      ;self._disturb = getdisturb( (self->adsec_status())->disturb_file(), recompute=self._recompute )
      if obj_valid(self->adsec_status()) then self._disturb = obj_new('AOdisturb', self, (self->adsec_status())->disturb_file(), recompute=self._recompute )
      self._modaldisturb = obj_new('AOmodaldisturb', self)
    endif
  endif else message, 'Wfs object not available: disturb object not initialized!', /info

  ; effective number of independent realizations in the real-time data acquired.
  if obj_valid(self._disturb) and obj_valid(self._frames_counter) then $
    self._n_periods = self._frames_counter->nframes() / self._disturb->ind_realizations() else $
    self._n_periods = 1L
  if self._n_periods eq 0 then self._n_periods = 1L

  ; slopes
  slopes_file = filepath(root=self._datadir,  'Slopes_'+tracknum+'.fits')
  self._slopes = obj_new('AOslopes', self, slopes_file, self._frames_counter, store_label='slopes')

  ; residual modes
  if obj_valid(self._slopes) and obj_valid(self._modal_rec) then $
    self._residual_modes = obj_new('AOresidual_modes', self, self._slopes, self._modal_rec, store_label='residual_modes')

  ; modes
  modes_fname = filepath(root=self._datadir,  'Modes_'+tracknum+'.fits')
  self._modes = obj_new('AOmodes', self, modes_fname, self._frames_counter)

  ; commands
  commands_fname = filepath(root=self._datadir,  'Commands_'+tracknum+'.fits')
  self._commands = obj_new('AOcommands', self, commands_fname, self._frames_counter)

  ; modalcommands
  self._modalcommands = obj_new('AOmodalcommands', self)

  ; positions
  positions_fname = filepath(root=self._datadir,  'Positions_'+tracknum+'.fits')
  self._positions = obj_new('AOpositions', self, positions_fname, self._frames_counter)

  ; modalpositions
  self._modalpositions = obj_new('AOmodalpositions', self)

  ; open loop modes
  self._olmodes = obj_new('AOolmodes', self)

  ; wfs camera frames
  frames_fname = filepath(root=self._datadir,  'Frames_'+tracknum+'.fits')
  antidrift_fname = filepath(root=self._datadir, 'AntiDrift_'+tracknum+'.fits')
  self._frames = obj_new('AOframes', self, frames_fname, antidrift_fname)

  ; system timestamps
  timestamps_fname = filepath(root=self._datadir,  'SystemTimestamp_'+tracknum+'.fits')
  self._timestamps = obj_new('AOtimestamps', self, timestamps_fname)

  ; TV
  tv_fnames=file_search(filepath(root=self._datadir, 'psf*.fits'))
  self._tv  = obj_new('AOTV', self, tv_fnames)

  ; IRTC
  irtc_fname = file_search(filepath(root=self._datadir, 'irtc.fits'))
  self._irtc = obj_new('AOIRTC', self, irtc_fname, dark_fname)

  ; PISCES
  ;pisces_fname = file_search(filepath(root=self._datadir, 'pisces.fits'))
  ;self._piscesold = obj_new('aopiscesold', self, pisces_fname, dark_fname)

  ; PISCES
  pisces_fname = file_search(filepath(root=self._datadir, 'pisces.fits.cq'))
  if file_test(pisces_fname) eq 0 then pisces_fname = file_search(filepath(root=self._datadir, 'pisces.fits'))
  self._pisces = obj_new('aopisces', self, pisces_fname, dark_fname)

  ; LUCI
  luci_fname = file_search(filepath(root=self._datadir, 'luci.fits'))
  self._luci = obj_new('AOLUCI', self, luci_fname, dark_fname)

  ; LMIRCAM
  lmircam_fname = file_search(filepath(root=self._datadir, 'lmircam.fits'))
  self._lmircam = obj_new('AOLMIRCAM', self, lmircam_fname, dark_fname)
  
  ; SHARK
  shark_fname = file_search(filepath(root=self._datadir, 'shark.fits'))
  self._shark = obj_new('AOSHARK', self, shark_fname, dark_fname)

  ; offload modes
  pos2mod_fname = filepath(root=ao_datadir(),  'matrix_proiezione_per_lorenzo.sav') ; TODO fix this name
  self._offloadmodes = obj_new('AOoffloadmodes', self, pos2mod_fname)

  ; accelerometers
  dir = ao_datadir()+path_sep()+'accel'
  proj = ao_elabdir()+path_sep()+'accelerometers'+path_sep()+'projection_matrix.fits'
  trnm_acc = tracknum
  temp0 = strmid(trnm_acc, 0, 8)
  temp1 = strmid(trnm_acc, 9)
  if temp0 ge 20101124 then begin
    self._accel = obj_new('AOaccel', self, proj, filepath(root=self._datadir, 'adsec.sav'))
  endif else begin
    flag_acc = 0
    if FILE_TEST(dir+path_sep()+trnm_acc) eq 0 then begin
      t = 'temp1='+temp1+'+1'
      f = execute(t)
      if temp1 lt 95959 then trnm_acc = temp0+'_0'+strtrim(temp1,2) $
      else trnm_acc = temp0+'_'+strtrim(temp1,2)
      if FILE_TEST(dir+path_sep()+trnm_acc) eq 0 then begin
        flag_acc = 1
      endif
    endif
    if flag_acc eq 0 then begin
      acc_file = dir+path_sep()+trnm_acc+path_sep()+'acc.sav'
      self._accel = obj_new('AOaccel', self, proj, acc_file)
    endif
  endelse

  ; slopes null
  if obj_valid(self->wfs_status()) then begin
    slopes_null_basename 	= (self->wfs_status())->slopes_null_fname()
    if slopes_null_basename ne "" then begin
      wunit  = (self->wfs_status())->wunit()
      binning = ((self->wfs_status())->camera())->binning()
      slopes_null_subdir 		= ['wfs_calib_'+wunit,'slopenulls','bin'+strtrim(binning,2)]
      slopes_null_fname = filepath(root=ao_datadir(), sub=slopes_null_subdir,  slopes_null_basename)
      self._slopes_null = obj_new('AOslopes', self, slopes_null_fname, self._frames_counter, store_label='slopes_null')
      if obj_valid(self._slopes_null) and obj_valid(self._modal_rec) then $
        self._modes_null = obj_new('AOresidual_modes', self, self._slopes_null, self._modal_rec, store_label='modes_null')
    endif
  endif else message, 'Wfs object not available: slopesnull object not initialized!', /info


  ;
  ; measurement type
  ; if slope null file ==nc.fits then 'SlopeNullMeas'
  ; else 'LoopMeas'
  if self._meas_type eq '' then begin
    self._meas_type = 'LOOP'
    if obj_valid(self->wfs_status()) then begin
      if strtrim(file_basename( (self->wfs_status())->slopes_null_fname()),2) eq 'nc.fits' then self._meas_type = 'NCPA'
    endif
  endif

  ; Sinusoidal acquisitions
  if obj_valid(self._disturb) then begin
    if (self._meas_type eq 'LOOP') and (self._disturb->type() eq 'sinusmode') then begin
      self._sinusacq = obj_new('AOsinus_acq', self)
    endif
  endif

  ; Telemetry
  telemetrydir = filepath(root=ao_datadir(), sub=['adsec_data', date, 'telemetry'], '')
  if n_elements(FILE_SEARCH(filepath(root=telemetrydir, '*_t.fits'))) ne 0 then begin
    self._telemetry = obj_new('aotelemetry', self)
  endif


  ; initialize help object and add methods and leafs
  if not self->AOhelp::Init('AOElab', 'Represents an AO measure') then return, 0
  if obj_valid(self._obj_tracknum) then self->addleaf, self._obj_tracknum, 'obj_tracknum'
  if obj_valid(self._adsec_status) then self->addleaf, self._adsec_status, 'adsec_status'
  if obj_valid(self._wfs_status) then self->addleaf, self._wfs_status, 'wfs_status'
  if obj_valid(self._tel) then self->addleaf, self._tel, 'tel'
  if obj_valid(self._sanitycheck) then self->addleaf, self._sanitycheck, 'sanitycheck'
  if obj_valid(self._control) then self->addleaf, self._control, 'control'
  if obj_valid(self._frames_counter) then self->addleaf, self._frames_counter, 'frames_counter'
  if obj_valid(self._valid_pixels) then self->addleaf, self._valid_pixels, 'valid_pixels'
  if obj_valid(self._delay) then self->addleaf, self._delay, 'delay'
  if obj_valid(self._slopes) then self->addleaf, self._slopes, 'slopes'
  if obj_valid(self._modal_rec) then self->addleaf, self._modal_rec, 'modal_rec'
  if obj_valid(self._intmat) then self->addleaf, self._intmat, 'intmat'
  if obj_valid(self._modeShapes) then self->addleaf, self._modeShapes, 'modeShapes'
  if obj_valid(self._residual_modes) then self->addleaf, self._residual_modes, 'residual_modes'
  if obj_valid(self._modes) then self->addleaf, self._modes, 'modes'
  if obj_valid(self._olmodes) then self->addleaf, self._olmodes, 'olmodes'
  if obj_valid(self._commands) then self->addleaf, self._commands, 'commands'
  if obj_valid(self._modalcommands) then self->addleaf, self._modalcommands, 'modalcommands'
  if obj_valid(self._positions) then self->addleaf, self._positions, 'positions'
  if obj_valid(self._modalpositions) then self->addleaf, self._modalpositions, 'modalpositions'
  if obj_valid(self._tv ) then self->addleaf, self._tv , 'tv'
  if obj_valid(self._frames) then self->addleaf, self._frames, 'frames'
  if obj_valid(self._timestamps) then self->addleaf, self._timestamps, 'timestamps'
  if obj_valid(self._disturb) then self->addleaf, self._disturb, 'disturb'
  if obj_valid(self._modaldisturb) then self->addleaf, self._modaldisturb, 'modaldisturb'
  if obj_valid(self._irtc) then self->addleaf, self._irtc, 'irtc'
  if obj_valid(self._pisces) then self->addleaf, self._pisces, 'pisces'
  if obj_valid(self._piscesold) then self->addleaf, self._piscesold, 'piscesold'
  if obj_valid(self._luci) then self->addleaf, self._luci, 'luci'
  if obj_valid(self._shark) then self->addleaf, self._shark, 'shark'
  if obj_valid(self._offloadmodes) then self->addleaf, self._offloadmodes, 'offloadmodes'
  if obj_valid(self._accel) then self->addleaf, self._accel, 'accel'
  if obj_valid(self._slopes_null) then self->addleaf, self._slopes_null, 'slopesnull'
  if obj_valid(self._modes_null) then self->addleaf, self._modes_null, 'modesnull'
  if obj_valid(self._override) then self->addleaf, self._override, 'override'
  if obj_valid(self._sinusacq) then self->addleaf, self._sinusacq, 'sinusacq'
  if obj_valid(self._telemetry) then self->addleaf, self._telemetry, 'telemetry'
  if obj_valid(self._lmircam) then self->addleaf, self._lmircam, 'lmircam'
  if obj_valid(self._luci) then self->addleaf, self._luci, 'luci'

  self->addMethodHelp, "tracknum()", "Tracknum (string)"
  self->addMethodHelp, "obj_tracknum()", "reference to tracknum object (AOtracknum)"
  self->addMethodHelp, "adsec_status()", "reference to adsec status object (AOadsec_status)"
  self->addMethodHelp, "wfs_status()", "reference to wfs status object (AOwfs_status)"
  self->addMethodHelp, "tel()", "reference to telescope object (AOtel_status)"
  self->addMethodHelp, "sanitycheck()", "reference to loop sanity check (AOsanitycheck)"
  self->addMethodHelp, "control()", "reference to control filter object (AOcontrol)"
  self->addMethodHelp, "frames_counter()", "reference to frames counter object (AOframes_counter)"
  self->addMethodHelp, "valid_pixels()", "reference to valid pixels object (AOvalid_pixels)"
  self->addMethodHelp, "slopes()", "reference to slopes object (AOslopes)"
  self->addMethodHelp, "residual_modes()", "reference to residual modes object (AOresidual_modes)"
  self->addMethodHelp, "modes()", "reference to integrated modes object (AOmodes)"
  self->addMethodHelp, "olmodes()", "reference to open loop modes object (AOolmodes)"
  self->addMethodHelp, "commands()", "reference to deltacommands object (AOcommands)"
  self->addMethodHelp, "modalcommands()", "reference to mirror modal commands object (AOmodalcommands)"
  self->addMethodHelp, "positions()", "reference to mirror positions object (AOpositions)"
  self->addMethodHelp, "modalpositions()", "reference to mirror modal positions object (AOmodalpositions)"
  self->addMethodHelp, "pisces()", "reference to PISCES object (AOscientificimage)"
  self->addMethodHelp, "piscesold()", "reference to old PISCES object (AOpsfabstract)"
  self->addMethodHelp, "irtc()", "reference to IRTC object (AOpsf)"
  self->addMethodHelp, "luci()", "reference to LUCI object (AOpsf)"
  self->addMethodHelp, "lmircam()", "reference to LUMIRCAM object (AOpsf)"
  self->addMethodHelp, "shark()", "reference to SHARK object (AOpsf)"
  self->addMethodHelp, "tv()", "reference to TV ccd47 object (AOpsf)"
  self->addMethodHelp, "modal_rec()", "reference to modal reconstructor object (AOrecmatrix)"
  self->addMethodHelp, "intmat()", "reference to interaction matrix object (AOintmat)"
  self->addMethodHelp, "modeShapes()", "reference to modal shapes object (AOmodeShapes)"
  self->addMethodHelp, "frames()", "reference to WFS frame object (AOframes)"
  self->addMethodHelp, "timestamps()", "reference to timestamps object (AOtimestamps)"
  self->addMethodHelp, "disturb()", "reference to disturb object (AOdisturb)"
  self->addMethodHelp, "modaldisturb()", "reference to modal disturb object (AOmodaldisturb)"
  self->addMethodHelp, "offloadmodes()", "reference to offload modes object (AOoffloadmodes)"
  self->addMethodHelp, "accel()", "reference to adsec accelerometer data object (0:1 centroid, 2 x, 3 y, 4 z, 5 Rx, 6 Ry, 7 Rz) (AOaccel)"
  self->addMethodHelp, "slopes_null()", "reference to a slopesnull object (AOslopes)"
  self->addMethodHelp, "modesnull()", "reference to a modesnull object (AOresidual_modes)"
  self->addMethodHelp, "override()", "reference to a override object (AOoverride)"
  self->addMethodHelp, "sinusacq()", "reference to a sinus acq object (AOsinus_acq)"
  self->addMethodHelp, "telemetry()", "reference to telemetry object"
  self->addMethodHelp, "isOK(cause=cause)", "return 1 if diagnostic flags are OK. 0 otherwise"
  self->addMethodHelp, "errorDescription()", "return the error description in case isOK return 0"
  self->addMethodHelp, "closedloop()", "return 1 if loop is closed"
  self->addMethodHelp, "mag()", "equivalent star magnitude (R)"
  self->addMethodHelp, "sr_from_positions()", "Strehl Ratio estimate (default H band)"
  self->addMethodHelp, "modalplot, /overplot, color=color", "Plot the modal performance evaluation"
  self->addMethodHelp, "operation_mode()", "Return ONSKY or RR (retroreflector)"
  self->addMethodHelp, "meas_type()", "Return the type of measurement: LOOP, NCPA (non-common path calibration), AG (autogain)"
  self->addMethodHelp, "psf", "quick psf display"
  self->addMethodHelp, "duration()", "Return the measurement duration in seconds, if applicable"
  ; free memory
  self->free

  return, 1
end

function AOelab::datadir
  return, self._datadir
end

function AOelab::elabdir
  return, self._elabdir
end

function AOelab::recompute
  return, self._recompute
end

function AOelab::reflcoef
  return, self._reflcoef
end

function AOelab::n_periods
  return, self._n_periods
end

function AOelab::operation_mode
  return, self._operation_mode
end

function AOelab::meas_type
  return, self._meas_type
end


;;;;;;;;;;;;; Shortcut to most important functions/macro ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function AOelab::tracknum
  return, (self._obj_tracknum)->Tracknum()
end

function AOelab::isOK, cause=cause
  imok=1B
  cause = ""
  if obj_valid(self->sanitycheck()) then imok *= (self->sanitycheck())->isOK(cause=cause)
  if obj_valid(self->frames_counter()) then imok *= (self->frames_counter())->isok(cause=cause)
  if OBJ_VALID(self->disturb()) then imok *= (self->disturb())->isok(cause=cause)
  if OBJ_VALID(self->intmat()) then $
    if OBJ_VALID(self->wfs_status()) and OBJ_VALID( (self->intmat())->wfs_status() )  then begin
    if round((self->wfs_status())->modulation()) ne round(((self->intmat())->wfs_status())->modulation()) then begin
      imok*=0B
      cause += ' - Pyramid modulation mismatch'
    endif
    if OBJ_VALID(self->wfs_status()) && OBJ_VALID((self->wfs_status())->pupils()) && $
      OBJ_VALID((self->intmat())->wfs_status()) && OBJ_VALID(((self->intmat())->wfs_status())->pupils()) then begin
      if ((self->wfs_status())->pupils())->pup_tracknum() ne (((self->intmat())->wfs_status())->pupils())->pup_tracknum() then begin
        ; TODO UGLY PATCH BECAUSE OF DIGITAL SHIFT OF THE PUPILS DONE IN June 2011 RUN
        ; Here we should have families of pupils that belongs to the same set and are just shifted to optimize
        ; camera lens pupil recentering
        if not ( ((self->wfs_status())->pupils())->pup_tracknum() eq '20110601-224135' and $
          (((self->intmat())->wfs_status())->pupils())->pup_tracknum() eq '20100525-171742' ) then begin
          imok*=0B
          cause += ' - Pupils mismatch'
        endif
      endif
    endif
  endif
  if OBJ_VALID(self->wfs_status()) then imok *= (self->wfs_status())->isok(cause=cause)
  if OBJ_VALID(self->irtc()) then imok *= (self->irtc())->isok(cause=cause)
  if OBJ_VALID(self->pisces()) then imok *= (self->pisces())->isok(cause=cause)
  if OBJ_VALID(self->luci()) then imok *= (self->luci())->isok(cause=cause)
  if OBJ_VALID(self->shark()) then imok *= (self->shark())->isok(cause=cause)
  return, imok
end

function AOelab::errorDescription
  ok=self->isOK(cause=cause)
  return, cause
end

function AOelab::closedloop
  return, (self->sanitycheck())->closedloop()
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                   SUMMARY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro AOelab::summary, PARAMS_ONLY=PARAMS_ONLY, TEXT=TEXT
  TEXT = [string(format='(%"| %-30s | %s |")','Tracknum',self->tracknum() )]
  TEXT = [TEXT, string(format='(%"| %-30s | %s |")','Closed Loop', self->closedloop() ? 'Yes' : 'No' )]
  TEXT = [TEXT, string(format='(%"| %-30s | %s %s |")','Is OK?', ( self->isOK(cause=cause) eq 1L) ? "OK" :  "No", cause  )]
  if obj_valid(self->disturb()) then begin
    if strmatch((self->disturb())->type(),'*atm*') then begin
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','seeing [arcsec]',(self->disturb())->seeing() )]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','Vwind [m/s]',(self->disturb())->vwind() )]
      if (self->disturb())->cor_nmodes() ne 0 then $
        TEXT = [TEXT, 'Modal pre-correction: '+string((self->disturb())->cor_nmodes())+' modes, starting from mode number '+strtrim((self->disturb())->cor_first_mode(),2)]
    endif
    if strmatch((self->disturb())->type(),'*vib*') then begin
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','number of vibrations',(self->disturb())->totnvib() )]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','type of vibration',(self->disturb())->casevib() )]
    endif
  endif
  if obj_valid(self->frames()) then begin
    TEXT = [TEXT, string(format='(%"| %-30s | %s |")','AntiDrift', (self->frames())->antidrift_status() ? 'ON':'OFF' )]
    TEXT = [TEXT, string(format='(%"| %-30s | %f |")','counts/sub/fr', (self->frames())->nphsub_per_int_av())]
  endif
  TEXT = [TEXT, string(format='(%"| %-30s | %f |")','Magnitude', self->mag())]
  if obj_valid(self->modal_rec()) then begin
    TEXT = [TEXT, string(format='(%"| %-30s | %d |")','# Modes', (self->modal_rec())->nmodes())]
    TEXT = [TEXT, string(format='(%"| %-30s | %s |")','Modal rec', file_basename( (self->modal_rec())->fname() ) )]
  endif
  if obj_valid(self->wfs_status()) then if obj_valid((self->wfs_status())->camera())  then begin
    TEXT = [TEXT, string(format='(%"| %-30s | %d |")','Binning', ((self->wfs_status())->camera())->binning())]
    TEXT = [TEXT, string(format='(%"| %-30s | %d |")','Frequency [Hz]', ((self->wfs_status())->camera())->framerate())]
    TEXT = [TEXT, string(format='(%"| %-30s | %f |")','Modulation', (self->wfs_status())->modulation() )]
    ;TEXT = [TEXT, string(format='(%"%-30s %s")','B0_a matrix', (self->control())->b0_a_fname())]
    if obj_valid((self->wfs_status())->filtw1()) then $
      TEXT = [TEXT, string(format='(%"| %-30s | %s |")','FW1', ((self->wfs_status())->filtw1())->name() )]
    if obj_valid((self->wfs_status())->filtw2()) then $
      TEXT = [TEXT, string(format='(%"| %-30s | %s |")','FW2', ((self->wfs_status())->filtw2())->name() )]
  endif
  if obj_valid(self->control()) then begin
    gaintemp = minmax( (self->control())->gain() )
    if gaintemp[0] eq -1 then TEXT = [TEXT, 'Gain: AUTO'] else $
      TEXT = [TEXT, string(format='(%"| %-30s | %f %f |")','Gain minmax', gaintemp)]
  endif
  if obj_valid(self->tel()) then begin
    TEXT = [TEXT, string(format='(%"| %-30s | %f |")','Telescope elevation', (self->tel())->el() )]
    TEXT = [TEXT, string(format='(%"| %-30s | %f |")','Wind speed', (self->tel())->wind_speed() )]
  endif
  if obj_valid(self->olmodes()) then begin
    TEXT = [TEXT, string(format='(%"| %-30s | %f |")','seeing from OL modes', (self->olmodes())->seeing() )]
  endif
  if obj_valid(self->sanitycheck()) then begin
    TEXT = [TEXT, string(format='(%"| %-30s | %d |")','number of skipped frames', (self->sanitycheck())->skippedFrames() )]
  endif
  if not keyword_set(PARAMS_ONLY) then begin
    ;TEXT = [TEXT, string(format='(%"%-30s %f")','SR@H  FQP',self->sr_from_positions())
    if obj_valid(self->irtc()) then begin
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','lambda [um]',(self->irtc())->lambda()*1e6)]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','exptime [s]',(self->irtc())->exptime())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','framerate [Hz]',(self->irtc())->framerate())]
      TEXT = [TEXT, string(format='(%"| %-30s | %d |")','no. frames',(self->irtc())->nframes())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','SR SE' ,(self->irtc())->sr_se())]
      TEXT = [TEXT, string(format='(%"| %-30s | %s |")','IRTC dark', file_basename( (self->irtc())->dark_fname()))]
    endif
    if obj_valid(self->pisces()) then begin
      TEXT = [TEXT, string(format='(%"| %-30s | %s |")','filter',(self->pisces())->filter_name())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','lambda [um]',(self->pisces())->lambda()*1e6)]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','exptime [s]',(self->pisces())->exptime())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','framerate [Hz]',(self->pisces())->framerate())]
      TEXT = [TEXT, string(format='(%"| %-30s | %d |")','no. frames',(self->pisces())->nframes())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','SR SE' ,(self->pisces())->sr_se())]
      TEXT = [TEXT, string(format='(%"| %-30s | %s |")','pisces dark', file_basename( (self->pisces())->dark_fname()))]
    endif
    if obj_valid(self->piscesold()) then begin
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','piscesold lambda [um]',(self->piscesold())->lambda()*1e6)]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','piscesold exptime [s]',(self->piscesold())->exptime())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','piscesold framerate [Hz]',(self->piscesold())->framerate())]
      TEXT = [TEXT, string(format='(%"| %-30s | %d |")','piscesold no. frames',(self->piscesold())->nframes())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','piscesold SR SE' ,(self->piscesold())->sr_se())]
      TEXT = [TEXT, string(format='(%"| %-30s | %s |")','piscesold dark', file_basename( (self->piscesold())->dark_fname()))]
    endif
    if obj_valid(self->luci()) then begin
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','lambda [um]',(self->luci())->lambda()*1e6)]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','exptime [s]',(self->luci())->exptime())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','framerate [Hz]',(self->luci())->framerate())]
      TEXT = [TEXT, string(format='(%"| %-30s | %d |")','no. frames',(self->luci())->nframes())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','SR SE' ,(self->luci())->sr_se())]
      TEXT = [TEXT, string(format='(%"| %-30s | %s |")','LUCI dark', file_basename( (self->luci())->dark_fname()))]
    endif
    if obj_valid(self->lmircam()) then begin
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','lambda [um]',(self->lmircam())->lambda()*1e6)]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','exptime [s]',(self->lmircam())->exptime())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','framerate [Hz]',(self->lmircam())->framerate())]
      TEXT = [TEXT, string(format='(%"| %-30s | %d |")','no. frames',(self->lmircam())->nframes())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','SR SE' ,(self->lmircam())->sr_se())]
      TEXT = [TEXT, string(format='(%"| %-30s | %s |")','LMIRCAM dark', file_basename( (self->lmircam())->dark_fname()))]
    endif
    if obj_valid(self->shark()) then begin
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','lambda [um]',(self->shark())->lambda()*1e6)]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','exptime [s]',(self->shark())->exptime())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','framerate [Hz]',(self->shark())->framerate())]
      TEXT = [TEXT, string(format='(%"| %-30s | %d |")','no. frames',(self->shark())->nframes())]
      TEXT = [TEXT, string(format='(%"| %-30s | %f |")','SR SE' ,(self->shark())->sr_se())]
      TEXT = [TEXT, string(format='(%"| %-30s | %s |")','SHARK dark', file_basename( (self->shark())->dark_fname()))]
    endif

  endif

  for i=0,n_elements(TEXT)-1 do print,TEXT[i]
end

pro AOelab::fullsummary
  if obj_valid(self._obj_tracknum) then if obj_hasmethod(self._obj_tracknum, 'summary') then self._obj_tracknum->summary
  if obj_valid(self._adsec_status) then if obj_hasmethod(self._adsec_status, 'summary') then self._adsec_status->summary
  if obj_valid(self._wfs_status) then if obj_hasmethod(self._wfs_status, 'summary') then self._wfs_status->summary
  if obj_valid(self._tel) then if obj_hasmethod(self._tel, 'summary') then self._tel->summary
  if obj_valid(self._sanitycheck) then if obj_hasmethod(self._sanitycheck, 'summary') then self._sanitycheck->summary
  if obj_valid(self._control) then if obj_hasmethod(self._control, 'summary') then self._control->summary
  if obj_valid(self._frames_counter) then if obj_hasmethod(self._frames_counter, 'summary') then self._frames_counter->summary
  if obj_valid(self._valid_pixels) then if obj_hasmethod(self._valid_pixels, 'summary') then self._valid_pixels->summary
  if obj_valid(self._delay) then if obj_hasmethod(self._delay, 'summary') then self._delay->summary
  if obj_valid(self._slopes) then if obj_hasmethod(self._slopes, 'summary') then self._slopes->summary
  if obj_valid(self._modal_rec) then if obj_hasmethod(self._modal_rec, 'summary') then self._modal_rec->summary
  if obj_valid(self._intmat) then if obj_hasmethod(self._intmat, 'summary') then self._intmat->summary
  if obj_valid(self._modeShapes) then if obj_hasmethod(self._modeShapes, 'summary') then self._modeShapes->summary
  if obj_valid(self._residual_modes) then if obj_hasmethod(self._residual_modes, 'summary') then self._residual_modes->summary
  if obj_valid(self._modes) then if obj_hasmethod(self._modes, 'summary') then self._modes->summary
  if obj_valid(self._olmodes) then if obj_hasmethod(self._olmodes, 'summary') then self._olmodes->summary
  if obj_valid(self._commands) then if obj_hasmethod(self._commands, 'summary') then self._commands->summary
  if obj_valid(self._modalcommands) then if obj_hasmethod(self._modalcommands, 'summary') then self._modalcommands->summary
  if obj_valid(self._positions) then if obj_hasmethod(self._positions, 'summary') then self._positions->summary
  if obj_valid(self._modalpositions) then if obj_hasmethod(self._modalpositions, 'summary') then self._modalpositions->summary
  if obj_valid(self._tv) then if obj_hasmethod(self._tv, 'summary') then self._tv->summary
  if obj_valid(self._frames) then if obj_hasmethod(self._frames, 'summary') then self._frames->summary
  if obj_valid(self._timestamps) then if obj_hasmethod(self._timestamps, 'summary') then self._timestamps->summary
  if obj_valid(self._disturb) then if obj_hasmethod(self._disturb, 'summary') then self._disturb->summary
  if obj_valid(self._modaldisturb) then if obj_hasmethod(self._modaldisturb, 'summary') then self._modaldisturb->summary
  if obj_valid(self._irtc) then if obj_hasmethod(self._irtc, 'summary') then self._irtc->summary
  if obj_valid(self._pisces) then if obj_hasmethod(self._pisces, 'summary') then self._pisces->summary
  if obj_valid(self._luci) then if obj_hasmethod(self._luci, 'summary') then self._luci->summary
  if obj_valid(self._lmircam) then if obj_hasmethod(self._lmircam, 'summary') then self._lmircam->summary
  if obj_valid(self._shark) then if obj_hasmethod(self._shark, 'summary') then self._shark->summary
  ;if obj_valid(self._piscesold) then if obj_hasmethod(self._piscesold, 'summary') then self._piscesold->summary
  if obj_valid(self._offloadmodes) then if obj_hasmethod(self._offloadmodes, 'summary') then self._offloadmodes->summary
  if obj_valid(self._accel) then if obj_hasmethod(self._accel, 'summary') then self._accel->summary
  if obj_valid(self._slopes_null) then if obj_hasmethod(self._slopes_null, 'summary') then self._slopes_null->summary
  if obj_valid(self._modes_null) then if obj_hasmethod(self._modes_null, 'summary') then self._modes_null->summary
  if obj_valid(self._override) then if obj_hasmethod(self._override, 'summary') then self._override->summary
  if obj_valid(self._sinusacq) then if obj_hasmethod(self._sinusacq, 'summary') then self._sinusacq->summary
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  PLOTS and SHORTCUTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; WFRESIDUALS: overplot the residual modes calculated from WFS slopes (for daytime)
; ARGOSCAL   : adapt to Argos calibration unit: disturbance and residuals computed
;              as for the RR case, but with single-pass normalization (as for the on-sky case)
;
; SYMSIZE    : set to the value for the SYMSIZE keyword in plot instructions
; CHARSIZE   : set to the value for the CHARSIZE keyword in plot instructions

pro AOelab::modalplot, OVERPLOT = OVERPLOT, COLOR=COLOR, OLCOLOR=OLCOLOR, $
  WFRESIDUALS=WFRESIDUALS, WFCOLOR=WFCOLOR, NOLEGEND=NOLEGEND, $
  thick=thick, _extra=ex, clvar=clvar, olvar=olvar, ARGOSCAL=ARGOSCAL

  if n_elements(SYMSIZE) then SYMSIZE=0.8
  if n_elements(CHARSIZE) then CHARSIZE=1.2
  if n_elements(OLCOLOR) eq 0 then OLCOLOR = '0000ff'x

  if self->operation_mode() eq "RR" or self->operation_mode() eq "ARGOScal" or keyword_set(ARGOSCAL) then begin
    nmodes = (self->modalpositions())->nmodes()
    clvar  = (self->modalpositions())->time_variance() * ((self->modalpositions())->norm_factor())^2.
    if self->operation_mode() eq "RR" and not keyword_set((self->tel())->isTracking()) then clvar /= 4.
    yrange = sqrt(minmax(clvar))
    if obj_valid(self._disturb) then begin
      if (self->adsec_status())->disturb_status() ne 0 then begin
        olvar  = (self->modaldisturb())->time_variance() * ((self->modaldisturb())->norm_factor())^2.
        if self->operation_mode() eq "RR" and not keyword_set((self->tel())->isTracking()) then olvar /= 4.
        yrange = sqrt(minmax([clvar,olvar]))
      endif
    endif
    if keyword_set(WFRESIDUALS) then begin
      wfres = (self->residual_modes())->modes(); * (self->residual_modes())->norm_factor()
      wfres *= (self->residual_modes())->norm_factor()
      if (self->wfs_status())->optg() lt 1 then wfres /= 2.
      wfres = rms(wfres,dim=1)
      yrange = minmax([yrange, minmax(wfres)])
    endif
    if not keyword_set(OVERPLOT) then begin
      plot_oo, lindgen(nmodes)+1, sqrt(clvar), psym=-1, symsize=SYMSIZE, charsize=CHARSIZE, ytitle='nm rms wf', xtitle='mode number', $
        title=self._obj_tracknum->tracknum(), yrange=yrange, thick=thick, _extra=ex
    endif else begin
      oplot, lindgen(nmodes)+1, sqrt(clvar), psym=-1, symsize=SYMSIZE, COLOR=COLOR, thick=thick
    endelse
    if obj_valid(self._disturb) then begin
      if (self->adsec_status())->disturb_status() ne 0 then begin
        oplot, lindgen(nmodes)+1, sqrt(olvar), psym=-2, symsize=SYMSIZE, COLOR=OLCOLOR, thick=thick
        if ~keyword_set(NOLEGEND) then begin
          elab_legend, ['disturbance','closed-loop'], color=[OLCOLOR,!P.color], psym=-[2,1], /right, thick=thick
        endif
      endif
    endif
    if keyword_set(WFRESIDUALS) then begin
      if n_elements(WFCOLOR) eq 0 then WFCOLOR = 'ff00ff'x
      print, 'WFS residual minmax:', minmax(wfres)
      oplot, lindgen(nmodes)+1, wfres, psym=-4, symsize=SYMSIZE, color=WFCOLOR, thick=thick
    endif

    ;; End of 'RR', begin 'ONSKY'
  endif else begin
    ;       nmodes = (self->residual_modes())->nmodes()
    clvar  = (self->residual_modes())->time_variance()
    if (self->wfs_status())->optg() lt 1 then clvar *= ((self->olmodes())->norm_factor())^2. $
    else clvar *= ((self->residual_modes())->norm_factor())^2.
    olvar  = (self->olmodes())->time_variance() * ((self->olmodes())->norm_factor())^2.
    modes_idx = (self->modal_rec())->modes_idx()
    clvar  = clvar[modes_idx]
    olvar  = olvar[modes_idx]
    yrange = sqrt(minmax([clvar,olvar]))
    if not keyword_set(OVERPLOT) then begin
      plot_oo, modes_idx+1, sqrt(clvar), psym=-1, symsize=SYMSIZE, charsize=CHARSIZE, ytitle='nm rms wf', xtitle='mode number', $
        title=self._obj_tracknum->tracknum(), yrange=yrange, thick=thick, _extra=ex
    endif else begin
      oplot, modes_idx+1, sqrt(clvar), psym=-1, symsize=SYMSIZE, COLOR=COLOR, thick=thick
    endelse
    oplot, modes_idx+1, sqrt(olvar), psym=-2, symsize=SYMSIZE, COLOR=OLCOLOR, thick=thick
  endelse
end


pro AOelab::modalSpecPlot, modenum, OVERPLOT=OVERPLOT, COLOR=COLOR, NOLEGEND=NOLEGEND, _extra=ex

  if n_params() ne 1 then begin
    message, 'Missing parameter. Usage: ...->modalSpecPlot, modenum', /info
    return
  endif

  if self->operation_mode() eq "RR" or self->operation_mode() eq "ARGOScalib" then begin
    nmodes = (self->modalpositions())->nmodes()
    if modenum ge nmodes then begin
      message, 'Mode number requested not available. The last mode available is '+strtrim(nmodes-1,2), /info
      return
    endif
    ; corrected PSD
    freq   = (self->modalpositions())->freq()
    psd    = (self->modalpositions())->psd() * ((self->modalpositions())->norm_factor())^2.
    ; disturbance PSD
    if obj_valid(self._disturb) then $
      if (self->disturb())->dist_freq() ne -1 then begin
      olfreq = (self->modaldisturb())->freq()
      olpsd  = (self->modaldisturb())->psd() * ((self->modaldisturb())->norm_factor())^2.
      yrange=sqrt(minmax([olpsd[1:*,modenum],psd[1:*,modenum]]))
    endif else begin
      message, 'disturbance frequency data not available', /info
      yrange=sqrt(minmax(psd[1:*,modenum]))
    endelse
  endif else begin
    nmodes = (self->modal_rec())->nmodes()
    if modenum ge nmodes then begin
      message, 'Mode number requested not available. The last mode available is '+strtrim(nmodes-1,2), /info
      return
    endif
    ; corrected PSD
    freq   = (self->residual_modes())->freq()
    psd    = (self->residual_modes())->psd() * ((self->residual_modes())->norm_factor())^2.
    ; OL rec PSD
    if obj_valid(self._olmodes) then begin
      olfreq = (self->olmodes())->freq()
      olpsd  = (self->olmodes())->psd() * ((self->olmodes())->norm_factor())^2.
      yrange=sqrt(minmax([olpsd[1:*,modenum],psd[1:*,modenum]]))
    endif else begin
      message, 'olmodes data not available', /info
      yrange=sqrt(minmax(psd[1:*,modenum]))
    endelse
  endelse

  if not keyword_set(OVERPLOT) then  begin
    !X.MARGIN = [12, 3]
    plot_oo, freq[1:*], sqrt(psd[1:*,modenum]), charsize=1.2, xtitle='frequency [Hz]', ytitle=textoidl('[nm Hz^{-1/2}]') $
      , title=self._obj_tracknum->tracknum()+', mode '+strtrim(modenum,2), yrange=yrange, ytickformat='(e9.1)', _extra=ex
    if n_elements(olfreq) ne 0 then begin
      oplot, olfreq[1:*], sqrt(olpsd[1:*,modenum]), color=250
      if ~keyword_set(NOLEGEND) then begin
        elab_legend, ['disturbance','closed-loop'], color=[250,!P.color], linestyle=[0,0], /bottom
      endif
    endif
  endif else oplot, freq[1:*], sqrt(psd[1:*,modenum]), COLOR=COLOR, _extra=ex

end


function AOelab::sr_from_positions, lambda_perf=lambda_perf
  if not keyword_set(lambda_perf) then lambda_perf = 1.65e-6 	; Default: H band
  pos_coef_var = (self->modalpositions())->time_variance() * (2*!PI*self->reflcoef()/lambda_perf)^2. ;in rad^2 @ lambda_perf
  return, exp(-total(pos_coef_var))
end

function AOelab::duration
  d_irtc=0
  d_pisces=0
  d_luci=0
  d_lmircam=0
  d_shark=0
  d_loop=0
  if obj_valid(self->irtc()) then $
    d_irtc = (self->irtc())->nframes() * (self->irtc())->exptime()
  if obj_valid(self->luci()) then $
    d_luci = (self->luci())->nframes() * (self->luci())->exptime()
  if obj_valid(self->lmircam()) then $
    d_lmircam = (self->lmircam())->nframes() * (self->lmircam())->exptime()
  if obj_valid(self->shark()) then $
    d_shark = (self->shark())->nframes() * (self->shark())->exptime()
  if obj_valid(self->pisces()) then $
    d_pisces = (self->pisces())->nframes() * (self->pisces())->exptime()
  if obj_valid(self->frames_counter()) then $
    d_loop = (self->frames_counter())->nframes() * (self->frames_counter())->deltat()

  return, max([d_irtc, d_luci, d_lmircam, d_shark, d_pisces, d_loop])
end

pro AOelab::psf, PARENT = PARENT, fullframe=fullframe, sr=sr
  loadct,3
  if ((not obj_valid(self->irtc())) and (not obj_valid(self->pisces())) and (not obj_valid(self->luci())) and (not obj_valid(self->lmircam())) and (not obj_valid(self->shark())) ) then return
  if keyword_set(sr) then begin
    if obj_valid(self->irtc()) then obj = self->irtc()
    if obj_valid(self->luci()) then obj = self->luci()
    if obj_valid(self->lmircam()) then obj = self->lmircam()
    if obj_valid(self->shark()) then obj = self->shark()
    if obj_valid(self->pisces()) then obj = self->pisces()
    image_show, /lab, /as, /sh, /log, title=self->tracknum(), pos=pos, obj->longexposure(/fullframe)>0.1
    nstars = obj->nstars()
    starpos = obj->star_position_px()
    sr = 100.*obj->star_sr()
    for i=0,nstars-1 do begin
      starposdev = starpos[*,i]/1024 * [pos[2]-pos[0], pos[3]-pos[1]] + [pos[0],pos[1]]
      xyouts, starposdev[0], starposdev[1], string(format='(%"%d - %4.1f")', i, sr[i]), charsi=1.5, col='ffffff'x, /dev, alig=0.5
    endfor
  endif else begin
    if obj_valid(self->irtc()) then psf = (self->irtc())->longexposure()
    if obj_valid(self->luci()) then psf = (self->luci())->longexposure(fullframe=fullframe)
    if obj_valid(self->lmircam()) then psf = (self->lmircam())->longexposure(fullframe=fullframe)
    if obj_valid(self->shark()) then psf = (self->shark())->longexposure()
    if obj_valid(self->pisces()) then psf = (self->pisces())->longexposure(fullframe=fullframe)
    xshow, /lab, /as, /sh, /log, title=self->tracknum(), pos=pos,   psf>0.1, PARENT = PARENT
  endelse

end

function AOelab::mag
  if obj_valid(self->frames()) then $
    if obj_valid(self->wfs_status()) then $
    if obj_valid( (self->wfs_status())->camera() ) then $

    if (self->wfs_status())->isSoul() then begin

    t = (self->wfs_status())->transmissivity()

    return, tell_me_the_mag_ocam((self->frames())->nph_per_int_av()/t, $
      ((self->wfs_status())->camera())->binning(), $
      ((self->wfs_status())->camera())->framerate(), $
      ((self->wfs_status())->camera())->emGain(), $
      (self->wfs_status())->zeromag_flux())

  endif else begin
    return, tell_me_the_mag((self->frames())->nph_per_int_av(), $
      ((self->wfs_status())->camera())->framerate() )
  endelse

  message, 'impossible to compute the magnitude', /info
  return, !values.f_nan
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           MEMBERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function AOelab::obj_tracknum
  return, (self._obj_tracknum)
end

function AOelab::adsec_status
  IF (OBJ_VALID(self._adsec_status)) THEN return, self._adsec_status else return, obj_new()
end

function AOelab::wfs_status
  IF (OBJ_VALID(self._wfs_status)) THEN return, self._wfs_status else return, obj_new()
end

function AOelab::tel
  IF (OBJ_VALID(self._tel)) THEN return, self._tel else return, obj_new()
end

function AOelab::sanitycheck
  IF (OBJ_VALID(self._sanitycheck)) THEN return, self._sanitycheck else return, obj_new()
end

function AOelab::control
  IF (OBJ_VALID(self._control)) THEN return, self._control else return, obj_new()
end

function AOelab::frames_counter
  IF (OBJ_VALID(self._frames_counter)) THEN return, self._frames_counter else return, obj_new()
end

function AOelab::valid_pixels
  IF (OBJ_VALID(self._valid_pixels)) THEN return, self._valid_pixels else return, obj_new()
end

function AOelab::frames
  IF (OBJ_VALID(self._frames)) THEN return, self._frames else return, obj_new()
end

function AOelab::timestamps
  IF (OBJ_VALID(self._timestamps)) THEN return, self._timestamps else return, obj_new()
end

function AOelab::delay
  IF (OBJ_VALID(self._delay)) THEN return, self._delay else return, obj_new()
end

function AOelab::slopes
  IF (OBJ_VALID(self._slopes)) THEN return, self._slopes else return, obj_new()
end

function AOelab::residual_modes
  IF (OBJ_VALID(self._residual_modes)) THEN return, self._residual_modes else return, obj_new()
end

function AOelab::modes
  IF (OBJ_VALID(self._modes)) THEN return, self._modes else return, obj_new()
end

function AOelab::olmodes
  IF (OBJ_VALID(self._olmodes)) THEN return, self._olmodes else return, obj_new()
end

function AOelab::commands
  IF (OBJ_VALID(self._commands)) THEN return, self._commands else return, obj_new()
end

function AOelab::modalcommands
  IF (OBJ_VALID(self._modalcommands)) THEN return, self._modalcommands else return, obj_new()
end

function AOelab::positions
  IF (OBJ_VALID(self._positions)) THEN return, self._positions else return, obj_new()
end

function AOelab::modalpositions
  IF (OBJ_VALID(self._modalpositions)) THEN return, self._modalpositions else return, obj_new()
end

function AOelab::tv
  IF (OBJ_VALID(self._tv )) THEN return, self._tv  else return, obj_new()
end

function AOelab::irtc
  IF (OBJ_VALID(self._irtc)) THEN return, self._irtc else return, obj_new()
end

function AOelab::luci
  IF (OBJ_VALID(self._luci)) THEN return, self._luci else return, obj_new()
end

function AOelab::lmircam
  IF (OBJ_VALID(self._lmircam)) THEN return, self._lmircam else return, obj_new()
end

function AOelab::shark
  IF (OBJ_VALID(self._shark)) THEN return, self._shark else return, obj_new()
end

function AOelab::pisces
  IF (OBJ_VALID(self._pisces)) THEN return, self._pisces else return, obj_new()
end

function AOelab::piscesold
  IF (OBJ_VALID(self._piscesold)) THEN return, self._piscesold else return, obj_new()
end

function AOelab::modal_rec
  IF (OBJ_VALID(self._modal_rec)) THEN return, self._modal_rec else return, obj_new()
end

function AOelab::intmat
  IF (OBJ_VALID(self._intmat)) THEN return, self._intmat else return, obj_new()
end

function AOelab::modeShapes
  IF (OBJ_VALID(self._modeShapes)) THEN return, self._modeShapes else return, obj_new()
end

function AOelab::disturb
  IF (OBJ_VALID(self._disturb)) THEN return, self._disturb else return, obj_new()
end

function AOelab::modaldisturb
  IF (OBJ_VALID(self._modaldisturb)) THEN return, self._modaldisturb else return, obj_new()
end

function AOelab::offloadmodes
  IF (OBJ_VALID(self._offloadmodes)) THEN return, self._offloadmodes else return, obj_new()
end

function AOelab::accel
  IF (OBJ_VALID(self._accel)) THEN return, self._accel else return, obj_new()
end

function AOelab::slopes_null
  IF (OBJ_VALID(self._slopes_null)) THEN return, self._slopes_null else return, obj_new()
end

function AOelab::modes_null
  IF (OBJ_VALID(self._modes_null)) THEN return, self._modes_null else return, obj_new()
end

function AOelab::override
  IF (OBJ_VALID(self._override)) THEN return, self._override else return, obj_new()
end

function AOelab::sinusacq
  IF (OBJ_VALID(self._sinusacq)) THEN return, self._sinusacq else return, obj_new()
end

function AOelab::telemetry
  IF (OBJ_VALID(self._telemetry)) THEN return, self._telemetry else return, obj_new()
end

function AOelab::ag
  IF (OBJ_VALID(self._ag)) THEN return, self._ag else return, obj_new()
end

function AOelab::ex, cmd,  isvalid=isvalid
  apex = string(39B)

  ;Split cmd string
  level=0
  pos=0
  for i=0,strlen(cmd)-1 do begin
    c = strmid(cmd, i, 1)
    if c eq '(' then level +=1
    if c eq ')' then level -=1
    if c eq '.' AND level eq 0 then begin
      s = strmid(cmd,pos,i-pos)
      if n_elements(cmds) eq 0 then cmds = s else cmds = [cmds,s]
      pos=i+1
    endif
  endfor
  s = strmid(cmd,pos,i-pos)
  if n_elements(cmds) eq 0 then cmds = s else cmds = [cmds,s]

  ;Start processing each substring
  isvalid=0
  tmpobj=self
  for j=0L, n_elements(cmds)-2 do begin
    method_name = (strsplit(cmds[j], '(', /extr))[0]
    add_brackets = strpos(cmds[j], '(') eq -1 ? '()' : ''
    r=execute('hasmethod = obj_hasmethod(tmpobj, '+apex+method_name+apex+')')
    r=execute('if (hasmethod) then tmpobj= tmpobj->'+cmds[j]+add_brackets)
    if not obj_valid(tmpobj) then break ;
  endfor

  if test_type(tmpobj, /struct) eq 0 then begin
    tmpstruct = tmpobj
    for k=j+1, n_elements(cmds)-1 do begin
      tag_name = cmds[k]
      r=execute('hastag = tag_exist(tmpstruct, '+apex+tag_name+apex+')')
      r=execute('if (hastag) then begin & tmpstruct = tmpstruct.'+tag_name+' & isvalid=1 & endif else isvalid=0')
    endfor
    value = tmpstruct
  endif else begin
    if j eq n_elements(cmds)-1 then begin
      method_name = (strsplit(cmds[j], '(', /extr))[0]
      add_brackets = strpos(cmds[j], '(') eq -1 ? '()' : ''
      r=execute('hasmethod = obj_hasmethod(tmpobj, '+apex+method_name+apex+')')
      r=execute('if (hasmethod) then begin & value = tmpobj->'+cmds[j]+add_brackets+'  & isvalid=1 & endif')
      if test_type(value, /obj_ref) eq 0 then if obj_valid(value) eq 0 then isvalid=0
    endif
  endelse

  if isvalid eq 1 then begin
    return, value
  endif else begin
    message, '...INVALID FUNCTION: '+cmd
  endelse

end

pro AOelab::test
  IF (OBJ_VALID(self._obj_tracknum)) THEN  self._obj_tracknum->test
  IF (OBJ_VALID(self._adsec_status)) THEN  self._adsec_status->test
  IF (OBJ_VALID(self._wfs_status)) THEN  self._wfs_status->test
  IF (OBJ_VALID(self._tel)) THEN  self._tel->test
  IF (OBJ_VALID(self._sanitycheck)) THEN  self._sanitycheck->test
end

pro AOelab::free
  IF (OBJ_VALID(self._adsec_status )) THEN  self._adsec_status->free
  IF (OBJ_VALID(self._wfs_status )) THEN  self._wfs_status->free
  IF (OBJ_VALID(self._control )) THEN  self._control->free
  IF (OBJ_VALID(self._frames_counter)) THEN  self._frames_counter->free
  IF (OBJ_VALID(self._valid_pixels)) THEN  self._valid_pixels->free
  IF (OBJ_VALID(self._delay)) THEN  self._delay->free
  IF (OBJ_VALID(self._slopes)) THEN  self._slopes->free
  IF (OBJ_VALID(self._residual_modes)) THEN  self._residual_modes->free
  IF (OBJ_VALID(self._modes)) THEN  self._modes->free
  IF (OBJ_VALID(self._olmodes)) THEN  self._olmodes->free
  IF (OBJ_VALID(self._commands)) THEN  self._commands->free
  IF (OBJ_VALID(self._modalcommands)) THEN  self._modalcommands->free
  IF (OBJ_VALID(self._positions)) THEN  self._positions->free
  IF (OBJ_VALID(self._modalpositions)) THEN  self._modalpositions->free
  IF (OBJ_VALID(self._tv)) THEN  self._tv->free
  IF (OBJ_VALID(self._irtc)) THEN  self._irtc->free
  IF (OBJ_VALID(self._luci)) THEN  self._luci->free
  IF (OBJ_VALID(self._lmircam)) THEN  self._lmircam->free
  IF (OBJ_VALID(self._shark)) THEN  self._shark->free
  IF (OBJ_VALID(self._pisces)) THEN  self._pisces->free
  IF (OBJ_VALID(self._piscesold)) THEN  self._piscesold->free
  IF (OBJ_VALID(self._modal_rec)) THEN  self._modal_rec->free
  IF (OBJ_VALID(self._intmat)) THEN  self._intmat->free
  IF (OBJ_VALID(self._modal_rec)) THEN  self._modal_rec->free
  IF (OBJ_VALID(self._modeShapes)) THEN  self._modeShapes->free
  IF (OBJ_VALID(self._frames)) THEN  self._frames->free
  IF (OBJ_VALID(self._timestamps)) THEN  self._timestamps->free
  IF (OBJ_VALID(self._disturb)) THEN self._disturb->free
  IF (OBJ_VALID(self._modaldisturb)) THEN self._modaldisturb->free
  IF (OBJ_VALID(self._offloadmodes)) THEN  self._offloadmodes->free
  IF (OBJ_VALID(self._accel )) THEN  self._accel->free
  IF (OBJ_VALID(self._slopes_null)) THEN  self._slopes_null->free
  IF (OBJ_VALID(self._modes_null)) THEN  self._modes_null->free
  IF (OBJ_VALID(self._override)) THEN  self._override->free
  IF (OBJ_VALID(self._sinusacq)) THEN  self._sinusacq->free
  IF (OBJ_VALID(self._telemetry)) THEN  self._telemetry->free
  IF (OBJ_VALID(self._ag)) THEN  self._ag->free
end

pro AOelab::Cleanup
  obj_destroy, self._obj_tracknum
  ;obj_destroy, self._adsec_status
  obj_destroy, self._wfs_status
  obj_destroy, self._tel
  obj_destroy, self._sanitycheck
  obj_destroy, self._control
  obj_destroy, self._frames_counter
  obj_destroy, self._valid_pixels
  obj_destroy, self._delay
  obj_destroy, self._slopes
  obj_destroy, self._residual_modes
  obj_destroy, self._modes
  obj_destroy, self._olmodes
  obj_destroy, self._commands
  obj_destroy, self._modalcommands
  obj_destroy, self._positions
  obj_destroy, self._modalpositions
  obj_destroy, self._tv
  obj_destroy, self._irtc
  obj_destroy, self._luci
  obj_destroy, self._lmircam
  obj_destroy, self._shark
  obj_destroy, self._pisces
  obj_destroy, self._piscesold
  ;    obj_destroy, self._modal_rec
  ;    obj_destroy, self._intmat
  ;	 obj_destroy, self._modeShapes
  obj_destroy, self._frames
  obj_destroy, self._timestamps
  obj_destroy, self._disturb
  obj_destroy, self._modaldisturb
  obj_destroy, self._offloadmodes
  obj_destroy, self._accel
  obj_destroy, self._slopes_null
  obj_destroy, self._modes_null
  obj_destroy, self._override
  obj_destroy, self._sinusacq
  obj_destroy, self._telemetry
  obj_destroy, self._ag
  self->AOhelp::Cleanup
end

pro AOelab__define
  struct = { AOelab, $
    _datadir           : "",        $
    _elabdir           : "",        $
    _recompute         : 0B,        $
    _n_periods		   : 0L,		$
    _operation_mode    : "",		$	; "RR": retroreflector, "ONSKY", idem.
    _meas_type         : "",		$	; "LOOP", "NCPA", "AG"
    _reflcoef		   : 0.,		$
    _obj_tracknum      : obj_new(), $
    _adsec_status      : obj_new(), $
    _wfs_status        : obj_new(), $
    _tel			   : obj_new(), $
    _sanitycheck       : obj_new(), $
    _control           : obj_new(), $
    _frames_counter    : obj_new(), $
    _valid_pixels      : obj_new(), $
    _delay             : obj_new(), $
    _slopes            : obj_new(), $
    _residual_modes    : obj_new(), $
    _modes             : obj_new(), $
    _olmodes           : obj_new(), $
    _commands          : obj_new(), $
    _modalcommands     : obj_new(), $
    _positions         : obj_new(), $
    _modalpositions    : obj_new(), $
    _tv                : obj_new(), $
    _irtc              : obj_new(), $
    _luci              : obj_new(), $
    _lmircam           : obj_new(), $
    _shark             : obj_new(), $
    _pisces            : obj_new(), $
    _piscesold         : obj_new(), $
    _modal_rec         : obj_new(), $
    _intmat			   : obj_new(), $
    _modeShapes		   : obj_new(), $
    _frames            : obj_new(), $
    _timestamps        : obj_new(), $
    _disturb           : obj_new(), $
    _modaldisturb      : obj_new(), $
    _offloadmodes      : obj_new(), $
    _accel             : obj_new(), $
    _slopes_null       : obj_new(), $
    _modes_null        : obj_new(), $
    _override          : obj_new(), $
    _sinusacq          : obj_new(), $
    _telemetry         : obj_new(), $
    _ag                : obj_new(), $
    INHERITS AOhelp $
  }
end





