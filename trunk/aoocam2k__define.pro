
;+
;
;-

function AOocam2k::Init, wfs_header, wunit

	self._header = wfs_header
	hdr = *(self._header)

        self._sensorSideX = 240L
        self._sensorSideY = 240L
        
	self._framerate     = float(aoget_fits_keyword(hdr, 'ocam2.FRAMERATE'))
	self._readout_speed = float(aoget_fits_keyword(hdr, 'ocam2.READOUT_SPEED'))
	self._binning 		= long(aoget_fits_keyword(hdr, 'ocam2.BINNING'))
	self._mode 		= long(aoget_fits_keyword(hdr, 'ocam2.MODE'))

    IF self._mode eq 1 THEN BEGIN
		self_.sensorSideX = 200
		self_.sensorSideY = 200
	ENDIF
        IF self._mode eq 2 THEN BEGIN
            self_.sensorSideX = 240
            self_.sensorSideY = 120
	ENDIF

        self._status  		= aoget_fits_keyword(hdr, 'ocam2.STATUS')

	dark_filename 		= aoget_fits_keyword(hdr,'ocam2.DARK_FILENAME')
	dark_subdir 		= ['wfs_calib_'+wunit,'ocam2','backgrounds','bin'+strtrim(self._binning,2)]
	self._dark_filename = filepath(root=ao_datadir(), sub=dark_subdir,  dark_filename)

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOocam2k', 'Represent OCAM2K') then return, 0
    self->addMethodHelp, "framerate()", "frame rate [Hz] (float)"
    self->addMethodHelp, "readout_speed ()", "readout speed [kpix/s] (float)"
    self->addMethodHelp, "binning()", "binning (long)"
    self->addMethodHelp, "status()", "status (string)"
;    self->addMethodHelp, "dark()", "dark frame"
    self->addMethodHelp, "dark_fname()", "filename of dark frame"

    return, 1
end

function AOocam2k::computeDelay
        fs = self._framerate
        T = 1/fs
        ; SOUL computation time (slope computation not parallelized)
        comp_time = 0.66e-3
        ; SOUL computation time (slope computation parallelized)
        ; comp_time = 0.42e-3
        ; ASM set time
        dm_set = 1e-3
        ;ocam2k characteristics for mode [1,2,3,4,5]
        ReadOutSpeed = [0.48, 0.24, 0.24, 0.20, 0.16]*1e-3
        CCD_speed = ReadOutSpeed[self._mode-1]
        ; delay in seconds
        delay = T/2. + CCD_speed + comp_time + dm_set/2. + T/2.
        return, delay
end

function AOocam2k::idealPupilDistance
	; SOUL: what is the correct value for this ?
	return 60L
end

function AOocam2k::mode
        return, self._mode
end

function AOocam2k::binnedSensorSideX
        return, self._sensorSideX/self._binning
end

function AOocam2k::binnedSensorSideY
        return, self._sensorSideY/self._binning
end

function AOocam2k::sensorSideX
        return, self._sensorSideX
end

function AOocam2k::sensorSideY
        return, self._sensorSideY
end

function AOocam2k::framerate
	return, self._framerate
end

function AOocam2k::readout_speed
	return, self._readout_speed
end

function AOocam2k::binning
	return, self._binning
end

function AOocam2k::status
	return, self._status
end

;function AOocam2k::dark
;	return, float(readfits(self._dark_filename, /silent))
;end

function AOocam2k::dark_fname
	return, self._dark_filename
end

pro AOocam2k::test
    d = self->framerate()
    d = self->readout_speed()
    d = self->binning()
    d = self->status()
;    d = self->dark()
    d = self->dark_fname()
end

pro AOocam2k::summary, COMPREHENSIVE=COMPREHENSIVE
    print, string(format='(%"%-30s %d")','Binning', self->binning() )
    print, string(format='(%"%-30s %f")','Frequency [Hz]', self->framerate() )
	if keyword_set(COMPREHENSIVE) then begin
    	print, string(format='(%"%-30s %f")','Readout speed [kpix/s]', self->readout_speed() )
   		print, string(format='(%"%-30s %s")','OCAM2K dark file', self->dark_fname() )
   		print, string(format='(%"%-30s %s")','OCAM2K status', self->status() )
	endif
end

pro AOocam2k::free
    ;if ptr_valid(self._header) then ptr_free, self._header
end

pro AOocam2k::Cleanup
    if ptr_valid(self._header) then ptr_free, self._header
    self->AOhelp::Cleanup
end


pro AOocam2k__define
    struct = { AOocam2k, $
        _header        : ptr_new(), $
        _framerate     : 0.0	  , $
        _readout_speed : 0.0	  , $
        _binning       : 0L	  , $
        _mode          : 0L       , $
        _sensorSideX   : 0L       , $
        _sensorSideY   : 0L       , $
        _dark_filename : ''       , $
        _status		   : ''	  , $
        INHERITS    AOhelp  $
    }
end
