
;+
;
;-

function AOocam2k::Init, wfs_header, wunit

	self._header = wfs_header
	hdr = *(self._header)

	self._framerate     = float(aoget_fits_keyword(hdr, 'ocam2k.FRAMERATE'))
	self._readout_speed = float(aoget_fits_keyword(hdr, 'ocam2k.READOUT_SPEED'))
	self._binning 		= long(aoget_fits_keyword(hdr, 'ocam2k.BINNING'))
	self._status  		= aoget_fits_keyword(hdr, 'ocam2k.STATUS')

	dark_filename 		= aoget_fits_keyword(hdr,'ocam2k.DARK_FILENAME')
	dark_subdir 		= ['wfs_calib_'+wunit,'ocam2k','backgrounds','bin'+strtrim(self._binning,2)]
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
        _framerate     : 0.0	, $
        _readout_speed : 0.0	, $
        _binning       : 0L	    , $
        _dark_filename : ''     , $
        _status		   : ''		, $
        INHERITS    AOhelp  $
    }
end
