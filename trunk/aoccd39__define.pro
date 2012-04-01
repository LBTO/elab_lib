
;+
;
;-

function AOccd39::Init, wfs_header, wunit

	self._header = wfs_header
	hdr = *(self._header)

	self._framerate     = float(aoget_fits_keyword(hdr, 'ccd39.FRAMERATE'))
	self._readout_speed = float(aoget_fits_keyword(hdr, 'ccd39.READOUT_SPEED'))
	self._binning 		= long(aoget_fits_keyword(hdr, 'ccd39.BINNING'))
	self._status  		= aoget_fits_keyword(hdr, 'ccd39.STATUS')

	dark_filename 		= aoget_fits_keyword(hdr,'ccd39.DARK_FILENAME')
	dark_subdir 		= ['wfs_calib_'+wunit,'ccd39','backgrounds','bin'+strtrim(self._binning,2)]
	self._dark_filename = filepath(root=ao_datadir(), sub=dark_subdir,  dark_filename)

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOccd39', 'Represent CCD39') then return, 0
    self->addMethodHelp, "framerate()", "frame rate [Hz] (float)"
    self->addMethodHelp, "readout_speed ()", "readout speed [kpix/s] (float)"
    self->addMethodHelp, "binning()", "binning (long)"
    self->addMethodHelp, "status()", "status (string)"
;    self->addMethodHelp, "dark()", "dark frame"
    self->addMethodHelp, "dark_fname()", "filename of dark frame"

    return, 1
end

function AOccd39::framerate
	return, self._framerate
end

function AOccd39::readout_speed
	return, self._readout_speed
end

function AOccd39::binning
	return, self._binning
end

function AOccd39::status
	return, self._status
end

;function AOccd39::dark
;	return, float(readfits(self._dark_filename, /silent))
;end

function AOccd39::dark_fname
	return, self._dark_filename
end

pro AOccd39::test
    d = self->framerate()
    d = self->readout_speed()
    d = self->binning()
    d = self->status()
;    d = self->dark()
    d = self->dark_fname()
end

pro AOccd39::summary, COMPREHENSIVE=COMPREHENSIVE
    print, string(format='(%"%-30s %d")','Binning', self->binning() )
    print, string(format='(%"%-30s %f")','Frequency [Hz]', self->framerate() )
	if keyword_set(COMPREHENSIVE) then begin
    	print, string(format='(%"%-30s %f")','Readout speed [kpix/s]', self->readout_speed() )
   		print, string(format='(%"%-30s %s")','CCD39 dark file', self->dark_fname() )
   		print, string(format='(%"%-30s %s")','CCD39 status', self->status() )
	endif
end

pro AOccd39::free
    ;if ptr_valid(self._header) then ptr_free, self._header
end

pro AOccd39::Cleanup
    if ptr_valid(self._header) then ptr_free, self._header
    self->AOhelp::Cleanup
end


pro AOccd39__define
    struct = { AOccd39, $
        _header        : ptr_new(), $
        _framerate     : 0.0	, $
        _readout_speed : 0.0	, $
        _binning       : 0L	    , $
        _dark_filename : ''     , $
        _status		   : ''		, $
        INHERITS    AOhelp  $
    }
end
