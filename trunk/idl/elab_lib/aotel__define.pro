function AOtel::Init, fitsfile
    if not file_test(fitsfile) then return,0
	hdr = headfits(fitsfile, /SILENT)

	angle_arcsec = float(aoget_fits_keyword(hdr, 'tel.ROTATOR.ANGLE'))
	if angle_arcsec ne -9999. then self._rot_angle = (angle_arcsec/206264.806)*(180./!PI) else $
								   self._rot_angle = !VALUES.F_NAN

	az = float(aoget_fits_keyword(hdr, 'tel.TEL.AZ'))
	if az ne -9999. then self._az = az else self._az = !VALUES.F_NAN

	el = float(aoget_fits_keyword(hdr, 'tel.TEL.EL'))
	if el ne -9999. then self._el = el else self._el = !VALUES.F_NAN

	istracking = long(aoget_fits_keyword(hdr, 'tel.TEL.ISTRACKING'))
	if istracking ne -9999 then self._istracking = istracking else self._istracking = -1L

;name                  type      description
;tel.AMB.WINDSPEED     real   wind speed in m/s
;tel.HEXAPOD.ABS_POS0  real   hexapod position X (mm)
;tel.HEXAPOD.ABS_POS1  real   hexapod position Y (mm)
;tel.HEXAPOD.ABS_POS2  real   hexapod position Z (mm)
;tel.HEXAPOD.ABS_POS3  real   hexapod rotation X (arcseconds)
;tel.HEXAPOD.ABS_POS4  real   hexapod rotation Y (arcseconds)
;tel.HEXAPOD.ABS_POS5  real   hexapod rotation Z (arcseconds)
;tel.HEXAPOD.STATUS    int    hexapod status (significato ignoto)

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOtel', 'Represent TELESCOPE status') then return, 0
    self->addMethodHelp, "rot_angle()", "AGW rotator angle in degrees (float)"
    self->addMethodHelp, "az()", "Telescope azimuth angle in arcseconds (float)"
    self->addMethodHelp, "el()", "Telescope elevation angle in arcseconds (float)"
    self->addMethodHelp, "istracking()", "Telescope tracking flag: 1L: Tracking. 0L: Not tracking. -1L: Unknown."

	return,1
end

function AOtel::rot_angle
	return, self._rot_angle
end

function AOtel::az
	return, self._az
end

function AOtel::el
	return, self._el
end

function AOtel::istracking
	return, self._ISTRACKING
end

pro AOtel::Cleanup
	self->AOhelp::Cleanup
end


pro AOtel__define
    struct = { AOtel, $
        _rot_angle					: 0.			, $ ; AGW rotator angle in degrees
        _az				            : 0.			, $	; Telescope azimuth in arcseconds
        _el							: 0.			, $ ; Telescope elevation in arcseconds
        _ISTRACKING					: 0L			, $ ; 1B if telescope is currently tracking, 0B otherwise.
        INHERITS AOhelp $
    }
end
