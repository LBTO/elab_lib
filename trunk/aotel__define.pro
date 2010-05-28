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

	indoor_wind = float(aoget_fits_keyword(hdr, 'tel.AMB.WINDSPEED'))
	if indoor_wind ne -9999. then self._indoor_wind = indoor_wind else self._indoor_wind = !VALUES.F_NAN

	hex0 = float(aoget_fits_keyword(hdr, 'tel.HEXAPOD.ABS_POS0'))
	hex1 = float(aoget_fits_keyword(hdr, 'tel.HEXAPOD.ABS_POS1'))
	hex2 = float(aoget_fits_keyword(hdr, 'tel.HEXAPOD.ABS_POS2'))
	hex3 = float(aoget_fits_keyword(hdr, 'tel.HEXAPOD.ABS_POS3'))
	hex4 = float(aoget_fits_keyword(hdr, 'tel.HEXAPOD.ABS_POS4'))
	hex5 = float(aoget_fits_keyword(hdr, 'tel.HEXAPOD.ABS_POS5'))
	if hex0 ne -9999. then self._hex[0] = hex0 else self._hex[0] = !VALUES.F_NAN
	if hex1 ne -9999. then self._hex[1] = hex1 else self._hex[1] = !VALUES.F_NAN
	if hex2 ne -9999. then self._hex[2] = hex2 else self._hex[2] = !VALUES.F_NAN
	if hex3 ne -9999. then self._hex[3] = hex3 else self._hex[3] = !VALUES.F_NAN
	if hex4 ne -9999. then self._hex[4] = hex4 else self._hex[4] = !VALUES.F_NAN
	if hex5 ne -9999. then self._hex[5] = hex5 else self._hex[5] = !VALUES.F_NAN

	swa = float(aoget_fits_keyword(hdr, 'tel.SWA.DEPLOYED'))
	if swa ne -9999. then self._swa = swa else self._swa = !VALUES.F_NAN

	ter0 = float(aoget_fits_keyword(hdr, 'tel.TERTIARY.ABS_POS0'))
	ter1 = float(aoget_fits_keyword(hdr, 'tel.TERTIARY.ABS_POS1'))
	ter2 = float(aoget_fits_keyword(hdr, 'tel.TERTIARY.ABS_POS2'))
	ter3 = float(aoget_fits_keyword(hdr, 'tel.TERTIARY.ABS_POS3'))
	if ter0 ne -9999. then self._ter[0] = ter0 else self._ter[0] = !VALUES.F_NAN
	if ter1 ne -9999. then self._ter[1] = ter1 else self._ter[1] = !VALUES.F_NAN
	if ter2 ne -9999. then self._ter[2] = ter2 else self._ter[2] = !VALUES.F_NAN
	if ter3 ne -9999. then self._ter[3] = ter3 else self._ter[3] = !VALUES.F_NAN

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
    self->addMethodHelp, "wind_speed()", "Wind speed in the dome(m/s)"
    self->addMethodHelp, "hexapod()", "Hexapod position XYZRxRyRz  (mm - arcsec)"
    self->addMethodHelp, "tertiary()", "Tertiary position Rx Ry Z alpha  (arcsec/arcsec/mm/degree)"
    self->addMethodHelp, "swing_arm()", "Swing arm position (?)"

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

function AOtel::wind_speed
	return, self._indoor_wind
end

function AOtel::hexapod
	return, self._hex
end

function AOtel::tertiary
	return, self._ter
end

function AOtel::swing_arm
	return, self._swa
end

pro AOtel::Cleanup
	self->AOhelp::Cleanup
end

pro AOtel::summary
    print, string(format='(%"%-30s %f")','Az angle', self->az() )
    print, string(format='(%"%-30s %f")','El angle', self->el() )
    print, string(format='(%"%-30s %f")','Rotator angle', self->rot_angle() )
    print, string(format='(%"%-30s %f  %f")','Is tracking', self->istracking() )
    print, string(format='(%"%-30s %f  %f  %f  %f  %f  %f")','Hexapod', self->hexapod() )
    print, string(format='(%"%-30s %f  %f  %f  %f")','Tertiary', self->tertiary() )
    print, string(format='(%"%-30s %f")','Swing arm', self->swing_arm() )
    print, string(format='(%"%-30s %f")','Dome Wind speed', self->wind_speed() )
end


pro AOtel__define
    struct = { AOtel, $
        _rot_angle					: 0.			, $ ; AGW rotator angle in degrees
        _az				            : 0.			, $	; Telescope azimuth in arcseconds
        _el							: 0.			, $ ; Telescope elevation in arcseconds
        _ISTRACKING					: 0L			, $ ; 1B if telescope is currently tracking, 0B otherwise.
        _indoor_wind				: 0.			, $ ; wind speed in dome
        _hex	    				: fltarr(6)		, $ ; hexapod
        _ter       					: fltarr(4)		, $ ; tertiary
        _swa    					: 0.     		, $ ; swing arm deployed
        INHERITS AOhelp $
    }
end
