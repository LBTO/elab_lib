function AOtel::Init, root, fitsfile
    if not file_test(fitsfile) then begin
        message, fitsfile + ' not found', /info
        return,0
    endif

	hdr = headfits(fitsfile, /SILENT, errmsg=errmsg)
    if errmsg ne '' then message, fitsfile+ ': '+ errmsg, /info


	angle_arcsec = float(aoget_fits_keyword(hdr, 'tel.ROTATOR.ANGLE'))
	if angle_arcsec ne -9999. then self._rot_angle = (angle_arcsec/206264.806)*(180./!PI) else $
								   self._rot_angle = !VALUES.F_NAN

	az = double(aoget_fits_keyword(hdr, 'tel.TEL.AZ'))
	if az ne -9999. then self._az = az else self._az = !VALUES.F_NAN

	el = double(aoget_fits_keyword(hdr, 'tel.TEL.EL'))
	if el ne -9999. then self._el = el else self._el = !VALUES.F_NAN

    dec =  double(aoget_fits_keyword(hdr, 'tel.TEL.DEC'))
	if dec ne -9999. then self._dec = dec * 180d/!pi else self._dec = !VALUES.F_NAN

    ra  =  double(aoget_fits_keyword(hdr, 'tel.TEL.RA'))
	if ra ne -9999. then self._ra = ra * 180d/!pi/15 else self._ra = !VALUES.F_NAN

    istracking = long(aoget_fits_keyword(hdr, 'tel.TEL.ISTRACKING'))
	if istracking ne -9999 then self._istracking = istracking else self._istracking = -1L

    isguiding  =  long(aoget_fits_keyword(hdr, 'tel.TEL.ISGUIDING'))
	if isguiding ne -9999 then self._isguiding = isguiding else self._isguiding = -1L

    self._tel_hbs_on            =  long(aoget_fits_keyword(hdr, 'tel.TEL.HBS_ON'))
	if self._tel_hbs_on eq -9999 then self._tel_hbs_on = -1L

    self._tel_vent_on           =  long(aoget_fits_keyword(hdr, 'tel.TEL.VENT_ON'))
	if self._tel_vent_on eq -9999 then self._tel_vent_on = -1L

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

    if root->tracknum() lt '20101127_000000' then begin
        self._dimm_seeing           =  float(aoget_fits_keyword(hdr, 'tel.DIMM.SEEING'))
        if self._dimm_seeing lt 0. then self._dimm_seeing = !VALUES.F_NAN
    endif else begin
        dimm = readfits(filepath(root=root->datadir(),  'Dimm_'+root->tracknum()+'.fits'), /SILENT)
        self._dimm_seeing = median(dimm[0,*])
        if self._dimm_seeing lt 0. then self._dimm_seeing = float(aoget_fits_keyword(hdr, 'tel.DIMM.SEEING'))	;file Dimm...fits contiene merda....
        if self._dimm_seeing lt 0. then self._dimm_seeing = !VALUES.F_NAN		;Tutto e' pieno di merda....
    endelse
    self._guidecam_centroid_x   =  float(aoget_fits_keyword(hdr, 'tel.GUIDECAM.CENTROID.X'))
    self._guidecam_centroid_y   =  float(aoget_fits_keyword(hdr, 'tel.GUIDECAM.CENTROID.Y'))
    extern_wind_direction =  float(aoget_fits_keyword(hdr, 'tel.EXTERN.WINDDIRECTION'))
    if extern_wind_direction ne -9999. then self._extern_wind_direction = extern_wind_direction else self._extern_wind_direction = !VALUES.F_NAN
    extern_wind_speed =  float(aoget_fits_keyword(hdr, 'tel.EXTERN.WINDSPEED'))
    if extern_wind_speed ne -9999. then self._extern_wind_speed = extern_wind_speed else self._extern_wind_speed = !VALUES.F_NAN
;name                  type      description
; tel.AMB.WINDSPEED     real   wind speed in m/s  (on the swing arm)
; tel.HEXAPOD.ABS_POS0  real   hexapod position X (mm)
; tel.HEXAPOD.ABS_POS1  real   hexapod position Y (mm)
; tel.HEXAPOD.ABS_POS2  real   hexapod position Z (mm)
; tel.HEXAPOD.ABS_POS3  real   hexapod rotation X (arcseconds)
; tel.HEXAPOD.ABS_POS4  real   hexapod rotation Y (arcseconds)
; tel.HEXAPOD.ABS_POS5  real   hexapod rotation Z (arcseconds)
; tel.HEXAPOD.STATUS    int    hexapod status (significato ignoto)
; tel.DIMM.SEEING = '0.736199021339'
; tel.EXTERN.WINDDIRECTION = '250.935348511'
; tel.EXTERN.WINDSPEED = '3.29999995232'
; tel.GUIDECAM.CENTROID.X = '0.674229739671'
; tel.GUIDECAM.CENTROID.Y = '0.674229739671'
; tel.ROTATOR.ANGLE = '1358070.0'
; tel.SWA.DEPLOYED = '-9999   '
; tel.TEL.AZ = '44739.3184691'
; tel.TEL.DEC = '3.72526308274'
; tel.TEL.EL = '306205.354398'
; tel.TEL.HBS_ON = '-9999   '
; tel.TEL.ISGUIDING = '-9999   '
; tel.TEL.ISTRACKING = '-9999   '
; tel.TEL.RA = '0.655775413092'
; tel.TEL.VENT_ON = '-9999   '
; tel.TERTIARY.ABS_POS0 = '-1087.47875977'
; tel.TERTIARY.ABS_POS1 = '18.0770874023'
; tel.TERTIARY.ABS_POS2 = '-0.0    '
; tel.TERTIARY.ABS_POS3 = '-43.1807785034'


    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOtel', 'Represent TELESCOPE status') then return, 0
    self->addMethodHelp, "rot_angle()", "AGW rotator angle in degrees (float)"
    self->addMethodHelp, "az()", "Telescope azimuth angle in degrees (float)"
    self->addMethodHelp, "el()", "Telescope elevation angle in degrees (float)"
    self->addMethodHelp, "ra()",  "Telescope pointing right ascension angle in hours (float)"
    self->addMethodHelp, "dec()", "Telescope pointing declination angle in degrees (float)"
    self->addMethodHelp, "istracking()", "Telescope tracking flag: 1L: Tracking. 0L: Not tracking. -1L: Unknown."
    self->addMethodHelp, "isguiding()", "Telescope guiding flag: 1L: Guiding. 0L: Not guiding. -1L: Unknown."
    self->addMethodHelp, "hbs_on()", "HBS flag: 1L: On, 0L: off, -1L: Unknown."
    self->addMethodHelp, "vent_on()", "Primary mirror ventilation flag: 1L: On, 0L: off, -1L: Unknown."
    self->addMethodHelp, "hexapod()", "Hexapod position XYZRxRyRz  (mm - arcsec)"
    self->addMethodHelp, "tertiary()", "Tertiary position Rx Ry Z alpha  (arcsec/arcsec/mm/degree)"
    self->addMethodHelp, "swing_arm()", "Swing arm position (?)"
    self->addMethodHelp, "wind_speed()", "Wind speed in the dome(m/s)"
    self->addMethodHelp, "extern_wind_speed()", "Wind speed external (m/s)"
    self->addMethodHelp, "extern_wind_direction()", "Wind direction (deg)"
    self->addMethodHelp, "dimm_seeing()", "DIMM seeing (arcsec)"
    self->addMethodHelp, "guidecam_fwhm_x()", "FWHM of guidecam x (arcsec)"
    self->addMethodHelp, "guidecam_fwhm_y()", "FWHM of guidecam y (arcsec)"

	return,1
end

function AOtel::dimm_seeing
	return, self._dimm_seeing
end

function AOtel::guidecam_fwhm_x
	return, self._guidecam_centroid_x
end

function AOtel::guidecam_fwhm_y
	return, self._guidecam_centroid_y
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

function AOtel::dec
	return, self._dec
end

function AOtel::ra
	return, self._ra
end

function AOtel::hbs_on, label=label
    if keyword_set(label) then begin
        case (self._tel_hbs_on) of
            1: return, 'YES'
            0: return, 'NO'
           -1: return, 'UNKNOWN'
        endcase
    endif
	return, self._tel_hbs_on
end

function AOtel::vent_on, label=label
    if keyword_set(label) then begin
        case (self._tel_vent_on) of
            1: return, 'YES'
            0: return, 'NO'
           -1: return, 'UNKNOWN'
        endcase
    endif
	return, self._tel_vent_on
end

;Telescope tracking flag: 1L: Tracking. 0L: Not tracking. -1L: Unknown.
function AOtel::istracking, label=label
    if keyword_set(label) then begin
        case (self._istracking) of
            1: return, 'YES'
            0: return, 'NO'
           -1: return, 'UNKNOWN'
        endcase
    endif
	return, self._ISTRACKING
end

function AOtel::isguiding, label=label
    if keyword_set(label) then begin
        case (self._isguiding) of
            1: return, 'YES'
            0: return, 'NO'
           -1: return, 'UNKNOWN'
        endcase
    endif
	return, self._isguiding
end

function AOtel::wind_speed
	return, self._indoor_wind
end

function AOtel::extern_wind_speed
	return, self._extern_wind_speed
end

function AOtel::extern_wind_direction
	return, self._extern_wind_direction
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

pro AOtel::test
    d=self->rot_angle()
    d=self->az()
    d=self->el()
    d=self->ra()
    d=self->dec()
    d=self->istracking()
    d=self->isguiding()
    d=self->hbs_on()
    d=self->vent_on()
    d=self->hexapod()
    d=self->tertiary()
    d=self->swing_arm()
    d=self->wind_speed()
    d=self->extern_wind_speed()
    d=self->extern_wind_direction()
    d=self->dimm_seeing()
    d=self->guidecam_fwhm_x()
    d=self->guidecam_fwhm_y()
end

pro AOtel::Cleanup
	self->AOhelp::Cleanup
end

pro AOtel::summary
    print, string(format='(%"%-30s %f")','Az angle', self->az() )
    print, string(format='(%"%-30s %f")','El angle', self->el() )
    print, string(format='(%"%-30s %f")','Rotator angle', self->rot_angle() )
    print, string(format='(%"%-30s %f")','RA [hour]', self->ra() )
    print, string(format='(%"%-30s %f")','Dec [degree]', self->dec() )
    print, string(format='(%"%-30s %s")','Is tracking', self->istracking(/label) )
    print, string(format='(%"%-30s %s")','Is guiding', self->isguiding(/label) )
    print, string(format='(%"%-30s %s")','Is HBS on', self->hbs_on(/label) )
    print, string(format='(%"%-30s %s")','Is mirror ventilation on', self->vent_on(/label) )
    print, string(format='(%"%-30s %f  %f  %f  %f  %f  %f")','Hexapod', self->hexapod() )
    print, string(format='(%"%-30s %f  %f  %f  %f")','Tertiary', self->tertiary() )
    print, string(format='(%"%-30s %f")','Swing arm', self->swing_arm() )
    print, string(format='(%"%-30s %f")','Dome Wind speed', self->wind_speed() )
    print, string(format='(%"%-30s %f")','External Wind speed [m/s]', self->extern_wind_speed() )
    print, string(format='(%"%-30s %f")','External Wind direction [deg]', self->extern_wind_direction() )
    print, string(format='(%"%-30s %f")','Dimm seeing [arcsec]', self->dimm_seeing() )
    print, string(format='(%"%-30s %f")','FWHM of guidecam x [arcsec]', self->guidecam_fwhm_x() )
    print, string(format='(%"%-30s %f")','FWHM of guidecam y [arcsec]', self->guidecam_fwhm_y() )
end


pro AOtel__define
    struct = { AOtel, $
        _rot_angle					: 0d			, $ ; AGW rotator angle in degrees
        _az				            : 0d			, $	; Telescope azimuth in arcseconds
        _el							: 0d			, $ ; Telescope elevation in arcseconds
        _ISTRACKING					: 0L			, $ ; 1B if telescope is currently tracking, 0B otherwise.
        _indoor_wind				: 0.			, $ ; wind speed in dome
        _hex	    				: fltarr(6)		, $ ; hexapod
        _ter       					: fltarr(4)		, $ ; tertiary
        _swa    					: 0.     		, $ ; swing arm deployed
        _dimm_seeing    			: 0.     		, $ ; dimm seeing
        _extern_wind_direction 		: 0.     		, $ ;
        _extern_wind_speed     		: 0.     		, $ ;
        _guidecam_centroid_x  		: 0.     		, $ ;
        _guidecam_centroid_y 		: 0.     		, $ ;
        _dec                 		: 0d     		, $ ;
        _ra                  		: 0d     		, $ ;
        _tel_hbs_on          		: 0L     		, $ ;
        _tel_vent_on         		: 0L     		, $ ;
        _isguiding           		: 0L     		, $ ;
        INHERITS AOhelp $
    }
end
