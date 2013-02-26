
;+
;
;-

function AOfiltw::Init, wfs_header, wunit, fw_number

	hdr = *wfs_header

	;The actual position of the filter wheel is known ONLY IF the FW status is one of the following:
	valid_status = ['STATE_OFF', 'STATE_READY', 'STATE_OPERATING', 'STATE_BUSY']
	idx = where(valid_status eq aoget_fits_keyword(hdr, 'fw'+fw_number+'.STATUS'))
	if idx eq [-1] then return, 0

	self._header = wfs_header
	self._fw_pos = round(float(aoget_fits_keyword(hdr, 'fw'+fw_number+'.POSITION')))
	self->filtw_data, wunit, fw_number
    self._fw_number = fw_number

       ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOfiltw', 'Represent WFS filter wheel object') then return, 0
    self->addMethodHelp, "fw_number()", "filter wheel number  (long)"
    self->addMethodHelp, "fw_pos()",  "filter position  (long)"
    self->addMethodHelp, "fw_data()", "{name, reflectivity, transmittivity, central lambda, bandwidth}  (long)"
    self->addMethodHelp, "name()", "filter name e.g. 'Dichroic 600-100 nm'"
    self->addMethodHelp, "reflectivity()", "Reflectivity"
    self->addMethodHelp, "transmissivity()", "Transmissivity"
    self->addMethodHelp, "cw()", "Central wavelength (nm)"
    self->addMethodHelp, "bw()", "Bandwidth (nm)"
    self->addMethodHelp, "header()", "header of fitsfile (strarr)"

	return, 1
end

;+
; Retrieves the characteristics of the filters installed on the filter wheel.

; The structure contains the following information:
;	NAME:	string variable summarizing the characteristics of the filter.
;   R:		fraction of light reflected
;	T:		fraction of light transmitted
;	CW:		Central wavelength
;	BW:		Bandwidth centered on CW
;-
pro AOfiltw::filtw_data, wunit, fw_number

  if wunit eq 'W1' then begin	;LBT W1 (FLAO2) 
	CASE fw_number OF

;		NOTE: in the case of FW1, the REFLECTED light goes towards the CCD47, and
;		      the TRANSMITTED light goes towards the CCD39
	'1': CASE self._fw_pos OF

			0: data_struct = CREATE_STRUCT("name", 'Dichroic 600-1000 nm', $
									  "R"	,0.05				, $
									  "T"	,0.95				, $
									  "CW"	,800.				, $
									  "BW"	,400.				)

			1: data_struct = CREATE_STRUCT("name", 'Dichroic 700-1000 nm', $
									  "R"	,0.05				, $
									  "T"	,0.95				, $
									  "CW"	,850.				, $
									  "BW"	,300.				)

			2: data_struct = CREATE_STRUCT("name", 'R = 50%, T= 50%'	, $
									  "R"	,0.50				, $
									  "T"	,0.50				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!values.F_INFINITY	)

			3: data_struct = CREATE_STRUCT("name", 'R = 90%, T= 10%'	, $
									  "R"	,0.90				, $
									  "T"	,0.10				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			4: data_struct = CREATE_STRUCT("name", 'Silver mirror (R=100%)'	, $
									  "R"	,1.0				, $
									  "T"	,0.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			5: data_struct = CREATE_STRUCT("name", 'R = 0.4%, T = 99.6%'	, $
									  "R"	,0.004				, $
									  "T"	,0.996				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			ELSE: data_struct = CREATE_STRUCT("name", 'UNKNOWN'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)

		 ENDCASE

	'2': CASE self._fw_pos OF

			0: data_struct = CREATE_STRUCT("name", 'empty'	, $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			1: data_struct = CREATE_STRUCT("name", 'FB850-10'	, $
									  "R"	,0.50				, $
									  "T"	,0.50				, $
									  "CW"	,850. 				, $
									  "BW"	,10.	)

			2: data_struct = CREATE_STRUCT("name", 'FB900-10'	, $
									  "R"	,0.50				, $
									  "T"	,0.50				, $
									  "CW"	,900. 				, $
									  "BW"	,10.	)

			3: data_struct = CREATE_STRUCT("name", 'empty'	, $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			4: data_struct = CREATE_STRUCT("name", 'FB700-10'	, $
									  "R"	,0.50				, $
									  "T"	,0.50				, $
									  "CW"	,700. 				, $
									  "BW"	,10.	)

			5: data_struct = CREATE_STRUCT("name", 'empty'	, $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			ELSE: data_struct = CREATE_STRUCT("name", 'UNKNOWN'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)
		 ENDCASE
	ENDCASE
    endif else if wunit eq 'W2' then begin	;LBT W2 (FLAO1) 
	CASE fw_number OF

;		NOTE: in the case of FW1, the REFLECTED light goes towards the CCD47, and
;		      the TRANSMITTED light goes towards the CCD39
	'1': CASE self._fw_pos OF

			0: data_struct = CREATE_STRUCT("name", 'Dichroic 600-1000 nm', $
									  "R"	,0.05				, $
									  "T"	,0.95				, $
									  "CW"	,800.				, $
									  "BW"	,400.				)

			1: data_struct = CREATE_STRUCT("name", 'Dichroic 700-1000 nm', $
									  "R"	,0.05				, $
									  "T"	,0.95				, $
									  "CW"	,850.				, $
									  "BW"	,300.				)

			2: data_struct = CREATE_STRUCT("name", 'R = 50%, T= 50%'	, $
									  "R"	,0.50				, $
									  "T"	,0.50				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!values.F_INFINITY	)

			3: data_struct = CREATE_STRUCT("name", 'R = 90%, T= 10%'	, $
									  "R"	,0.90				, $
									  "T"	,0.10				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			4: data_struct = CREATE_STRUCT("name", 'Silver mirror (R=100%)'	, $
									  "R"	,1.0				, $
									  "T"	,0.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			5: data_struct = CREATE_STRUCT("name", 'R = 0.4%, T = 99.6%'	, $
									  "R"	,0.004				, $
									  "T"	,0.996				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			ELSE: data_struct = CREATE_STRUCT("name", 'UNKNOWN'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)

		 ENDCASE

	'2': CASE self._fw_pos OF

			0: data_struct = CREATE_STRUCT("name", 'empty'	, $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			1: data_struct = CREATE_STRUCT("name", 'FB850-10'	, $
									  "R"	,0.50				, $
									  "T"	,0.50				, $
									  "CW"	,850. 				, $
									  "BW"	,10.	)

			2: data_struct = CREATE_STRUCT("name", 'FB900-10'	, $
									  "R"	,0.50				, $
									  "T"	,0.50				, $
									  "CW"	,900. 				, $
									  "BW"	,10.	)

			3: data_struct = CREATE_STRUCT("name", 'empty'	, $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			4: data_struct = CREATE_STRUCT("name", 'FB950-10'	, $
									  "R"	,0.50				, $
									  "T"	,0.50				, $
									  "CW"	,950. 				, $
									  "BW"	,10.	)

			5: data_struct = CREATE_STRUCT("name", 'empty'	, $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			ELSE: data_struct = CREATE_STRUCT("name", 'UNKNOWN'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)
		 ENDCASE
	ENDCASE

    endif else if wunit eq 'MAG' then begin

	CASE fw_number OF

;		NOTE: in the case of FW1, the REFLECTED light goes towards the CCD47, and
;		      the TRANSMITTED light goes towards the CCD39
	'1': CASE self._fw_pos OF

			0: data_struct = CREATE_STRUCT("name", 'R = 50%, T= 50%'	, $
									  "R"	,0.50				, $
									  "T"	,0.50				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!values.F_INFINITY	)

			1: data_struct = CREATE_STRUCT("name", 'empty', $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			2: data_struct = CREATE_STRUCT("name", 'DARK', $
									  "R"	,0.0				, $
									  "T"	,0.0				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			3: data_struct = CREATE_STRUCT("name", 'empty', $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			4: data_struct = CREATE_STRUCT("name", 'empty', $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			5: data_struct = CREATE_STRUCT("name", 'LPF950nm'	, $		; lambda>950nm to CCD39
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)

			ELSE: data_struct = CREATE_STRUCT("name", 'UNKNOWN'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)

		 ENDCASE

	'2': CASE self._fw_pos OF

			0: data_struct = CREATE_STRUCT("name", 'empty'	, $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			1: data_struct = CREATE_STRUCT("name", 'SDSS z'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,0.50				, $
									  "CW"	,902. 				, $
									  "BW"	,300.	)

			2: data_struct = CREATE_STRUCT("name", 'SDSS r'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,0.70				, $
									  "CW"	,625. 				, $
									  "BW"	,150.	)

			3: data_struct = CREATE_STRUCT("name", 'SDSS i'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,0.80				, $
									  "CW"	,765.			    , $
									  "BW"	,150.	)

			4: data_struct = CREATE_STRUCT("name", 'LPF950'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,0.05				, $
									  "CW"	,0.981		 		, $
									  "BW"	,200.	)

			5: data_struct = CREATE_STRUCT("name", 'empty'	, $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			ELSE: data_struct = CREATE_STRUCT("name", 'UNKNOWN'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)
		 ENDCASE
	ENDCASE



    endif else if wunit eq 'LBTIDX' then begin

	CASE fw_number OF

;		NOTE: in the case of FW1, the REFLECTED light goes towards the CCD47, and
;		      the TRANSMITTED light goes towards the CCD39
	'1': CASE self._fw_pos OF

			0: data_struct = CREATE_STRUCT("name", 'Window'	, $
									  "R"	,0.05				, $
									  "T"	,0.95				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!values.F_INFINITY	)

			1: data_struct = CREATE_STRUCT("name", 'ND, OD1, T=10.0%', $
									  "R"	,0.0				, $
									  "T"	,0.1				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			2: data_struct = CREATE_STRUCT("name", 'ND, OD2, T=1.0%', $
									  "R"	,0.0				, $
									  "T"	,0.01				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			3: data_struct = CREATE_STRUCT("name", 'ND, OD3, T=0.1%', $
									  "R"	,0.0				, $
									  "T"	,0.001				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			4: data_struct = CREATE_STRUCT("name", '400 - 700nm', $
									  "R"	,0.05				, $
									  "T"	,0.95				, $
									  "CW"	,550 	, $
									  "BW"	,300	)

			5: data_struct = CREATE_STRUCT("name", 'Blank'	, $		; lambda>950nm to CCD39
									  "R"	, 0.0		, $
									  "T"	, 0.0		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)

			ELSE: data_struct = CREATE_STRUCT("name", 'UNKNOWN'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)

		 ENDCASE

	'2': CASE self._fw_pos OF

			0: data_struct = CREATE_STRUCT("name", 'OD=2.5, T=0.3%'	, $
									  "R"	,0.0				, $
									  "T"	,0.003				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			1: data_struct = CREATE_STRUCT("name", 'OD=3.0, T=0.01'	, $
									  "R"	,0.0		, $
									  "T"	,0.001				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			2: data_struct = CREATE_STRUCT("name", 'OD=1.0, T=10%'	, $
									  "R"	,0.0		, $
									  "T"	,0.1				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			3: data_struct = CREATE_STRUCT("name", 'OPEN'	, $
									  "R"	,0.0		, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY	    , $
									  "BW"	,!VALUES.F_INFINITY	)

			4: data_struct = CREATE_STRUCT("name", 'OD=2.0, T=1%'	, $
									  "R"	,0.0	, $
									  "T"	,0.01				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			5: data_struct = CREATE_STRUCT("name", 'empty'	, $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			ELSE: data_struct = CREATE_STRUCT("name", 'UNKNOWN'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)
		 ENDCASE
	ENDCASE



    endif else if wunit eq 'LBTISX' then begin

	CASE fw_number OF

;		NOTE: in the case of FW1, the REFLECTED light goes towards the CCD47, and
;		      the TRANSMITTED light goes towards the CCD39
	'1': CASE self._fw_pos OF

			0: data_struct = CREATE_STRUCT("name", 'Window'	, $
									  "R"	,0.05				, $
									  "T"	,0.95				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!values.F_INFINITY	)

			1: data_struct = CREATE_STRUCT("name", 'ND, OD1, T=10.0%', $
									  "R"	,0.0				, $
									  "T"	,0.1				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			2: data_struct = CREATE_STRUCT("name", 'ND, OD2, T=1.0%', $
									  "R"	,0.0				, $
									  "T"	,0.01				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			3: data_struct = CREATE_STRUCT("name", 'ND, OD3, T=0.1%', $
									  "R"	,0.0				, $
									  "T"	,0.001				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			4: data_struct = CREATE_STRUCT("name", '400 - 700nm', $
									  "R"	,0.05				, $
									  "T"	,0.95				, $
									  "CW"	,550 	, $
									  "BW"	,300	)

			5: data_struct = CREATE_STRUCT("name", 'Blank'	, $		; lambda>950nm to CCD39
									  "R"	, 0.0		, $
									  "T"	, 0.0		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)

			ELSE: data_struct = CREATE_STRUCT("name", 'UNKNOWN'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)

		 ENDCASE

	'2': CASE self._fw_pos OF

			0: data_struct = CREATE_STRUCT("name", 'OD=2.5, T=0.3%'	, $
									  "R"	,0.0				, $
									  "T"	,0.003				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			1: data_struct = CREATE_STRUCT("name", 'OD=3.0, T=0.01'	, $
									  "R"	,0.0		, $
									  "T"	,0.001				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			2: data_struct = CREATE_STRUCT("name", 'OD=1.0, T=10%'	, $
									  "R"	,0.0		, $
									  "T"	,0.1				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			3: data_struct = CREATE_STRUCT("name", 'OPEN'	, $
									  "R"	,0.0		, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY	    , $
									  "BW"	,!VALUES.F_INFINITY	)

			4: data_struct = CREATE_STRUCT("name", 'OD=2.0, T=1%'	, $
									  "R"	,0.0	, $
									  "T"	,0.01				, $
									  "CW"	,!VALUES.F_INFINITY	, $
									  "BW"	,!VALUES.F_INFINITY	)

			5: data_struct = CREATE_STRUCT("name", 'empty'	, $
									  "R"	,0.0				, $
									  "T"	,1.0				, $
									  "CW"	,!VALUES.F_INFINITY , $
									  "BW"	,!VALUES.F_INFINITY	)

			ELSE: data_struct = CREATE_STRUCT("name", 'UNKNOWN'	, $
									  "R"	,!VALUES.F_NAN		, $
									  "T"	,!VALUES.F_NAN		, $
									  "CW"	,!VALUES.F_NAN 		, $
									  "BW"	,!VALUES.F_NAN	)
		 ENDCASE
	ENDCASE



  ENDIF

	self._fw_data = ptr_new(data_struct, /no_copy)

end


function AOfiltw::fw_data
	return, *self._fw_data
end

function AOfiltw::fw_pos
	return, self._fw_pos
end

function AOfiltw::fw_number
	return, self._fw_number
end

function AOfiltw::name
	return, (*self._fw_data).name
end

function AOfiltw::reflectivity
	return, (*self._fw_data).R
end

function AOfiltw::transmissivity
	return, (*self._fw_data).T
end

function AOfiltw::cw
	return, (*self._fw_data).cw
end

function AOfiltw::bw
	return, (*self._fw_data).bw
end

function AOfiltw::header
	return, *(self._header)
end

pro AOfiltw::test
    d=self->fw_number()
    d=self->fw_pos()
    d=self->fw_data()
    d=self->name()
    d=self->reflectivity()
    d=self->transmissivity()
    d=self->cw()
    d=self->bw()
    d=self->header()
end

pro AOfiltw::free
end

pro AOfiltw::Cleanup
	ptr_free, self._fw_data
    self->AOhelp::Cleanup
end

pro AOfiltw::summary, COMPREHENSIVE=COMPREHENSIVE
    if keyword_set(COMPREHENSIVE) then begin
		print, 'Filter Wheel #'+strtrim(self->fw_number(),2)+':'
    	print_struct, self->fw_data()
	endif else begin
		print, string(format='(%"%-30s %s")','FW#'+strtrim(self->fw_number(),2), self->name() )
	endelse
end

pro AOfiltw__define
    struct = { AOfiltw, $
        _header     : ptr_new()	, $
        _fw_pos  	: 0			, $
        _fw_data	: ptr_new() , $
        _fw_number	: 0         , $
        INHERITS    AOhelp  $
    }
end
