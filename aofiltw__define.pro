
;+
;
;-

function AOfiltw::Init, wfs_header, wunit, fw_number, julianday

	hdr = *wfs_header

	;The actual position of the filter wheel is known ONLY IF the FW status is one of the following:
	valid_status = ['STATE_OFF', 'STATE_READY', 'STATE_OPERATING', 'STATE_BUSY']
	idx = where(valid_status eq aoget_fits_keyword(hdr, 'fw'+fw_number+'.STATUS'))
	if idx eq [-1] then return, 0

	self._header = wfs_header
	self._fw_pos = round(float(aoget_fits_keyword(hdr, 'fw'+fw_number+'.POSITION')))
        self._julianday = julianday
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
function AOfiltw::filters_LUT, wunit, fw_number, pos


;  Structure definition, also works for invalid lookups

  INVALID = {filtw, name:'UNKNOWN', R:!VALUES.F_NAN, T:!VALUES.F_NAN, CW:!VALUES.F_NAN, BW:!VALUES.F_NAN} 

;		NOTE: in the case of FW1, the REFLECTED light goes towards the CCD47, and
;		      the TRANSMITTED light goes towards the CCD39

  W1_FW1 = replicate(INVALID, 6)
  W1_FW1[0] = {filtw, 'Dichroic 600-1000 nm',   0.05,  0.95,  800., 400.}
  W1_FW1[1] = {filtw, 'Dichroic 700-1000 nm',   0.05,  0.95,  850., 300.}
  W1_FW1[2] = {filtw, 'R = 50%, T= 50%',        0.50,  0.50,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W1_FW1[3] = {filtw, 'R = 90%, T= 10%',        0.90,  0.10,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W1_FW1[4] = {filtw, 'Silver mirror (R=100%)', 1.00,  0.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W1_FW1[5] = {filtw, 'R = 0.4%, T = 99.6%',    0.004, 0.996, !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  W1_FW2 = replicate(INVALID, 6)
  W1_FW2[0] = {filtw, 'empty',     0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W1_FW2[1] = {filtw, 'FB850-10',  0.50,  0.50,  850., 10.}
  W1_FW2[2] = {filtw, 'FB900-10',  0.50,  0.50,  900., 10.}
  W1_FW2[3] = {filtw, 'empty',     0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W1_FW2[4] = {filtw, 'FB700-10',  0.50,  0.50,  700., 10.}
  W1_FW2[5] = {filtw, 'empty',     0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  W1SOUL_FW1 = W1_FW1

  W1SOUL_FW2 = replicate(INVALID, 6)
  W1SOUL_FW2[0] = {filtw, 'empty',     0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W1SOUL_FW2[1] = {filtw, 'empty',     0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W1SOUL_FW2[2] = {filtw, 'FB650-10',  0.50,  0.50,  650., 10.}
  W1SOUL_FW2[3] = {filtw, 'FB900-10',  0.50,  0.50,  900., 10.}
  W1SOUL_FW2[4] = {filtw, 'FB700-10',  0.50,  0.50,  700., 10.}

  W2_FW1 = replicate(INVALID, 6)
  W2_FW1[0] = {filtw, 'Dichroic 600-1000 nm',   0.05,  0.95,  800., 400.}
  W2_FW1[1] = {filtw, 'Dichroic 700-1000 nm',   0.05,  0.95,  850., 300.}
  W2_FW1[2] = {filtw, 'R = 50%, T= 50%',        0.50,  0.50,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W2_FW1[3] = {filtw, 'R = 90%, T= 10%',        0.90,  0.10,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W2_FW1[4] = {filtw, 'Silver mirror (R=100%)', 1.00,  0.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W1_FW1[5] = {filtw, 'R = 0.4%, T = 99.6%',    0.004, 0.996, !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  W2_FW2 = replicate(INVALID, 6)
  W2_FW2[0] = {filtw, 'empty',     0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W2_FW2[1] = {filtw, 'FB850-10',  0.50,  0.50,  850., 10.}
  W2_FW2[2] = {filtw, 'FB900-10',  0.50,  0.50,  900., 10.}
  W2_FW2[3] = {filtw, 'empty',     0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  W2_FW2[4] = {filtw, 'FB950-10',  0.50,  0.50,  950., 10.}
  W2_FW2[5] = {filtw, 'empty',     0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  W2SOUL_FW1 = W2_FW1
  W2SOUL_FW2 = W2_FW2

  MAG_FW1 = replicate(INVALID, 6)
  MAG_FW1[0] = {filtw, 'R = 50%, T= 50%',      0.50,  0.50,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  MAG_FW1[1] = {filtw, 'empty',                0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  MAG_FW1[2] = {filtw, 'DARK',                 0.00,  0.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  MAG_FW1[3] = {filtw, 'empty',                0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  MAG_FW1[4] = {filtw, 'empty',                0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  MAG_FW1[5] = {filtw, 'LPF950nm',             !VALUES.F_NAN,  !VALUES.F_NAN,  !VALUES.F_NAN, !VALUES.F_NAN }

  MAG_FW2 = replicate(INVALID, 6)
  MAG_FW2[0] = {filtw, 'empty',                0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  MAG_FW2[1] = {filtw, 'SDSS z',               !VALUES.F_NAN,  0.50,  902., 300. }
  MAG_FW2[2] = {filtw, 'SDSS r',               !VALUES.F_NAN,  0.70,  625., 150. }
  MAG_FW2[3] = {filtw, 'SDSS i',               !VALUES.F_NAN,  0.80,  765., 150. }
  MAG_FW2[4] = {filtw, 'LPF950',               !VALUES.F_NAN,  0.05,  0.981, 200. }  ; TODO the CW seems wrong
  MAG_FW2[5] = {filtw, 'empty',                0.00,  1.00,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  LBTIDX_OLD_FW1 = replicate(INVALID, 6)
  LBTIDX_OLD_FW1[0] = {filtw, 'Window',           0.05,  0.95,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_OLD_FW1[1] = {filtw, '400 - 700nm',      0.05,  0.95,  550., 300. }
  LBTIDX_OLD_FW1[2] = {filtw, 'ND, OD3, T=0.1%',  0.00,  0.001,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_OLD_FW1[3] = {filtw, 'ND, OD2, T=1.0%',  0.00,  0.01 ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_OLD_FW1[4] = {filtw, 'ND, OD1, T=10.0%', 0.00,  0.1  ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_OLD_FW1[5] = {filtw, 'Blank',            0.00,  0.00 ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  LBTIDX_OLD_FW2 = replicate(INVALID, 6)
  LBTIDX_OLD_FW2[0] = {filtw, 'OD=2.5, T=0.3%',   0.00,  0.003,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_OLD_FW2[1] = {filtw, 'OD=3, T=0.01',     0.00,  0.001,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_OLD_FW2[2] = {filtw, 'OD=1, T=10%',      0.00,  0.1,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_OLD_FW2[3] = {filtw, 'OPEN',             0.00,  1.0,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_OLD_FW2[4] = {filtw, 'OD=2, T=1%',       0.00,  0.01,   !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_OLD_FW2[5] = {filtw, 'empty',            0.00,  1.0,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  LBTIDX_SOUL_FW1 = replicate(INVALID, 6)
  LBTIDX_SOUL_FW1[0] = {filtw, 'Blank',            0.00,  0.00 ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_SOUL_FW1[1] = {filtw, '400 - 700nm',      0.05,  0.813,  550., 300. }
  LBTIDX_SOUL_FW1[2] = {filtw, 'ND, OD3, T=0.1%',  0.00,  0.001,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_SOUL_FW1[3] = {filtw, 'ND, OD2, T=1.0%',  0.00,  0.01 ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_SOUL_FW1[4] = {filtw, 'ND, OD1, T=10.0%', 0.00,  0.1  ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_SOUL_FW1[5] = {filtw, 'Window',           0.05,  1.64 ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  LBTIDX_SOUL_FW2 = replicate(INVALID, 6)
  LBTIDX_SOUL_FW2[0] = {filtw, 'OD=2.5, T=0.3%',   0.00,  0.003,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_SOUL_FW2[1] = {filtw, 'OD=3, T=0.01',     0.00,  0.001,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_SOUL_FW2[2] = {filtw, 'OD=1, T=10%',      0.00,  0.1,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_SOUL_FW2[3] = {filtw, 'OPEN',             0.00,  1.0,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_SOUL_FW2[4] = {filtw, 'OD=2, T=1%',       0.00,  0.01,   !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTIDX_SOUL_FW2[5] = {filtw, 'empty',            0.00,  1.0,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  LBTISX_OLD_FW1 = replicate(INVALID, 6)
  LBTISX_OLD_FW1[0] = {filtw, 'Window',           0.05,  0.95,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_OLD_FW1[1] = {filtw, 'ND, OD1, T=10.0%', 0.00,  0.1  ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_OLD_FW1[2] = {filtw, 'ND, OD2, T=1.0%',  0.00,  0.01 ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_OLD_FW1[3] = {filtw, 'ND, OD3, T=0.1%',  0.00,  0.001,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_OLD_FW1[4] = {filtw, '400 - 700nm',      0.05,  0.95,  550., 300. }
  LBTISX_OLD_FW1[5] = {filtw, 'Blank',            0.00,  0.00 ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  LBTISX_OLD_FW2 = replicate(INVALID, 6)
  LBTISX_OLD_FW2[0] = {filtw, 'OD=2.5, T=0.3%',   0.00,  0.003,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_OLD_FW2[1] = {filtw, 'OD=3, T=0.01',     0.00,  0.001,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_OLD_FW2[2] = {filtw, 'OD=1, T=10%',      0.00,  0.1,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_OLD_FW2[3] = {filtw, 'OPEN',             0.00,  1.0,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_OLD_FW2[4] = {filtw, 'OD=2, T=1%',       0.00,  0.01,   !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_OLD_FW2[5] = {filtw, 'empty',            0.00,  1.0,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  LBTISX_SOUL_FW1 = replicate(INVALID, 6)
  LBTISX_SOUL_FW1[0] = {filtw, 'Blank',            0.00,  0.00 ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_SOUL_FW1[1] = {filtw, '400 - 700nm',      0.05,  0.813,  550., 300. }
  LBTISX_SOUL_FW1[2] = {filtw, 'ND, OD3, T=0.1%',  0.00,  0.001,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_SOUL_FW1[3] = {filtw, 'ND, OD2, T=1.0%',  0.00,  0.01 ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_SOUL_FW1[4] = {filtw, 'ND, OD1, T=10.0%', 0.00,  0.1  ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_SOUL_FW1[5] = {filtw, 'Window',           0.05,  1.64 ,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  LBTISX_SOUL_FW2 = replicate(INVALID, 6)
  LBTISX_SOUL_FW2[0] = {filtw, 'OD=2.5, T=0.3%',   0.00,  0.003,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_SOUL_FW2[1] = {filtw, 'OD=3, T=0.01',     0.00,  0.001,  !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_SOUL_FW2[2] = {filtw, 'OD=1, T=10%',      0.00,  0.1,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_SOUL_FW2[3] = {filtw, 'OPEN',             0.00,  1.0,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_SOUL_FW2[4] = {filtw, 'OD=2, T=1%',       0.00,  0.01,   !VALUES.F_INFINITY, !VALUES.F_INFINITY }
  LBTISX_SOUL_FW2[5] = {filtw, 'empty',            0.00,  1.0,    !VALUES.F_INFINITY, !VALUES.F_INFINITY }

  if (wunit eq 'W1') and (fw_number eq 1) then data = W1_FW1 $ 
  else if (wunit eq 'W1') and (fw_number eq 2) then data = W1_FW2 $
  else if (wunit eq 'W2') and (fw_number eq 1) then data = W2_FW1 $
  else if (wunit eq 'W2') and (fw_number eq 2) then data = W2_FW2 $
  else if (wunit eq 'MAG') and (fw_number eq 1) then data = MAG_FW1 $ 
  else if (wunit eq 'MAG') and (fw_number eq 2) then data = MAG_FW2 $
  else if (wunit eq 'W1SOUL') and (fw_number eq 1) then data = W1SOUL_FW1 $
  else if (wunit eq 'W1SOUL') and (fw_number eq 2) then data = W1SOUL_FW2 $
  else if (wunit eq 'W2SOUL') and (fw_number eq 1) then data = W2SOUL_FW1 $
  else if (wunit eq 'W2SOUL') and (fw_number eq 2) then data = W2SOUL_FW2 $
  else if (wunit eq 'LBTIDX_OLD') and (fw_number eq 1) then data = LBTIDX_OLD_FW1 $
  else if (wunit eq 'LBTIDX_OLD') and (fw_number eq 2) then data = LBTIDX_OLD_FW2 $
  else if (wunit eq 'LBTIDX_SOUL') and (fw_number eq 1) then data = LBTIDX_SOUL_FW1 $
  else if (wunit eq 'LBTIDX_SOUL') and (fw_number eq 2) then data = LBTIDX_SOUL_FW2 $
  else if (wunit eq 'LBTISX_OLD') and (fw_number eq 1) then data = LBTISX_OLD_FW1 $
  else if (wunit eq 'LBTISX_OLD') and (fw_number eq 2) then data = LBTISX_OLD_FW2 $
  else if (wunit eq 'LBTISX_SOUL') and (fw_number eq 1) then data = LBTISX_SOUL_FW1 $
  else if (wunit eq 'LBTISX_SOUL') and (fw_number eq 2) then data = LBTISX_SOUL_FW2 $
  else message, "Invalid WUNIT and/or filterwheel number. Wunit="+wunit+', fw='+strtrim(fw_number,2)

  if (pos lt 0) or (pos ge n_elements(data)) then return, INVALID $
  else return, data[pos]

end

pro AOfiltw::filtw_data, wunit, fw_number

    LBTISX_SOUL_DATE = julday(06, 01, 2018, 0, 0, 0)  ; M,D,Y  h,m,s
    LBTIDX_SOUL_DATE = julday(02, 28, 2019, 0, 0, 0)  ; M,D,Y, h,m,s
  
    if wunit eq 'LBTIDX' then begin
          if self._julianday lt LBTIDX_SOUL_DATE then wunit = 'LBTIDX_OLD' $ 
          else wunit = 'LBTIDX_SOUL'
    endif

    if wunit eq 'LBTISX' then begin
          if self._julianday lt LBTISX_SOUL_DATE then wunit = 'LBTISX_OLD' $
          else wunit = 'LBTISX_SOUL'
    endif

    data_struct = self->filters_LUT(wunit, fw_number, self._fw_pos)

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
        _julianday      : 0.0d,         $
        INHERITS    AOhelp  $
    }
end
