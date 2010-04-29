

;+
;
;-



function AOadsec_status::Init, adsec_status_struct
    ; convert filepaths /usr/local/adopt/calib/left/adsec/*  in adsec_calib/*
    tmp_struct = adsec_status_struct
    self->ConvertFilePath, tmp_struct
    self._fsm_state           = tmp_struct.fsm_state
    self._b0_a_file           = tmp_struct.b0_a
    self._a_delay_file        = tmp_struct.a_delay
    self._b_delay_a_file      = tmp_struct.b_delay_a
    self._c_file              = tmp_struct.m2c
    self._g_gain_a_file       = tmp_struct.g_gain_a
    self._disturb_file        = tmp_struct.disturb
    self._disturb_status      = tmp_struct.disturb_status
    self._shape_file          = tmp_struct.shape
    self._ff_matrix_file      = tmp_struct.ff_matrix

	; Variables added in a later stage to adsec.sav.
	; To guarantee backward compatibility an existence check should be done.

   	;Huge mess to find the oversampling time....
   	;...variable name changed... furthermore, it may sometimes contain a frequency sometimes a time....
	find_tag, 'oversampling_*', tmp_struct, ovsamp_tag_name, ovsamp_tag_idx, /SILENT
	if n_elements(ovsamp_tag_name) ne 1 then message,'oops! too many tags found here! Correct this part of the code.'
    if ovsamp_tag_name ne '' then begin
    	time_or_freq = tmp_struct.(ovsamp_tag_idx)
    	if time_or_freq lt 1. then self._ovsamp_time = time_or_freq else $		; it's a time!
   								   self._ovsamp_time = 1./time_or_freq			; it's a freq!
	endif else self._ovsamp_time = -1.

	; electrical index of working actuators
	act_wcl_fname = filepath(root=ao_elabdir(), 'act_wcl.sav')	;valid for solar tower data!!!!
	if file_test(act_wcl_fname) then begin
		restore, act_wcl_fname
		self._act_wcl = ptr_new(act_wcl)
	endif

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOadsec_status', 'Represents the AdSec status') then return, 0
    self->addMethodHelp, "fsm_state()", "return AdSec FSM state (string)"
    self->addMethodHelp, "b0_a_file()", "return b0_a filename (string)"
    self->addMethodHelp, "b_delay_a_file()", "return b_delay_a filename (string)"
    self->addMethodHelp, "a_delay_file()", "return a_delay filename (string)"
    self->addMethodHelp, "c_file()",    "return c filename (usually m2c) (string)"
    self->addMethodHelp, "g_gain_a_file()", "return gain matrix filename (string)"
    self->addMethodHelp, "disturb_file()", "return disturb filename (string)"
    self->addMethodHelp, "disturb_status()", "return disturb enable/disable (int)"
    self->addMethodHelp, "shape_file()", "return shape filename (string)"
    self->addMethodHelp, "ff_matrix_file()", "return ff matrix filename (string)"
    return, 1
end

pro AOadsec_status::ConvertFilePath, struct
    if struct.b0_a      ne "" then struct.b0_a           = 'adsec_calib/'+strjoin((strsplit(struct.b0_a, '/', /extr))[6:*], '/')
    if struct.a_delay   ne "" then struct.a_delay        = 'adsec_calib/'+strjoin((strsplit(struct.a_delay, '/', /extr))[6:*], '/')
    if struct.b_delay_a ne "" then struct.b_delay_a      = 'adsec_calib/'+strjoin((strsplit(struct.b_delay_a, '/', /extr))[6:*], '/')
    if struct.m2c       ne "" then struct.m2c            = 'adsec_calib/'+strjoin((strsplit(struct.m2c, '/', /extr))[6:*], '/')
    if FILE_BASENAME(struct.g_gain_a) eq 'tmp_gain.fits' then struct.g_gain_a = ""
    if struct.g_gain_a  ne "" then struct.g_gain_a       = 'adsec_calib/'+strjoin((strsplit(struct.g_gain_a, '/', /extr))[6:*], '/')
    if struct.disturb   ne "" then struct.disturb        = 'adsec_calib/'+strjoin((strsplit(struct.disturb, '/', /extr))[6:*], '/')
    if struct.shape     ne "" then struct.shape          = 'adsec_calib/'+strjoin((strsplit(struct.shape, '/', /extr))[6:*], '/')
    if struct.ff_matrix ne "" then struct.ff_matrix      =  'adsec_calib/'+strjoin((strsplit(struct.ff_matrix, '/', /extr))[6:*], '/')
end

function AOadsec_status::fsm_state
    return, self._fsm_state
end

function AOadsec_status::b0_a_file
    return, self._b0_a_file
end

function AOadsec_status::a_delay_file
    return, self._a_delay_file
end

function AOadsec_status::b_delay_a_file
    return, self._b_delay_a_file
end

function AOadsec_status::c_file
    return, self._c_file
end

function AOadsec_status::g_gain_a_file
    return, self._g_gain_a_file
end

function AOadsec_status::disturb_file
    return, self._disturb_file
end

function AOadsec_status::disturb_status
    return, self._disturb_status
end

function AOadsec_status::shape_file
    return, self._shape_file
end

function AOadsec_status::ff_matrix_file
    return, self._ff_matrix_file
end

function AOadsec_status::ovsamp_time
    return, self._ovsamp_time
end

function AOadsec_status::act_wcl
	if ptr_valid(self._act_wcl) then return, *self._act_wcl else return, 0
end

pro AOadsec_status::Cleanup
    self->AOhelp::Cleanup
    ptr_free, self._act_wcl
end

pro AOadsec_status__define
    struct = { AOadsec_status, $
        _fsm_state               : "", $
        _b0_a_file               : "", $
        _a_delay_file            : "", $
        _b_delay_a_file          : "", $
        _c_file                  : "", $
        _g_gain_a_file           : "", $
        _disturb_file            : "", $
        _disturb_status          : 0L, $
        _shape_file              : "", $
        _ff_matrix_file          : "", $
        _ovsamp_time			 : 0., $
        _act_wcl				 : ptr_new(), $
        INHERITS AOhelp $
    }
end

