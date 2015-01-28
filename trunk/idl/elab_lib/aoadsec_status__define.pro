

;+
;
;-



function AOadsec_status::Init, root_obj, adsec_fname   ; adsec_status_struct
    
    if file_test(adsec_fname) eq 0 then begin
    	message, 'Cannot find adsec_status file: '+adsec_fname+'. adsec_status object not available!' , /info
    	return,0
    endif

    savObj = obj_new('IDL_Savefile', adsec_fname)
    savObj->restore, 'status'
    found_ncpa=0
    names = savobj->names()
    for i=0,n_elements(names)-1 do if names[i] eq 'NCPA' then found_ncpa=1
    if found_ncpa then begin
       savObj->restore, 'ncpa'
       self._ncpa = ncpa
    endif else begin
       self._ncpa = fltarr(672)
    endelse

    obj_destroy, savObj

    ;restore, adsec_fname ; contains status, [accel, data, mygr, myadsec, myadsec_shell, mysc]

    ; convert filepaths /usr/local/adopt/calib/left/adsec/*  in adsec_calib/*
    tmp_struct = status
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

 


  ;Search for struct containing information on adsec (used mainly for display of positions)
;  	files = filepath("adsec_struct_*.sav", root_dir=ao_elabdir())
;   	all_files = file_search(files, count=nfiles)
;	if nfiles gt 0 then begin
;		thisJulday = (root_obj->obj_tracknum())->julday()
;		all_files_julday = dblarr(nfiles)
;		for ii=0, nfiles-1 do begin
;			file_date = strmid(file_basename(all_files[ii]), 13, 8)
;			y = fix(strmid(file_date, 0, 4))
;			m = fix(strmid(file_date, 4, 2))
;			d = fix(strmid(file_date, 6, 2))
;			all_files_julday[ii] = julday(m,d,y,0,0,0)
;		endfor
;		diffJulday = thisJulday - all_files_julday
;		idx = where(diffJulday  ge 0d, count)
;                if count gt 0 then begin
;		   idx1 = idx[where( diffJulday[idx] eq min(diffJulday[idx]))]
;		   self._adsec_struct_file = all_files[idx1]
;                endif else self._adsec_struct_file = ''
;	endif
    ; con il filename corrispondente (e.g. 'adsec_structs_20100101.sav') lo passiamo al multiton che ti rende la referenza a quell'oggetto
;   self._adsec_structs = getadsecstructs(self._adsec_struct_file)
;   NOTE if getadsecstructs is re-used, please remove it from the cleanup and free methods below
   
    ; at 20120408 adsec.sav contains also mygr, myadsec, myadsec_shell, mysc
    self._adsec_struct_file = adsec_fname
    self._adsec_structs = obj_new('AOadsec_struct', self._adsec_struct_file)


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
    self->addMethodHelp, "ovsamp_time()", "return oversampling period (s)"
    self->addMethodHelp, "adsec_struct_file()", "return adsec_struct filename (string)"
    self->addMethodHelp, "struct_adsec()", "(struct)"
    self->addMethodHelp, "struct_adsec_shell()", "(struct)"
    self->addMethodHelp, "struct_gr()", "(struct)"
    self->addMethodHelp, "struct_sc()", "(struct)"
    self->addMethodHelp, "act_coordinates()", "return adsec coordinates (vect[2,672])"
    self->addMethodHelp, "act_w_cl()", "index vector of active actuators"
    self->addMethodHelp, "act_wo_cl()", "index vector of inactive actuators"    
    self->addMethodHelp, "ncpa()", "return NCPA vector fltarr(672)"
    return, 1
end

pro AOadsec_status::summary
    print, string(format='(%"fsm_state = %s")', self->fsm_state() )
    print, string(format='(%"b0_a_file = %s")', self->b0_a_file() )
    print, string(format='(%"b_delay_a_file = %s")', self->b_delay_a_file() )
    print, string(format='(%"a_delay_file = %s")', self->a_delay_file() )
    print, string(format='(%"c_file = %s")', self->c_file() )
    print, string(format='(%"g_gain_a_file = %s")', self->g_gain_a_file() )
    print, string(format='(%"disturb_file = %s")', self->disturb_file() )
    print, string(format='(%"disturb_status = %d")', self->disturb_status() )
    print, string(format='(%"shape_file = %s")', self->shape_file() )
    print, string(format='(%"ff_matrix_file = %s")', self->ff_matrix_file() )
    print, string(format='(%"ovsamp_time = %f")', self->ovsamp_time() )
    print, string(format='(%"adsec_struct_file = %s")', self->adsec_struct_file() )
end

pro AOadsec_status::test
    d = self->fsm_state()
    d = self->b0_a_file()
    d = self->b_delay_a_file()
    d = self->a_delay_file()
    d = self->c_file()
    d = self->g_gain_a_file()
    d = self->disturb_file()
    d = self->disturb_status()
    d = self->shape_file()
    d = self->ff_matrix_file()
    d = self->adsec_struct_file()
    d = self->struct_adsec()
    d = self->struct_adsec_shell()
    d = self->struct_gr()
    d = self->struct_sc()
    d = self->act_coordinates()
    d = self->ncpa()
end

pro AOadsec_status::ConvertFilePath, struct
    ; NB this conversion is critical when the folder scheme is messed up
    ; search for /bla/bla/bla/adsec/foo1/foo2/... and convert into adsec_calib/foo1/foo2/...
    if struct.b0_a  ne "" then begin
        extr = strsplit(struct.b0_a, '/', /extr)
        idx = (where(stregex(extr, 'adsec*', /BOOLEAN),count))[0]
        if count gt 0 then struct.b0_a  = 'adsec_calib/'+strjoin(extr[idx+1:*], '/')
    endif
    if struct.a_delay  ne "" then begin
        extr = strsplit(struct.a_delay, '/', /extr)
        idx = (where(stregex(extr, 'adsec*', /BOOLEAN),count))[0]
        if count gt 0 then struct.a_delay  = 'adsec_calib/'+strjoin(extr[idx+1:*], '/')
    endif
    if struct.b_delay_a  ne "" then begin
        extr = strsplit(struct.b_delay_a, '/', /extr)
        idx = (where(stregex(extr, 'adsec*', /BOOLEAN),count))[0]
        if count gt 0 then struct.b_delay_a  = 'adsec_calib/'+strjoin(extr[idx+1:*], '/')
    endif
    if struct.m2c  ne "" then begin
        extr = strsplit(struct.m2c, '/', /extr)
        idx = (where(stregex(extr, 'adsec*', /BOOLEAN),count))[0]
        if count gt 0 then struct.m2c  = 'adsec_calib/'+strjoin(extr[idx+1:*], '/')
    endif
    if FILE_BASENAME(struct.g_gain_a) eq 'tmp_gain.fits' then struct.g_gain_a = ""
    if struct.g_gain_a  ne "" then begin
        extr = strsplit(struct.g_gain_a, '/', /extr)
        idx = (where(stregex(extr, 'adsec*', /BOOLEAN),count))[0]
        if count gt 0 then struct.g_gain_a  = 'adsec_calib/'+strjoin(extr[idx+1:*], '/')
    endif
    if struct.disturb  ne "" then begin
        extr = strsplit(struct.disturb, '/', /extr)
        idx = (where(stregex(extr, 'adsec*', /BOOLEAN),count))[0]
        if count gt 0 then struct.disturb  = 'adsec_calib/'+strjoin(extr[idx+1:*], '/')
    endif

    ;if struct.a_delay   ne "" then struct.a_delay        = 'adsec_calib/'+strjoin((strsplit(struct.a_delay, '/', /extr))[6:*], '/')
    ;if struct.b_delay_a ne "" then struct.b_delay_a      = 'adsec_calib/'+strjoin((strsplit(struct.b_delay_a, '/', /extr))[6:*], '/')
    ;if struct.m2c       ne "" then struct.m2c            = 'adsec_calib/'+strjoin((strsplit(struct.m2c, '/', /extr))[6:*], '/')
    ;if struct.g_gain_a  ne "" then struct.g_gain_a       = 'adsec_calib/'+strjoin((strsplit(struct.g_gain_a, '/', /extr))[6:*], '/')
    ;if struct.disturb   ne "" then struct.disturb        = 'adsec_calib/'+strjoin((strsplit(struct.disturb, '/', /extr))[6:*], '/')
    ;;if struct.shape     ne "" then struct.shape          = 'adsec_calib/'+strjoin((strsplit(struct.shape, '/', /extr))[6:*], '/')
    ;;if struct.ff_matrix ne "" then struct.ff_matrix      =  'adsec_calib/'+strjoin((strsplit(struct.ff_matrix, '/', /extr))[6:*], '/')
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

function AOadsec_status::adsec_struct_file
	return, self._adsec_struct_file
end

function AOadsec_status::struct_adsec
  return, self._adsec_structs->adsec()
end

function AOadsec_status::struct_adsec_shell
  return, self._adsec_structs->adsec_shell()
end

function AOadsec_status::struct_gr
  return, self._adsec_structs->gr()
end

function AOadsec_status::struct_sc
  return, self._adsec_structs->sc()
end

function AOadsec_status::act_coordinates
  return, self._adsec_structs->act_coordinates()
end

function AOadsec_status::act_wo_cl
  return, self._adsec_structs->act_wo_cl()
end

function AOadsec_status::act_w_cl
  return, self._adsec_structs->act_w_cl()
end

function AOadsec_status::ncpa
  return, self._ncpa
end

pro AOadsec_status::free
    if obj_valid(self._adsec_structs) then self._adsec_structs->free
end

pro AOadsec_status::Cleanup
    obj_destroy, self._adsec_structs
    self->AOhelp::Cleanup
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
        _ncpa                    : fltarr(672), $
        _adsec_struct_file		 : "", $
        _adsec_structs           :obj_new(), $
        INHERITS AOhelp $
    }
end

