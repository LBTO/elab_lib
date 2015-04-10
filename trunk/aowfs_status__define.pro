
;+
;
;-

function AOwfs_status::Init, root_obj, fitsfile
    if not file_test(fitsfile) then begin
        message, fitsfile + ' not found', /info
        return,0
    endif
    self._fitsfile   = fitsfile
    self._header = ptr_new(headfits(fitsfile, /SILENT), /no_copy)

	; ---------------- W_UNIT determination --------------------------
	; Before the W_UNIT keyword was introduced, we know that:
	;  W_UNIT = 'W1' for all tests done before 01 September 2009
	;  W_UNIT = 'W2' for all tests done after  01 September 2009.
	wunit = aoget_fits_keyword(self->header(), 'W_UNIT')
    if wunit eq '' then begin
    	switch_date = julday(09, 01, 2009, 00, 00, 00)
		if obj_isa(root_obj, 'AOelab')   then thisdate = (root_obj->obj_tracknum())->JulDay()
		if obj_isa(root_obj, 'AOintmat') then begin
			thisdate = strsplit(aoget_fits_keyword(self->header(), 'DATE'), '-', /extract)
			thisdate = julday(thisdate[1], thisdate[2], thisdate[0], 00, 00, 00)
		endif
		if thisdate LT switch_date then wunit='W1' else wunit='W2'
	endif
	self._wunit = strtrim(wunit,2)

	self._modulation = float(aoget_fits_keyword(self->header(), 'tt.LAMBDA_D'))
	;TODO add here the case for magellan, for which tt.lambda_d is wrong

    self._stages[0] = float(aoget_fits_keyword(self->header(), 'stagex.POSITION'))
    self._stages[1] = float(aoget_fits_keyword(self->header(), 'stagey.POSITION'))
    self._stages[2] = float(aoget_fits_keyword(self->header(), 'stagez.POSITION'))

    self._rerotator = float(aoget_fits_keyword(self->header(), 'rerot.POSITION'))

    self._camera_lens[0] = float(aoget_fits_keyword(self->header(), 'lens.POSITION_X'))
    self._camera_lens[1] = float(aoget_fits_keyword(self->header(), 'lens.POSITION_Y'))

    self._lamp = float(aoget_fits_keyword(self->header(), 'lamp.INTENSITY'))
    self._cuberot   = float(aoget_fits_keyword(self->header(), 'cuberot.POSITION'))
    self._cubestage = float(aoget_fits_keyword(self->header(), 'cubestage.POSITION'))

    self._optg = float(aoget_fits_keyword(self->header(), 'sc.OPTG'))
    self._ncpa_trigger = byte(aoget_fits_keyword(self->header(), 'sc.NCPA_TRIGGER'))

    self._ccd39  = obj_new('AOccd39',  self._header, self._wunit)
    self._pupils = obj_new('AOpupils', self._header, self._wunit)
    self._filtw1 = obj_new('AOfiltw' , self._header, self._wunit, '1')
    self._filtw2 = obj_new('AOfiltw' , self._header, self._wunit, '2')

    self._slopes_null_fname = string(aoget_fits_keyword(self->header(), 'sc.SLOPENULL'))


    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOwfs_status', 'Represent WFS status') then return, 0
    self->addMethodHelp, "fitsfile()", "wfs status fitsfile name (string)"
    self->addMethodHelp, "header()", "header of wfs status fitsfile (strarr)"
    self->addMethodHelp, "wunit()",  "W unit number (1,2,...)"
    self->addMethodHelp, "ccd39()", "reference to ccd39 object"
    self->addMethodHelp, "pupils()", "reference to pupils object"
    self->addMethodHelp, "filtw1()", "reference to filter wheel 1 object"
    self->addMethodHelp, "filtw2()", "reference to filter wheel 2 object"
    self->addMethodHelp, "modulation()",  "TT modulation (lambda/D)"
    self->addMethodHelp, "rerotator()",  "rirotator angle (degrees)"
    self->addMethodHelp, "camera_lens()", "Position [x,y] of camera lens (mm)"
    self->addMethodHelp, "stages()", "Position [x,y,z] of stages (mm)"
    self->addMethodHelp, "lamp_intensity()",  "lamp intensity (a.u.)"
    self->addMethodHelp, "cube_angle()",  "cube rotator angle (degree)"
    self->addMethodHelp, "cube_stage()",  "cube stage position (mm)"
    self->addMethodHelp, "slopes_null_fname()",  "slopesnull vector fitsfile name (string)"
    self->addMethodHelp, "summary", "Summary of WFS status"
    if obj_valid(self._ccd39) then self->addleaf, self._ccd39, 'ccd39'
    if obj_valid(self._pupils) then self->addleaf, self._pupils, 'pupils'
    if obj_valid(self._filtw1) then self->addleaf, self._filtw1, 'filtw1'
    if obj_valid(self._filtw2) then self->addleaf, self._filtw2, 'filtw2'
    return, 1
end

function AOwfs_status::header
    if (PTR_VALID(self._header)) THEN return, *(self._header) else return, ""
end

function AOwfs_status::fitsfile
    return, self._fitsfile
end

function AOwfs_status::ccd39
	return, self._ccd39
end

function AOwfs_status::pupils
	return, self._pupils
end

function AOwfs_status::filtw1
	return, self._filtw1
end

function AOwfs_status::filtw2
	return, self._filtw2
end

function AOwfs_status::wunit
	return, self._wunit
end

function AOwfs_status::modulation
	return, self._modulation
end

function AOwfs_status::rerotator
	return, self._rerotator
end

function AOwfs_status::camera_lens
	return, self._camera_lens
end

function AOwfs_status::stages
	return, self._stages
end

function AOwfs_status::stagex
	return, self._stages[0]
end

function AOwfs_status::stagey
	return, self._stages[1]
end

function AOwfs_status::stagez
	return, self._stages[2]
end

function AOwfs_status::lamp_intensity
	return, self._lamp
end

function AOwfs_status::cube_angle
	return, self._cuberot
end

function AOwfs_status::cube_stage
	return, self._cubestage
end

function AOwfs_status::slopes_null_fname
	return, self._slopes_null_fname
end

function AOwfs_status::optg
  return, self._optg
end

pro AOwfs_status::summary, COMPREHENSIVE=COMPREHENSIVE
    print, string(format='(%"%-30s %s")','Unit number', self->wunit() )
    print, string(format='(%"%-30s %f")','Modulation', self->modulation() )
    if obj_valid(self->ccd39())  then  (self->ccd39())->summary, COMPREHENSIVE=COMPREHENSIVE
    if obj_valid(self->pupils()) then (self->pupils())->summary, COMPREHENSIVE=COMPREHENSIVE
    if obj_valid(self->filtw1()) then (self->filtw1())->summary, COMPREHENSIVE=COMPREHENSIVE
    if obj_valid(self->filtw2()) then (self->filtw2())->summary, COMPREHENSIVE=COMPREHENSIVE

    if keyword_set(COMPREHENSIVE) then begin
	    print, string(format='(%"%-30s %f")','Rerotator', self->rerotator() )
    	print, string(format='(%"%-30s %f  %f")','Camera lens', self->camera_lens() )
    	print, string(format='(%"%-30s %f  %f  %f")','Stages XYZ', self->stages() )
    	print, string(format='(%"%-30s %f")','Lamp', self->lamp_intensity() )
    	print, string(format='(%"%-30s %f")','Cube angle', self->cube_angle() )
    	print, string(format='(%"%-30s %f")','Cube stage', self->cube_stage() )
   		print, string(format='(%"%-30s %s")','SlopesNull fname', self->slopes_null_fname() )
   	endif
end

pro AOwfs_status::test
    d = self->fitsfile()
    d = self->header()
    d = self->wunit()
    d = self->ccd39()
    d = self->pupils()
    d = self->filtw1()
    d = self->filtw2()
    d = self->modulation()
    d = self->rerotator()
    d = self->camera_lens()
    d = self->stages()
    d = self->lamp_intensity()
    d = self->cube_angle()
    d = self->cube_stage()
    d = self->slopes_null_fname()
    (self->ccd39())->test
    (self->pupils())->test
    (self->filtw1())->test
    (self->filtw2())->test
end

function AOwfs_status::isok, cause=cause
    imok = 1B
    if obj_valid(self->pupils()) then imok *= (self->pupils())->isok(cause=cause)
    return, imok
end

pro AOwfs_status::free
    ;if ptr_valid(self._header) then ptr_free, self._header
    IF (OBJ_VALID(self._ccd39 )) THEN  self._ccd39->free
    IF (OBJ_VALID(self._pupils )) THEN  self._pupils->free
    IF (OBJ_VALID(self._filtw1 )) THEN  self._filtw1->free
    IF (OBJ_VALID(self._filtw2 )) THEN  self._filtw2->free
end

pro AOwfs_status::Cleanup
    ptr_free, self._header
    obj_destroy, self._ccd39
    obj_destroy, self._pupils
    obj_destroy, self._filtw1
    obj_destroy, self._filtw2
    self->AOhelp::Cleanup
end

pro AOwfs_status__define
    struct = { AOwfs_status, $
        _fitsfile       : "",    $
        _modulation     : 0d,    $
        _rerotator      : 0d,    $
        _lamp           : 0d,    $
        _cuberot        : 0d,    $
        _cubestage      : 0d,    $
        _stages         : [0.0, 0.0, 0.0],    $
        _camera_lens    : [0.0, 0.0],    $
        _header         : ptr_new(), $
        _ccd39          : obj_new(), $
        _pupils         : obj_new(), $
        _filtw1         : obj_new(), $
        _filtw2         : obj_new(), $
        _optg           : 0.,        $
        _ncpa_trigger   : 0b,        $
        _wunit          : ""	   , $
        _slopes_null_fname : ""    , $
        INHERITS    AOhelp  $
    }

end

