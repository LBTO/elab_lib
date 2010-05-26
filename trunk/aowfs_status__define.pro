
;+
;
;-

function AOwfs_status::Init, root_obj, fitsfile
    if not file_test(fitsfile) then return,0
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

    self._stages[0] = float(aoget_fits_keyword(self->header(), 'stagex.POSITION'))
    self._stages[1] = float(aoget_fits_keyword(self->header(), 'stagey.POSITION'))
    self._stages[2] = float(aoget_fits_keyword(self->header(), 'stagez.POSITION'))

    self._ccd39  = obj_new('AOccd39',  self._header, self._wunit)
    self._pupils = obj_new('AOpupils', self._header, self._wunit)
    self._filtw1 = obj_new('AOfiltw' , self._header, '1')
    self._filtw2 = obj_new('AOfiltw' , self._header, '2')


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
    self->addMethodHelp, "stages()", "Position [x,y,z] of stages"
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

function AOwfs_status::stages
	return, self._stages
end

pro AOwfs_status::summary
    print, string(format='(%"%-30s %s")','Unit number', self->wunit() )
    print, string(format='(%"%-30s %f")','Frequency [Hz]', (self->ccd39())->framerate())
    print, string(format='(%"%-30s %d")','Binning', (self->ccd39())->binning())
    print, string(format='(%"%-30s %s")','Pup trackn', (self->pupils())->pup_tracknum() )
    print, string(format='(%"%-30s %d")','Total num. of supabs', (self->pupils())->nsub())
    print, string(format='(%"%-30s %f")','Modulation', self->modulation() )
    print, string(format='(%"%-30s %s")','FW1', (self->filtw1())->name() )
    print, string(format='(%"%-30s %s")','FW2', (self->filtw2())->name() )
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
        _fitsfile   : "",    $
        _modulation : 0d,    $
        _stages : [0.0, 0.0, 0.0],    $
        _header : ptr_new(), $
        _ccd39  : obj_new(), $
        _pupils : obj_new(), $
        _filtw1 : obj_new(), $
        _filtw2 : obj_new(), $
        _wunit  : ""	   , $
        INHERITS    AOhelp  $
    }

end

