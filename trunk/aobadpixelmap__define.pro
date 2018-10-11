
;+
;
;-


function AObadpixelmap::Init, fname
    self._fname  = fname
    if not file_test(self->fname()) then begin 
        message, 'bad pixel file '+self->fname()+' not existing. Assume all pixels are good.', /info
        message, 'bad pixel file can be created with make_bad_pixel_map procedure', /info
    endif

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AObadpixelmap', 'Represent the badpixels map of a camera') then return, 0
    self->addMethodHelp, "fname()",   "Name of the file containing informations created by make_bad_pixel_map (string)"
    self->addMethodHelp, "files_list()", "Dark files used for of creation"
    self->addMethodHelp, "badpixelmap()", "badpixelmap (float) [w,h]"
    self->addMethodHelp, "frame_w()", "badpixelmap width (long)"
    self->addMethodHelp, "frame_h()", "badpixelmap heigth (long)"
    self->addMethodHelp, "triangulation()", "Structure to be used for trigrid and badpixel removal "
    return, 1
end

function AObadpixelmap::fname
    return, self._fname
end

function AObadpixelmap::triangulation
	if not PTR_VALID(self._triangulate) then self->restoreData
	return, *(self._triangulate)
end

function AObadpixelmap::badpixelmap
	if not PTR_VALID(self._badpixelmap) then self->restoreData
	return, *(self._badpixelmap)
end

function AObadpixelmap::frame_w
	if self._frame_w eq 0 then self->restoreData
	return, self._frame_w
end

function AObadpixelmap::frame_h
	if self._frame_h eq 0 then self->restoreData
	return, self._frame_h
end

function AObadpixelmap::files_list
	if not PTR_VALID(self._files_list) then self->restoreData
	return, *(self._files_list)
end

pro AObadpixelmap::restoreData
    if file_test(self->fname()) then begin
        restore, self->fname()
        self._frame_w = n_elements(badpixels[*,0])
        self._frame_h = n_elements(badpixels[0,*])
        self._badpixelmap = ptr_new(badpixels,  /no_copy)
        self._triangulate = ptr_new(bpstr, /no_copy)
        self._files_list = ptr_new(files, /no_copy)
    endif else begin
        self._frame_w = 2
        self._frame_h = 2
        self._badpixelmap = ptr_new(fltarr(self->frame_w(), self->frame_h()), /no_copy)
        self._triangulate = ptr_new(create_struct('np', 0L), /no_copy)
        self._files_list = ptr_new('', /no_copy)
    endelse
end

pro AObadpixelmap::free
    if ptr_valid(self._badpixelmap) then ptr_free, self._badpixelmap
    if ptr_valid(self._triangulate) then ptr_free, self._triangulate
    if ptr_valid(self._files_list) then ptr_free, self._files_list
end

pro AObadpixelmap::Cleanup
    if ptr_valid(self._badpixelmap) then ptr_free, self._badpixelmap
    if ptr_valid(self._triangulate) then ptr_free, self._triangulate
    if ptr_valid(self._files_list) then ptr_free, self._files_list
    self->AOhelp::Cleanup
end

pro AObadpixelmap__define
    struct = { AObadpixelmap, $
        _fname                          : ""			, $
        _badpixelmap                    :  ptr_new()	, $
        _triangulate	                :  ptr_new()	, $
        _files_list	                    :  ptr_new()	, $
        _frame_w	                    :  0L	, $
        _frame_h	                    :  0L	, $
        INHERITS AOhelp $
    }
end

