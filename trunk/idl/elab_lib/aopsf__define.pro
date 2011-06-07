
;+
; AOpsf object initialization
;
; 
;
;-

function AOpsf::Init, psf_fname, dark_fname, pixelscale, lambda, framerate, $
     	roi=roi, badpixelmap_obj=badpixelmap_obj, label=label, store_radix=store_radix, recompute=recompute

    if psf_fname eq '' then return,0

	; initialize PSF object
    if not self->AOpsfAbstract::Init(psf_fname, dark_fname, pixelscale, lambda, framerate, $
    	roi=roi, badpixelmap_obj=badpixelmap_obj, label=label, store_radix=store_radix, recompute=recompute) then return,0

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOpsf', 'Generic PSF image') then return, 0
    self->AOpsf::addHelp, self

    return, 1
end

pro AOpsf__define
    struct = { AOpsf					, $
        INHERITS    AOpsfAbstract,  $
        INHERITS    AOhelp  $
    }
end
