function AOintmat_sinus::Init

    im_header = self->header()
	self._cl_im_fname = aoget_fits_keyword(im_header, 'CL_IM')
	n_calib_tracknums = long(aoget_fits_keyword(im_header, 'N_TRN'))
	calib_tracknums   = strarr(n_calib_tracknums)
	for ii=0, n_calib_tracknums-1 do calib_tracknums[ii] = $
						aoget_fits_keyword(im_header, 'TRN'+string(ii,format='(i05)'))
	self._calib_tracknums = ptr_new(calib_tracknums)

	return,1
end

pro AOintmat_sinus::addHelp, obj
	obj->addMethodHelp, "cl_im_fname()", "IM used in CL during acquisition (string)"
	obj->addMethodHelp, "calib_tracknums()", "tracknums associated with sinusoidal IM acquisition"
end

function AOintmat_sinus::cl_im_fname
	return, self._cl_im_fname
end

function AOintmat_sinus::calib_tracknums
	return, *self._calib_tracknums
end



pro AOintmat_sinus__define
    struct = { AOintmat_sinus							, $
        _cl_im_fname				  	  : ''			, $
        _calib_tracknums        		  : ptr_new()	  $
    }
end