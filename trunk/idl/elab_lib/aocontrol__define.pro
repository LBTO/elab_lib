
;+
;
;-

function AOcontrol::Init, root_obj, b0_a_fname, a_delay_fname, b_delay_a_fname, c_fname, gain_fname, adsec_sav_fname
    self._root_obj = root_obj
    self._b0_a_fname = b0_a_fname
    self._a_delay_fname = a_delay_fname
    self._b_delay_a_fname = b_delay_a_fname
    self._c_fname  = c_fname
    self._gain_fname  = gain_fname
    self._adsec_sav_fname = adsec_sav_fname

	if self->b0_a_fname() ne "" then begin
    	header = headfits(ao_datadir()+path_sep()+self->b0_a_fname(), /SILENT, errmsg=errmsg)
        if errmsg ne ''  then message, ao_datadir()+path_sep()+self->b0_a_fname()+ ': '+ errmsg, /info
    	self._b0_a_fitsheader = ptr_new(header, /no_copy)
    endif
    if self->c_fname() ne "" then begin
    	header = headfits(ao_datadir()+path_sep()+self->c_fname(), /SILENT, errmsg=errmsg)
        if errmsg ne ''  then message, ao_datadir()+path_sep()+self->c_fname()+ ': '+ errmsg, /info
    	self._c_fitsheader = ptr_new(header, /no_copy)
    endif
    if self->gain_fname() ne "" then begin
    	header = headfits(ao_datadir()+path_sep()+self->gain_fname(), /SILENT, errmsg=errmsg)
        if errmsg ne ''  then message, ao_datadir()+path_sep()+self->gain_fname()+ ': '+ errmsg, /info
    	self._gain_fitsheader = ptr_new(header, /no_copy)
    endif

    self._kalman     = aoget_fits_keyword(self->b0_a_header(), 'KFMODES') eq '' ? 0B : 1B

    if self->isKalman() then begin
        self._intmat_fname = aoget_fits_keyword(self->b0_a_header(), 'INTMAT')
        self._m2c_fname    = aoget_fits_keyword(self->b0_a_header(), 'M2C')
    endif else begin
    	temp = strsplit(self._b0_a_fname,'Rec_', /extract, /regex)
		if n_elements(temp) eq 2 then $
        	self._intmat_fname = temp[0]+'Intmat_'+temp[1]
        self._m2c_fname    = self._c_fname
    endelse

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOcontrol', 'Represents AO control filter') then return, 0
    self->addMethodHelp, "b0_a_fname()", "b0_a filename (string)"
    self->addMethodHelp, "b_delay_a_fname()", "b_delay_a filename (string)"
    self->addMethodHelp, "a_delay_fname()", "a_delay filename (string)"
    self->addMethodHelp, "c_fname()", "c filename (string)"
    self->addMethodHelp, "gain_fname()", "gain filename (string)"
    self->addMethodHelp, "b0_a()", "b0_a matrix"
    self->addMethodHelp, "b_delay_a()", "b_delay_a matrix"
    self->addMethodHelp, "a_delay()", "a_delay matrix"
    self->addMethodHelp, "m2c_fname()", "m2c matrix filename (tipically = c_fname) (string)"
    self->addMethodHelp, "m2c()", "m2c matrix (modal -> zonal) "
    self->addMethodHelp, "c2m()", "c2m matrix (zonal -> modal) "
    self->addMethodHelp, "gain()", "gain vector"
    self->addMethodHelp, "maxgain()", "max of gain vector"
    self->addMethodHelp, "mingain()", "min of gain vector"
    self->addMethodHelp, "zerogain()", "zero gain means open loop (boolean)"
    self->addMethodHelp, "group_gains()",     "return the distinct values of the gain vector"
    self->addMethodHelp, "group_first_mode()", "return the indexes of first mode in each group"
    self->addMethodHelp, "ngaingroups()", "return the number of modal groups"
    self->addMethodHelp, "ttgain()",     "return the tip-tilt gain"
    self->addMethodHelp, "mogain()",     "return the medium-order gain (2nd group), or -1 if not present"
    self->addMethodHelp, "hogain()",     "return the high-order gain (3rd group), or -1 if not present"
    self->addMethodHelp, "zeroed_modes()", "return the indexes of modes forced to zero gain"
    ;self->addMethodHelp, "nmodes()", "number of non-null row in b0_a matrix"
    ;self->addMethodHelp, "modes_idx()", "index vector of non-null row in b0_a matrix"
    ;self->addMethodHelp, "rec()", "reconstructor matrix (not b0_a() in case of Kalman filter)"
    self->addMethodHelp, "b0_a_header()",     "header of b0_a fitsfile (strarr)"
    self->addMethodHelp, "c_header()",      "header of c fitsfile (strarr)"
    ;self->addMethodHelp, "m2c_header()", "header of m2c fitsfile (tipically = c_header) (strarr)"
    self->addMethodHelp, "gain_header()",     "header of gain fitsfile (strarr)"
    self->addMethodHelp, "isKalman()",     "tell if Kalman filter is used"
    self->addMethodHelp, "intmat_fname()",     "interaction matrix filename (from which b0_a has been derived)"
    self->addMethodHelp, "TTdirections()",     "return TIP TILT direction [rad] in respect to the coordinate system of the telescope"
    return, 1
end

;function AOcontrol::FitsHeader
;   if (PTR_VALID(self._fitsheader)) THEN return, *(self._fitsheader) else return, 0d
;end

function AOcontrol::b0_a_fname
    return, self._b0_a_fname
end

function AOcontrol::a_delay_fname
    return, self._a_delay_fname
end

function AOcontrol::b_delay_a_fname
    return, self._b_delay_a_fname
end

function AOcontrol::c_fname
    return, self._c_fname
end

function AOcontrol::m2c_fname
    return, self._m2c_fname
end

function AOcontrol::gain_fname
    return, self._gain_fname
end

function AOcontrol::b0_a
    _b0_a = readfits(ao_datadir()+path_sep()+self->b0_a_fname(), /SILENT)
    if not ptr_valid(self._modes_idx) then begin
        self._modes_idx = ptr_new(where(total(_b0_a^2.,1) ne 0, t_nmodes), /no_copy)
        if t_nmodes eq 0 then message, 'Matrix b0_a is null'
        self._nmodes = t_nmodes
    endif
    return, _b0_a
end

function AOcontrol::a_delay
    return, readfits(ao_datadir()+path_sep()+self->a_delay_fname(), header, /SILENT)
end

function AOcontrol::b_delay_a
    return, readfits(ao_datadir()+path_sep()+self->b_delay_a_fname(), header, /SILENT)
end

function AOcontrol::m2c
    m2cobj = getm2c(self->m2c_fname(), recompute= self._root_obj->recompute() )
    if OBJ_VALID(m2cobj) then return, m2cobj->m2c() else return, -1
    ;return, readfits(ao_datadir()+path_sep()+self->m2c_fname(), header, /SILENT)
end

function AOcontrol::c2m
    m2cobj = getm2c(self->m2c_fname(), recompute= self._root_obj->recompute() )
    act_w_cl  = ((self._root_obj)->adsec_status())->act_w_cl()
    if OBJ_VALID(m2cobj) then return, m2cobj->c2m(act_w_cl=act_w_cl) else return, -1
end

function AOcontrol::gain
    if not ptr_valid(self._gain) then begin

    	; Gain vector is saved within adsec.sav of each TN (starting from 20120424_192237 on SX, DX TBD).
		; If not found, load gain vector from traditional location if not found in adsec.sav
    	savObj = obj_new('IDL_Savefile', self._adsec_sav_fname)
    	if total(strmatch(savObj->names(), 'g_gain',/fold)) eq 1 then begin
    		savObj->restore, 'g_gain'
	  		obj_destroy, savObj
    	    g_gain = reform(g_gain)
    	endif else begin
    		g_gain = readfits(ao_datadir()+path_sep()+self->gain_fname(), header, /SILENT)
	  		obj_destroy, savObj
		endelse

	    if obj_valid(self._root_obj->modal_rec()) then begin
	    	modes_idx = (self._root_obj->modal_rec())->modes_idx()
	    	g_gain = g_gain[0:max(modes_idx)]
	    endif

	    ;if max(g_gain)-min(g_gain) eq 0. then g_gain = g_gain[0]
        self._gain = ptr_new(g_gain, /no_copy)
    endif
    return, *self._gain
end

function AOcontrol::maxgain
    return, max(self->gain())
end

function AOcontrol::mingain
    return, min(self->gain())
end

function AOcontrol::zerogain
    if total(self->gain()) eq 0. then return,1 else return, 0
end

function AOcontrol::ttgain
    return, (self->gain())[0]
end

pro AOcontrol::modalgain_groups
    g = self->gain()
    nonzero_idx = where(g ne 0, comp=zero_idx, count)
	self._zeroed_modes = ptr_new([zero_idx])

    if count eq 0 then begin
		gg = [0.]
		ml = [0.]
    endif else begin
	    gg = g[nonzero_idx]
	    idx_gg = rem_dup(gg)
	    idx_gg = idx_gg[sort(idx_gg)]	;order w.r.t. to the mode number
	   	ngains = n_elements(idx_gg)

	   	if ngains gt 1 then begin
			ggidx = where(gg-shift(gg,1) ne 0)
	       	gg = gg[ggidx]
	       	ml = nonzero_idx[ggidx]
	    endif else begin
	    	gg = [gg[0]]
	    	ml = [0.]
	    endelse
    endelse

	self._group_gain = ptr_new(gg)
	self._group_first_mode = ptr_new(ml)
end

function AOcontrol::group_gains
	if not ptr_valid(self._group_gain) then self->modalgain_groups
	return, *self._group_gain
end

function AOcontrol::group_first_mode
	if not ptr_valid(self._group_first_mode) then self->modalgain_groups
	return, *self._group_first_mode
end

function AOcontrol::zeroed_modes
	if not ptr_valid(self._zeroed_modes) then self->modalgain_groups
	return, *self._zeroed_modes
end

function AOcontrol::ngaingroups
	gg = self->group_gains()
	return, n_elements(gg)
end

function AOcontrol::mogain
    if self->ngaingroups() ge 2 then return, (self->group_gains())[1] else return, -1
end

function AOcontrol::hogain
    if self->ngaingroups() ge 3 then return, (self->group_gains())[2] else return, -1
end

; number of non-null rows in b0_a matrix
function AOcontrol::nmodes
    message, 'control->nmodes() is obsolete. Use (ee->modal_rec())->nmodes() instead'
    return, 0
    ;if (self._nmodes eq 0L) then r=self->b0_a()
    ;return, self._nmodes
end

; indexes of non-null rows in rec matrix
function AOcontrol::modes_idx
    message, 'control->modes_idx() is obsolete. Use (ee->modal_rec())->modes_idx() instead'
    return, 0
    ;if not ptr_valid(self._modes_idx) then r=self->b0_a()
    ;if (PTR_VALID(self._modes_idx)) THEN return, *(self._modes_idx) else return, 0d
end

; alias
function AOcontrol::rec
    message, 'control->rec() is obsolete. Use (ee->modal_rec())->rec() instead'
    return, 0
    ;return, self->b0_a()
end

;function AOadsec_status::gain, header=header
;    return, readfits(!ao_env.root+path_sep()+self->g_gain_a_fname(), header, /SILENT)
;    ;; eventually store it into self._g_gain_a
;end

function AOcontrol::b0_a_header
    if ptr_valid(self._b0_a_fitsheader) then return, *(self._b0_a_fitsheader) else return, ""
end

function AOcontrol::c_header
    if ptr_valid(self._c_fitsheader) then return, *(self._c_fitsheader) else return, ""
end

function AOcontrol::gain_header
    if ptr_valid(self._gain_fitsheader) then return, *(self._gain_fitsheader) else return, ""
end

function AOcontrol::isKalman
    return, self._kalman
end

function AOcontrol::intmat_fname
    return, self._intmat_fname
end

function AOcontrol::ttdirections, plot=plot, verbose=verbose
  if not keyword_set(plot) then plot = 0
  if not keyword_set(verbose) then verbose = 0

  if n_elements(self->m2c()) gt 1 then begin
  	; creates two surfaces, the second one have only the 'good' actuators
	surf1=fltarr(3,672)
	; (self->m2c())[0,*] is a tip in actuators' space
  	surf1[0,*]=(self->m2c())[0,*]
	; actuators coordinates
  	surf1[1:2,*]=(self._root_obj->adsec_status())->act_coordinates()
  	surf1a=fltarr(3,n_elements(((self._root_obj->adsec_status())->struct_adsec()).ACT_W_CL))
  	surf1a=surf1[*,((self._root_obj->adsec_status())->struct_adsec()).ACT_W_CL]
  	; a, b, c is the coefficients of the plane in x y z
	fit_plane, surf1a, a=a1, b=b1, c=c1, plane=plane1, num=100.
	; the first coefficient is the rotation angle [rad]
  	tip_ang=a1
  	if verbose then begin
  	  print, a1, b1, c1
  	  print, 'TIP angle = '+strtrim(tip_ang/!pi*180.,2)
  	  print, 'error std = '+strtrim(sqrt(variance(surf1[0,((self._root_obj->adsec_status())->struct_adsec()).ACT_W_CL]$
  	                                              -plane1[((self._root_obj->adsec_status())->struct_adsec()).ACT_W_CL])))
  	  print, 'TIP std = '+strtrim(sqrt(variance(surf1[0,((self._root_obj->adsec_status())->struct_adsec()).ACT_W_CL])))
  	endif
  	if plot then begin
  	  plane1=fltarr(672)
  	  for ii=0, 671 do plane1[ii] = b1*(surf1[1,ii]*cos(a1)+surf1[2,ii]*sin(a1))+c1
  	  loadct, 39
  	  window, /free
  	  display, surf1[0,*], /as,/sh, $
  	  adsec_save=(self._root_obj->adsec_status())->struct_adsec(), ADSEC_SHELL_SAVE=(self._root_obj->adsec_status())->struct_adsec_shell(), $
  	  SC_SAVE=(self._root_obj->adsec_status())->struct_sc(), GR_SAVE=(self._root_obj->adsec_status())->struct_gr() ,rot=0., /no_number
  	  window, /free
  	  display, plane1, /as,/sh, $
  	  adsec_save=(self._root_obj->adsec_status())->struct_adsec(), ADSEC_SHELL_SAVE=(self._root_obj->adsec_status())->struct_adsec_shell(), $
  	  SC_SAVE=(self._root_obj->adsec_status())->struct_sc(), GR_SAVE=(self._root_obj->adsec_status())->struct_gr() ,rot=0., /no_number
  	endif
	; creates two surfaces, the second one have only the 'good' actuators
  	surf2=fltarr(3,672)
	; (self->m2c())[0,*] is a tilt in actuators' space
  	surf2[0,*]=(self->m2c())[1,*]
	; actuators coordinates
  	surf2[1:2,*]=(self._root_obj->adsec_status())->act_coordinates()
  	surf2a=fltarr(3,n_elements(((self._root_obj->adsec_status())->struct_adsec()).ACT_W_CL))
  	surf2a=surf2[*,((self._root_obj->adsec_status())->struct_adsec()).ACT_W_CL]
  	; a, b, c is the coefficients of the plane in x y z
  	fit_plane, surf2a, a=a2, b=b2, c=c2, plane=plane2, num=100.
	; the first coefficient is the rotation angle [rad]
  	tilt_ang=a2
  	if verbose then begin
  	  print, a2, b2, c2
  	  print, 'TILT angle = '+strtrim(tilt_ang/!pi*180.,2)
  	  print, 'error std = '+strtrim(sqrt(variance(surf2[0,((self._root_obj->adsec_status())->struct_adsec()).ACT_W_CL]$
  	                                             -plane2[((self._root_obj->adsec_status())->struct_adsec()).ACT_W_CL])))
  	  print, 'TILT std = '+strtrim(sqrt(variance(surf2[0,((self._root_obj->adsec_status())->struct_adsec()).ACT_W_CL])))
  	endif
  	if plot eq 1 then begin
  	  plane2=fltarr(672)
  	  for ii=0, 671 do plane2[ii] = b2*(surf2[1,ii]*cos(a2)+surf2[2,ii]*sin(a2))+c2
  	  window, /free
  	  display, surf2[0,*], /as,/sh, $
  	  adsec_save=(self._root_obj->adsec_status())->struct_adsec(), ADSEC_SHELL_SAVE=(self._root_obj->adsec_status())->struct_adsec_shell(), $
  	  SC_SAVE=(self._root_obj->adsec_status())->struct_sc(), GR_SAVE=(self._root_obj->adsec_status())->struct_gr() ,rot=0., /no_number
  	  window, /free
  	  display, plane2, /as,/sh, $
  	  adsec_save=(self._root_obj->adsec_status())->struct_adsec(), ADSEC_SHELL_SAVE=(self._root_obj->adsec_status())->struct_adsec_shell(), $
  	  SC_SAVE=(self._root_obj->adsec_status())->struct_sc(), GR_SAVE=(self._root_obj->adsec_status())->struct_gr() ,rot=0., /no_number
  	endif
  	return, [tip_ang,tilt_ang]
  endif else begin
	return, -1
  endelse
end

pro AOcontrol::free
    if ptr_valid(self._modes_idx ) then ptr_free, self._modes_idx
    if ptr_valid(self._gain) then ptr_free, self._gain
    if ptr_valid(self._zeroed_modes) then ptr_free, self._zeroed_modes
    if ptr_valid(self._group_gain) then ptr_free, self._group_gain
    if ptr_valid(self._group_first_mode) then ptr_free, self._group_first_mode
end

pro AOcontrol::Cleanup
    if ptr_valid(self._b0_a_fitsheader) then ptr_free, self._b0_a_fitsheader
    if ptr_valid(self._c_fitsheader) 	then ptr_free, self._c_fitsheader
    if ptr_valid(self._gain_fitsheader) then ptr_free, self._gain_fitsheader
    if ptr_valid(self._modes_idx) 		then ptr_free, self._modes_idx
    if ptr_valid(self._gain) 			then ptr_free, self._gain
    if ptr_valid(self._zeroed_modes) 	then ptr_free, self._zeroed_modes
    if ptr_valid(self._group_gain) 		then ptr_free, self._group_gain
    if ptr_valid(self._group_first_mode) then ptr_free, self._group_first_mode
    self->AOhelp::Cleanup
end

pro AOcontrol__define
    struct = { AOcontrol, $
        _root_obj                 : obj_new(), $
        _adsec_sav_fname		  : "", $
        _b0_a_fname               : "", $
        _a_delay_fname            : "", $
        _b_delay_a_fname          : "", $
        _c_fname                  : "", $
        _m2c_fname                : "", $
        _gain_fname               : "", $
        _b0_a_fitsheader         : ptr_new(), $
        _c_fitsheader            : ptr_new(), $
        _gain_fitsheader         : ptr_new(), $
        _nmodes                  : 0L		, $
        _modes_idx               : ptr_new(), $
        _gain                    : ptr_new(), $
        _zeroed_modes			 : ptr_new(), $
        _group_gain				 : ptr_new(), $
        _group_first_mode		 : ptr_new(), $
        _kalman                  : 0B, $
        _intmat_fname            : "", $
        INHERITS AOhelp $
    }
end

