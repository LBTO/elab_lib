pro strehl_vs_mag, set, from=from, to=to, rec = rec, tab_res = tab_res_out2, tns = tns, noplot = noplot, $
  lambda = lambda_, onsky = onsky, calib = calib, max_dark = max_dark, min_exp = min_exp, dimm = dimm, $
  vs_seeing = vs_seeing, filename = filename, vs_flux = vs_flux, xr = xr_, aux = tab_aux, simpath = simpath, $
  ncpa = ncpa_

  if keyword_set(from) and keyword_set(to) then begin
    set = obj_new('aodataset',from=from,to=to,rec=rec)
    if keyword_set(filter) then set = set->where('luci.filter_name()','eq',filter)
    set = set->where('luci.sr_se()','between',[0,1])
    tns = set->tracknums()
  endif else begin
    if size(set,/type) eq 11 then begin
      if obj_class(set) eq 'AOELAB' then tns = set->tracknum()
      if obj_class(set) eq 'AODATASET' then tns = set->tracknums()
    endif else tns = set
  endelse

  nfiles = n_elements(tns)

  if tns[0] ne '' then begin

    tab_res = fltarr(nfiles,4)-1 ;Strehl, seeing, R magnitude/flux, binning

    count = 0

    for i = 0, nfiles-1 do begin

      cur_ee = getaoelab(tns[i],rec=rec)
      if not obj_valid(cur_ee) then continue
      if not obj_valid(cur_ee->luci()) then continue
      if not obj_valid(cur_ee->wfs_status()) then continue
      if not obj_valid((cur_ee->wfs_status())->pupils()) then continue
      if keyword_set(onsky) and cur_ee->operation_mode() ne 'ONSKY' then continue
      if keyword_set(calib) then begin
        if cur_ee->operation_mode() eq 'ONSKY' and keyword_set((ee->tel())->isTracking()) then continue
      endif
      if keyword_set((cur_ee->luci())->filter_name()) then begin
        filter = (cur_ee->luci())->filter_name()
        lambda0 = (cur_ee->luci())->lambda()*1e9
      endif else continue
      if not keyword_set(lambda_) then lambda = lambda0 else lambda = lambda_

      sr_tmp = (cur_ee->luci())->sr_se()
      if sr_tmp gt 1 or sr_tmp lt 0 then continue
      tab_res[i,0] = sr_tmp^((lambda0/lambda)^2.)
      if obj_valid(cur_ee->olmodes()) then tab_res[i,1] = (cur_ee->olmodes())->seeing()
      if (cur_ee->adsec_status())->disturb_status() ne 0 then begin
        if keyword_set(cur_ee->disturb()) then begin
          if (cur_ee->disturb())->type() eq 'atm' or (cur_ee->disturb())->type() eq 'atm+sinus' then begin
            tab_res[i,1] = (cur_ee->disturb())->seeing()
            if not keyword_set((cur_ee->tel())->isTracking()) or $
              cur_ee->operation_mode() eq 'ARGOScal' then tab_res[i,1] /= 2.
          endif
        endif
      endif
      if finite((cur_ee->tel())->dimm_seeing()) then begin
        tab_res[i,1] = (cur_ee->tel())->dimm_seeing()
        if finite((cur_ee->tel())->dimm_seeing_elevation()) then tab_res[i,1] = (cur_ee->tel())->dimm_seeing_elevation()
      endif else begin
        if keyword_set(dimm) then continue
      endelse
      if tab_res[i,1] le 0 then begin
        tab_res[i,1]=-1
        continue
      endif
      if finite(cur_ee->mag()) then begin
        if keyword_set(vs_flux) then begin
          dum = recalc_mag((cur_ee->frames())->nphsub_per_int_av(), trans = (cur_ee->wfs_status())->transmissivity(), $
            nsubap = ((cur_ee->wfs_status())->pupils())->nsub(), freq = ((cur_ee->wfs_status())->camera())->framerate(), $
            EMgain = ((cur_ee->wfs_status())->camera())->emGain(), binning = ((cur_ee->wfs_status())->camera())->binning(), $
            flux = flux)
          tab_res[i,2] = flux
        endif else tab_res[i,2] = cur_ee->mag()
      endif else continue

      cur_bin = ((cur_ee->wfs_status())->camera())->binning()
      tab_res[i,3] = cur_bin

      if n_elements(index) eq 0 then index = i else index = [index,i]

      case cur_bin of
        1: if n_elements(index1) eq 0 then index1 = count else index1 = [index1,count]
        2: if n_elements(index2) eq 0 then index2 = count else index2 = [index2,count]
        3: if n_elements(index3) eq 0 then index3 = count else index3 = [index3,count]
        4: if n_elements(index4) eq 0 then index4 = count else index4 = [index4,count]
      endcase

      count += 1

    endfor

    if n_elements(index) eq 0 then begin
      print, 'No valid TN found'
      return
    endif

    tns = tns[index]

    tab_res_out = tab_res[index,*]

    if keyword_set(vs_seeing) then begin
      xdata = tab_res_out[*,1]
      rcolor = [min(tab_res_out[*,2]),max(tab_res_out[*,2])]
      if n_elements(tab_res2) ne 0 then begin
        xdata2 = tab_res2[*,1]
        tmp = bytscl([rcolor,tab_res2[*,2]])
        color2 = tmp[2:*]
        rcolor = [min([tab_res_out[*,2],tab_res2[*,2]]),max([tab_res_out[*,2],tab_res2[*,2]])]
      endif
      vcolor = (bytscl([rcolor,tab_res_out[*,2]]))[2:*]
      xtit = 'Seeing (arcsec)'
      ctit = 'R mag.'
    endif else begin
      xdata = tab_res_out[*,2]
      rcolor = [min(tab_res_out[*,1]),max(tab_res_out[*,1])]
      if n_elements(tab_res2) ne 0 then begin
        xdata2 = tab_res2[*,2]
        tmp = bytscl([rcolor,tab_res2[*,1]])
        color2 = tmp[2:*]
        rcolor = [min([tab_res_out[*,1],tab_res2[*,1]]),max([tab_res_out[*,1],tab_res2[*,1]])]
      endif
      vcolor = (bytscl([rcolor,tab_res_out[*,1]]))[2:*]
      if keyword_set(vs_flux) then begin
        xtit = 'Flux (ph/s)'
        xlog = 1
      endif else begin
        xtit = 'R mag.'
        xlog = 0
      endelse
      ctit = 'Seeing (arcsec)'
    endelse
    if not keyword_set(xr_) then xr = [floor(min(xdata))-1,ceil(max(xdata))+1] else xr = xr_
    legpos = [.95,.95]
    if keyword_set(vs_seeing) then xr[0] = xr[0] > 0
    if keyword_set(vs_flux) then begin
      if not keyword_set(xr_) then begin
        xr = [10.^(floor(min(alog10(xdata)))),10.^(ceil(max(alog10(xdata))))]
        xr = reverse(xr)
      endif else xr = xr_
      legpos[0] = 1-legpos[0]
    endif

    if not keyword_set(noplot) then begin
      p = plot(xdata,tab_res_out[*,0],'o',/sym_filled, rgb_table = 34, vert_color = vcolor, $
        xtit = xtit, ytit = 'SR ('+strtrim(fix(lambda),2)+' nm)',axis_style = 2, sym_size = 1, margin = 0.2, font_size = 15, $
        yr = [0,1], xr = xr,/nodata, xlog = xlog)
      if n_elements(index1) ne 0 then begin
        p1 = plot(xdata[index1],tab_res_out[index1,0],'+',/sym_filled, rgb_table = 34, $
          vert_color = vcolor[index1], sym_size = 1, /over, name = 'BIN 1', sym_thick = 2)
      endif
      if n_elements(index2) ne 0 then begin
        p2 = plot(xdata[index2],tab_res_out[index2,0],'s', rgb_table = 34, $
          vert_color = vcolor[index2], sym_size = 1, /over, name = 'BIN 2', sym_thick = 2)
      endif
      if n_elements(index3) ne 0 then begin
        p3 = plot(xdata[index3],tab_res_out[index3,0],'tu',/sym_filled, rgb_table = 34, $
          vert_color = vcolor[index3], sym_size = 1, /over, name = 'BIN 3', sym_thick = 2)
      endif
      if n_elements(index4) ne 0 then begin
        p4 = plot(xdata[index4],tab_res_out[index4,0],'X',/sym_filled, rgb_table = 34, $
          vert_color = vcolor[index4], sym_size = 1, /over, name = 'BIN 4', sym_thick = 2)
      endif
      if n_elements(tab_res2) ne 0 then begin
        dum = plot(xdata2,tab_res2[*,0],'-k', name = 'Simulations', /over,transparency = 100, thick = 2)
        if n_elements(idx_uniq) eq 1 then p = plot(xdata2,tab_res2[*,0],'-',color=reform(rgb_table[color2[idx_uniq],*]), $
          /over, thick = 2)
        for i = 0, n_elements(idx_uniq)-1 do begin
          if i eq n_elements(idx_uniq)-1 then idx_last = n_elements(xdata2)-1 else idx_last = idx_uniq[i+1]-1
          p = plot(xdata2[idx_uniq[i]:idx_last], tab_res2[idx_uniq[i]:idx_last,0],'-', $
            color=reform(rgb_table[color2[idx_uniq[i]],*]),/over, thick = 2)
        endfor
        target = dum
      endif else target = []
      c = colorbar(range = rcolor, orientation = 1, $
        rgb_table = 34,title = ctit, font_size = 15,textpos = 1, tickdir = 1, position = [.85,.2,.9,.8],/norm)
      if obj_valid(p1) then target = [target,p1]
      if obj_valid(p2) then target = [target,p2]
      if obj_valid(p3) then target = [target,p3]
      if obj_valid(p4) then target = [target,p4]
      if n_elements(target) gt 1 then l = legend(target = target,position=legpos,/relative, font_size = 15)
    endif

    tab_res_out2 = {tns:tns, sr:tab_res_out[*,0],seeing:tab_res_out[*,1],bin:tab_res_out[*,3]}
    if keyword_set(vs_flux) then tab_res_out2 = create_struct(tab_res_out2,'flux',tab_res_out[*,2]) $
    else tab_res_out2 = create_struct(tab_res_out2,'Rmag',tab_res_out[*,2])

  endif else begin
    print,'No TN in this time period'
    return
  endelse

  if keyword_set(filename) and obj_valid(p) then p.save, filename+'.png'

end
