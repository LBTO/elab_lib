pro modalplot_ced, data, color = color, overplot = overplot, wfresiduals = wfresiduals, thick = thick, yr = yr, $
noargos = noargos, std = std, title = title_, lbt = lbt, dm_fact = dm_fact_, wfs_fact = wfs_fact_, smooth = smooth_, $
clear = clear
  
  if not keyword_set(dm_fact_) then dm_fact = 1 else dm_fact = dm_fact_
  if not keyword_set(wfs_fact_) then wfs_fact = 1 else wfs_fact = wfs_fact_
  
  device,decompose = 0
  tek_color
  
  background = abs(!p.background)
  
  if size(data,/type) eq 11 then begin
    if obj_class(data) eq 'AOELAB' then tns = data->tracknum()
    if obj_class(data) eq 'AODATASET' then tns = data->tracknums()
  endif else tns = data
  

  ndata = n_elements(tns)
  if not keyword_set(title_) then begin
    if ndata eq 1 then title = tns[0] else title = tns[0]+' to '+tns[ndata-1]
  endif else title = title_
  
  if n_elements(color) eq 0 then begin
    if ndata gt 1 then color = indgen(ndata)+2 else color = [fix(1-background),2,3]
  endif
  if n_elements(thick) eq 0 then thick = 1

  for i = 0, ndata-1 do begin
    cur_data = getaoelab(tns[i])
    if not obj_valid(cur_data) then continue
    
    if i eq 0 and ndata eq 1 then begin
      clcolor = color[0]
      olcolor = color[1]
      wfrcolor = color[2]
    endif else begin
      clcolor = color[i]
      olcolor = color[i]
      wfrcolor = color[i]
    endelse
    
    if cur_data->operation_mode() eq "RR" or cur_data->operation_mode() eq "ARGOScal" then begin
      nmodes = (cur_data->modalpositions())->nmodes()
      clvar  = (cur_data->modalpositions())->time_variance() * ((cur_data->modalpositions())->norm_factor()*dm_fact)^2.
      yrange = sqrt(minmax(clvar))
      xrange = [1,10.^ceil(alog10(nmodes))]
      if obj_valid(cur_data->disturb()) then begin
        if (cur_data->adsec_status())->disturb_status() ne 0 then begin
          olvar  = (cur_data->modaldisturb())->time_variance() * ((cur_data->modaldisturb())->norm_factor())^2.
          yrange = sqrt(minmax([clvar,olvar]))
        endif
      endif
      if keyword_set(WFRESIDUALS) then begin
        norm_fact_wfs = (cur_data->residual_modes())->norm_factor()
        if (cur_data->wfs_status())->optg() lt 1 then norm_fact_wfs /= 2
        wfres = (cur_data->residual_modes())->modes() * norm_fact_wfs*wfs_fact
        if not keyword_set(std) then wfres = rms(wfres,dim=1) else begin
          tmp = fltarr((size(wfres,/dim))[1])
          for j = 0, (size(wfres,/dim))[1]-1 do tmp[j] = stddev(wfres[*,j])
          wfres = tmp
        endelse
        yrange = minmax([yrange, minmax(wfres)])
      endif
      yrange = 10.^[floor(alog10(yrange[0])),ceil(alog10(yrange[1]))]
      if not keyword_set(OVERPLOT) and i eq 0 then begin
        if keyword_set(yr) then yrange = yr
        plot_oo, lindgen(nmodes)+1, lindgen(nmodes)+1, psym=-1, symsize=1.2, charsize=1.5, ytitle='nm rms wf', xtitle='mode number', $
          title=title, yrange=yrange, xrange=xrange, thick=thick, _extra=ex,/nodata
      endif
      if keyword_set(smooth_) then clvar_plot = [sqrt(clvar[0:1]),smooth(sqrt(clvar[2:*]),smooth_)] else clvar_plot = sqrt(clvar)
      oplot, lindgen(nmodes)+1, clvar_plot, psym=-1, symsize=1.2, COLOR=clcolor, thick=thick

      if obj_valid(cur_data->disturb()) then begin
        if (cur_data->adsec_status())->disturb_status() ne 0 then begin
          if keyword_set(smooth_) then olvar_plot = [sqrt(olvar[0:1]),smooth(sqrt(olvar[2:*]),smooth_)] else olvar_plot = sqrt(olvar)
          oplot, lindgen(nmodes)+1, olvar_plot, psym=-2, symsize=1.2, COLOR=olcolor, thick=thick
        endif
      endif
      if keyword_set(WFRESIDUALS) then begin
        if keyword_set(smooth_) then wfres_plot = [wfres[0:1],smooth(wfres[2:*],smooth_)] else wfres_plot = wfres
        print, tns[i]+' WFS residual min, max, TT, tot (no TT): ', strtrim(min(wfres),2)+', '+ strtrim(max(wfres),2)+', '+ $
          strtrim(sqrt(total(wfres[0:1]^2.)),2)+ ', '+strtrim(sqrt(total(wfres[2:*]^2.)),2)
        oplot, lindgen(nmodes)+1, wfres_plot, psym=-3, symsize=0.8, color=wfrcolor,line=2, thick = thick+2
      endif

      if ndata eq 1 then begin
        if keyword_set(wfresiduals) then elab_legend, ['disturbance','closed-loop', 'wfres'], color=[olcolor,clcolor,wfrcolor], $
          psym=-[2,1,3], linestyle = [0,0,2], /top, /right, thick=thick+[0,0,2], clear = clear, back = background $
        else elab_legend, ['disturbance','closed-loop'], color=[olcolor,clcolor], psym=-[2,1], /top, /right, thick=thick, clear = clear, $
          back = background
      endif

      ;; End of 'RR', begin 'ONSKY'
    endif else begin
      ;       nmodes = (cur_data->residual_modes())->nmodes()
      test_argos = 0
      if obj_valid(cur_data->disturb()) then if (cur_data->adsec_status())->disturb_status() ne 0 then $
        if obj_valid(cur_data->disturb()) then $
        if (cur_data->disturb())->type() eq 'atm' and not keyword_set(noargos) then test_argos = 1
      if obj_valid(cur_data->tel()) then $
        if (cur_data->tel())->istracking() ne 1 and not keyword_set(noargos) then test_argos = 1
      if not obj_valid(cur_data->modalpositions()) then test_argos = 0
      if not keyword_set(test_argos) and not obj_valid(cur_data->residual_modes()) then continue
      norm_fact_wfs = (cur_data->residual_modes())->norm_factor()
      if (cur_data->wfs_status())->optg() lt 1 then norm_fact_wfs /= 2

      if keyword_set(test_argos) then clvar  = (cur_data->modalpositions())->time_variance() $
        * ((cur_data->modalpositions())->norm_factor()*dm_fact)^2. $
      else begin
        ;clvar  = (cur_data->residual_modes())->time_variance() * (norm_fact_wfs*wfs_fact)^2.
        wfres = (cur_data->residual_modes())->modes() * norm_fact_wfs*wfs_fact
        if not keyword_set(std) then clvar = rms(wfres,dim=1)^2. else begin
          tmp = fltarr((size(wfres,/dim))[1])
          for j = 0, (size(wfres,/dim))[1]-1 do tmp[j] = variance(wfres[*,j])
          clvar = tmp
        endelse
      endelse
      
      if keyword_set(test_argos) then nmodes = (cur_data->modalpositions())->nmodes() else $
        nmodes = (cur_data->residual_modes())->nmodes()
      
      if not keyword_set(test_argos) or not obj_valid(cur_data->disturb()) then $
        olvar  = (cur_data->olmodes())->time_variance() * ((cur_data->olmodes())->norm_factor())^2. $
      else olvar  = (cur_data->modaldisturb())->time_variance() * ((cur_data->modaldisturb())->norm_factor())^2.
;      modes_idx = (cur_data->modal_rec())->modes_idx()
;      clvar  = clvar[modes_idx]
;      olvar  = olvar[modes_idx]
      yrange = sqrt(minmax([clvar,olvar]))
      xrange = [1,10.^ceil(alog10(nmodes))]
      if keyword_set(WFRESIDUALS) and test_argos then begin
        wfres = (cur_data->residual_modes())->modes() * norm_fact_wfs*wfs_fact
        if not keyword_set(std) then wfres = rms(wfres,dim=1) else begin
          tmp = fltarr((size(wfres,/dim))[1])
          for j = 0, (size(wfres,/dim))[1]-1 do tmp[j] = stddev(wfres[*,j])
          wfres = tmp
        endelse
        yrange = minmax([yrange, minmax(wfres)])
      endif
      yrange = 10.^[floor(alog10(yrange[0])),ceil(alog10(yrange[1]))]
      if not keyword_set(OVERPLOT) and i eq 0 then begin
        if keyword_set(yr) then yrange = yr
        plot_oo, lindgen(nmodes)+1, lindgen(nmodes)+1, psym=-1, symsize=1.2, charsize=1.5, ytitle='nm rms wf', xtitle='mode number', $
          title=title, yrange=yrange, xrange=xrange, thick=thick, _extra=ex, /nodata
      endif 
      if keyword_set(smooth_) then clvar_plot = [sqrt(clvar[0:1]),smooth(sqrt(clvar[2:*]),smooth_)] else clvar_plot = sqrt(clvar)
      if keyword_set(smooth_) then olvar_plot = [sqrt(olvar[0:1]),smooth(sqrt(olvar[2:*]),smooth_)] else olvar_plot = sqrt(olvar)
      oplot, lindgen(nmodes)+1, clvar_plot, psym=-1, symsize=1.2, COLOR=clcolor, thick=thick
      oplot, lindgen(nmodes)+1, olvar_plot, psym=-2, symsize=1.2, COLOR=olcolor, thick=thick
      if keyword_set(WFRESIDUALS) and test_argos then begin
        if keyword_set(smooth_) then wfres_plot = [wfres[0:1],smooth(wfres[2:*],smooth_)] else wfres_plot = wfres
        print, tns[i]+' WFS residual min, max, TT, tot (no TT): ', strtrim(min(wfres),2)+', '+ strtrim(max(wfres),2)+', '+ $
          strtrim(sqrt(total(wfres[0:1]^2.)),2)+ ', '+strtrim(sqrt(total(wfres[2:*]^2.)),2)
        oplot, lindgen(nmodes)+1, wfres_plot, psym=-3, symsize=0.8, color=wfrcolor, line = 2, thick = thick+2
      endif
      
      if ndata eq 1 then begin
        if keyword_set(test_argos) and keyword_set(wfresiduals) then elab_legend, ['disturbance','closed-loop', 'wfres'], color=[olcolor,clcolor,wfrcolor], $
          psym=-[2,1,3], linestyle = [0,0,2], /top, /right, thick=thick+[0,0,2], clear = clear, back = background $
          else elab_legend, ['disturbance','closed-loop'], color=[olcolor,clcolor], psym=-[2,1], /top, /right, thick=thick, clear = clear, $
          back = background
      endif
    endelse
    
    print, tns[i]+' CL res (TT, tot w/o TT): ', strtrim(sqrt(total(clvar[0:1])),2)+', '+strtrim(sqrt(total(clvar[2:*])),2)
    
    if n_elements(index_valid) eq 0 then index_valid = i else index_valid = [index_valid,i]

  endfor
  
  if ndata gt 1 then begin
    elab_legend, tns[index_valid], color = color[index_valid], linestyle = 0, /top,/right, font = 0.5, linsize = 0.5, clear = clear, $
      back = background
    if keyword_set(wfresiduals) then elab_legend, ['disturbance','closed-loop', 'wfres'], color = !p.color, $
          psym=-[2,1,3], linestyle = [0,0,2], /bottom, /left, thick=thick+[0,0,2], clear = clear, back = background $
          else elab_legend, ['disturbance','closed-loop'], color= !p.color, psym=-[2,1], /bottom, /left, thick=thick, clear = clear, $
          back = background
  endif
  
  if keyword_set(lbt) then device,/decompose

end