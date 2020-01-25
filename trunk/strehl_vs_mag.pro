
pro strehl_vs_mag, set, from=from, to=to, rec = rec, tab_res = tab_res_out, tns = tns, noplot = noplot, $
  lambda = lambda_, onsky = onsky, calib = calib, max_dark = max_dark, min_exp = min_exp, dimm = dimm, $
  vs_seeing = vs_seeing, filename = filename

  if size(set,/type) eq 8 then begin
    if total(tag_names(set) eq 'SEEING_OL') eq 0 then testol=0 else testol=1
    test = set.sr gt 0 and set.seeing gt 0 and set.mag ne -1 and strmid(set.filter,0,1) ne '*'
    if keyword_set(onsky) then test = test and set.mode eq 'ONSKY'
    if keyword_set(calib) then test = test and set.mode ne 'ONSKY'
    if keyword_set(max_dark) then test = test and abs(set.dark_time) le max_dark
    if keyword_set(min_exp) then test = test and abs(set.exp_time) ge min_exp
    if keyword_set(dimm) then test = test and set.dimm eq 1
    index = where(test)
    if total(index) eq -1 then begin
      print, 'No valid TN in this set (no SR or no seeing or no mag)
      return
    endif
    nfiles = n_elements(index)
    tns = set.tn[index]
    tab_res = [[set.sr[index]],[set.seeing[index]],[set.mag[index]],[set.bin[index]], [index]]
    for i = 0,nfiles-1 do begin
      case set.filter[index[i]] of 
        'clear J': lambda0 = 1250.
        'P_beta clear': lambda0 = 1280.
        'FeII clear': lambda0 = 1646.
        'clear H': lambda0 = 1646.
        'H2 clear': lambda0 = 2124.
        else: begin
          print, 'Unknown filter for '+tns[i]+' (#'+strtrim(fix(i),2)+')'
          if not keyword_set(lambda_) then begin
            print, 'Set lambda to avoid terminating the procedure here'
            return
          endif else lambda0 = lambda_
        end  
      endcase
      if not keyword_set(lambda_) then lambda = lambda0 else lambda = lambda_
      tab_res[i,0] = tab_res[i,0]^((lambda0/lambda)^2.)
      if set.mode[index[i]] eq 'ARGOScalib' then begin
        if not keyword_set(testol) then tab_res[i,1] /= 2. else begin
          if not keyword_set(set.seeing_ol[index[i]]) then tab_res[i,1] /= 2.
        endelse
      endif
    endfor
    
    index1 = where(tab_res[*,3] eq 1)
    index2 = where(tab_res[*,3] eq 2)
    index3 = where(tab_res[*,3] eq 3)
    index4 = where(tab_res[*,3] eq 4)
    
    if keyword_set(vs_seeing) then begin
      xdata = tab_res[*,1]
      vcolor = bytscl(tab_res[*,2])
      rcolor = [min(tab_res[*,2]),max(tab_res[*,2])]
      xtit = 'Seeing (arcsec)'
      ctit = 'R mag.'
    endif else begin
      xdata = tab_res[*,2]
      vcolor = bytscl(tab_res[*,1])<1.5
      rcolor = [min(tab_res[*,1]),1.5]; max(tab_res[*,1])]
      xtit = 'R mag.'
      ctit = 'Seeing (arcsec)'
    endelse
    xr = [floor(min(xdata))-1,ceil(max(xdata))+1]
    if keyword_set(vs_seeing) then xr[0] = xr[0] > 0
    if not keyword_set(noplot) then begin
      p = plot(xdata,tab_res[*,0],'o',/sym_filled, rgb_table = 34, vert_color = vcolor, $
        xtit = xtit, ytit = 'SR ('+strtrim(fix(lambda),2)+' nm)',axis_style = 2, sym_size = 1, margin = 0.2, font_size = 15, $
        yr = [0,1], xr = xr, dimensions = [1000,700],/nodata)
      if total(index1) ne -1 then begin
        p1 = plot(xdata[index1],tab_res[index1,0],'+',/sym_filled, rgb_table = 34, $
          vert_color = vcolor[index1], sym_size = 1, /over, name = 'BIN 1', sym_thick = 2)
      endif
      if total(index2) ne -1 then begin
        p2 = plot(xdata[index2],tab_res[index2,0],'s', rgb_table = 34, $
          vert_color = vcolor[index2], sym_size = 1, /over, name = 'BIN 2', sym_thick = 2)
      endif
      if total(index3) ne -1 then begin
        p3 = plot(xdata[index3],tab_res[index3,0],'tu',/sym_filled, rgb_table = 34, $
          vert_color = vcolor[index3], sym_size = 1, /over, name = 'BIN 3', sym_thick = 2)
      endif
      if total(index4) ne -1 then begin
        p4 = plot(xdata[index4],tab_res[index4,0],'X',/sym_filled, rgb_table = 34, $
          vert_color = vcolor[index4], sym_size = 1, /over, name = 'BIN 4', sym_thick = 2)
      endif
      c = colorbar(range = rcolor, orientation = 1, $
        rgb_table = 34,title = ctit, font_size = 15,textpos = 1, tickdir = 1, position = [.85,.2,.9,.8],/norm)
      target = []
      if obj_valid(p1) then target = [target,p1]
      if obj_valid(p2) then target = [target,p2]
      if obj_valid(p3) then target = [target,p3]
      if obj_valid(p4) then target = [target,p4]
      if n_elements(target) gt 0 then l = legend(target = target,position=[.9,.9],/relative,color=0,sample_width=0)
    endif
    
    tab_res_out = tab_res
    
  endif else begin
    if keyword_set(from) and keyword_set(to) then begin
      set = obj_new('aodataset',from=from,to=to,rec=rec)
      if keyword_set(filter) then set = set->where('luci.filter_name()','eq',filter)
      set = set->where('luci.sr_se()','between',[0,1])
      tns = set->tracknums()
    endif else tns = set

    nfiles = n_elements(tns)

    if tns[0] ne '' then begin

      tab_res = fltarr(nfiles,3)-1 ;Strehl, seeing, R magnitude

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
        endif else continue
        case filter of
          'clear J': lambda0 = 1250.
          'P_beta clear': lambda0 = 1280.
          'FeII clear': lambda0 = 1646.
          'clear H': lambda0 = 1646.
          'H2 clear': lambda0 = 2124.
          else: begin
            print, 'Unknown filter for '+tns[i]+' (#'+strtrim(fix(i),2)+')'
            if not keyword_set(lambda_) then begin
              print, 'Set lambda to avoid terminating the procedure here'
              return
            endif else lambda0 = lambda_
          end
        endcase
        if not keyword_set(lambda_) then lambda = lambda0 else lambda = lambda_
        
        sr_tmp = (cur_ee->luci())->sr_se()
        if sr_tmp gt 1 or sr_tmp lt 0 then continue
        tab_res[i,0] = sr_tmp^((lambda0/lambda)^2.)
        tab_res[i,1] = (cur_ee->olmodes())->seeing()
        if (cur_ee->adsec_status())->disturb_status() ne 0 then begin
          if keyword_set(cur_ee->disturb()) then begin
            if (cur_ee->disturb())->type() eq 'atm' then begin
              tab_res[i,1] = (cur_ee->disturb())->seeing()
              if not keyword_set((ee->tel())->isTracking()) then tab_res[i,1] /= 2.
            endif
          endif
        endif
        if finite((cur_ee->tel())->dimm_seeing()) then tab_res[i,1] = (cur_ee->tel())->dimm_seeing() else begin
          if keyword_set(dimm) then continue
        endelse  
        if tab_res[i,1] le 0 then begin
          tab_res[i,1]=-1
          continue
        endif
        if finite(cur_ee->mag()) then tab_res[i,2] = cur_ee->mag() else continue

        cur_bin = ((cur_ee->wfs_status())->camera())->binning()

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
      vcolor = bytscl(tab_res_out[*,2])
      rcolor = [min(tab_res_out[*,2]),max(tab_res_out[*,2])]
      xtit = 'Seeing (arcsec)'
      ctit = 'R mag.'
    endif else begin
      xdata = tab_res_out[*,2]
      vcolor = bytscl(tab_res_out[*,1]<1.4)
      rcolor = [min(tab_res_out[*,1]),1.4];,max(tab_res_out[*,1])]
      xtit = 'R mag.'
      ctit = 'Seeing (arcsec)'
    endelse
    xr = [floor(min(xdata))-1,ceil(max(xdata))+1]
    
    
    ;simulations Error budget
    mags                = [7.50000,      8.50000,      9.50000,      10.5000,      11.5000,      12.5000,      13.5000,      14.5000,      15.5000,      16.5000,      17.5000,      18.5000]
    srs10bandHv_eb      = [82.1000,      81.3000,      79.7000,      75.7000,      67.3000,      53.1000,      35.4000,      16.4000,      5.80000,      1.50000,     0.100000,      0.00000]/100.
    srs10bandH_eb       = [87.0000,      86.6000,      85.7000,      83.7000,      78.9000,      71.7000,      58.9000,      42.1000,      21.2000,      7.70000,      2.10000,     0.100000]/100.
    idxs10              =  where(abs(tab_res_out[*,1]-1) LT 0.05)

;   FLAO simulation Error Budget from ErrorBudgetFinal.xls
    FLAOsrs08bandH_eb   = [88.5,         87.8,         84.7,         80.2,         69.2,         60.9,         51.8,         30.5,         14.8,         4.6,          0.3,         0.0     ]/100.
    FLAOsrs08bandHv_eb  = [75.9,         73.7,         67.2,         52.7,         30.8,         22.8,         13.6,         6.6,          1.6,          0.0,          0.0,         0.0     ]/100.

     ; scaling from 0.8 to 1.0" of seeing
      FLAOvars08bandH_eb   = -alog(FLAOsrs08bandH_eb) 
      FLAOvars10bandH_eb   = FLAOvars08bandH_eb * (1.0/0.8)^(5./3.)
      FLAOsrs10bandH_eb    = exp(-1.*FLAOvars10bandH_eb)
      FLAOvars08bandHv_eb  = -alog(FLAOsrs08bandHv_eb)
      FLAOvars10bandHv_eb  = FLAOvars08bandHv_eb * (1.0/0.8)^(5./3.)
      FLAOsrs10bandHv_eb    = exp(-1.*FLAOvars10bandHv_eb)
      

    
   
   
    if keyword_set(vs_seeing) then xr[0] = xr[0] > 0
    
    xr = [0.5,3.]
    
    if not keyword_set(noplot) then begin
      p = plot(xdata,tab_res_out[*,0],'o',/sym_filled, rgb_table = 34, vert_color = vcolor,$
        xtit = xtit, ytit = 'SR ('+strtrim(fix(lambda),2)+' nm)',axis_style = 2, sym_size = 1, margin = 0.2, font_size = 15, $
        yr = [0,1], dimensions = [1000,700], xr = xr, /nodata, title='SOUL-LUCI1 on-sky')  ;
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
;      ; SOUL sim seeing 1.0"
;      psim10 = plot(mags,srs10bandH_eb,'o',line=1, rgb_table = 34, $
;        vert_color = replicate(vcolor(idxs10[0]),n_elements(mags)), sym_size = 1, /over, name = 'SOUL-Sim (No Vib)', sym_thick = 2)
;      psim10V = plot(mags,srs10bandHv_eb,'o',line=1,/sym_filled, rgb_table = 34, $
;        vert_color = replicate(vcolor(idxs10[0]),n_elements(mags)), sym_size = 1, /over, name = 'SOUL-Sim (Vib)', sym_thick = 2)
;     ; FLAO simul seeing 1.0"   
;        psim10F = plot(mags,FLAOsrs10bandH_eb,'D',line=1, rgb_table = 34, $
;        vert_color = replicate(vcolor(idxs10[0]),n_elements(mags)), sym_size = 1, /over, name = 'FLAO-Sim (No Vib)', sym_thick = 2)
;      psim10VF = plot(mags,FLAOsrs10bandHv_eb,'D',line=1,/sym_filled, rgb_table = 34, $
;        vert_color = replicate(vcolor(idxs10[0]),n_elements(mags)), sym_size = 1, /over, name = 'FLAO-Sim (Vib)', sym_thick = 2)
      c = colorbar(range = rcolor, orientation = 1, $
        rgb_table = 34,title = ctit, font_size = 15,textpos = 1, tickdir = 1, position = [.85,.2,.9,.8],/norm)
        target = []
        if obj_valid(p1) then target = [target,p1]
        if obj_valid(p2) then target = [target,p2]
        if obj_valid(p3) then target = [target,p3]
        if obj_valid(p4) then target = [target,p4]
;        target = [target, psim10]
;        target = [target, psim10V]
;        target = [target, psim10F]
;        target = [target, psim10VF]
        if n_elements(target) gt 1 then l = legend(target = target,position=[.9,.9],/relative,color=0,sample_width=0)
      endif
      
    endif else begin
      print,'No TN in this time period'
      return
    endelse
  endelse
  
  if keyword_set(filename) and obj_valid(p) then p.save, filename+'.png'

end

TNs_FeII = ['20181101_024428', '20181101_024723', '20181101_024920', '20181101_025019', '20181101_025641', '20181101_025722', $
  '20181101_025803', '20181101_030044', '20181101_030125', '20181101_030205', '20181101_045157', '20181101_045401', $
  '20181101_050716', '20181101_053622', '20181101_053855', '20181101_053936', '20181101_061813', '20181101_062032', $
  '20181101_062334', '20181101_065509', '20181101_065923', '20181101_070428', '20181101_071555', '20181102_013702', $
  '20181102_013801', '20181102_013900', '20181102_051156', '20181102_051237', '20181102_051425', '20181102_051556', $
  '20181102_051721', '20181102_051804', '20181102_052655', '20181102_052744', '20181103_020356', '20181103_020513', $
  '20181103_020626', '20181103_021248', '20181103_021359', '20181103_021504', '20181103_035306', '20181103_035542', $
  '20181103_041043', '20181103_041227', '20181103_041408', '20181103_041502', '20181103_041543', '20181103_041624', $
  '20181103_045347', '20181103_045500', '20181103_045623', '20181103_050927', '20181103_051040', '20181103_051204', $
  '20181103_051806', '20181103_051917', '20181103_052822', '20181103_052928', '20181103_053027', '20181103_063731', $
  '20181103_063851', '20181103_064009', '20181103_064333', '20181103_064454', '20181103_064610']

TNs_H2clear = ['20181101_033919', '20181101_034034', '20181101_034116', '20181101_034151', '20181101_034233', $
  '20181101_034429', '20181101_034532', '20181101_040121', '20181101_040213', '20181101_040318', '20181101_040402', $
  '20181101_040453', '20181101_040545', '20181101_040644', '20181101_043633', '20181101_043733', '20181101_043837', $
  '20181101_044609', '20181101_044832']
  
TNs_clearH = ['20181102_062251', '20181102_062345', '20181102_072601', '20181102_072649', '20181102_072813', $
  '20181102_072906', '20181102_073000', '20181102_073103']
  
TNs_H = [TNs_FeII,TNs_clearH]


mags_FeII = [fltarr(10)+9.5,fltarr(6)+12.5,fltarr(3)+12.1,fltarr(4)+14.4,fltarr(3)+8,fltarr(8)+9.2,fltarr(6)+12.3, $
  fltarr(8)+9.2,fltarr(11)+12.6,fltarr(6)+13.1]
seeing_FeII = [1.69,1.31,1.95,1.51,1.52,1.30,1.23,1.47,1.40,1.42,1.71,1.42,1.66,1.35,1.55,1.60,1.04,0.95,0.94,1.05, $
  1.72,1.29,1.09,0.95,0.96,0.98,1.22,1.24,1.07,1.01,1.09,1.02,1.10,1.15,0.82,0.93,1.00,0.89,0.86,0.81,0.85,0.72,0.75, $
  0.78,0.75,0.83,0.86,0.84,0.97,0.95,0.88,0.93,0.93,1.07,0.89,0.78,0.84,0.77,0.82,0.85,0.87,0.90,0.81,0.84,0.88]
sr_FeII = [0.378,0.668,0.566,0.707,0.766,0.755,0.636,0.707,0.532,0.710,0.465,0.357,0.115,0.115,0.115,0.146,0.235,$
  0.254,0.273,0.036,0.020,0.059,0.038,0.467,0.466,0.460,0.509,0.576,0.521,0.598,0.596,0.575,0.518,0.523,0.572,0.535,$
  0.523,0.569,0.593,0.577,0.175,0.188,0.647,0.617,0.622,0.601,0.594,0.648,0.499,0.475,0.472,0.479,0.461,0.467,0.508,$
  0.491,0.210,0.219,0.200,0.146,0.137,0.158,0.177,0.189,0.180]


mags_H2clear = [fltarr(14)+9.4,fltarr(5)+12.4]
seeing_H2clear = [0.99,1.35,1.64,1.81,1.54,1.48,1.30,0.74,0.70,0.81,0.83,0.84,0.79,0.86,1.07,1.04,1.06,1.47,1.31]
sr_H2clear = [0.876,0.684,0.613,0.853,0.662,0.182,0.181,0.869,0.873,0.861,0.861,0.171,0.170,0.174,0.716,0.698,0.721,0.513,0.651]

mags_clearH = fltarr(8)+12.5
seeing_clearH = [1.21,1.16,1.02,0.94,1.00,0.94,0.89,0.90]
sr_clearH = [0.118,0.095,0.416,0.410,0.416,0.427,0.401,0.408]

mags_H = [mags_feII,mags_clearH]
seeing_H = [seeing_feII, seeing_clearH]
sr_H = [sr_feII, sr_clearH]

tns = tns_H
sr = sr_H
seeing = seeing_H
mags = mags_H

tab_res = fltarr(n_elements(tns),3)
tab_res[*,0] = sr
tab_res[*,1] = seeing
tab_res[*,2] = mags

;FeII
;index1 = [indgen(16),indgen(36)+23]
;index4 = [indgen(7)+16,indgen(6)+59]

;H2 clear
;index1 = indgen(19)
;index4 = []

;clear H
;index1 = indgen(6)+2
;index4 = [0,1]

;FeII + clear H
index1 = [indgen(16),indgen(36)+23,indgen(6)+67]
index4 = [indgen(7)+16,indgen(6)+59,65,66]

;simulations Error budget
mags            = [7.50000,      8.50000,      9.50000,      10.5000,      11.5000,      12.5000,      13.5000,      14.5000,      15.5000,      16.5000,      17.5000,      18.5000]
srs10bandHv_eb  = [82.1000,      81.3000,      79.7000,      75.7000,      67.3000,      53.1000,      35.4000,      16.4000,      5.80000,      1.50000,     0.100000,      0.00000]




p = plot(tab_res[*,2],tab_res[*,0],'o',/sym_filled, rgb_table = 34, vert_color = bytscl(tab_res[*,1]), $
  xtit = 'R mag.', ytit = 'SR',axis_style = 2, sym_size = 1, margin = 0.2, font_size = 15, $
  yr = [0,1], xr = [floor(min(tab_res[*,2]))-1,ceil(max(tab_res[*,2]))+1], dimensions = [1000,700],/nodata)
if n_elements(index1) ne 0 then begin
  p1 = plot(tab_res[index1,2],tab_res[index1,0],'o',/sym_filled, rgb_table = 34, $
    vert_color = (bytscl(tab_res[*,1]))[index1], sym_size = 1, /over, thick = 2, name = 'BIN 1')
endif
if n_elements(index2) ne 0 then begin
  p2 = plot(tab_res[index2,2],tab_res[index2,0],'*', rgb_table = 34, $
    vert_color = (bytscl(tab_res[*,1]))[index2], sym_size = 1, /over, thick = 2, name = 'BIN 2')
endif
if n_elements(index3) ne 0 then begin
  p3 = plot(tab_res[index3,2],tab_res[index3,0],'tu',/sym_filled, rgb_table = 34, $
    vert_color = (bytscl(tab_res[*,1]))[index3], sym_size = 1, /over, thick = 2, name = 'BIN 3')
endif
if n_elements(index4) ne 0 then begin
  p4 = plot(tab_res[index4,2],tab_res[index4,0],'D',/sym_filled, rgb_table = 34, $
    vert_color = (bytscl(tab_res[*,1]))[index4], sym_size = 1, /over, thick = 2, name = 'BIN 4')
endif
 
c = colorbar(range = [min(tab_res[*,1]),max(tab_res[*,1])], orientation = 1, $
  rgb_table = 34,title = 'Seeing (arcsec)', font_size = 15,textpos = 1, tickdir = 1, position = [.85,.2,.9,.8],/norm)
target = []
if obj_valid(p1) then target = [target,p1]
if obj_valid(p2) then target = [target,p2]
if obj_valid(p3) then target = [target,p3]
if obj_valid(p4) then target = [target,p4]
if n_elements(target) gt 1 then l = legend(target = target,position=[.9,.9],/relative,color=0,sample_width=0)

end