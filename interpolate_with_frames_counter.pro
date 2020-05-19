
;+
;
;-

function interpolate_with_frames_counter, data, fc_obj, nocrop=nocrop
    ; TODO perform some check on data
    ss = size(data)
    niter   = ss[1]
    nseries = ss[2]

    ; interpolate missing elements if
    ; fc_obj is valid and there are missed frames and the data timesteps have the same size of the fc_obj 
    if obj_valid(fc_obj) then if obj_isa(fc_obj, 'AOframes_counter') then if fc_obj->n_jumps() gt 0 then if niter eq fc_obj->nframes() then begin
        lost_frames_idx = fc_obj->lost_frames_idx()
        lost_frames     = fc_obj->lost_frames()
        niterfull =  niter + total(lost_frames)  
        datafull = fltarr(niterfull, nseries) 
        fcn = lindgen( niterfull ) 
        xx  = lindgen( niter)
        for i=0L, fc_obj->n_jumps()-1 do begin
            xx[lost_frames_idx[i]+1:*] += lost_frames[i]
        endfor
        for j=0L, nseries-1 do begin
            sss = data[*,j]
            if total(minmax(sss)-[0,0]) eq 0 then continue 
            datafull[*,j] = interpol(sss, xx, fcn) 
        endfor
        if n_elements(nocrop) eq 0 then datafull=datafull[0:niter-1,*]
        return, datafull
    endif 
    return, data
end
