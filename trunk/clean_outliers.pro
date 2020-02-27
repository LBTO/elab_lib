
function clean_outliers, data, THRESHOLD=THRESHOLD, do3sigmaDiffFiltering=do3sigmaDiffFiltering

    n_frames = n_elements(data[*,0])
    n_acts   = n_elements(data[0,*])

    for i=0,n_acts-1 do begin

        series = data[*,i]

        if keyword_set(do3sigmaDiffFiltering) then begin
            ; additional 3 sigma filtering on differential series
            delta_series = series[1:*,*]-series[0:n_frames-2,*]
            th3sigma = 3*stddev(delta_series)
            idx_t = where(abs(delta_series) gt th3sigma, count)
            if count gt 0 then begin
                delta_series[idx_t] = delta_series[idx_t]/abs(delta_series[idx_t]) * th3sigma
                series = replicate(series[0],length_a)
                series[1:*] += total(delta_series,/cum)
            endif
        endif
        
        med = median(series)
        idx_up = where(series gt med + THRESHOLD, count_up)
        idx_dn = where(series lt med - THRESHOLD, count_dn)
        if count_up gt 0 then series[ idx_up ] = med
        if count_dn gt 0 then series[ idx_dn ] = med

        data[*,i] = series

    endfor

    return, data
end
