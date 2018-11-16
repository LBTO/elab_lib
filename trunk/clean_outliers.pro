
function clean_outliers, data, THRESHOLD=THRESHOLD

    n_frames = n_elements(data[*,0])
    n_acts   = n_elements(data[0,*])

    for i=0,n_acts-1 do begin

        series = data[*,i]
        med = median(series)
        idx_up = where(series gt med + THRESHOLD, count_up)
        idx_dn = where(series lt med - THRESHOLD, count_dn)
        if count_up gt 0 then series[ idx_up ] = med
        if count_dn gt 0 then series[ idx_dn ] = med
        data[*,i] = series

    endfor

    return, data
end
