pro pisces_slopes_null
    set = obj_new('aodataset', ['20110608_172127', '20110606_205259', '20110607_194008', '20110606_190913', '20110606_181530', '20110606_172553', '20110607_223736'])
    rerotang =  set->value('wfs_status.rerotator')
    ncpa = set->value('modes_null.surface')
    npoints=set->count()
    for i=0,npoints-1 do begin image_show, /as,/sh, ncpa[*,*,i] & wait,1 & endfor
end
