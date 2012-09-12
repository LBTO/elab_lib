
pro progress, num_item, infostr
common progress, progress_start, progress_numitems

now = systime(/sec)
if now-progress_start gt 2 then begin
    print, format='($, %"%s, %d%% done\r")',infostr, max([fix(num_item*100.0/progress_numitems-1),0])
endif
if num_item eq progress_numitems-1 then print
end


