pro SR_conversion, sr_in,l_in,l_out, sr_out

sr_out = exp((l_in/l_out)^2*(alog(sr_in)))
print, 'SR @',l_out,' = ', sr_out
end

pro sr_conversion_plot, l_in, l_out

sr_in = findgen(100)/100.
sr_out = sr_in

for i= 0,99 do begin
sr_i = sr_in[i]
SR_conversion, sr_i,l_in,l_out, sr_o
sr_out[i] = sr_o
endfor
plot, sr_in, sr_out, xtitle='SR @'+strtrim(l_in,2), ytitle='SR @'+strtrim(l_out,2)

end