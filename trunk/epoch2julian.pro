

; Convert an Unix-epoch timestamp into a julian date

function epoch2julian, timestamp

   timestamp = long(timestamp)
   secPerDay = 24L*60L*60L
   day1 = julday(1,1,1970)
   return, day1 + float(timestamp/secPerDay)

end
