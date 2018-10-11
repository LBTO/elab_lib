; set  = tracknum (string) or aodataset
pro pisces_save_dark, setin

if test_type(setin, /string) eq 0 then set = obj_new('aodataset', setin) else set=setin

;from_tn = '20110610_095353'
;to_tn = '20110610_095758'
;set = obj_new('aodataset', from=from_tn, to=to_tn)

setl   = set->where('meas_type', 'eq', 'LOOP') ; only LOOP, no AutoGain, SlopesNull etc
tracknums = setl->tracknums()
ra  = setl->value('tel.ra')
dec = setl->value('tel.dec')
xsta= (setl->value('wfs_status.stages'))[0,*]
ysta= (setl->value('wfs_status.stages'))[1,*]
; TODO check the pointing 
; TODO check that PSFs have been dithered

; check exptime
exptimes = setl->value('pisces.exptime')
if max(exptimes)-min(exptimes) ne 0 then message, 'exptime is not constant in this set'
exptime = mean(exptimes)

; check filter_name
filter_name = setl->value('pisces.filter_name')
dumi = where(filter_name ne filter_name[0], cnt)
if cnt gt 0 then message, 'filter name is not constant in this set'
filter_name = filter_name[0]

; julday
jds = setl->value('obj_tracknum.julday')
jd = mean(jds)
caldat, jd, m,d,y,hh,mm,ss
tracknum = string(format='(%"%04d%02d%02d_%02d%02d%02d")', y,m,d,hh,mm,ss)


; compute dark
imas = setl->value('pisces.rawimage')
dark= fltarr(1024,1024) 
for i=0,1023 do for j=0,1023 do dark[i,j]=median(imas[i,j,*])

; compose dark names
ee = getaoelab(setl->get(pos=0))
dark_subdir = ['wfs_calib_'+(ee->wfs_status())->wunit(),'pisces','backgrounds','bin1']
dark_fname      = filepath(root=ao_datadir(), sub=dark_subdir,  tracknum+'.fits')
dark_cube_fname = filepath(root=ao_datadir(), sub=dark_subdir,  tracknum+'.fits_cube.fits')

; compose header
mkhdr, darkhdr, dark, /extend
sxaddpar, darkhdr, 'EXPTIME', exptime
sxaddpar, darkhdr, 'FILTER', filter_name
sxaddpar, darkhdr, 'DETECTOR', 'PISCES'
for i=0, n_elements(tracknums)-1 do $
    sxaddpar, darkhdr, string(format='(%"TN%d")', i), tracknums[i]

; Write fits files
writefits, dark_fname, dark, darkhdr
print, 'file '+dark_fname+' written'
writefits, dark_cube_fname, dark, darkhdr
print, 'file '+dark_cube_fname+' written'


end


