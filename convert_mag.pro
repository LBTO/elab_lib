;+
;
; This code returns SOUL's WFS magnitude as a function of an input magnitude and the stellar spectral type or
; the magnitudes of 2 different bands. The conversion LUT has been done for SX only.
;
; INPUTS:
;   mag1           Magnitude in the band defined by band1.
;   mag2           Magnitude in the band defined by band2 (optional, not taken into account if type is defined).
;
; OUTPUT:
;                  WFS magnitude. If there was an error, the output is +Inf.
;
; KEYWORDS:
;   band1           Spectral band for mag1.
;   band2           Spectral band for mag2. Not taken into account if type is defined.
;   type            Spectral type of the AO guide star. Has the priority over the second magnitude.
;   filename        Path of the file that contains the data for the conversion. Default: path to my directory.
;
; HISTORY:
;   Written by:     C. Plantet (cedric.plantet@inaf.it) September, 2020
;
;-

function convert_mag, mag1, mag2, band1 = band1, band2 = band2, type = type, filename = filename

  if n_elements(mag1) eq 0 then message, 'You must define at least one magnitude'
  if n_elements(band1) eq 0 then message, 'You must define the band for the magnitude'
  if (n_elements(band2) eq 0 or n_elements(mag2) eq 0) and n_elements(type) eq 0 then message, $
    'You must define either the stellar type or the magnitude in a second spectral band'

  if n_elements(filename) eq 0 then filename = '/data6/plantet/Data/SOUL/conv_mag_SRcalc_SOUL.sav'
  if file_test(filename) then restore, filename else $
    message, 'You must define the name of the file containing the data for the conversion'

  idx_band1 = where(band1 eq bands)
  if total(idx_band1) eq -1 then begin
    message, 'This spectral band is not available. Available bands:',/info
    print,bands
    return, 1e99
  endif

  ;If the spectral type is not defined, we try to get it from the color between the 2 bands.
  if n_elements(mag2) ne 0 and n_elements(type) eq 0 then begin
    idx_band2 = where(band2 eq bands)
    if total(idx_band2) eq -1 then begin
      message, 'The second spectral band is not available. Available bands:',/info
      print,bands
      return, 1e99
    endif
    idx_color = closest(mag1-mag2, color)
    if total(idx_color) eq -1 then begin
      message, 'The color is too high, max color = 5',/info
      return, 1e99
    endif
    type = col2type[idx_band1,idx_band2,idx_color] ;LUT for the type from the color
    idx_type = where(type[0] eq types) ;type[0] to avoid problems linked to 1-element arrays
    if total(idx_type) eq -1 then begin ;Explore possible colors if the type was not found
      message, 'The color could not be matched to a spectral type, taking the closest available',/info
      ncolors = n_elements(color)
      for i = 0, ncolors-1 do begin
        if col2type[idx_band1,idx_band2,i] ne '' then begin
          cur_diff = idx_color-i
          if n_elements(min_diff) eq 0 then min_diff = cur_diff
          if abs(cur_diff) le abs(min_diff) then begin ;We take the type that has the closest color
            type = col2type[idx_band1,idx_band2,i]
            min_diff = cur_diff
          endif
        endif
      endfor
    endif
    if n_elements(min_diff) eq 0 then min_diff = 0
    message, 'Chosen color: '+string(color[idx_color-min_diff],format='(F0.2)'),/info
    message, 'Resulting type: '+type,/info
    idx_type = where(type[0] eq types)
  endif

  if n_elements(idx_type) eq 0 then begin
    idx_type = where(type[0] eq types)
    if total(idx_type) eq -1 then begin
      message, 'This spectral type is not available. Available types:',/info
      print,types
      return, 1e99
    endif
  endif

  return, mag1 + delta_mag[idx_type,idx_band1]

end