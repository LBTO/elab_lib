;
; Displays an image and saves it on disk
;
; The image will be generated by calling EXECUTE
; on the string passed in the <commands> keyword.
;
; <commands> can be either a string, or a string array.
; In the second case, all the array strings will be
; executed in sucesssin with the EXECUTE function.
;
; If any local variable must be referenced, it must be
; passed into the <vars> string array.

pro display_and_save, XSIZE=XSIZE, YSIZE=YSIZE, FILENAME=FILENAME, COMMANDS=COMMANDS, $
                      VARS=VARS, WNUM=WNUM, TITLE=TITLE, NOGRAPHICS=NOGRAPHICS, PNG=PNG, BMP=BMP, $
                      USEWINDOW=USEWINDOW

    !p.multi=0
    if not keyword_set(TITLE) then TITLE=''
    if (not keyword_set(BMP)) and (not keyword_set(PNG)) then PNG=1

    for i=0,n_elements(VARS)-1 do begin
      cmd = VARS[i]+' = SCOPE_VARFETCH("'+VARS[i]+'", LEVEL=-1)'
      void = execute(cmd)
    endfor

    set_plot,'z'
    device, SET_RESOLUTION=[XSIZE,YSIZE], SET_PIXEL_DEPTH=24
    erase
    for i=0,n_elements(COMMANDS)-1 do void = execute(COMMANDS[i])
    image = tvrd(/true)
    if keyword_set(PNG) then write_png, FILENAME, image
    if keyword_set(BMP) then write_bmp, FILENAME, image

    if not keyword_set(NOGRAPHICS) then begin
        set_plot, 'X'
        if keyword_set(USEWINDOW) then begin
            wset, USEWINDOW
        endif else begin
            window, WNUM, retain=2, XSIZE=XSIZE, YSIZE=YSIZE, TITLE=TITLE
        endelse
        tvscl,image, /true
    endif

end

