; +
; to use elab_legend on IDL > 7.1 al_legend from astrolib is required
; -
pro elab_legend, items, _EXTRA=extra

  ; search al_legend
  if float(!version.release) gt float('7.1.0') then use_al_legend = 1B else use_al_legend = 0  

  if use_al_legend then $
    al_legend, items, _EXTRA=extra $
      else legend, items, _EXTRA=extra
end
