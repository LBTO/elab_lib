pro log_twiki, aodataset, ref_star=ref_star
    if not keyword_set(ref_star) then ref_star='???'

    objref =  aodataset->Get(/all)

    print, "| *TrackNo* | *RefStar* | *Mag* | *El* | *Wind* | *DIMM* | *Rec* | *bin* | *#mod* | *freq* | *gain* | *mod* | *nph* | *SR* | *band* | *exp* | *#frames* | *disturb* | *notes* |"

    for i=0, aodataset->Count()-1 do begin
        ee = getaoelab(objref[i])
        if obj_valid(ee) eq 0 then begin
            message, objref[i] + ' skipped because it lacks required data', /info
            continue
        endif 
        
        gaintemp = [-1, -1]
        if obj_valid(ee->modal_rec()) then $
            if obj_valid(ee->control()) then begin
                gaintemp = minmax( ((ee->control())->gain())[(ee->modal_rec())->modes_idx()] )
                if gaintemp[0] eq -1 then gaintemp=[-1, -1]
            endif
        if obj_valid(ee->irtc()) then begin
            case round( (ee->irtc())->lambda()*1e9) of
                1070: band = 'J'
                1600: band = 'H'
                else: band = '?'
            endcase
        endif else begin
            band = '?'
        endelse
        if ee->operation_mode() eq 'RR' then begin
        	if obj_valid(ee->disturb()) then disturb='ON' else disturb='OFF'
        endif else disturb='ONSKY'

        print, string(format='(%"| %s | %s | %4.1f | %d | %d | %5.2f | %s | %d | %d | %d | %4.1f %4.1f | %d | %d | %6.1f | %s | %d | %d | %s | %s |")', $
            ee->tracknum(), $
            ref_star, $
            ee->mag(), $
            obj_valid(ee->tel()) ? round( (ee->tel())->el()/3600.) : -1 , $
            obj_valid(ee->tel()) ? round( (ee->tel())->extern_wind_speed() ) : -1 , $
            obj_valid(ee->tel()) ?  (ee->tel())->dimm_seeing() : -1 , $
            obj_valid(ee->modal_rec()) ? strmid(file_basename((ee->modal_rec())->fname()), 13, 6 ) : ' ', $
            obj_valid(ee->wfs_status()) ? ((ee->wfs_status())->ccd39())->binning() : -1, $
            obj_valid(ee->modal_rec()) ? round((ee->modal_rec())->nmodes()) : -1, $
            obj_valid(ee->wfs_status()) ?  round(((ee->wfs_status())->ccd39())->framerate()) : -1, $
            gaintemp[0], gaintemp[1] ,$
            obj_valid(ee->wfs_status()) ? round( (ee->wfs_status())->modulation() ) : -1, $
            obj_valid(ee->frames()) ? round((ee->frames())->nphsub_per_int_av()) : -1, $
            obj_valid(ee->irtc()) ?  (ee->irtc())->sr_se()*100 : -1, $
            band , $
            obj_valid(ee->irtc()) ? round( (ee->irtc())->exptime()*1e3) : -1 , $
    		obj_valid(ee->irtc()) ? (ee->irtc())->nframes() : -1 , $
			disturb,  $
            ee->isOK(cause=cause) eq 1L ? "" :  cause $ 
        )
    endfor

end
