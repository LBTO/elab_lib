pro log_twiki, aodataset, ref_star=ref_star
    if not keyword_set(ref_star) then ref_star='???'

    objref =  aodataset->Get(/all)
    
    print, "| *TrackNo* | *RefStar* | *Mag* | *El* | *Wind* | *Rec* | *bin* | *#mod* | *freq* | *gain* | *mod* | *nph* | *SR* | *band* | *exp* | *#frames* | "

    for i=0, aodataset->Count()-1 do begin
        ee = objref[i]
        gaintemp = minmax( ((ee->control())->gain())[(ee->modal_rec())->modes_idx()] )
        if gaintemp[0] eq -1 then gaintemp=[-1, -1] 
        case round( (ee->irtc())->lambda()*1e9) of
            1070: band = 'J'
            1600: band = 'H'
            else: band = '?'
        endcase
        print, string(format='(%"| %s | %s | %4.1f | %d | %d | %s | %d | %d | %d | %4.1f %4.1f | %d | %d | %d | %s | %d | %d | ")', $
            ee->tracknum(), $
            ref_star, $
            ee->mag(), $
            round( (ee->tel())->el()/3600.), $
            round( (ee->tel())->wind_speed() ),$
            strmid(file_basename((ee->modal_rec())->fname()), 13, 6 ) , $
            ((ee->wfs_status())->ccd39())->binning(), $
            round((ee->modal_rec())->nmodes()), $
            round(((ee->wfs_status())->ccd39())->framerate()), $
            gaintemp[0], gaintemp[1] ,$
            (ee->wfs_status())->modulation(), $
            round((ee->frames())->nphsub_per_int_av()), $
            round( (ee->irtc())->sr_se()*100), $
            band , $
            round( (ee->irtc())->exptime()*1e3) , $
    		(ee->irtc())->nframes() $

        )
    endfor

end
